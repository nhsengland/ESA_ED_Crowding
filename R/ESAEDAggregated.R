#' Class which acts as a container for ECDS aggregated dataset.
#' Takes in an ESAEDPatientLevel objects, list of ESASlots, an ESAAcuity Obj
#' and a queue level
#' Flags are derived for each of the variables per slot, over the acuity and
#' queue level
#'
ESAEDAggregated <- R6Class(
  classname='ESAEDAggregated',
  public=list(
    data=NULL,
    initialize=function(esaPatientLevel,
                        acuity,
                        slots,
                        provider.col,
                        providerSite.col,
                        queue=NULL,
                        cubicles=NULL,
                        cubicles.site=NULL,
                        cubicles.date=NULL,
                        cubicles.col=NULL,
                        otherFlags=NULL){
      # data must be data.table
      if (!(is(esaPatientLevel,'ESAEDPatientLevel') | is(esaPatientLevel,'data.table'))){
        stop('esaPatientLevel must be an ESAEDPatientLevel object.')
      }
      # acuity must be ESAcuity object
      if (!is(acuity, 'ESAAcuity')){
        stop('Acuity must be ESAAcuity object.')
      }
      # slots must be list of ESASlot objects
      if (!Reduce('&', lapply(slots, is, 'ESASlot'))){
        stop('Slots must be list of ESASlot objects')
      }
      # queue object must be ESAQueue if not null
      if (!is.null(queue) & !is(queue, 'ESAQueue')){
        stop('Queue must be ESAQueue object if not null.')
      }
      # if acuity is cubicles, must provide cubicle data to link with
      if (acuity$name == 'cubicles'&is(esaPatientLevel,'ESAEDPatientLevel')){
        if (is.null(cubicles) | is.null(cubicles.site) | is.null(cubicles.col)){
          stop('If running cubicle model, must submit cubicle data and at
               least a site column to match on, as well as the column which contains
               the number of cubicles. User can also provide date column
               to match on.')
        }
      }
      # create the identifier - acuity_queue if not null
      private$identifier <- paste0(acuity$name, ifelse(!is.null(queue), paste0('_', queue$name), ''))
      # set private attributes - these will have corresponding getter methods
      # to allow user to see what instance was initialised with [but not modify]
      private$esaPatientLevel <- esaPatientLevel
      private$acuity <- acuity
      private$queue <- queue
      private$slots <- slots
      private$provider <- provider.col
      private$providerSite <- providerSite.col

      if (is(esaPatientLevel, 'ESAEDPatientLevel')){
        # perform the aggregation and assign output to public attribute data
        self$data <- private$doAggregation(provider.col=provider.col,
                                           providerSite.col=providerSite.col,
                                           slots=slots,
                                           queue=queue,
                                           cubicles=cubicles,
                                           cubicles.site=cubicles.site,
                                           cubicles.date=cubicles.date,
                                           cubicles.col=cubicles.col,
                                           otherFlags=otherFlags)
      } else if (is(esaPatientLevel,'data.table')) {
        self$data <- esaPatientLevel
      } else {
        stop('Could not initialize ESAEDAggregated object.')
      }

    },
    mergeAdmitted=function(esaAdmittedAggregated){
      if (!is(esaAdmittedAggregated, 'ESAAdmittedAggregated')){
        stop('Must be ESAAdmittedAggregated object')
      }
      self$data <- merge.data.table(x=self$data,
                                    y=esaAdmittedAggregated$getData(),
                                    by.x=c(private$providerSite, 'final_date'),
                                    by.y=c(esaAdmittedAggregated$getProviderSite(),'final_date'),
                                    all.x=TRUE)
    },
    #' @description Merge a list of ESAExternalData objects
    #' @param objects a list of ESAExternalData objects
    mergeExternalDataObjects=function(objects){
      # check objects is a list, and that it contains only ESAExternalData objects
      if (!is(objects,"list")) stop("objects must be a list.")
      if (!Reduce("&",lapply(objects, function(x) is(x,"ESAExternalData")))) stop("list must only contain ESAExternalData objects.")
      # internal function which returns the keys for each element in the enum
      keysForMergeType <- function(type){
        if (type$siteOnly) return(c(self$getProviderSite()))
        else if (type$dateOnly) return(c(self$getDate()))
        else if (type$siteAndDate) return(c(self$getProviderSite(), self$getDate()))
        else if (type$provOnly) return(c(self$getProvider()))
        else if (type$provAndDate) return(c(self$getProvider(), self$getDate()))
      }
      # loop through all the elements in objects and merge appropriately
      for (obj in objects){
        # set the keys as per the objects' merge type
        keysToSet <- keysForMergeType(obj$getMergeType())
        data.table::setkeyv(self$data, keysToSet)
        # merge data in obj to data attribute
        self$data <- merge(x=self$data, y=obj$data, all=TRUE)
      }
    },
    getAvailableVars=function(){
      # get list of available variables (columns)
      cols <- colnames(self$data)
      # remove prefix of slot x acuity x queue
      removeStr <- paste0(unlist(lapply(private$slots,function(x) x$slotID)),'_',
                          ifelse(is.null(private$queue),
                                 private$acuity$name,
                                 paste0(private$queue$name,'_',private$acuity$name)),
                          '_')
      removeStr <- paste(removeStr,collapse='|',sep='')
      cols <- unique(gsub(removeStr,'',cols))
      # remove any other slot variables as these are automatically handled
      cols <- cols[!grepl('slot', cols)]
      return(cols)
    },
    # getter methods
    getIdentifer=function(){
      return(private$identifier)
    },
    getAcuity=function(){
      return(private$acuity)
    },
    getESAPatientLevel=function(){
      return(private$esaPatientLevel)
    },
    getQueue=function(){
      return(private$queue)
    },
    getSlots=function(){
      return(private$slots)
    },
    getProviderSite=function(){
      return(private$providerSite)
    },
    getDate=function(){
      return('final_date')
    },
    getProvider=function(){
      return(private$provider)
    }
  ),
  private=list(
    esaPatientLevel=NULL,
    acuity=NULL,
    queue=NULL,
    slots=NULL,
    identifier=NULL,
    providerSite=NULL,
    provider=NULL,
    doAggregation=function(provider.col,
                           providerSite.col,
                           slots,
                           queue=NULL,
                           cubicles=NULL,
                           cubicles.site=NULL,
                           cubicles.date=NULL,
                           cubicles.col=NULL,
                           otherFlags=NULL){

      df <- data.table::copy(private$esaPatientLevel$data)

      allOutputCols <- list()
      otherOutputCols <- list()
      # the flags created need to be multiplied by each slot x acuity x queue length
      for (slot in slots){
        # slot column used as multiplier
        slotMultiplier <- paste0(slot$slotID,'_', ifelse(is.null(private$queue),private$acuity$name,paste0(private$queue$name,'_',private$acuity$name)))
        # flag columns
        flagCols <- c(unlist(lapply(private$esaPatientLevel$getFlags(), function(x) x$name)), otherFlags)
        # output columns
        flagOutputCols <- paste0(slotMultiplier,'_', flagCols)
        # multiply flags by the slot multiplier
        df[, (flagOutputCols) := lapply(.SD, function(x) x*df[[slotMultiplier]]), .SDcols=flagCols]
        # additionally need to aggregate the columns of the other acuities
        otherAccs <- private$acuity$other.acuities()
        if (!is.null(otherAccs)){
          otherAccsPrefix <- paste0(slot$slotID, ifelse(is.null(private$queue),'', paste0('_', private$queue$name)), '_')
          otherAccsCols <- paste0(otherAccsPrefix, otherAccs)
          otherOutputCols[[slotMultiplier]] <- otherAccsCols
        }
        # add output columns to the all output cols (these will be aggregated)
        allOutputCols[[slotMultiplier]] <- flagOutputCols

      }
      # all columns to aggregate
      allColsAgg <- c(unlist(allOutputCols), unlist(otherOutputCols), names(allOutputCols))
      # aggregate (sum)
      dfAgg <- df[,lapply(.SD, sum, na.rm=TRUE),
                  by=c(provider.col, providerSite.col, 'final_date'),
                  .SDcols=allColsAgg]
      # calculate for output cols (ie flag x acuity x queue x slot)
      for (name in names(allOutputCols)){
        shareOutputCols <- paste0(allOutputCols[[name]], '_share')
        dfAgg[, (shareOutputCols) := lapply(.SD, function(x) x/dfAgg[[name]]), .SDcols=allOutputCols[[name]]]
      }
      # derive cubicles if this is a cubicle container
      if (private$acuity$name=='cubicles'){
        # for the cubicle model, calculate cubicles by merging in cubicle data,
        # and dividing cubicles (which is majors + resus) by number of cubicles.
        cub.x <- ifelse(!is.null(cubicles.date),c(providerSite.col),c(providerSite.col,'final_date'))
        cub.y <- ifelse(!is.null(cubicles.date),c(cubicles.site),c(cubicles.site,cubicles.date))
        # merge in cubicles on site (and date if provided)
        dfAgg <- merge.data.table(x=dfAgg,
                                  y=cubicles,
                                  by.x=cub.x,
                                  by.y=cub.y,
                                  all.x=TRUE)
        # sits missing cubicle data
        cub.sitesMissing <- unique(dfAgg[is.na(dfAgg[[cubicles.col]])][[providerSite.col]])
        message(paste0(length(cub.sitesMissing), ' are missing cubicle data (',
                       paste(cub.sitesMissing, sep='', collapse=','), ')'))
        # calculate cubicles - cubicle counts are the names of allOutputCols
        cub.cubCols <- names(allOutputCols)
        dfAgg[, (cub.cubCols) := lapply(.SD, function(x) x/dfAgg[[cubicles.col]]), .SDcols=cub.cubCols]
      }
      # add some day of the week factors
      dfAgg[, 'date' := as.factor(format(final_date, format='%D'))]
      # calculate earliest date, set this as baseline for control_date
      date.earliest <- min(dfAgg[['final_date']], na.rm=TRUE)
      dfAgg[, 'date' := relevel(date, ref=format(date.earliest,format='%D'))]
      # create day of the week variable
      dfAgg[, 'day_of_week' := format(final_date, format='%A')]
      # convert dow to factor, set baseline to wednesday
      dfAgg[, 'day_of_week' := as.factor(day_of_week)]
      dfAgg[, 'day_of_week' := relevel(day_of_week, ref='Wednesday')]
      # create month variable
      dfAgg[, 'month' := format(final_date, format='%B')]
      dfAgg[, 'month' := as.factor(month)]
      dfAgg[, 'month' := relevel(month, ref=format(date.earliest,format='%B'))]
      return(dfAgg)
    }
  )
)

