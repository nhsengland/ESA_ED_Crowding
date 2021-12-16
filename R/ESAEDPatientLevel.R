#' Title: ESAEDPatientLevel
#' Author: Edmund Haacke
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:
#' Contain for Emergency Care data patient level

ESAEDPatientLevel <- R6Class(
  classname='ESAEDPatientLevel',
  public=list(
    data=NULL,
    initialize=function(data, acuities, flags, slots, queues, col.arrival, col.departure, col.uniqueIdent){
      self$data=data
      # ensure acuities all ESAAcuity objects
      if (!Reduce('&', lapply(acuities, is, 'ESAAcuity'))){
        stop('Acuities must be ESAAcuity objects')
      }
      private$acuities <- acuities
      private$flags <- flags
      # since ESAAcuity objects are a subclass of ESADataFlags, can just use
      # the create.flags function to create flags for acuities.
      private$create.flags(acuities)
      # create flags
      private$create.flags(flags)
      # create slots
      private$create.slots(slots=slots,
                        queues=queues,
                        col.arrival=col.arrival,
                        col.departure=col.departure,
                        col.uniqueIdent=col.uniqueIdent)
      # create additional columns which are acuity * queues * slots
      # iterate through acuities
      for (acuity in acuities){
        for (slot in slots){
          for (i in 1:(length(queues)+1)){
            if (i <= length(queues) & acuity$name != 'cubicles'){
              queue <- queues[[i]]
              self$data[, paste0(slot$slotID,'_',queue$name,'_',acuity$name) :=
                          self$data[[acuity$name]]*self$data[[paste0(slot$slotID,'_',queue$name)]]]
            } else {
              self$data[, paste0(slot$slotID, '_',acuity$name) :=
                          self$data[[acuity$name]]*self$data[[slot$slotID]]]
            }
          }
        }
      }
    },
    # some getter methods
    getAcuities=function(){
      return(private$acuities)
    },
    getFlags=function(){
      return(private$flags)
    }
  ),
  private=list(
    hasRunSlots=FALSE,
    acuities=NULL,
    flags=NULL,
    create.flags=function(flags){
      if(missing(flags)){
        stop('Missing flags arguement.')
      }
      # ensure all objects in list of flags are ESADataFlag objects
      if(!Reduce('&', lapply(flags, is, 'ESADataFlag'))){
        stop('Flags must by of list of ESADataFlag objects.')
      }
      # ensure all the columns references in flags are within the data
      if (!Reduce('&', lapply(flags, function(x) searchWithin(x$columns, colnames(self$data))))){
        stop(paste0('A column was not found. Ensure all the columns referenced in
             ESADataFlag objects are present in the data. The columns available
             are: \n', paste(colnames(self$data), collapse=', ', sep='')))
      }
      df <- data.table::copy(self$data)
      # iterate through all the elements in flags
      for (x in flags){
        # search element across the columns
        # if an element in the search vector is 'NA', search for is.na()
        if ('NA' %in% x$search){
          newSearch <- x$search[x$search != 'NA']
          # if there is NA present - search for both na, and whatever else
          # in search terms
          if (length(newSearch) > 0){
            # if searching other terms beyond NA, search for those terms too
            # grepl(pattern=paste(newSearch, collapse='|'), x)
            df[, paste0(x$name) := as.integer(
              Reduce('|', lapply(.SD, function(y) is.na(y) | grepl(pattern=paste(paste0('^',newSearch,'$'), collapse='|'), y)))
            ), .SDcols=x$columns]
          } else {
            # if no other search terms, only search for NA
            df[, paste0(x$name) := as.integer(
              Reduce('|', lapply(.SD, function(y) is.na(y)))
            ), .SDcols=x$columns]
          }
        } else {
          # not searching for NA
          df[, paste0(x$name) := as.integer(
            Reduce('|', lapply(.SD, function(y) grepl(pattern=paste(paste0('^',x$search,'$'), collapse='|'), y)))
          ), .SDcols=x$columns]
        }
      }
      self$data <- df
    },
    create.slots=function(slots, queues, col.arrival, col.departure, col.uniqueIdent){
      # check if already run slots - this is because slots does a duplication
      # of rows, - limit user to only run once!
      if (private$hasRunSlots == TRUE){
        stop('Have already created slots')
      }
      # ensure all the objects in slots are ESASlot objects
      if (!Reduce('&', lapply(slots, is, 'ESASlot'))){
        stop('Slots must be ESASlot objects')
      }
      # ensure all the objects in queues are ESAQueue objects
      if (!Reduce('&', lapply(queues, is, 'ESAQueue'))){
        stop('Queues must be ESAQueue objects')
      }

      df <- data.table::copy(self$data)
      # additional data cleaning steps?
      # i.e check date time columns are readable via POSIX as datetime and valid

      # extract arrival date and departure date
      df[, 'esa_arr_date' := as.Date(df[[col.arrival]])]
      df[, 'esa_dep_date' := as.Date(df[[col.departure]])]
      # calculate difference between arrival and departure
      df[, 'esa_arr_dep_diff' := as.integer(esa_dep_date-esa_arr_date)]
      # only retain values where departure - arrival date is 0 or 1 days
      rC <- nrow(df)
      df <- df[esa_arr_dep_diff == 0 | esa_arr_dep_diff == 1]
      message(paste0(
        'Retaining only where departure date-arrival date is 0 or 1. Dropping: ',
        rC-nrow(df),'. (', round(((rC-nrow(df))/rC)*100,3),')'
      ))
      # drop records where arrival date time > departure date time
      rC <- nrow(df)
      df <- df[df[[col.departure]] >= df[[col.arrival]]]
      message(paste0(
        'Dropped due to arrival time exceeding departure time: ', rC-nrow(df),
        '/', rC
      ))
      # where an ED attendance is 1 day, create a copy of those, and bind to df
      # this is so that an individual can be accounted for in terms of slot
      # appearances on the next day, for example, if an individual arrives at
      # 23:00 and leaves at 23:00 the next day, the individual would appear in
      # slot 6 on day 1, and slot 1, 2, 3, 4, 5, 6 on the next day
      df <- rbindlist(list(df, data.table::copy(df[esa_arr_dep_diff == 1])))
      # count the number of records, and create a row number - so that one can be
      # used for day 1, and another for day 2 (where individual is >1 day in ED)
      df[, `:=` (count=.N, index=seq_len(.N)), by=df[[col.uniqueIdent]]]
      # create a new date column - final date - which relates to the date the
      # record refers to. For multi-day attendance, idx=1 is arrival date, idx=2
      # it is the departure date.
      df[, 'final_date' := .(
        fcase(
          (count==1), esa_arr_date,
          (count==2 & index==1), esa_arr_date,
          (count==2 & index==2), esa_dep_date,
          default=NA
        )
      )]
      # ensure that the slots are in order (by the first element of the slot
      # hours (remember that already ordered slotHours ascending so the smallest
      # value will be first))
      slots <- slots[order(sapply(slots, function(x) x$slotHours[[1]]))]
      # calculate the slot in which departure hour and arrival hour is in
      df[, 'esa_arr_hour' := hour(df[[col.arrival]])]
      df[, 'esa_dep_hour' := hour(df[[col.departure]])]
      # function to calculate which slot a column is in (based on the hour)
      slotForHrCol <- function(name, col.hr){
        df[, paste0(name) := -1]
        for (i in 1:length(slots)){
          slotObj <- slots[[i]]
          # use the index to indicate which slot the col.hr resides in - since
          # ordered earliest to latest (of slots) - can then use this index as
          # comparator later
          df[, paste0(name) := fifelse(df[[col.hr]] %in% slotObj$slotHours, i, df[[name]])]
        }
      }
      slotForHrCol('esa_arr_slot', 'esa_arr_hour')
      slotForHrCol('esa_dep_slot', 'esa_dep_hour')
      # iterate through the slots, and create columns for them
      for (i in 1:length(slots)){
        slotObj <- slots[[i]]
        df[, paste0(slotObj$slotID) := fifelse(
          (count==1 & i>= esa_arr_slot & i <= esa_dep_slot) |
            (count==2 & index==1 & i>=esa_arr_slot) |
            (count==2 & index==2 & i<=esa_dep_slot), 1, 0
        )]
      }
      # define arrival and departure time in minutes
      df[, 'esa_arr_mins' := (esa_arr_hour*60)+minute(df[[col.arrival]])]
      df[, 'esa_dep_mins' := (esa_dep_hour*60)+minute(df[[col.departure]])]
      # calculate any queues passed into method
      for (queue in queues){
        # iterate through all the slots
        for (slot in slots){
          maxHr <- slot$maxHour
          maxMin <- slot$maxMins
          offMins <- queue$offset.mins
          df[, paste0(slot$slotID,'_',queue$name) := fcase(
            # for same day arrival and departure
            (count==1&df[[slot$slotID]]==1&esa_dep_hour<=maxHr&(esa_dep_mins-esa_arr_mins)>offMins),1L,
            (count==1&df[[slot$slotID]]==1&esa_dep_hour>maxHr&(maxMin-esa_arr_mins)>offMins),1L,
            # for arrival and departure on different calendar dates
            (count==2&index==1&df[[slot$slotID]]==1&(maxMin-esa_arr_mins)>offMins),1L,
            (count==2&index==2&df[[slot$slotID]]==1&esa_dep_hour<=maxHr&(esa_dep_mins+(1440-esa_arr_mins))>offMins),1L,
            (count==2&index==2&df[[slot$slotID]]==1&esa_dep_hour>maxHr&(maxMin+(1440-esa_arr_mins))>offMins),1L,
            default=0L
          )]
        }
      }

      # remove calculation columns
      df[, c('esa_arr_slot', 'esa_arr_hour', 'esa_dep_slot', 'esa_dep_hour',
             'esa_arr_date', 'esa_dep_date', 'esa_arr_dep_diff',
             'esa_arr_mins', 'esa_dep_mins') := NULL]

      self$data <- df
      private$hasRunSlots <- TRUE
    }
  )
)
