#' Title: ESAModel
#' Author: Economics and Strategic Analysis Team
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:
#'
ESAModel <- R6Class(
  classname='ESAModel',
  public=list(
    initialize=function(ESAEDAggregated,
                        model.type=c('poisfe', 'olsfe'),
                        bedOccupancy=NULL,
                        slotVars=NULL,
                        nonSlotVars=NULL,
                        fixedEffects=NULL,
                        clusterSE=NULL,
                        printSummary=FALSE,
                        withAME=FALSE,
                        vcovFn=NULL,
                        vcovArgs=NULL){
      if(!is(ESAEDAggregated, 'ESAEDAggregated')){
        stop('ESAEDAggregated must be ESAEDAggregated Object.')
      }
      # check vcov is function and vcovargs is list if fixed effects is null
      if (is.null(fixedEffects)){
        if (!is.null(vcovArgs)){
          if (!is(vcovArgs,'list')){
            stop('vcovArgs must be list if not null')
          }
        }
      }
      private$esaEDObj <- ESAEDAggregated
      private$bedOccupancyVars <- bedOccupancy
      private$modelType <- model.type
      private$fixedEffects <- fixedEffects
      # get formuli
      formuli <- private$getFormulas(obj=ESAEDAggregated,
                                     bedOccupancy=bedOccupancy,
                                     slotVars=slotVars,
                                     nonSlotVars=nonSlotVars,
                                     fixedEffects = fixedEffects)
      # run model
      private$runModels(obj=ESAEDAggregated,
                        formuli=formuli,
                        model.type=model.type,
                        printSummary=printSummary,
                        withAME=withAME,
                        fixedEffects=fixedEffects,
                        vcovFn=vcovFn,
                        vcovArgs=vcovArgs,cl=clusterSE)
    },
    runScenario=function(scenario){
      if(!is(scenario,'ESAModelScenario')){
        stop('Scenario must be ESAModelScenario object.')
      }
      stop('Not supported')
    },
    results.slot.avg=function(sig=0.1,min.slot.sig=3){
      return(private$model.results.avg(sig=sig,min.slot.sig=min.slot.sig))
    },
    getResults=function(sig=0.1, min.slot.sig=3, useSampleAvgs=TRUE,from='max',to='mean'){
      resAll <- private$model.results.avg(sig=sig,min.slot.sig = min.slot.sig)
      if (is.null(resAll)){
        warning('There were no results obtained from this model.')
        return(NULL)
      }
      if ((!from %in% c('max','mean','min'))| (!to %in% c('max','mean','min'))){
        warning('from or to arguments invalid. defaulting to from=max,to=mean')
        from <- 'max'
        to <- 'mean'
      }
      # get sample averages
      sampAvgs <- private$model.sample.averages(useSampleAvgs)
      # merge the two
      resAll <- merge.data.table(x=resAll,
                                 y=sampAvgs,
                                 by='varname',
                                 all.x=TRUE)
      slots <- unlist(lapply(private$esaEDObj$getSlots(), function(x) x$slotID))
      # calculate from -> to from sample averages, then multiply by
      # corresponding coefficient
      outCols <- paste0(slots,paste0('_',from,'_to_',to))
      resAll[, (outCols) := lapply(slots, function(x) (resAll[[paste0(x,'_',to)]]-resAll[[paste0(x,'_',from)]])*resAll[[x]])]
      # calculate an average of output
      resAll[, paste0('avg_',from,'_to_',to) := fcase(should_retain,rowMeans(.SD, na.rm=TRUE),default=NA), .SDcols=outCols]
      return(resAll)
    },
    getCoefficientPlots=function(exclusions=NULL){
      # create coefficient plots for all of the models
      # get models
      models <- unique(private$modelResults.all$model_key)
      plots <- lapply(models, function(x){
        df <- private$modelResults.all[model_key==x]
        # remove rows relating to bed occupancy
        if (!is.null(private$bedOccupancyVars)){
          df <- df[!grepl(pattern=paste(private$bedOccupancyVars,collapse='|'),factor),]
        }
        # remove slot/acuity/queue prefix
        df[, factor := gsub('_',' ', gsub(paste0(x,'_'),'',factor))]
        # remove any exlusions if there are any
        if (!is.null(exclusions)){
          df <- df[!grepl(pattern=paste(exclusions,collapse='|',sep=''),x=factor)]
        }
        # create coefficient plots
        plot <- ggplot(df, aes(x=df[['factor']], y=df[['estimate']])) +
                geom_hline(yintercept = 0, colour=gray(1/2), lty= 2) +
                geom_point(aes(x=df[['factor']], y=df[['estimate']])) +
                geom_linerange(aes(x=df[['factor']], ymin=df[['lower_ci_90']], ymax=df[['upper_ci_90']]), lwd=1) +
                geom_linerange(aes(x=df[['factor']], ymin=df[['lower_ci_95']], ymax=df[['upper_ci_95']]), lwd=1/2) +
                ggtitle(paste0('Coefficient plot for ',x)) + coord_flip() + ylab('coefficient') + xlab('variable')
        print(plot)
        return(plot)
      })
      names(plots) <- models
      return(plots)
    },
    getBedOccupancyPlots=function(sig=0.1, min.slot.sig=3, useSampleAvgs=TRUE, groupSize=5, reverse=FALSE){
      # get sample averages
      sampAvgs <- private$model.sample.averages(useSampleAvgs)
      # get results
      results <- private$model.results.avg(sig=sig, min.slot.sig=min.slot.sig)
      if (is.null(results)){
        warning('There were no results obtained from this model')
        return(NULL)
      }
      bedOccVars <- private$bedOccupancyVars
      if (is.null(bedOccVars)){
        warning('There are no bed occupancy variables set.')
        return(NULL)
      }
      # get the sample average for the dependent variable of the model in
      # each of the 6 slots
      modelName <- private$getModelSuff(private$esaEDObj)
      slots <- private$esaEDObj$getSlots()
      slotsNames <- paste0(unlist(lapply(slots,function(x) x$slotID)))
      slotsNamesMeans <- paste0(slotsNames,'_mean')
      depAvgs <- as.list(sampAvgs[varname==modelName,][,..slotsNamesMeans])
      # mean of all the slot means for dep
      meanDepAvgs <- mean(unlist(depAvgs),na.rm=TRUE)
      # columns to retain from results for this calculation
      resultsCols <- c('varname', slotsNames)
      results <- results[,..resultsCols]
      # empty list to store all plots in
      allPlots <- list()
      # create charts for each different bed occupancy variable
      for (occ in bedOccVars){
        # filter for results containing bed occupancy variable in varname
        df <- results[grepl(occ,varname)]
        # calculate the percentage difference with the slot sample average for
        # each slot
        df[, (paste0(slotsNames,'_perc_eff')) := lapply(slotsNames, function(x) (df[[x]]+depAvgs[[paste0(x,'_mean')]])/depAvgs[[paste0(x,'_mean')]])]
        # calculate average effect across slots, as well as min and max
        withCallingHandlers({
          suppressWarnings(
            df[, `:=` (
              occ_avg_perc=rowMeans(.SD, na.rm=TRUE),
              occ_min_perc=apply(.SD,1,FUN=min,na.rm=TRUE),
              occ_max_perc=apply(.SD,1,FUN=max,na.rm=TRUE)
            ), .SDcols=paste0(slotsNames,'_perc_eff')]
          )
        })
        # calculations for min/max chart, get effect by subtracting 1
        df[, (c('occ_avg','occ_min','occ_max')) := lapply(.SD, function(x) ifelse(x>=1,(x-1)*100,(1-x)*100)),
           .SDcols=c('occ_avg_perc','occ_min_perc','occ_max_perc')]
        # remove bed occupancy text from varname [as R for factor will set bedOcc1,bedOcc2]
        # note this will extract the first numeric chars found and use for ordering
        df[, 'kTempName' := as.numeric(sub('\\D*(\\d+).*', '\\1', varname))]
        # order table
        setorderv(df,c('kTempName'),order=ifelse(reverse==TRUE,-1,1))
        # set a row number for index
        df[, 'kIndex' := .I]
        #### plot overall bed occupancy chart
        mainChart <- ggplot(df) +
          theme_classic() +
          theme(axis.text.x=element_text(angle=90, vjust=0),
                panel.grid.major.y = element_line(colour='grey'),
                panel.grid.minor.y=element_line(colour='lightgrey'),
                plot.title=element_text(hjust=0.5)) +
          labs(y='Percentage change in average crowding metric',
               x='Bed occupancy intervals') +
          # add title for plot
          ggtitle(label=paste0(modelName,' ', occ, ' Bed Occupancy Chart'),
                  subtitle = 'The blue line represents the average effect of bed occupancy on crowding across the slots. The error bars represent the maximum and minimum effects on crowding from the individual slots.') +
          # create line for chart
          geom_path(aes(x=kIndex,
                        y=occ_avg,
                        group=1),
                    size=2,
                    colour='steelblue') +
          # create error bars, which are just the min&max across slots
          geom_errorbar(aes(x=kIndex,
                            ymin=occ_min,
                            ymax=occ_max),
                        colour='grey',
                        size=1,
                        width=.5,
                        position=position_dodge(.9)) +
          scale_x_discrete(limits=df$varname, labels=df$varname) +
          # black circle on each y point
          geom_point(aes(x=kIndex,y=occ_avg), size=2.1, colour='black')
        # add plot to return list
        allPlots[[paste0(occ,'_main')]] <- mainChart
        # add data powering plot to return list
        allPlots[[paste0(occ,'_main_data')]] <- df

        ###### derive waterfall bed occupancy charts

        # calculate buckets to group into
        indexVec <- 1:nrow(df)
        # number of rows must be divisible by number of groups
        if (nrow(df)%%groupSize != 0){
          warning(paste0('Cannot reconcile groups for waterfall. Levels: ',nrow(df)))
        } else{
          groupingsIdx <- c()
          currentGroup <- 1
          for (i in indexVec){
            groupingsIdx <- c(groupingsIdx,currentGroup)
            if (i%%groupSize==0){
              currentGroup <- currentGroup+1
            }
          }
          # create temporary table of groupings
          groupingsDF <- data.table(index=indexVec,group=groupingsIdx)
          # join this to df
          df <- merge.data.table(x=df,y=groupingsDF,by.x='kIndex',by.y='index',all.x=TRUE)
          # create grouping name per group
          df[, 'kGroupName' := paste0(min(kTempName),'-',max(kTempName)),by='group']
          # group by group and find mean of average bed occupancy
          waterfallDF <- df[, .(average=mean(occ_avg_perc,na.rm=TRUE)), by=c('kGroupName')]
          # if there are any infinite or na values, skip from the bed occ loop
          if (sapply(waterfallDF, function(x) sum(is.na(x)|is.infinite(x)))[['average']]>0){
            warning('Could not plot waterfall due to NAs/Infs')
            next
          }
          # multiply percentage by average of number of people in each slot
          waterfallDF[, 'effect' := average*meanDepAvgs]
          # add start and end point to results [for the start and end bars]
          x.all <- rbindlist(
            list(
              list('baseline',NA,meanDepAvgs),
              waterfallDF,
              list('max', NA, waterfallDF[nrow(waterfallDF),]$effect)
              )
            )
          # add xmin xmax for bars
          x.all[, 'xmin' := fcase(.I <= nrow(x.all),.I,default=NA)]
          x.all[, 'xmax' := xmin+1]
          # add ymin and ymax
          x.all[, 'ymin' := effect]
          x.all[, 'ymax' := fcase(!is.na(shift(effect)),shift(effect, n=1),default=0)]
          x.all[nrow(x.all), 'ymin':=0]
          x.all[, 'bar_cat' := fcase(.I==nrow(x.all), 'end', .I==1,'start', default='value')]
          # difference in values
          x.all[, 'difference' := effect-shift(effect,n=1)]
          x.all[, 'kGroupName' := as.factor(kGroupName)]
          # calculate effect of moving from baseline to max, and in % terms
          effectStart <- x.all[1,]$effect
          effectEnd <- x.all[nrow(x.all),]$effect
          effectDiff <- round(effectEnd-effectStart,0)
          effectPercDiff <- round(((effectEnd-effectStart)/effectStart)*100,0)
          effectDesc <- paste0(effectDiff, ifelse(effectDiff<0,' fewer',' more'), ' patients (',effectPercDiff,'%)')
          # width of bars
          w <- 0.5
          # create the waterfall plot
          plot <- ggplot(x.all) +
            theme_classic() +
            theme(legend.position='none',
                  panel.grid.major.y=element_line(colour='grey'),
                  panel.grid.minor.y = element_line(colour='lightgrey'),
                  axis.text.x=element_text(angle=0),
                  plot.title=element_text(hjust=0.5)) +
            labs(y='Average number of patients',
                 x='Bed Occupancy Intervals',
                 title=paste0(modelName,' ', occ, ' Waterfall Bed Occupancy Chart')) +
            # plot the individual bars for each bed ocupancy level
            geom_rect(aes(xmin=xmin-w/2,xmax=xmin+w/2,ymin=ymin,ymax=ymax, fill=bar_cat)) +
            # create a line between each of the bars
            geom_segment(data=x.all[1:(nrow(x.all)-1),], aes(x=xmin-w/2,xend=xmax+w/2,y=ymin,yend=ymin),size=0.7) +
            # display total number of people in crowding based on bed occupancy level (vs baseline)
            geom_text(aes(x=xmin,y=effect+1,label=round(effect,1))) +
            # show difference in effect between each step
            geom_text(aes(x=xmin-w,y=effect+0.5,label=round(difference,1)),size=3) +
            scale_x_discrete(limits=x.all$kGroupName, labels=x.all$kGroupName) +
            # create fill for the bars
            scale_fill_manual(values=(c('end'='#005EB8','value'='steelblue', 'start'='grey'))) +
            # add some padding to graph
            scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
            # create dotted line from starting effect (sample mean of y), to max [i.e when bed occ 100%]
            geom_hline(yintercept = effectStart, linetype='dotted',colour='#7C2855') +
            geom_hline(yintercept = effectEnd, linetype='dotted',colour='#7C2855') +
            annotate('text', x=nrow(x.all), y=effectEnd+3,label=effectDesc, colour='#7C2855')
          # add plot to return list
          allPlots[[paste0(occ,'_waterfall')]] <- plot
          # add plot data to returnlist
          allPlots[[paste0(occ,'_waterfall_data')]] <- x.all
        }
      }
      return(allPlots)
    },
    getRegressionDatasets=function(){
      return(private$modelRegression.datasets)
    },
    getSampleAvgs=function(bo=TRUE){
      return(private$model.sample.averages(bo))
    },
    getRegressionTable=function(exclusions=NULL){
      u <- private$modelObjects
      if (length(u)==0|is.null(u)) return(NULL)
      # get all variables across all models
      allVars <- unlist(lapply(u, function(x) rownames(x$coeftable)),use.names = FALSE)
      # get all the slot names
      slots <- unlist(lapply(private$esaEDObj$getSlots(),function(x) x$slotID))
      model.suff <- private$getModelSuff(private$esaEDObj)
      # remove slot & model identifier from all variables (so they align in table)
      repl <- paste(paste0(slots,'_',model.suff,'_'),collapse='|',sep='')
      toRepl <- allVars[grepl(repl,allVars)]
      subRepl <- gsub(repl,'',toRepl)
      # create dictionary for etable
      names(subRepl) <- toRepl
      if (!length(subRepl)>0){
        subRepl <- NULL
      }
      # create exclusions string if there are any
      exclude <- ifelse(is.null(exclusions),'!',paste(paste0('^',exclusions),collapse='|',sep=''))
      # some vectors of model statistics
      extralineList <- list()
      omitColinear <- unlist(lapply(u,function(x) paste(x$collin.var,collapse=',',sep='')))
      numOmitColinear <- unlist(lapply(u,function(x) length(x$collin.var)))
      feSize <- unlist(lapply(u,function(x) x$fixef_sizes))
      aic <- unlist(lapply(u,function(x) AIC(x)))
      bic <- unlist(lapply(u,function(x) BIC(x)))
      if (!is.null(omitColinear)) extralineList[['^_Omitted due to collinearity']] <- omitColinear
      if (!is.null(numOmitColinear)) extralineList[['^_Number omitted due to collinear']] <- numOmitColinear
      if (!is.null(feSize)) extralineList[['^_Fixed effect size']] <- feSize
      if (!is.null(aic)) extralineList[['^_AIC']] <- aic
      if (!is.null(bic)) extralineList[['^_BIC']] <- bic
      # return a data.frame via fixest's etable method, with some additional stats
      return(etable(u,drop=exclude,
                    extraline=extralineList,
                    dict = subRepl))
    },
    getAMEs = function(){
      return(private$modelAMEs)
    },
    getModelObjs=function(){
      return(private$modelObjects)
    }
  ),
  private=list(
    esaEDObj=NULL,
    modelResults.all=NULL,
    modelResultsWide.all=NULL,
    modelRegression.datasets=NULL,
    bedOccupancyVars=NULL,
    modelType=NULL,
    modelObjects=list(),
    modelAMEs=NULL,
    fixedEffects=NULL,
    getFormulas=function(obj, bedOccupancy=NULL, slotVars=NULL, nonSlotVars=NULL,fixedEffects=NULL){
      # method to get formulas for the various slot models. return labeled
      # list containing a formula object.
      queue <- obj$getQueue()
      acuity <- obj$getAcuity()
      slots <- obj$getSlots()
      # model suffix (ie acuity x queue)

      model.suff <- private$getModelSuff(obj)
      otherAcuities <- acuity$other.acuities()
      formuli <- list()
      # iterate through the slots
      for (slot in slots){
        dep <- paste0(slot$slotID,'_',model.suff)
        # get other acuities
        otherAccsPrefix <- paste0(slot$slotID, ifelse(is.null(queue),'', paste0('_', queue$name)), '_')
        indep.otheraccs <- paste0(otherAccsPrefix, otherAcuities)
        indep.slots <- unlist(lapply(slotVars, function(x) paste0(dep,'_',x)))
        indep.all <- c(indep.otheraccs,indep.slots,bedOccupancy,nonSlotVars)
        # check all variables are in the data
        #if(!searchWithin(search=c(dep,indep.all), colnames(obj$data),quietly=FALSE)){
        #  stop('Could not identify a variable in the data.')
        #}
        form <- as.formula(paste0(dep,'~',paste(indep.all, collapse='+',sep=''), ifelse(is.null(fixedEffects),'',paste0('|', paste(fixedEffects, collapse = '+')))))
        formuli[[dep]] <- form
      }
      return(formuli)
    },
    runModels=function(obj,formuli,model.type,printSummary,withAME,fixedEffects,vcovFn,vcovArgs,cl){
      # create empty results dataframe to store results of all the models...
      results.cols <- c('factor','estimate','std_err','t_value','p_value',
                        'lower_ci_90','upper_ci_90','lower_ci_95','upper_ci_95',
                        'model_key')
      reg.df <- data.table::copy(obj$data)
      reg.df[, paste0(obj$getProviderSite()) := as.factor(reg.df[[obj$getProviderSite()]])]
      # force clustering on final_date if there is only one site
      if (length(unique(reg.df[[obj$getProviderSite()]]))==1 & is.null(cl) & is.null(fixedEffects)){
        cl <- 'final_date'
      }
      # run all the models within the list of formuli
      model.results <- lapply(names(formuli), function(x){
        model <- tryCatch({
          message(paste0('running model... ',x))
          clusVar <- fixedEffects
          if (model.type=='poisfe'){
            model <- fepois(formuli[[x]], data=reg.df, cluster=clusVar, glm.iter = 100)
          } else if (model.type=='olsfe'){
            model <- feols(formuli[[x]], data=reg.df, cluster=clusVar)
          } else {
            stop('Invalid model detected.')
          }
        },
        error=function(cond){
          message(paste0('an error occured with this model: ',x))
          message(cond)
          return(NULL)
        },
        warning=function(cond){
          message(paste0('a warning occured with this model: ',x))
          message(cond)
          return(NULL)
        })
        if (!is.null(model)){
          res <- NULL
          if (!is.null(fixedEffects)|(is.null(fixedEffects)&is.null(vcovFn))){
            # if fixed effects in use, then use summary from fixest package
            if (printSummary){
              print(summary(model))
              print(model$collin.var)
            }
            res <- cbind(coeftable(model), confint(model,level=0.9))
            res <- cbind(res, confint(model,level=0.95))
          } else if (is.null(fixedEffects)&is.null(cl)&!is.null(vcovFn)){
            # pass thru a variance co-variance function
            vcovNew <- do.call(vcovFn,args=append(list(x=model),vcovArgs),envir=environment())
            vcovRes <- lmtest::coeftest(model,vcov=vcovNew)
            if (printSummary){
              print(vcovRes)
              print(model$collin.var)
            }
            res <- cbind(vcovRes, confint(vcovRes,level=0.9))
            res <- cbind(res, confint(vcovRes,level=0.95))
          }
          # create data.table of results
          res <- setDT(as.data.frame(res), keep.rownames=TRUE)[]
          res[, model_key := paste0(x)]
          # rename columns to match the results table
          setnames(res, old=colnames(res), new=results.cols)
          # store model objects
          private$modelObjects[[x]] <- model
          return(res)
        }
        return(NULL)
      })
      # get the equivalent datasets used in the regression model. Checking
      # for complete cases across all the variables included in the formula
      # is how this is achieved.
      model.complete.df <- lapply(names(formuli), function(x){
        model.df <- data.table::copy(obj$data)
        model.vars <- all.vars(formuli[[x]])
        cols.retain <- unique(c(obj$getProviderSite(),'final_date',model.vars))
        model.df <- model.df[,..cols.retain]
        model.df[,is_complete_case:=FALSE]
        model.df[complete.cases(model.df[,..model.vars]),is_complete_case:=TRUE]
        return(model.df)
      })
      names(model.complete.df) <- names(formuli)
      private$modelRegression.datasets <- model.complete.df
      # Derive the average marginal effects, if specified by the user. Use
      # the complete cases dataset derived above to calculate
      listAMES <- list()
      if (withAME){
        # loop through models
        for (nm in names(private$modelObjects)){
          # get model object
          mdl <- private$modelObjects[[nm]]
          # check is not null
          if (!is.null(mdl)){
            # calculate average marginal effects, using the regression sample for that model
            # if poisson model, use link, otherwise if OLS then use reponse type
            mfx <- marginaleffects(model=mdl,
                                   newdata=model.complete.df[[nm]][is_complete_case==TRUE],
                                   type=ifelse(grepl('pois',model.type),'link','response'))
            if (printSummary){
              print(summary(mfx))
            }
            # add to list of average marginal effects
            listAMES[[nm]] <- setDT(tidy(mfx))[, model:=nm]
          }
        }
      }
      # union all the average marginal effects
      private$modelAMEs <- rbindlist(listAMES,use.names=TRUE)
      # combine the model results
      # drop those outputs which are null before unioning the model results.
      model.results <- model.results[!sapply(model.results, is.null)]
      results <- rbindlist(model.results, use.names=TRUE)
      private$modelResults.all <- results
    },
    model.results.avg=function(sig=0.1, min.slot.sig=3){
      # minimum of slots that should be significant in must be less or equal to
      # the number of slots
      if (min.slot.sig > length(private$esaEDObj$getSlots())){
        stop('Minimum slots to define as significant must not exceed the number of slots.')
      }
      df <- data.table::copy(private$modelResults.all)
      # check if all model results table is null, or empty, and return null if so
      if (is.null(df)|(!is.null(df)&(nrow(df)==0|length(colnames(df))==0))){
        warning('No results found')
        return(NULL)
      }
      # remove slot specific prefixes for factor names
      model.suff <- private$getModelSuff(private$esaEDObj)
      model.slots <- unlist(lapply(private$esaEDObj$getSlots(), function(x) x$slotID))
      df[, varname := gsub(paste(paste0(model.slots,'_'),collapse='|',sep=''), '',factor)]
      df[, varname := gsub(paste0(model.suff,'_'),'',varname)]
      # reshape wide, retaining only estimate and p value
      df[, model_grouper := gsub(paste0('_',model.suff),'',model_key)]
      # get unique list of models identified
      modelsAvail <- unique(df$model_grouper)
      wide <- dcast(df, formula=varname~model_grouper, value.var=c('estimate', 'p_value'), fill=NA)
      # if some slots did not obtain any results, create empty NA column.
      modelsUnavail <- model.slots[!model.slots %in% modelsAvail]
      if (length(modelsUnavail)>0){
        colsModelsUnavail <- c(paste0('estimate_',modelsUnavail),paste0('p_value_',modelsUnavail))
        wide[, (colsModelsUnavail) := NA]
      }
      # for the poisson model, take the exponential of the coefficients (since
      # the Poisson model utilises a log link)
      if (grepl('pois',private$modelType)){
        wide[, (paste0('estimate_',model.slots)) := lapply(.SD, exp),.SDcols=paste0('estimate_',model.slots)]
      }
      pval.cols <- paste0('p_value_', unique(df[['model_grouper']]))
      pval.cols.out <- paste0(pval.cols,'_flag')
      # create flags whether p value is less than significance level define
      wide[, (pval.cols.out) := lapply(.SD, function(x) fcase(x<sig,1,default=NA)), .SDcols=pval.cols]
      # multiply each slot estimate by whether estimate is significant or not
      wide[, (model.slots) := lapply(model.slots, function(x) wide[[paste0('estimate_',x)]]*wide[[paste0('p_value_',x,'_flag')]])]
      # create flag if meets the minumum number of significant if if count NA <
      wide[, count_na := Reduce('+', lapply(.SD, is.na)), .SDcols=model.slots]
      # count how many positive/negative coefficients there are for consistency
      wide[, has_pos := fifelse(Reduce('|', lapply(.SD, function(x) x>=0))==TRUE,TRUE,FALSE,FALSE),.SDcols=model.slots]
      wide[, has_neg := fifelse(Reduce('|', lapply(.SD, function(x) x<0))==TRUE,TRUE,FALSE,FALSE),.SDcols=model.slots]
      # flag whether the factor meets consistency & significant condition
      wide[, should_retain := (!(has_pos==has_neg) & (length(model.slots)-count_na) >= min.slot.sig)]
      # calculate row means, min and max
      withCallingHandlers({
        suppressWarnings(
          wide[, `:=` (
            slot_avg=fcase(should_retain,rowMeans(.SD, na.rm=TRUE), default=NA),
            slot_min=fcase(should_retain,apply(.SD,1,FUN=min,na.rm=TRUE),default=NA),
            slot_max=fcase(should_retain,apply(.SD,1,FUN=max,na.rm=TRUE),default=NA)
          ), .SDcols=model.slots]
        )
      },
      warning=function(){
        return(NULL)
      })
      private$modelResultsWide.all <- wide
      return(wide)
    },
    model.sample.averages=function(only.modelled=TRUE){
      # calculate sample averages
      sample.avgs <- lapply(names(private$modelRegression.datasets), function(x){
        # filter for those records which were modelled.
        df <- data.table::copy(private$modelRegression.datasets[[x]])
        if (only.modelled){
          df <- df[is_complete_case==TRUE]
        }
        # find columns which are numeric
        cols.numeric <- names(which(unlist(lapply(df, is.numeric))))
        site.code <- private$esaEDObj$getProviderSite()
        # calculate per site code, the mean, min & max of all numeric columns
        stats <- df[, as.list(unlist(lapply(.SD, function(y){
          list(mean=mean(y,na.rm=TRUE),
               min=min(y, na.rm=TRUE),
               max=max(y, na.rm=TRUE))
        }))), by=site.code, .SDcols=cols.numeric]
        # where no levels to average/max; min/max may return Inf/-Inf, remove and
        # replace with NA
        invisible(lapply(names(stats),function(.name) set(stats, which(is.infinite(stats[[.name]])),j=.name,value=NA)))
        stats.cols.numeric <- names(which(unlist(lapply(stats, is.numeric))))
        # calculate the average of all
        stats <- stats[, unlist(lapply(.SD, mean, na.rm=TRUE)), .SDcols=stats.cols.numeric]
        # create data.table of results
        stats <- setDT(as.data.frame(stats), keep.rownames=TRUE)
        colnames(stats) <- c('varname', 'value')
        # remove the model prefix
        model.suff <- private$getModelSuff(private$esaEDObj)
        model.slots <- unlist(lapply(private$esaEDObj$getSlots(), function(x) x$slotID))
        stats[, varname := gsub(paste(paste0(model.slots,'_'),collapse='|',sep=''), '',varname)]
        stats[, varname := gsub(paste0(model.suff,'_'),'',varname)]
        # seperate the .min, .max, and .mean into another column
        stats[, metric := .(
          fcase(
            grepl('.mean', varname, fixed=TRUE), 'mean',
            grepl('.max', varname, fixed=TRUE), 'max',
            grepl('.min', varname, fixed=TRUE), 'min',
            default = 'unknown'
          )
        )]
        # remove .mean, .max, .min from varname
        stats[, varname := gsub('\\.mean|\\.max|\\.min', '', varname)]
        slotsName <- gsub(pattern=private$getModelSuff(private$esaEDObj),'',x)
        stats[, metric := paste0(slotsName,metric)]
        # reshape wide so that min, max and mean are in seperate columns
        statsWide <- dcast(stats, formula=varname~metric, value.var=c('value'), fill=NA)
        # calculate for factor variables
        cols.factors <- names(which(unlist(lapply(df, is.factor))))
        factorCounts <- lapply(cols.factors, function(y){
          # calculate counts per factor, per site, then calculate mean, max and
          # min of this count overall, per factor level
          z <- df[, .(count=.N), by=c(y,site.code)][,.(mean=mean(count, na.rm=TRUE),
                                                       max=max(count,na.rm=TRUE),
                                                       min=min(count,na.rm=TRUE)),
                                                    by=y]
          setnames(z, old=colnames(z), new=c('varname', paste0(slotsName,c('mean','max','min'))))
          z[, varname := paste0(y,varname)]
          return(z)
        })
        factorSummary <- rbindlist(factorCounts, use.names=TRUE)
        # bind factor summaries to statswide
        statsWide <- rbindlist(list(statsWide,factorSummary), use.names=TRUE)
        # set varname as key for later merges
        setkeyv(statsWide, 'varname')
        return(statsWide)
      })
      # merge each of the slot sample averages together
      all.sample.avgs <- Reduce(merge,sample.avgs)
      return(all.sample.avgs)
    },
    getModelSuff=function(obj){
      suff <- paste0(ifelse(is.null(obj$getQueue()),obj$getAcuity()$name,
                    paste0(obj$getQueue()$name,'_',obj$getAcuity()$name)))
      return(suff)
    }
  )
)
