
ESAExternalData <- R6Class(
  classname = "ESAExternalData",
  public = list(
    data = NULL,
    #' @description Initialize an ESAExternalData object
    #' @param data a data.table or data.frame
    #' @param siteCol character of column containing site code
    #' @param dateCol character of column containing date
    #' @param copyData boolean whether or not to undertake a copy of the data
    #' @return ESAExternalData
    initialize = function(data, siteCol=NULL, provCol=NULL, dateCol=NULL, copyData=FALSE){
      # verify is data.table/data.frame (if is data.frame, convert to data.table)
      if (is(data, "data.table")){
        if (copyData){
          self$data <- data.table::copy(data)
        } else {
          self$data <- data
        }
      } else if (is(data, "data.frame")){
        self$data <- data.table::setDT(data)
      } else {
        stop("data must be either data.table or data.frame.")
      }
      print(colnames(self$data))
      # if site code column provided, check whether it is in the data
      if (!is.null(siteCol)){
        if (siteCol %in% colnames(self$data)){
          private$siteCol <- siteCol
        } else {
          stop("Site column could not be found in the data.")
        }
      }
      # if dateCol is provided, check whether present
      if (!is.null(provCol)){
        if (provCol %in% colnames(self$data)){
          private$provCol <- provCol
        } else {
          stop("Provider column could not be found in the data.")
        }
      }
      if (!is.null(provCol) & !is.null(siteCol)){
        stop("Only one of provider or site column can be supplied.")
      }
      # if the date column provided, check whether it is in the data
      if (!is.null(dateCol)){
        if (dateCol %in% colnames(self$data)){
          private$dateCol <- dateCol
        } else {
          stop("Date column could not be found in the data.")
        }
      }
      # work through the different combinations to set relevant keys
      keys <- NULL
      if (!is.null(siteCol) & is.null(provCol) & is.null(dateCol)){
        private$mergeType <- ESAEDMergeTypes$siteOnly
        keys <- c(siteCol)
      } else if (is.null(siteCol) & !is.null(provCol) & is.null(dateCol)){
        private$mergeType <- ESAEDMergeTypes$provOnly
        keys <- c(provCol)
      } else if (is.null(siteCol) & is.null(provCol) & !is.null(dateCol)){
        private$mergeType <- ESAEDMergeTypes$dateOnly
        keys <- c(dateCol)
      } else if (!is.null(siteCol) & is.null(provCol) & !is.null(dateCol)){
        private$mergeType <- ESAEDMergeTypes$siteAndDate
        keys <- c(siteCol, dateCol)
      } else if (is.null(siteCol) & !is.null(provCol) & !is.null(dateCol)){
        private$mergeType <- ESAEDMergeTypes$provAndDate
        keys <- c(provCol, dateCol)
      }
      # set the keys for the data.table for future merges - site first, then date
      data.table::setkeyv(self$data, cols=keys)
    },
    #' @description Return an ESAEDMergeTypes enum on which columns were provided
    #' @return ESAEDMergeTypes
    getMergeType = function(){
      return(private$mergeType)
    }
  ),
  private = list(
    siteCol = NULL,
    dateCol = NULL,
    provCol = NULL,
    mergeType = NULL
  )
)
