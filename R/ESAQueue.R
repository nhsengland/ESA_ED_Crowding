#' Title: ESAQueue
#' Author: Economics and Strategic Analysis Team
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:

ESAQueue <- R6Class(
  classname='ESAQueue',
  public=list(
    name = NULL,
    offset.hours=NULL,
    offset.mins=NULL,
    initialize=function(name, offset.hours){
      if (!offset.hours>0){
        stop('offset hours must be greater than 0')
      }
      self$name <- name
      self$offset.hours <- offset.hours
      self$offset.mins <- offset.hours*60
    }
  )
)
