#' Title: ESADataFlag
#' Author: Economics and Strategic Analysis Team
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:
#' class to define a 0/1 flag derived from searching a column

ESADataFlag <- R6Class(
  classname='ESADataFlag',
  public=list(
    name=NULL,
    columns=NULL,
    search=NULL,
    initialize=function(name, columns, search){
      if (grepl('^\\s*$', name)){
        warning("ESADataFlag: Space Found. replacing with '_'")
        self$name <- gsub(' ', '_', name)
      } else {
        self$name <- name
      }
      self$columns <- columns
      self$search <- search
    },
    print=function(){
      message(paste0('ESADataFlag: creating: ', self$name,
                     ', by searching ', paste(self$columns, collapse=',', sep=' '),
                     ', for: ', paste(self$search, collapse=',', sep=' ')))
    }
  )
)
