#' @import R6
#' Title: ESAAcuity
#' Author: Economics and Strategic Analysis Team
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:
#' Class to define an acuity
#' There are 4 levels of acuity: minors, majors, resus and cubicles.
#' Cubicles are derived from majors + resus / cubicles
#' This class is a subclass of ESADataFlag (as the same search functionality
#' is desired)

ESAAcuity <- R6Class(
  classname='ESAAcuity',
  inherit=ESADataFlag,
  public=list(
    initialize=function(name,columns,search){
      # only allow the name to be minors,majors,resus or cubicles
      if(!name %in% c('minors', 'majors', 'resus', 'cubicles')){
        stop('Acuity must be either majors, minors, resus or cubicles')
      }
      self$name <- name
      self$columns <- columns
      self$search <- search
    },
    other.acuities=function(){
      # function to get other related acuities - for cubicles it is minors (as
      # cubicles is defined as resus + majors). For majors/minors/resus it is two
      # acuities which are not this current acuity. Predominately this is used in
      # the regression model, as the other acuities are control variables.
      if(self$name=='cubicles'){
        return(c('minors'))
      } else {
        acuities <- c('minors', 'majors', 'resus')
        return(acuities[acuities != self$name])
      }
    },
    print=function(){
      message(paste0('ESAAcuity: ', self$name, ', col: ', self$columns, ', search:', paste(self$search, collapse=',')))
    }
  )
)

