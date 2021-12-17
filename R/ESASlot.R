#' Title: ESASlot
#' Author: Economics and Strategic Analysis Team
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:
#' Class to define a slot

ESASlot <- R6Class(
  classname='ESASlot',
  public=list(
    slotID=NULL,
    slotHours=NULL,
    maxMins=NULL,
    maxHour=NULL,
    initialize=function(slotID, slotHours){
      if(Reduce('&', slotHours %in% 0:23)){
        self$slotHours <- slotHours[order(slotHours)]
        self$maxMins <- (tail(self$slotHours,1)*60)+59
        self$maxHour <- tail(self$slotHours,1)
      } else {
        stop('Could not initialize ESASlot. A day contains 0 to 23 hours')
      }
      self$slotID <- slotID
    },
    print=function(){
      message(paste0('slot id: ', slotID, ' for hours: ', paste(slotHours, collapse=',', sep=' ')))
    }
  )
)
