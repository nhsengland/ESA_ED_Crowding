#' Load data via an odbc connection
#'
#' @param odbcName String: The name of the ODBC connection name to utilise
#' @param odbcQuery String: The sql query to execute via the ODBC connection
#' @return A data.table of the ODBC connections' response
#' @example
#'  \dontrun{
#'  getDataFromODBC("ncdr", "select * from dbo)
#' }
#' @export
getDataFromODBC <- function(odbcName, odbcQuery){

  response <- tryCatch(
    {
      # establish odbc connection
      cxn <- DBI::dbConnect(odbc::odbc(), odbcName)
      # send odbcQuery
      cxnQuery <- DBI::dbSendQuery(cxn, odbcQuery)
      # get the response
      cxnResponse <- DBI::dbFetch(cxnQuery)
      # return data.table of response
      return(data.table::setDT(cxnResponse))
    },
    error=function(cond){
      message(paste("An error occured connecting to: ", odbcName))
      message("The error message: ")
      message(cond)
      return(NULL)
    },
    warning=function(cond){
      message(paste("A warning occured connecting to: ", odbcName))
      message("The warning message:")
      message(cond)
      return(NULL)
    },
    finally={
      # executed no matter what the above.
    }
  )
  return(response)
}

#' Check whether a string/vector is contained within a vector
#' function which, for a given dataset, checks whether vector, or text, is
#' contained within another vector.
#' @param search A vector or string which is being searched for.
#' @param within A vector in which to search for
#' @param quietly Boolean whether to print messages
#' @return Boolean: True (Found), False (Not Found)
#' @example
#' searchWithin(search="a", within=c("a", "b"))
#' searchWithin(search=c("a","b"), within=c("a", "b", "c"))
searchWithin <- function(search, within, quietly=TRUE){
  # if the vector to search thru is null, then can't find.
  if(is.null(within)){
    return(FALSE)
  }
  # check whether search is a vector or string
  if(!(is.vector(search) | is.character(search))){
    if(!quietly){
      message("the search term is not of vector or character class")
    }
    return(NULL)
  }
  # declare not in function
  `%notin%` <- function(x,y) !(x %in% y)
  # find intersection
  intersection <- intersect(search, within)
  if(isTRUE(all.equal(intersection, search))){
    return(TRUE)
  }
  notFound <- search[search %notin% within]
  if(!quietly){
    message("the following items were not found:")
    message(paste(notFound, sep=","))
  }

  return(FALSE)
}

#' Check if an object is date type
#' is.Date returns TRUE if x is a date
#' @param x an R object
#' @return boolean
#' @example
#' is.Date(as.Date('2021-01-01'))
is.Date <- function(x){
  return(inherits(x, c("Date", "POSIXt")))
}

#' Check for comma seperated entries in vector
#' returns unique vector of individual columns
#' @param vector vector needing checking
#' @return
unique.vector <- function(vector){
  returnVector <- c()
  for (i in 1:length(vector)){
    element <- unlist(strsplit(vector[i], split=', '))
    returnVector <- c(returnVector, element)
  }
  return(unique(returnVector))
}

#' Check whether element in x ends in any item in y
#'
#' @param x vector to search
#' @param y vector of endsWiths
#' @return vector of booleans
#' @example
#' vector.endsWith(c('hello', 'world'), c('ld'))
vector.endsWith <- function(x,y){
  # for all elements in x, see if it ends with an element in y
  find <- lapply(x, endsWith, suffix=paste(y))
  # reduce across all y for each x
  find <- lapply(find, function(z) Reduce('|', z))
  # unlist the end product
  return(unlist(find))
}
