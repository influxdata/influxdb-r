#' @import httr
#' @import rjson
NULL

#' Query an InfluxDB database
#' 
#' @param host Character vector with IP address or hostname
#' @param port Port number
#' @param database The name of the database
#' @param query Character vector containing the InfluxDB query
#' @return A named list of data frames, where the names are the series names,
#'   and the data frames contain the points.
#'
#' @export
influxdb_query <- function (host, port, database, query) {
  
  url <- paste0(host, ":", port, "/query?")
  args <- list(db = database,
               q = query)
  
  getCommand <- httr::GET(url, query=args)
  
  # Check for error. Not familiar enough with httr, there may be other ways it
  # communicates failure.
  if (getCommand$status < 200 || getCommand$status >= 300) {
    
    stop("Influx query failed with HTTP status code ", getCommand$status)
  
  } else {
    
    response <- httr::content(getCommand, "parsed")
    if (is.null(response$results[[1]]$error)) {
      response <- response$results[[1]]$series[[1]]
      colNames <- unlist(response$columns)
      nr <- length(response$values)
      
      # build the dataframe
      for (i in 1:nr) {
        r <- t(unlist(response$values[[i]]))
        if (i == 1) {
          output <- data.frame(r, stringsAsFactors = FALSE)
        } else {
          output <- rbind(output, data.frame(r))
        }
      }
      colnames(output) <- colNames
      return(output)
      
    } else {
      
      warning(paste0("Influx returned the following error: ", response$results[[1]]$error))
      return(data.frame())
      
    }
  }
  
}
