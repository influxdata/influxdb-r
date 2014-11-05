#' @import httr
#' @import rjson
NULL

#' Query an InfluxDB database
#' 
#' @param host Character vector with IP address or hostname
#' @param port Port number
#' @param username InfluxDB user
#' @param password InfluxDB password (will be passed in plain text)
#' @param database The name of the database
#' @param query Character vector containing the InfluxDB query
#' @param time_precision Specifies whether the time should be returned in 
#'   seconds (\code{s}), milliseconds (\code{m}), or microseconds (\code{u}) 
#'   from epoch (January 1, 1970, 00:00:00 UTC).
#' @return A named list of data frames, where the names are the series names,
#'   and the data frames contain the points.
#'
#' @export
influxdb_query <- function(host, port, username, password, database, query,
                        time_precision=c("s", "m", "u")) {
  response <- GET(
    "", scheme = "http", hostname = host, port = port,
    path = sprintf("db/%s/series", URLencode(database)),
    query = list(
      u = username,
      p = password,
      q = query,
      time_precision = match.arg(time_precision),
      chunked = "false"
    )
  )
  
  ## original code moved to a function to share with influxdb_write 
  check_reponse_status(response)
  
  response_data <- fromJSON(rawToChar(response$content))
  
  # response_data at this point is a hot mess of nested lists; turn it into
  # something nicer to work with. I'm sure there is a faster/better way to
  # do this.
  responseObjects <- sapply(response_data, function(seriesObj) {
    # TODO: Should stringsAsFactors be used or not?
    df <- as.data.frame(t(sapply(seriesObj$points, rbind)))
    # It's a data frame but each column is a list instead of atomic vector; 
    # let's fix that
    df <- as.data.frame(lapply(df, unlist))
    names(df) <- seriesObj$columns
    structure(list(df), names=seriesObj$name)
  })
  return(responseObjects)
}


#' Write data to an InfluxDB database
#' 
#' @param host Character vector with IP address or hostname
#' @param port Port number
#' @param username InfluxDB user
#' @param password InfluxDB password (will be passed in plain text)
#' @param database The name of the database
#' @param series A named list of data frames, where the names are the series names, and the data frames contain the points.
#' @param time_precision Specifies whether the time should be returned in 
#'   seconds (\code{s}), milliseconds (\code{m}), or microseconds (\code{u}) 
#'   from epoch (January 1, 1970, 00:00:00 UTC).
#' @return The httr response object (invisibly) 
#'
#' @export
influxdb_write <- function(host, port, username, password, database, series , time_precision=c("s", "m", "u")) {
  
  data_formated <- mapply(format_data,names(series),series,USE.NAMES = FALSE,SIMPLIFY = FALSE)
  
  data_json <-  toJSON(data_formated) 
  
  response <- POST(
    "", scheme = "http", hostname = host, port = port,
    path = sprintf("db/%s/series", URLencode(database)),
    query = list(
      u = username,
      p = password,
      time_precision = match.arg(time_precision)
    ),
    body=data_json
  )
  
  check_reponse_status(response)
  
  invisible(response)
}

check_reponse_status<-function(response){
  # Check for error. Not familiar enough with httr, there may be other ways it
  # communicates failure.
  if (response$status_code < 200 || response$status_code >= 300) {
    if (length(response$content) > 0)
      warning(rawToChar(response$content))
    stop("Influx query failed with HTTP status code ", response$status_code)
  }
  
}

format_data <- function(name,df){
  columns <- as.list(colnames(df))
  split_list <- split(df, 1:nrow(df)) 
  
  # We need to drop all names from the points structure 
  # for rjson to produce the expected output
  # There is probably a much more optimized way to do so
  points<-mapply(drop_names,split_list,USE.NAMES = FALSE,SIMPLIFY = FALSE)  
  list(
    name = name,
    columns = columns,
    points =  points
  )
}

drop_names <- function(x){
  x_list <- as.list(x)
  names(x_list) <-NULL
  x_list
}
