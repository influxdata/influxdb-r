#' @import utils httr RJSONIO
NULL

#' Query an InfluxDB database
#' 
#' @import utils httr RJSONIO
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
influxdb_query <- function(host, port, username, password, database, query, time_precision=c("s", "m", "u"), stringsAsFactors=FALSE) {
  json <- influxdb_queryJson(host, port, username, password, database, query, time_precision=c("s", "m", "u"))
  asDataFrame <- influxdb_json2dataframe(json, stringsAsFactors)
  return(asDataFrame)
}

#' @rdname influxdb_query
#' @export
influxdb_queryJson <- function(host, port, username, password, database, query, time_precision=c("s", "m", "u")) {
  responseObjects <- 
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
  
  check_reponse_status(response)
  return(rawToChar(response$content))
}

#' Transform influxdb json result into a data.frame
#' @param json raw influxdb json result
#' @return json as data.frame
#' @export
influxdb_json2dataframe <- function(json, stringsAsFactors=FALSE){
  response_data <- fromJSON(json, nullValue=NA)
  responseObjects <- sapply(response_data, function(seriesObj) {
    df <- as.data.frame(t(sapply(seriesObj$points, rbind)))
    df <- as.data.frame(lapply(df, unlist), stringsAsFactors=stringsAsFactors)
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
  data_json <-  toJSON(format_series(series))
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
  return(response)
}

###############################
########### UTILE #############
###############################

check_reponse_status<-function(response){
  if (response$status_code < 200 || response$status_code >= 300) {
    if (length(response$content) > 0)
      warning(rawToChar(response$content))
    stop("Influx query failed with HTTP status code ", response$status_code)
  }
}

format_series <- function(series){
  return (mapply(format_data, names(series), series, USE.NAMES = FALSE, SIMPLIFY = FALSE))
}

format_data <- function(name,df){
  columns <- as.list(colnames(df))
  split_list <- split(df, 1:nrow(df)) 
  
  # We need to drop all names from the points structure 
  # for json output
  points<-mapply(drop_names,split_list,USE.NAMES = FALSE,SIMPLIFY = FALSE)  
  list(
    name = name,
    columns = columns,
    points =  points
  )
}

drop_names <- function(x){
  x_list <- as.list(x)
  x_list[sapply(x_list, is.na)] <- list(NULL) ## Transforms NA values into NULL. This is required for toJSON  to provide the rigth representation of missing data
  names(x_list) <-NULL
  x_list
}
