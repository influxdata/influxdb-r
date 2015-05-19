\name{influxdb_write}
\alias{influxdb_write}
\title{Write data to an InfluxDB database}
\usage{
influxdb_write(host, port, username, password, database, series,
  time_precision = c("s", "m", "u"))
}
\arguments{
  \item{host}{Character vector with IP address or hostname}

  \item{port}{Port number}

  \item{username}{InfluxDB user}

  \item{password}{InfluxDB password (will be passed in
  plain text)}

  \item{database}{The name of the database}

  \item{series}{A named list of data frames, where the
  names are the series names, and the data frames contain
  the points.}

  \item{time_precision}{Specifies whether the time should
  be returned in seconds (\code{s}), milliseconds
  (\code{m}), or microseconds (\code{u}) from epoch
  (January 1, 1970, 00:00:00 UTC).}
}
\value{
The httr response object (invisibly)
}
\description{
Write data to an InfluxDB database
}

