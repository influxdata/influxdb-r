influxdb-r
==========

R library for InfluxDB. Currently only supports querying.

Install using devtools:

```
> if (!require(devtools))
    install.packages('devtools')
> devtools::install_github('influxdb-r', 'influxdb')
```

Read from InfluxDB
===
```
> library(influxdb)
> results <- influxdb_query('sandbox.influxdb.org', 9061, 'myLogin', 'myPassword', 'myDataBase', 'SELECT * FROM /.*/ order asc limit 10;')
> print(results)
$name.of.my.serie
        time sequence_number             email state value
1 1386405189          802637       foo@bar.com    CO 191.3
2 1386405182          802636 paul@influxdb.org    NY    23

$name.of.an.other.serie
        time sequence_number        email state value
1 1386405625          802640 baz@quux.com    MA    63


> summary(results$name.of.my.serie$value)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   65.08  107.20  107.20  149.20  191.30 
```

Write to InfluxDB
===
```
> library(influxdb)
> country <- data.frame(countryId=c(33, 86, 32), countryName=c("France","China","Belgium"))
> userStatus <- data.frame(userId=c(122,333), countryName=c("online","offline"))
> all <- structure(list(country,userStatus), names=c("country.serie", "userStatus"))
> influxdb_write('sandbox.influxdb.org', 9061, 'myLogin', 'myPassword', 'myDataBase', all)
```


Pull request proposal By [Anomaly Detection](https://anomaly.io)
