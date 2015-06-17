influxdb-r
==========

# This library is not updated for InfluxDB 0.9.0. There are breaking changes to the API, use at your own risk.

We encourage you to submit a pull request if you have a contribution.

----------



R library for InfluxDB. Currently only supports querying.

Install using devtools:

```
> if (!require(devtools))
    install.packages('devtools')
> devtools::install_github('influxdb-r', 'influxdb')
```

Example usage:

```
> library(influxdb)
> results <- influxdb_query('sandbox.influxdb.org', 9061, 'jcheng', 'xxxxxxxx', 'joetest',
    'SELECT * FROM /.*/')
$some_series
        time sequence_number             email state value
1 1386405189          802637       foo@bar.com    CO 191.3
2 1386405182          802636 paul@influxdb.org    NY    23

$some_series2
        time sequence_number        email state value
1 1386405625          802640 baz@quux.com    MA    63
> summary(results$some_series$value)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   65.08  107.20  107.20  149.20  191.30 
```
