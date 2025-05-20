
# GTFShift

<!-- badges: start -->
<!-- badges: end -->

GTFShift emerged from the necessity to understand how to get an overview of where bus lanes 
should be prioritized for a given territory, using General Transit Feed Specification (GTFS) files.

It compiles the methods developed for this purpose, aiming to contribute to an open source culture.

## Installation

You can install the development version of GTFShift from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("GTFShift/GTFShift")
```

## Load the package

```{r setup}
library(GTFShift)
```

## Key functions

GTFShift provides methods for the entire workflow of bus network density analysis. 
For detailed examples on their functionality, refer to the articles at https://u-shift.github.io/GTFShift/.

### Read
Starting with the load of the GTFS feed, with `load_feed()`, fixing any integrity errors that 
might hinder the analysis process. 

### Manipulate 
Then, to expand the scope analysis, it includes a method for aggregating multiple feeds, `unify()`. 

### Filter
Narrowing the scope is also possible, using filtering methods according to multiple parameters, such as 
`filter_by_agency()`, `filter_by_modes()` and `filter_by_route_name()`. 

### Analyse
Finally, network density can be analyzed at the stop level, with `get_stop_frequency_hourly()`, or at the 
route level, with `get_route_frequency_hourly()`. This analysis is aggregated by stop, route, and hour.

### External data
If the GTFS feed location is unknown, it can be queried using `query_mobilitydatabase()`, a method that 
asks the Mobility Database API for the feeds that match the parameters provided, such as the municipality, 
country, or even a boundary box.


