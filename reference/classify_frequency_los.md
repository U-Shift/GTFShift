# Classify bus frequency level of service based on HCM

Classify bus frequency level of service based on HCM

## Usage

``` r
classify_frequency_los(frequencies, frequency_col = "frequency")
```

## Arguments

- frequencies:

  data.frame. Data frame with frequency information.

- frequency_col:

  String (Default "frequency"). Name of the column with frequency
  values.

## Value

data.frame. Input data frame with an additional column `frequency_los`
indicating the LOS classification.

## Details

Classifies bus frequency level of service (LOS) based on the Highway
Capacity Manual (HCM) 2000 guidelines on "Service Frequency LOS for
Urban Scheduled Transit Service" (Exhibit 27-1).

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs = GTFShift::load_feed("gtfs.zip")
frequency_analysis = GTFShift::get_route_frequency_hourly(gtfs)
frequency_los = GTFShift::classify_frequency_los(frequency_analysis)
} # }
```
