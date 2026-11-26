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

Refer to `vignette("classify")` for more details on this classification.

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))

# Get route frequency 
frequency_analysis <- GTFShift::get_route_frequency_hourly(
  gtfs, 
  date = gtfs$calendar$start_date[1]
) 
#> Analysing GTFS for 2026-06-10...
#> > Filtering by reference date 2026-06-10...

# Compute LOS
frequency_los = GTFShift::classify_frequency_los(frequency_analysis)

frequency_los |> 
  sf::st_drop_geometry() |>
  dplyr::select(route_id, frequency_los)
#> # A tibble: 6 × 2
#>   route_id       frequency_los
#>   <chr>          <chr>        
#> 1 3_3-SA-TERM_R2 E            
#> 2 3_3-TER-CS_CAS E            
#> 3 3_3-TER-SA_CAS E            
#> 4 3_3-TER-SA_CAS E            
#> 5 3_3-TERM-SA_LC E            
#> 6 3_3-TERM-SA_R2 E            
```
