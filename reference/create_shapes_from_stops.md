# Build shapes from GTFS stops data

Build shapes from GTFS stops data

## Usage

``` r
create_shapes_from_stops(gtfs)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

## Value

tidygtfs. The GTFS feed with the shapes table defined and the trips
table updated with the matching shape_id.

## Details

The function builds the shapes.txt file from the stop_times.txt and
stops.txt files, by grouping trips with the same stop sequence and
assigning them the same shape_id. The resulting shapes are a simplified
version of the original ones, as they do not take into account the
actual path followed by the vehicles, but only the stop sequence. This
can be useful for some applications that do not require high precision
in the shapes, and can be used as a fallback when the original feed does
not include shapes.txt file.

## Examples

``` r
# Load GTFS without shapes
gtfs <- tidytransit::read_gtfs(
  system.file("extdata/samples", "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
)

summary(gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, calendar, calendar_dates, stops
#> agency       TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips      100
#> # stop_ids    3
#> # stop_names  3
#> # shapes      0

# Create shapes from GTFS stops data
gtfs_with_shapes <- GTFShift::create_shapes_from_stops(gtfs)

head(gtfs_with_shapes$shapes)
#> # A tibble: 6 × 4
#>   shape_id shape_pt_sequence shape_pt_lon shape_pt_lat
#>   <chr>                <int>        <dbl>        <dbl>
#> 1 shape-1                  1        -9.08         38.7
#> 2 shape-1                  2        -9.10         38.6
#> 3 shape-2                  1        -9.10         38.6
#> 4 shape-2                  2        -9.08         38.7
#> 5 shape-3                  1        -9.10         38.6
#> 6 shape-3                  2        -9.15         38.7

head(
  gtfs_with_shapes$trips |> 
    dplyr::select(trip_id, shape_id) |> 
    dplyr::distinct(shape_id, .keep_all = TRUE)
)
#> # A tibble: 5 × 2
#>   trip_id     shape_id
#>   <chr>       <chr>   
#> 1 0303A0800D1 shape-3 
#> 2 0305A0830D1 shape-4 
#> 3 0303A1015S9 shape-2 
#> 4 0316A1030S9 shape-1 
#> 5 0305A1045S9 shape-5 

summary(gtfs_with_shapes)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, calendar, calendar_dates, stops
#> agency       TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips      100
#> # stop_ids    3
#> # stop_names  3
#> # shapes      5
```
