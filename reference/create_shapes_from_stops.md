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

The gtfs feed with the shapes table defined and the trips table updated
with the matching shape_id.

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
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
gtfs$shapes <- GTFShift::create_shapes_from_stops(gtfs)
} # }
```
