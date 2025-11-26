# Filter GTFS feed by agency

Filter GTFS feed by agency

## Usage

``` r
filter_by_agency(gtfs, id = NA, name = NA)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- id:

  Integer (Optional when name). Ids of the agency.

- name:

  String (Optional when id). Name of the agency.

## Value

A tidygtfs object with the filtered feed.

## Details

Allows to filter a GTFS feed for the agency, using the id, name or both.
Returns empty feed it none provided.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
gtfs_filtered_by_id <- GTFShift::filter_by_agency(gtfs, agency_id=2)
gtfs_filtered_by_name <- GTFShift::filter_by_agency(gtfs, agency_name="City bus company")
} # }
```
