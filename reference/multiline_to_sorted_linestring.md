# Convert a MULTILINESTRING to a sorted LINESTRING

Convert a MULTILINESTRING to a sorted LINESTRING

## Usage

``` r
multiline_to_sorted_linestring(multilinestring, start_point = NULL)
```

## Arguments

- multilinestring:

  sf object with MULTILINESTRING geometry

- start_point:

  (Optional) sf point geometry. If provided, the sorting of the
  linestrings will start from this point.

## Value

A `sf` object with LINESTRING geometry.

## Details

The function takes a MULTILINESTRING object and converts it to a
LINESTRING object by sorting the linestrings and combining them in the
correct order.

The sorting criteria is based on the geometric distance between the
endpoint of the current linestring and the endpoints of the remaining
linestrings. The linestring with the minimum distance to the endpoint of
the current linestring is selected as the next linestring. The algorithm
starts either from the `start_point` (if provided) or from the first
linestring in the MULTILINESTRING object and continues until all
linestrings are sorted.
