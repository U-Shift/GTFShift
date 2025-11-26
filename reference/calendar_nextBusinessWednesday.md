# Get next business Wednesday

Get next business Wednesday

## Usage

``` r
calendar_nextBusinessWednesday(start_date = Sys.Date(), country_code = "PT")
```

## Arguments

- start_date:

  String (Default [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).
  Reference date.

- country_code:

  String (Default PT). Country code in the format `ISO 3166-1 alpha-2`.
  When provided, public holidays are considered.

## Value

Date

## Details

Find the next Wednesday that is not a holiday. When country is given,
public holidays are considered, using
[Nager.Date](https://date.nager.at/Api) API.

## Examples

``` r
if (FALSE) { # \dontrun{
next_wednesday = GTFShift::calendar_nextBusinessWednesday(country_code="PT")
} # }
```
