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

Date. The next business Wednesday date.

## Details

Find the next Wednesday that is not a holiday. When country is given,
public holidays are considered, using
[Nager.Date](https://date.nager.at/Api) API.

## Examples

``` r
# Example of Portuguese holiday (10/06/2026) ignored
GTFShift::calendar_nextBusinessWednesday(start_date = "2026-06-09", country_code="PT")
#> [1] "2026-06-17"

# Example of Hong Kong holiday (01/07/2026) ignored
GTFShift::calendar_nextBusinessWednesday(start_date = "2026-06-30", country_code="HK")
#> [1] "2026-07-08"
```
