# Query Mobility Database API for GTFS feeds

Query Mobility Database API for GTFS feeds

## Usage

``` r
query_mobilitydatabase(
  access_token = NA,
  refresh_token = NA,
  bounding_filter_method = "partially_enclosed",
  limit = 10,
  offset = 0,
  country_code = NA,
  subdivision_name = NA,
  municipality = NA,
  bbox = NA,
  is_official = NA
)
```

## Arguments

- access_token:

  String (Optional when refresh_token). Access token.

- refresh_token:

  String (Optional when access_token). Refresh token.

- bounding_filter_method:

  String (Default partially_enclosed). Filtering method to use with the
  dataset_latitudes and dataset_longitudes parameters.

- limit:

  Integer (Default 10). The number of items to be returned.

- offset:

  Integer (Default 0). Offset of the first item to return.

- country_code:

  String (Optional). Filter feeds by their exact country code.

- subdivision_name:

  String (Optional). List only feeds with the specified value. Can be a
  partial match.

- municipality:

  String (Optional). List only feeds with the specified value. Can be a
  partial match. Case insensitive.

- bbox:

  bbox (Optional). Area from which to get GTFS feeds. Converted to API
  dataset_latitudes and dataset_longitudes URL parameters.

- is_official:

  Boolean (Optional). If TRUE, only return official feeds.

## Value

data.frame. Query results from Mobility Database.

## Details

This method queries [Mobility Database](https://mobilitydatabase.org/)
API, allowing to get a list of GTFS feeds documented at this platform.
To use it, an access or a refresh token must be provided. It can be
obtained for free at Mobility Database website.  
  
For more details on the parameters, refer to
<https://mobilitydata.github.io/mobility-feed-api/SwaggerUI/index.html#/feeds/getGtfsFeeds>.  
  
Some useful columns of the returned data.frame (refer to the API
documentation for a full list) are:

- provider:

  The name of the GTFS provider.

- status:

  Tells if the feed is active, inactive or deprecated.

- producer_url:

  The GTFS feed URL. Can be used to download.

## Examples

``` r
feeds <- GTFShift::query_mobilitydatabase(
  refresh_token = Sys.getenv("MOBILITY_DATABASE"),
  country_code = "PT",
  is_official = TRUE
)

head(feeds |> dplyr::select(id, provider, producer_url))
#>          id                         provider
#> 1  tld-4316       A Onda - Mobilidade Urbana
#> 2  tld-4318 Apanha-me! - Transportes Urbanos
#> 3  tld-4257                         AUTNA SL
#> 4 tdg-52605                    BlaBlaCar Bus
#> 5  mdb-2929                           Carris
#> 6  mdb-2027             Carris Metropolitana
#>                                                                       producer_url
#> 1 https://drive.google.com/uc?export=download&id=1aPfsxHqopxxcjV8HlRzImzxh_a6zRxGp
#> 2 https://drive.google.com/uc?export=download&id=1w92h129CWNSoImBRZQOWT6KRPzSFwJ42
#> 3 https://drive.google.com/uc?export=download&id=1gah1x10RyFu7gJPweBcCXPd9vcFJFQ7c
#> 4   https://www.data.gouv.fr/api/1/datasets/r/fd54f81f-4389-4e73-be75-491133d011c3
#> 5                            https://gateway.carris.pt/gateway/gtfs/api/v2.11/GTFS
#> 6                                          https://api.carrismetropolitana.pt/gtfs
```
