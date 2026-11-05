# GTFShift <img align="right" src="man/figures/logo.png" alt="logo" width="180">

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/GTFShift)](https://cran.r-project.org/package=GTFShift/)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) 
[![CRAN downloads total](https://cranlogs.r-pkg.org/badges/grand-total/GTFShift?color=lightgrey)](https://cran.r-project.org/package=GTFShift)
[![](https://github.com/U-Shift/GTFShift/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/U-Shift/GTFShift/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/U-Shift/GTFShift/graph/badge.svg?token=RWVWEGGOF8)](https://app.codecov.io/gh/U-Shift/GTFShift)
<!-- badges: end -->

**GTFShift** encompasses a complete bundle of methods to harmonize GTFS and OSM data, enabling the integration and exploration of different layers of transit data, starting with the planned operations (GTFS), but also the infrastructure topology (OSM) and real-time information (GTFS-RT).

## Installation

You can install the stable version of **GTFShift** from CRAN:

``` r
install.packages('GTFShift')
```

For the development version, fetch from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("U-Shift/GTFShift")
```

## Load the package

``` r
library(GTFShift)
```

## Get started

For more details on the package and how to get started, please visit the [Get started](https://u-shift.github.io/GTFShift/articles/GTFShift.html) page.


## Main use cases 

### Harmonize GTFS geometries with OSM data 

The lack of standardization on GTFS shape geometries hampers aggregated analysis of different feeds and the association of planned service information with other urban dimensions such as the infrastructure topology.

To solve this problem, **GTFShift** provides a bundle of methods to harmonize GTFS shapes with OSM road network geometries. Refer to [Get OSM data for bus routes](https://u-shift.github.io/GTFShift/articles/osm.html#get-osm-data-for-bus-routes) and [Generate GTFS with OSM geometries](https://u-shift.github.io/GTFShift/articles/gtfs_from_osm.html) for more details.

![](man/figures/geometry_harmonization_barreiro.png)

> Example of GTFS original shapes (salmon) and harmonized GTFS shapes with OSM data (blue) for TCB, Barreiro, Portugal

### Bus Lane Prioritisation

**GTFShift** emerged from the necessity to understand how to get an
overview of where bus lanes should be prioritised for a given territory,
using General Transit Feed Specification (GTFS) and OpenStreetMap (OSM) data.

It provides a comprehensive bundle of methods that cover several dimensions of this 
problem, namely:

-   Frequency of buses (and trams) per hour and direction, at a peak hour;
-   Number of lanes in the same direction;
-   Existing traffic conditions;
-   Existing bus lanes in the area (from a network continuity perspective).

Together, these can be used to identify road segments where bus lanes should be implemented, 
enabling for a transparent and data-driven decision-making process, suitable to different contexts
and criteria. 

![](man/figures/prioritisation.png)

> Example of bus lane prioritisation analysis for Lisbon city, considering road segments with
a minimum frequency of 10 buses/hour, average speed below 9.7 km/h and more than 1 lane per direction. 

#### Dashboard 

**GTFShift** provides an interactive dashboard that allows users to explore and visualize results for real case studies, 
aiming to illustrate its potential and capabilities to a non-technical audience, while disseminating the outputs of these real world scenarios.

Visit it at [ushift.pt/apps/gtfshift](https://ushift.tecnico.ulisboa.pt/apps/gtfshift/).

[![](man/figures/web.png)](https://ushift.tecnico.ulisboa.pt/apps/gtfshift/)

## Spatial domain of applicability

**GTFShift** operates on **two-dimensional (`XY`)** spatial geometries (`POINT`, `LINESTRING`, and `MULTILINESTRING` represented via the `sf` package) across **both curvilinear (geographic/ellipsoidal) and Cartesian (projected/planar) domains**:

- By default (`metric_crs = NULL`), spatial distance, length, and buffer operations use spherical geometry (`s2` via `sf`) directly on geographic coordinates (`EPSG:4326` / WGS 84, as defined by the GTFS and OpenStreetMap specifications).
- Users may optionally supply a local projected coordinate reference system via the `metric_crs` parameter (compliant with `PROJ >= 6` / `WKT2`) to perform planar Cartesian calculations in the units of that projection.

## Lifecycle

**GTFShift** is currently in an **active and stable** state of development (published on CRAN and actively maintained by the [U-Shift](https://ushift.tecnico.ulisboa.pt) research group). Future development focuses on maintaining compatibility with evolving GTFS/GTFS-RT specifications and upstream spatial packages (`sf`, `tidytransit`, `gtfstools`, `osmdata`, `osmextract`, `stplanr`), expanding real-time transit performance indicators, and addressing community bug reports and feature requests via [GitHub Issues](https://github.com/U-Shift/GTFShift/issues).

## Related packages

-   [`{tidytransit}`](https://github.com/r-transit/tidytransit)
-   [`{gtfstools}`](https://github.com/ipea/gtfstools/)
-   [`{gtfsrouter`}](https://github.com/UrbanAnalyst/gtfsrouter)
-   [`{GTFSwizard}`](https://github.com/nelsonquesado/GTFSwizard)

## Acknowledgement

**GTFShift** is developed and maintained by
[U-Shift](https://ushift.tecnico.ulisboa.pt) urban mobility research
group, part of [CERIS](https://ceris.pt/) research unit, at [Instituto
Superior Técnico](https://tecnico.ulisboa.pt/pt/), Lisbon, Portugal.

<br/>

<img src="man/figures/logo_acknowledgement.png" width="75%">

