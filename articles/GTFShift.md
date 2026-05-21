# Get Started

## Installation

You can install the development version of `GTFShift` from GitHub with:

``` r

# install.packages("remotes")
remotes::install_github("U-Shift/GTFShift")
```

## Load the package

``` r

library(GTFShift)
```

## Key functions

**GTFShift** provides methods for the entire workflow of bus lane
implementation prioritization, but also other useful functions for GTFS
and OSM data gathering and manipulation. For detailed examples on their
functionality, refer to the
[articles](https://u-shift.github.io/GTFShift/articles/index.md).

#### [Prioritize](https://u-shift.github.io/GTFShift/articles/prioritize.md)

The main purpose of **GTFShift** is to support the decision-making
process for bus lane implementation prioritization. This article
presents a step-by-step guide on how to use the package to achieve this
goal, from data gathering to analysis and visualization.

#### [Getting transit data](https://u-shift.github.io/GTFShift/articles/download.md)

Starting with a valid GTFS feed is the key for a successful analysis.
**GTFShift** includes a method to load feeds that simultaneously scans
for any integrity errors and fixes them automatically.

If the feed location is unknown, it also provides a database listing
GTFS for Portugal and a method to query worldwide open catalogues by
city or country names or even a bounding box.

#### [Filter](https://u-shift.github.io/GTFShift/articles/filter.md)

GTFS feeds do not have a defined scope regarding its coverage of the
transportation system. Some can be bounded to one agency, whereas others
can aggregate several modes in the same city, or even national wise.

From the simpler to the most complex feeds, some analysis require to
narrow the perspective. **GTFShift** provides some to help in this
process.

#### [Aggregate](https://u-shift.github.io/GTFShift/articles/unify.md)

Public transit analysis takes advantage of the standardized GTFS format.
However, its provision by operator makes it difficult for network
aggregated analysis, considering connectivity and multimodality.

**GTFShift** includes a method to easily generate an aggregated GTFS
file given several instances.

![](figures/unify.png)

> Aggregated GTFS for Fertagus and Transportes Coletivos do Barreiro
> operators

#### [Analyse](https://u-shift.github.io/GTFShift/articles/analyse.md)

Analyzing public transit feeds is important to understand its
territorial coverage and dynamics, both on its spatial and temporal
dimensions.

**GTFShift** provides several methods that encapsulate pre-defined
methodologies for them, for instance, analysing hourly frequency per
stop, route or road segment.

![](figures/analyse_aggregated_frequencies.png)

> Aggregated route frequency for Carris Lisboa operator, at 8:00

#### [OSM Data](https://u-shift.github.io/GTFShift/articles/osm.md)

OpenStreetMaps (OSM) is an important data source for transit analysis,
due to its rich, open, and detailed geographic data.

**GTFShift** includes some methods that allow to access its information
directly, namely to export bus lanes, get centerlines for the road
network and export the OSM transit routes.

![](figures/osm_buslanes.png)

> OSM exported bus lanes for Lisbon

#### [Real Time transit data](https://u-shift.github.io/GTFShift/articles/rt.md)

Real time operational data provides valuable insights about how planned
operation performs in practice and how it interacts with the urban
traffic conditions.

**GTFShift** provides several methods to enable this data collection and
analysis.

#### [Contribute to OSM](https://u-shift.github.io/GTFShift/articles/osm_update.md)

OpenStreetMap (OSM) is a collaborative project that relies on community
contributions.

**GTFShift** includes methods to facilitate the process of contributing
to OSM, specifically for updating bus lane information based on the
analysis results.
