% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_by_route_name.R
\name{filter_by_route_name}
\alias{filter_by_route_name}
\title{Filter GTFS feed by route name}
\usage{
filter_by_route_name(gtfs, values, short_name = TRUE, exact_match = TRUE)
}
\arguments{
\item{gtfs}{tidygtfs. GTFS feed.}

\item{values}{String[]. List of the route names to filter the feed.}

\item{short_name}{Boolean. If true, query for route_short_name, otherwise, route_long_name is considered.}

\item{exact_match}{Boolean. If true, route name is queried for an exact match, otherwise, partial match is considered.}
}
\value{
A `tidygtfs` object with the filtered feed.
}
\description{
Filter GTFS feed by route name
}
\details{
On a GTFS feed, the `route_id` rarely matches the real name of the route, that can range from numbers, letters, words or combinations of both.
This method allows to filter the feed for the route short or long name, with a partial or exact match.
}
\examples{
\dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
gtfs_filtered <- GTFShift::filter_by_route_name(gtfs, list("Blue line", "Red line"))
}

}
