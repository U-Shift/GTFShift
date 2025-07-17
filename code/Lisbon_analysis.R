library(GTFShift)
library(tidytransit)
library(sf)

date = "2025-05-14"

# Get bbox for Lisbon
aml <- st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
aml <- st_transform(aml, 4326)

lisboa <- aml[aml$Concelho == "Lisboa", ]

bbox = st_bbox(lisboa)

# Get GTFS, filtered by Lisbon area and reference date
data <- read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_list <- lapply(c("lisboa", "AML"), function(ID) {
  feed = GTFShift::load_feed(data$URL[data$ID == ID])
  feed = tidytransit::filter_feed_by_area(feed, bbox)
  feed = tidytransit::filter_feed_by_date(feed, date)
  summary(feed)
  return(feed)
})

gtfs_united <- GTFShift::unify(gtfs_list, create_transfers=FALSE)
summary(gtfs_united)

frequencies_stop <- GTFShift::get_stop_frequency_hourly(gtfs_united, date=date)
summary(frequencies_stop)
mapview(frequencies_stop)
mapview::mapview(frequencies_stop %>% filter(hour==8 & frequency>2), zcol="frequency", legend=TRUE, cex=4, layer.name = "Frequency (hour)")

frequencies_route <- GTFShift::get_route_frequency_hourly(gtfs_united)
summary(frequencies_route)
mapview(frequencies_route)
mapview::mapview(
  frequencies_route %>% filter(arrival_hour==8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)

# Overline
network <- sf::st_read(system.file("extdata", "centerline_carris.gpkg", package = "GTFShift"))

frequencies_route_overline_improved = GTFShift::network_overline(
  network,
  frequencies_route %>% filter(arrival_hour==8),
  attr = "frequency"
)

quantile(frequencies_route_overline_improved$frequency)

mapview::mapview(
  frequencies_route_overline_improved %>% filter(frequency > quantile(frequencies_route_overline_improved$frequency, probs=0.75)),
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)
