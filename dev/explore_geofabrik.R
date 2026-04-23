library(GTFShift)
library(osmextract)
library(mapview)
library(sf)
library(dplyr)

# Initial validation
oe_download_directory()

# Download data for PT, for today
osm_data <- oe_download(
    "https://download.geofabrik.de/europe/portugal-latest.osm.pbf",
    file_basename = sprintf("%s_%s.osm.pbf", "PT", format(Sys.Date(), "%Y%m%d"))
)
osm_data

# BUS LANES ----------------------------------------------------

# Test bus lanes query
aml <- sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
lisboa <- aml |>
    dplyr::filter(Concelho == "Lisboa") |>
    sf::st_bbox()
mapview(lisboa)

# Get version from osmdata
bus_lanes_osmdata <- sf::st_read("https://github.com/U-Shift/busclar/releases/download/0.1/bus_lanes_osm_lisbon.gpkg", quiet = TRUE)
mapview(bus_lanes_osmdata)

# Get version from osmextract
# > Get agnostic version, with "other_tags" column
bus_lanes_oe_base <- oe_read(osm_data, boundary = lisboa, quiet = FALSE)
head(bus_lanes_oe_base$other_tags)


bus_lanes_oe_other_cols <- oe_get_keys(bus_lanes_oe_base)
bus_lanes_oe_other_cols

cols_to_check_access <- grep("psv:lanes|bus:lanes", bus_lanes_oe_other_cols, value = TRUE)
cols_to_check_count <- grep("lanes:psv|lanes:bus", bus_lanes_oe_other_cols, value = TRUE)
cols_to_check <- c(cols_to_check_access, cols_to_check_count, "psv")
cols_to_check

bus_lanes_oe <- oe_read(
    osm_data,
    boundary = lisboa,
    quiet = FALSE,
    query = "SELECT * FROM lines WHERE highway IS NOT NULL",
    extra_tags = cols_to_check
)
bus_lanes_oe

# Refactor col names to replace "_" by ":"
names(bus_lanes_oe)
names(bus_lanes_oe) <- gsub("_", ":", names(bus_lanes_oe))
names(bus_lanes_oe)

bus_lanes_geofabrik <- GTFShift:::filter_osm_bus_lanes(bus_lanes_oe)
mapview(bus_lanes_geofabrik |> select(-`other:tags`))

bus_lanes_geofabrik_implementation <- GTFShift::osm_bus_lanes(lisboa, osm_file = osm_data)
names(bus_lanes_geofabrik_implementation)
mapview(bus_lanes_geofabrik_implementation |> select(-`other:tags`))

# RELATIONS ----------------------------------------------------
library(rosmium) # Needed to get relations

bus_relations_pbf <- tags_filter(
    osm_data,
    "nwr/route=bus",
    output = paste(oe_download_directory(), "bus_only.osm.pbf", sep = "/"),
    overwrite = TRUE
)


library(sf)

# Read the filtered PBF
# This returns an sf object where the 'members' information is preserved
raw_bus_data <- tags_filter(bus_relations_pbf, "r/route=bus")

# Inspect the members
# This column will contain a list of the types (node/way) and their IDs
head(raw_bus_data$members)

xml <- show_content(bus_relations_pbf, object_type = c("relation"), output_format = "xml")
xml


# HELPERS ---------------------------------------------------------------
regexp_keys <- gregexpr(
    # The other_tags field uses the following structure:
    # "KEY1"=>"VALUE1","KEY2"=>"VALUE2" and so on
    # The following regex should match all characters that:
    # 1. Follow ^" or ," (where ^ denotes the start of a line)
    # and
    # 2. Precede the character "=>" (i.e. the delimiter)
    pattern = '(?<=^\\"|\\",\\").+?(?=\\"=>\\")',
    text = text,
    perl = TRUE
)
