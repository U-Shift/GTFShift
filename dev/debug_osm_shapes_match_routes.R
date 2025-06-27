# DEBUG
mapview::mapview(osm_route_name, zcol="osm_id")
mapview::mapview(osm_route_name |> filter(osm_id %in% c(19101947)), zcol="osm_id")

mapview::mapview(gtfs_route_name, zcol="shape_id")
mapview::mapview(gtfs_route_name |> filter(shape_id %in% c("2150_1_2_Q4V5D")), zcol="shape_id")

gtfs_route_name_result$map_name = paste(gtfs_route_name_result$route_short_name," | ",gtfs_route_name_result$shape_id, " | ", gtfs_route_name_result$osm_id, " | ", sprintf("%.1f", gtfs_route_name_result$distance_diff), " | ", sprintf("%.1f", gtfs_route_name_result$points_diff))

mapview::mapview(gtfs_route_name_result, zcol="map_name", layer.name="route_short_name | shape_id | osm_id | distance_diff | points_diff")

