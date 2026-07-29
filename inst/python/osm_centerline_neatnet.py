import osmnx as ox
import pandas as pd
import geopandas as gpd
import neatnet
from shapely.ops import unary_union


def safe_features(bbox, study_area, tags):
    try:
        return (ox.features_from_bbox if bbox else ox.features_from_place)(
            bbox if bbox else study_area, tags=tags
        )
    except ox._errors.InsufficientResponseError:
        return None


def explode_multilines(gdf):
    gdf = gdf.explode(index_parts=False)
    gdf = gdf[gdf.geometry.type == "LineString"]
    return gdf


def filter_ground_level(gdf):
    if gdf is None:
        return None
    # Ensure 'layer' column exists
    if "layer" not in gdf.columns:
        gdf["layer"] = "0"
    # Filter out non-ground-level features (where layer != 0)
    gdf = gdf[~gdf["layer"].astype(str).str.contains(r"^-?[1-9]\d*", na=False)]
    return gdf


def get_centerline(bbox, study_area, use_buildings, output_path, osm_file=None):
    # Code adapted from https://uscuni.org/neatnet/intro.html by https://github.com/miguelpires01

    if osm_file is not None:
        # pyrefly: ignore [missing-import]
        import pyrosm

        # Initialize pyrosm with local PBF file
        osm = pyrosm.OSM(osm_file, bounding_box=bbox if bbox else None)

        # --------------------------------------------------------------------------
        # 1 Retrieving "highway" network
        network_gdf = osm.get_network(network_type="all")

        if network_gdf is not None and not network_gdf.empty:
            highway_types = [
                "motorway",
                "trunk",
                "primary",
                "secondary",
                "tertiary",
                "residential",
                "unclassified",
                "living_street",
            ]
            if "highway" in network_gdf.columns:
                network_gdf = network_gdf[network_gdf["highway"].isin(highway_types)]
    else:
        # --------------------------------------------------------------------------
        # 1 Retrieving "highway" network (and cleaning for processing)
        ## 1.1 Defining custom filter for specific "highway" features
        cf = (
            '["highway"~"motorway|trunk|primary|'
            "secondary|tertiary|residential|"
            'unclassified|living_street"]'
        )
        cf = cf + '["area"!~"yes"]'  # Exclude areas
        ## 1.2 Calling OSM for the network data
        network = (ox.graph_from_bbox if bbox else ox.graph_from_place)(
            bbox if bbox else study_area,
            network_type="all",
            custom_filter=cf,
            retain_all=False,
        )

        ## 1.3 Converting the network to GeoDataFrame (edges only)
        network_gdf = ox.graph_to_gdfs(network, nodes=False, edges=True)

    ## 1.4 Drop None geometries (if any)
    network_gdf = network_gdf[network_gdf.geometry.notnull()]

    ## 1.5 Keep only linear geometries (LineString and MultiLineString)
    network_gdf = network_gdf[
        network_gdf.geometry.type.isin(["LineString", "MultiLineString"])
    ]

    ## 1.6 Removing geometries with area attributes in columns
    if "area" in network_gdf.columns:
        network_gdf = network_gdf[network_gdf["area"].isnull()]

    ## 1.7 Reproject to EPSG:3857 (for processing)
    network_gdf_3857 = network_gdf.to_crs(epsg=3857)

    ## 1.8 Keep only the geometry column
    network_gdf_3857 = network_gdf_3857[["geometry"]]

    ## 1.9 Explode MultiLineStrings into LineStrings
    network_gdf_3857 = explode_multilines(network_gdf_3857)

    # --------------------------------------------------------------------------
    # 2 Creating "exclusion_mask" with OSM building footprints
    if use_buildings:
        if osm_file is not None:
            buildings = osm.get_buildings()
            construction = osm.get_landuse(custom_filter={"landuse": ["construction"]})
            schools = osm.get_pois(custom_filter={"amenity": ["school"]})
            pitches = osm.get_pois(custom_filter={"leisure": ["pitch"]})
            cemeteries = osm.get_landuse(custom_filter={"landuse": ["cemetery"]})
        else:
            ## 2.1 Retrieving buildings
            buildings = safe_features(bbox, study_area, {"building": True})
            ## 2.2 Retrieving construction areas
            construction = safe_features(bbox, study_area, {"landuse": "construction"})
            ## 2.3 Retrieving schools
            schools = safe_features(bbox, study_area, {"amenity": "school"})
            ## 2.4 Retrieving pitches
            pitches = safe_features(bbox, study_area, {"leisure": "pitch"})
            ## 2.5 Retrieving cemeteries
            cemeteries = safe_features(bbox, study_area, {"landuse": "cemetery"})

        if buildings is not None and not buildings.empty:
            # Apply filters only if the columns exist
            if "building" in buildings.columns:
                buildings = buildings[buildings["building"] != "roof"]
                buildings = buildings[buildings["building"] != "container"]
                buildings = buildings[buildings["building"] != "kiosk"]
                buildings = buildings[buildings["building"] != "memorial"]
                buildings = buildings[buildings["building"] != "service"]
                buildings = buildings[buildings["building"] != "guardhouse"]

            if "amenity" in buildings.columns:
                buildings = buildings[buildings["amenity"] != "shelter"]
                buildings = buildings[buildings["amenity"] != "fountain"]
                buildings = buildings[buildings["amenity"] != "toilets"]

            if "artwork_type" in buildings.columns:
                buildings = buildings[buildings["artwork_type"] != "statue"]

            if "historic" in buildings.columns:
                buildings = buildings[buildings["historic"] != "monument"]
                buildings = buildings[buildings["historic"] != "memorial"]

            if "memorial" in buildings.columns:
                buildings = buildings[buildings["memorial"] != "statue"]
                buildings = buildings[buildings["memorial"] != "bust"]

            if "shop" in buildings.columns:
                buildings = buildings[buildings["shop"] != "kiosk"]

            if "bridge:support" in buildings.columns:
                buildings = buildings[buildings["bridge:support"] != "yes"]

            buildings = filter_ground_level(buildings)

        if construction is not None and not construction.empty:
            construction = filter_ground_level(construction)
        if schools is not None and not schools.empty:
            schools = filter_ground_level(schools)
        if pitches is not None and not pitches.empty:
            pitches = filter_ground_level(pitches)
        if cemeteries is not None and not cemeteries.empty:
            cemeteries = filter_ground_level(cemeteries)

        ## 2.6 Reprojecting all features to EPSG:3857
        for gdf in [buildings, construction, schools, pitches, cemeteries]:
            if gdf is not None and not gdf.empty:
                gdf.to_crs(network_gdf_3857.crs, inplace=True)

        ## 2.7 Combining all "exclusion_mask" geometries
        valid_exclusions = [
            gdf[["geometry"]]
            for gdf in [buildings, construction, schools, pitches, cemeteries]
            if gdf is not None and not gdf.empty
        ]
        if valid_exclusions:
            all_exclusions = pd.concat(valid_exclusions, ignore_index=True)
        else:
            all_exclusions = pd.DataFrame()


        ## 2.8 Dissolving to a single geometry mask
        if not all_exclusions.empty:
            exclusion_mask = gpd.GeoSeries(
                unary_union(all_exclusions.geometry), crs=network_gdf_3857.crs
            )
        else:
            exclusion_mask = None
    else:
        exclusion_mask = None

    # --------------------------------------------------------------------------
    # 3 Deriving the street centerlines with "neatnet"
    street_lines = neatnet.neatify(
        network_gdf_3857,
        exclusion_mask=exclusion_mask if (use_buildings and exclusion_mask is not None) else None,
    )


    # --------------------------------------------------------------------------
    # 4 Reprojecting layers to EPSG:4326 and storing to output path provided
    street_lines_4326 = street_lines.to_crs(epsg=4326)
    street_lines_4326.to_file(output_path, layer="street_lines", driver="GPKG")

    return street_lines_4326

