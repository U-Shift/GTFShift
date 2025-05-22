import osmnx as ox
import pandas as pd
import geopandas as gpd
import neatnet
from shapely.ops import unary_union

def get_centerline(study_area, output_path):
  # Code adapted from https://uscuni.org/neatnet/intro.html by https://github.com/miguelpires01
  
  # --------------------------------------------------------------------------
  # 02. Retrieving the study area polygon
  ## 01. Converting Geocode (text call) to GeoDataFrame
  study_area_gdf = ox.geocode_to_gdf(study_area)
  ## 02. Get projected polygon of the study area (in meters)
  study_area_projected = ox.projection.project_gdf(study_area_gdf)
  
  # --------------------------------------------------------------------------
  # 03.Retrieving "highway" network (and cleaning for processing)
  # 01. Defining custom filter for specific "highway" features
  cf = ('["highway"~"motorway|trunk|primary|'
        'secondary|tertiary|residential|'
        'unclassified|living_street"]'
       )
  cf = cf + '["area"!~"yes"]'  # Exclude areas
  # 02. Calling the network data
  network = ox.graph_from_place(
      study_area,
      network_type='all',
      custom_filter=cf,
      retain_all=False
  )
  
  # 03. Converting the network to GeoDataFrame (edges only)
  network_gdf = ox.graph_to_gdfs(network, nodes=False, edges=True)
  
  # 04. Drop None geometries (if any)
  network_gdf = network_gdf[network_gdf.geometry.notnull()]
  
  # 05. Keep only linear geometries (LineString and MultiLineString)
  network_gdf = network_gdf[network_gdf.geometry.type.isin(['LineString', 'MultiLineString'])]
  
  # 06. Removing geometries with area attributes in columns
  if 'area' in network_gdf.columns:
      network_gdf = network_gdf[network_gdf['area'].isnull()]
  
  # 06. Reproject to EPSG:3857 (for processing)
  network_gdf_3857 = network_gdf.to_crs(epsg=3857)
  
  # 07. Keep only the geometry column
  network_gdf_3857 = network_gdf_3857[['geometry']]
  
  # 08. Explode MultiLineStrings into LineStrings
  def explode_multilines(gdf):
      gdf = gdf.explode(index_parts=False)
      gdf = gdf[gdf.geometry.type == "LineString"]
      return gdf
  
  network_gdf_3857 = explode_multilines(network_gdf_3857)
  
  
  # --------------------------------------------------------------------------
  # 04.Creating "exclusion_mask"
  def filter_ground_level(gdf):
      # Ensure 'layer' column exists
      if "layer" not in gdf.columns:
          gdf["layer"] = "0"
      # Filter out non-ground-level features (where layer != 0)
      gdf = gdf[~gdf["layer"].astype(str).str.contains(r"^-?[1-9]\d*", na=False)]
      return gdf
  ## 01.Retrieving buildings
  buildings = ox.features_from_place(study_area, tags={"building": True})
  
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
  
  ## 02.Retrieving construction areas
  construction = ox.features_from_place(study_area, tags={"landuse": "construction"})
  construction = filter_ground_level(construction)
  ## 03.Retrieving schools
  schools = ox.features_from_place(study_area, tags={"amenity": "school"})
  schools = filter_ground_level(schools)
  ## 04.Retrieving pitches
  pitches = ox.features_from_place(study_area, tags={"leisure": "pitch"})
  pitches = filter_ground_level(pitches)
  ## 05.Retrieving cemeteries
  cemeteries = ox.features_from_place(study_area, tags={"landuse": "cemetery"})
  cemeteries = filter_ground_level(cemeteries)
  ## 06.Reprojecting all features to EPSG:3857
  for gdf in [buildings, construction, schools, pitches, cemeteries]:
      gdf.to_crs(network_gdf_3857.crs, inplace=True)
  ## 07.Combining all "exclusion_mask" geometries
  all_exclusions = pd.concat([
      buildings[['geometry']],
      construction[['geometry']],
      schools[['geometry']],
      pitches[['geometry']],
      cemeteries[['geometry']]    
  ], ignore_index=True)
  ## 08.Dissolving to a single geometry mask
  exclusion_mask = gpd.GeoSeries(unary_union(all_exclusions.geometry), crs=network_gdf_3857.crs)
  
  
  # --------------------------------------------------------------------------
  # 05. Deriving the street centerlines with "neatnet"
  street_lines = neatnet.neatify(network_gdf_3857,  exclusion_mask = exclusion_mask.geometry)
  
  ## 01. Reprojecting layers to EPSG:4326 for compatibility with folium
  street_lines_4326 = street_lines.to_crs(epsg=4326)
  street_lines_4326.to_file(output_path, layer="street_lines", driver="GPKG")
