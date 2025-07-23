# Addapted from https://github.com/metaodi/osmapi/blob/16aeb189c9ff8db607cc118842133b8cd7b60971/examples/oauth2.py
# API docs https://osmapi.metaodi.ch/osmapi/OsmApi.html

# install oauthlib for requests:  pip install requests-oauth2client
from requests_oauth2client import OAuth2Client, OAuth2AuthorizationCodeAuth
import requests
import webbrowser
import osmapi
from dotenv import load_dotenv, find_dotenv
import os
import pandas as pd
import logging

load_dotenv(find_dotenv())

# Logging set up
logger = logging.getLogger('my_logger')
logger.setLevel(logging.DEBUG)  # Set the base logging level
file_handler = logging.FileHandler('../../releases/osm_routes_add_shapes.log')
file_handler.setLevel(logging.DEBUG)
console_handler = logging.StreamHandler()
console_handler.setLevel(logging.INFO)
formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
file_handler.setFormatter(formatter)
console_handler.setFormatter(formatter)
logger.addHandler(file_handler)
logger.addHandler(console_handler)

# Credentials you get from registering a new application
# register here: https://master.apis.dev.openstreetmap.org/oauth2/applications
# or on production: https://www.openstreetmap.org/oauth2/applications
# Parameters:
# Redirect uri: urn:ietf:wg:oauth:2.0:oob
# Permissions: write_api, write_notes, read_prefs

# PROD ------------------------
client_id = os.getenv("OSM_OAUTH_CLIENT_ID_PROD") # To edit env vars on RStudio, use usethis::edit_r_environ()
client_secret = os.getenv("OSM_OAUTH_CLIENT_SECRET_PROD")
authorization_base_url = "https://www.openstreetmap.org/oauth2/authorize"
token_url = "https://www.openstreetmap.org/oauth2/token"
api_url = "https://api.openstreetmap.org"

# DEV  ------------------------
client_id = os.getenv("OSM_OAUTH_CLIENT_ID_DEV") # To edit env vars on RStudio, use usethis::edit_r_environ()
client_secret = os.getenv("OSM_OAUTH_CLIENT_SECRET_DEV")
authorization_base_url = "https://master.apis.dev.openstreetmap.org/oauth2/authorize"
token_url = "https://master.apis.dev.openstreetmap.org/oauth2/token"
api_url = "https://api06.dev.openstreetmap.org"

# Initial parameters
redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
oauth2client = OAuth2Client(
    token_endpoint=token_url,
    authorization_endpoint=authorization_base_url,
    redirect_uri=redirect_uri,
    client_id=client_id,
    client_secret=client_secret,
    auth_method="client_secret_post",
    code_challenge_method=None
)

# Open OSM website to authrorize user using the write_api and write_notes scope
scope = ["write_api", "write_notes", "read_prefs"]
az_request = oauth2client.authorization_request(scope=scope)
print(f"Authorize user using this URL: {az_request.uri}")
webbrowser.open(az_request.uri) # If on studio web, this might not work. If so, just open the link printed in the command before and jump to next line.

# Create a new requests session using the OAuth authorization
auth_code = input("Paste the authorization code here: ") # This code is presented on the authentication page you openned before, right after logging in
auth_code
auth = OAuth2AuthorizationCodeAuth(
    oauth2client,
    auth_code,
    redirect_uri=redirect_uri,
)
oauth_session = requests.Session()
oauth_session.auth = auth

# Test authentication (should return 200 status code)
resp = oauth_session.get(f"{api_url}/api/0.6/user/details")
print(resp.status_code)
print(resp.text)

# Connect to API
api = osmapi.OsmApi(api=api_url, session=oauth_session)

# Load your CSV file
df = pd.read_csv("osm_match.csv") # CSV with columns oms_id, shape_id and osm_id (optional)
df = df[(df['distance_diff'] < 1000) & (df['points_diff'] < 500)] # Filter to only update those that meet threshold

# Create change set, updating relations with tag gtfs:shape_id
# The changeset comment can be customized to better describe the change submitted 
with api.Changeset({"comment": "GTFS shapes association", "review_requested": "no", "locale": "pt", "source": "local knowledge"}) as changeset_id:
  logger.info(f"Running changeset {changeset_id} for {len(df)} relations")
  logger.info(f"{'route_id':20s}{'shape_id':20s} | {'osm_id':20s} | {'osm_route_id':20s}{'updated?':10s}{'osm_shape_id':20s}{'updated?':10s} | {'operation status':20s}")
  for idx, row in df.iterrows():
    route_id = row["route_id"] if "route_id" in row else None
    shape_id = row["shape_id"]
    osm_id = int(row["osm_id"])
    
    relation = api.RelationGet(osm_id)
    relation_shape = relation["tag"]["gtfs:shape_id"] if "gtfs:shape_id" in relation["tag"] else "-"
    relation_route = relation["tag"]["gtfs:route_id"] if "gtfs:route_id" in relation["tag"] else "-"
    
    status = "-"
    if relation_shape != shape_id or route_id and (relation_route != route_id):
      relation["tag"]["gtfs:shape_id"] = shape_id # https://wiki.openstreetmap.org/wiki/Key:gtfs:shape_id
      if route_id:
        relation["tag"]["gtfs:route_id"] = route_id # https://wiki.openstreetmap.org/wiki/Key:gtfs:route_id
      update = api.RelationUpdate(relation)
      status = "Updated"
    else:
      status = "Skipped"
    
    logger.info(f"{route_id:20s}{shape_id:20s} | {osm_id:20d} | {relation_route:20s}{('⬜' if relation_route==route_id else '✏️'):10s}{relation_shape:20s}{('⬜' if relation_shape==shape_id else '✏️'):10s} | {status:20s}")

# Rollback AVOID THIS! It should be used only if you made a mistake in the previous update and you want to rollback your changes
# with api.Changeset({"comment": "GTFS shapes association rollback", "review_requested": "no", "locale": "pt", "source": "local knowledge"}) as changeset_id:
#   for idx, row in df.iterrows():
#     osm_id = int(row["osm_id"])
#     relation = api.RelationGet(osm_id)
#     relation_prev = api.RelationGet(osm_id, RelationVersion=relation["version"]-1)
#     logger.info(f"{osm_id} {relation['tag']['gtfs:shape_id'] if 'gtfs:shape_id' in relation['tag'] else '-'} Current {relation['version']} Previous {relation_prev['version']}")
#     relation_prev["version"] = relation["version"] # We need to set version to last to enable update
#     update = api.RelationUpdate(relation_prev)

# Validate changes
logger.info("Validating changes...")
logger.info(f"{'route_id':20s}{'shape_id':20s} | {'osm_id':20s} | {'osm_route_id':20s}{'osm_route_id==route_id':10s}{'osm_shape_id':20s}{'osm_shape_id==shape_id':10s}")
for idx, row in df.iterrows():
    shape_id = row["shape_id"]
    route_id = row["route_id"] if "route_id" in row else None
    osm_id = int(row["osm_id"])
    
    relation = api.RelationGet(osm_id)
  
    relation_shape = relation["tag"]["gtfs:shape_id"] if "gtfs:shape_id" in relation["tag"] else "-"
    relation_route = relation["tag"]["gtfs:route_id"] if "gtfs:route_id" in relation["tag"] else "-"
  
    if relation_shape!=shape_id or relation_route!=route_id: 
      logger.info(f"{route_id:20s}{shape_id:20s} | {osm_id:20d} | {relation_route:20s}{('✅' if relation_route==route_id else '❌'):10s}{relation_shape:20s}{('✅' if relation_shape==shape_id else '❌'):10s}")
logger.info("Validation finished!")

