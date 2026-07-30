# Convert a MULTILINESTRING to a sorted LINESTRING

Convert a MULTILINESTRING to a sorted LINESTRING

## Usage

``` r
multiline_to_sorted_linestring(
  multilinestring,
  points = NULL,
  metric_crs = 3857
)
```

## Arguments

- multilinestring:

  sf object with MULTILINESTRING geometry

- points:

  (Optional) collection of sorted point geometries used to guide
  ordering. If provided, the first point defines the initial segment and
  the second point (when available) is used as tie-break guidance for
  its orientation. Remaining points are used as iterative tie-break
  guidance.

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  distances and lengths during sorting.

## Value

sfc. LINESTRING geometry object.

## Details

The function takes a MULTILINESTRING object and converts it to a
LINESTRING object by sorting the linestrings and combining them in the
correct order.

The algorithm is formulated as follows: Let \\\mathcal{L} = \\L_1,
\dots, L_n\\\\ be the set of individual LINESTRING components. Each
component \\L_i\\ is characterized by its start point \\S(L_i)\\ and end
point \\E(L_i)\\.

**1. Initialization**

If guiding points are provided, let \\\mathrm{start\\point}=P_1\\ be the
first point and \\P_2\\ the second point (if available). The initial
segment is chosen as \$\$L^{(1)} = \operatorname\*{argmin}\_{L \in
\mathcal{L}} d(\mathrm{start\\point}, L).\$\$ where \\d(\cdot)\\ is the
Euclidean distance. If no points are provided, \\L^{(1)} = L_1\\
(assuming the input MULTILINESTRING is ordered).

Additionally, the orientation of \\L^{(1)}\\ is determined by comparing
the distances from its edges to the remaining segments in \\\mathcal{L}
\setminus \\L^{(1)}\\\\. The edge that is closest to any remaining
segment is designated as the end of \\L^{(1)}\\.

If both edges are equidistant to the remaining segments, the orientation
is determined by the proximity to \\P_2\\ (if available) or by orienting
away from \\P_1\\.

**2. Iterative Step**

At iteration \\k\\, with current segment endpoint \\e^{(k)} =
E(L^{(k)})\\, define for each remaining segment \\L \in
\mathcal{R}^{(k)}\\: \$\$d_s(L) = d\\\left(e^{(k)}, S(L)\right), \qquad
d_e(L) = d\\\left(e^{(k)}, E(L)\right).\$\$ Segments with geometry equal
to \\L^{(k)}\\ are excluded. Candidate segments minimize endpoint
proximity: \$\$\mathcal{C}^{(k)} = \left\\L \in \mathcal{R}^{(k)} :
\min\big(d_s(L), d_e(L)\big) = m^{(k)}\right\\, \quad m^{(k)} = \min\_{J
\in \mathcal{R}^{(k)}} \min\big(d_s(J), d_e(J)\big).\$\$ Ties are broken
as follows: \$\$\text{(i) if next unvisited point } Q \text{ exists,
choose closest candidate, minimizing } d(Q,L) \text{ over } L \in
\mathcal{C}^{(k)};\$\$ \$\$\text{(ii) if still tied, choose candidate
closest to the current endpoint } e^{(k)}, \text{ minimizing }
d\\\left(e^{(k)}, S(L)\right) \text{ over } L \in \mathcal{C}^{(k)}.\$\$

**3. Verification and Assembly**

Let \\L^\*\\ be the selected candidate. If \$\$d\\\left(L^{(k)},
L^\*\right) \> \operatorname{len}\\\left(L^{(k)}\right) +
\operatorname{len}(L^\*),\$\$ then \\L^\*\\ is removed from the
remaining set and the loop restarts. Otherwise, \\L^\*\\ is oriented to
connect from \\e^{(k)}\\ and appended to the ordered sequence. When
points are provided, consecutive unvisited points \\Q\\ are marked
visited when \$\$d\\\left(L^{(k+1)}, Q\right) \leq \min\_{J \in
\mathcal{R}^{(k+1)}} d(J, Q).\$\$

The ordered segments are concatenated into a single `LINESTRING` and
transformed back to the original CRS of `multilinestring`.

## Examples

``` r
# Get OSM route geometries (MULTILINESTRING)  
osm_routes <- sf::st_read(
  system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift"),
  quiet = TRUE
) |> dplyr::sample_n(1)

head(osm_routes)
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.078345 ymin: 38.64127 xmax: -9.025695 ymax: 38.66195
#> Geodetic CRS:  WGS 84
#>     osm_id    shape_id      route_id                           geom
#> 1 18957690 2-TERM-QVBB 2_2-TERM-QVBB MULTILINESTRING ((-9.078345...

# Convert geometry to LINESTRING
osm_routes <- osm_routes |>  dplyr::mutate(
  geom = GTFShift::multiline_to_sorted_linestring(geom, metric_crs = 3763)
)

head(osm_routes)
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.078345 ymin: 38.64127 xmax: -9.025695 ymax: 38.66195
#> Geodetic CRS:  WGS 84
#>     osm_id    shape_id      route_id                           geom
#> 1 18957690 2-TERM-QVBB 2_2-TERM-QVBB LINESTRING (-9.078345 38.65...
```
