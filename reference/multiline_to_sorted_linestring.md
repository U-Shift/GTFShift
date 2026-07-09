# Convert a MULTILINESTRING to a sorted LINESTRING

Convert a MULTILINESTRING to a sorted LINESTRING

## Usage

``` r
multiline_to_sorted_linestring(
  multilinestring,
  start_point = NULL,
  metric_crs = 3857
)
```

## Arguments

- multilinestring:

  sf object with MULTILINESTRING geometry

- start_point:

  (Optional) sf point geometry. If provided, the sorting of the
  linestrings will start from this point.

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  distances and lengths during sorting.

## Value

A `sfc` object with LINESTRING geometry.

## Details

The function takes a MULTILINESTRING object and converts it to a
LINESTRING object by sorting the linestrings and combining them in the
correct order.

The algorithm is formulated as follows: Let \\\mathcal{L} = \\L_1,
\dots, L_n\\\\ be the set of individual LINESTRING components. Each
component \\L_i\\ is characterized by its start point \\S(L_i)\\ and end
point \\E(L_i)\\.

**1. Initialization:** Let \\L^{(1)}\\ be the first segment in the
sorted sequence.

If a `start_point` \\P\_{\text{start}}\\ is provided: \$\$L^{(1)} =
\operatorname{argmin}\_{L \in \mathcal{L}} d(P\_{\text{start}}, L)\$\$
where \\d(\cdot)\\ is the Euclidean distance. If \\d(P\_{\text{start}},
S(L^{(1)})) \> d(P\_{\text{start}}, E(L^{(1)}))\\, the component's
geometry is reversed so that it starts near \\P\_{\text{start}}\\.

If no `start_point` is provided: \$\$L^{(1)} = L_1\$\$

The set of remaining segments is initialized as \\\mathcal{R}^{(1)} =
\mathcal{L} \setminus \\L^{(1)}\\\\.

**2. Iterative Step:** For each step \\k \ge 1\\, let \\L^{(k)}\\ be the
current segment and \\E^{(k)} = E(L^{(k)})\\ its endpoint. The algorithm
searches the remaining components \\\mathcal{R}^{(k)}\\ for the closest
segment: \$\$L\_{\text{start}} = \operatorname{argmin}\_{L \in
\mathcal{R}^{(k)}} d(E^{(k)}, S(L))\$\$ \$\$L\_{\text{end}} =
\operatorname{argmin}\_{L \in \mathcal{R}^{(k)}} d(E^{(k)}, E(L))\$\$
Let \\d\_{\text{start}} = d(E^{(k)}, S(L\_{\text{start}}))\\ and
\\d\_{\text{end}} = d(E^{(k)}, E(L\_{\text{end}}))\\. The candidate
segment \\L^\*\\ is: \$\$L^\* = \begin{cases} L\_{\text{start}} &
\text{if } d\_{\text{start}} \leq d\_{\text{end}} \\ L\_{\text{end}} &
\text{otherwise} \end{cases}\$\$

**3. Verification & Assembly:** If the distance between the current
segment and the candidate exceeds their combined length: \$\$d(L^{(k)},
L^\*) \> \text{len}(L^{(k)}) + \text{len}(L^\*)\$\$ then \\L^\*\\ is
discarded, \\\mathcal{R}^{(k+1)} = \mathcal{R}^{(k)} \setminus
\\L^\*\\\\ and we find the next candidate. Otherwise, \\L^{(k+1)} =
L^\*\\ (reversed if \\L^\* = L\_{\text{end}}\\) and
\\\mathcal{R}^{(k+1)} = \mathcal{R}^{(k)} \setminus \\L^\*\\\\.

The process repeats until no segments remain, and the components are
merged into a single `LINESTRING`.
