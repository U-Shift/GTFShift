#' srr_stats
#'
#' Package-level statistical software standards compliance and remaining TODOs.
#' Individual function and test compliance tags are distributed across `R/` and
#' `tests/testthat/`.
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} Primary academic reference is documented in `DESCRIPTION` and `README.md` (Barata et al., 2023, <https://doi.org/10.5281/zenodo.8124235>), alongside the Highway Capacity Manual (HCM 2000) in `classify_frequency_los()`.
#' @srrstats {G1.1} Package documentation, `README.md`, and function-level `@details` document how `GTFShift` provides novel GTFS-to-OSM route matching (`osm_shapes_match_routes()`), topological multilinestring sorting (`multiline_to_sorted_linestring()`), real-time speed profiling (`rt_average_speed()`, `get_trip_speed_profile()`), and integrated bus lane prioritisation (`prioritise_lanes()`) building upon `tidytransit`, `gtfstools`, `sf`, `osmdata`, `osmextract`, and `stplanr`.
#' @srrstats {G1.3} All statistical, transit, and geometric terminology (such as commercial speed, alternative commercial speed, percentiles, interquartile range, closeness metric, and level of service) is unambiguously defined using mathematical formulas and prose in function `@details`.
#' @srrstats {G1.4} All exported functions are documented using `roxygen2`.
#' @srrstats {EA1.0} Target audiences (public transport planners, urban mobility researchers, and transit agencies/operators) are identified in `DESCRIPTION`, `README.md`, and package vignettes.
#' @srrstats {EA1.1} The kinds of data the software analyses (static GTFS schedules, GTFS-Realtime vehicle position feeds in JSON and Protocol Buffers formats, and OpenStreetMap road networks) are documented in `DESCRIPTION`, `README.md`, and function documentation.
#' @srrstats {EA1.2} The exploratory and analytical questions addressed by the software (network extension, stop/route/way service frequencies, level of service classification, bus lane prioritisation, and real-time commercial speed profiling) are identified in `README.md` and vignettes.
#' @srrstats {SP2.1} `GTFShift` uses the `sf` package exclusively for all spatial representations and operations and does not use `sp`.
#' @srrstats {SP2.2} `GTFShift` ensures compatibility with established spatial and transit R packages (`sf`, `tidytransit`, `gtfstools`, `gtfsrouter`, `osmdata`, `osmextract`, `stplanr`).
#' @srrstats {SP2.2a} Documentation, `@examples`, and vignettes demonstrate how `GTFShift` routines integrate directly into workflows using `sf`, `tidytransit`, `osmdata`, `osmextract`, and `stplanr`.
#'
#' @srrstatsTODO {G1.2} *Statistical Software should include a* Life Cycle Statement *describing current and anticipated future states of development.*
#' @srrstatsTODO {SP1.0} *Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in Cartesian/rectilinear/geometric domains, curvilinear/geographic domains, or both.*
#' @srrstatsTODO {SP1.1} *Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or three dimensions only, or whether there are any other restrictions on dimensionality.*
#' @srrstatsTODO {SP6.2} *Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees.*
#' @noRd
NULL

#' NA_standards
#'
#' Non-applicable standards with justifications for why each standard is deemed
#' not applicable to `GTFShift`.
#'
#' @srrstatsNA {G1.5} Associated publications focus on urban transit planning methodology and case studies rather than making computational speed or performance claims.
#' @srrstatsNA {G1.6} `GTFShift` does not make computational performance claims against alternative implementations in other R packages.
#' @srrstatsNA {G2.4d} `GTFShift` does not require or perform internal conversions to `factor` types.
#' @srrstatsNA {G2.4e} `GTFShift` does not accept or process `factor` inputs requiring conversion.
#' @srrstatsNA {G2.5} No functions in `GTFShift` expect inputs of `factor` type.
#' @srrstatsNA {G2.14} Functions in `GTFShift` operate on standardized GTFS and OpenStreetMap data structures where missingness handling is domain-specific (such as linearly interpolating missing `stop_times` in `load_feed()` or filtering `NA` speed observations in `get_trip_speed_profile()` and `rt_extend_prioritisation()`) rather than controlled via generic user-specified `NA` handling options.
#' @srrstatsNA {G2.14a} Missing data in GTFS and real-time feeds are handled according to domain semantics (structural validation errors for missing required columns/CRS vs. exclusion of `NA` speed observations) rather than a generic error-on-NA toggle.
#' @srrstatsNA {G2.14b} Missing data handling is domain-specific (e.g., `load_feed()` warns when interpolating missing `stop_times` or generating missing `shapes.txt`, while summary functions exclude `NA` observations via `na.rm = TRUE`).
#' @srrstatsNA {G2.14c} `GTFShift` does not perform statistical imputation of missing values beyond domain-specific linear stop-time interpolation in `load_feed()`.
#' @srrstatsNA {G3.1} `GTFShift` does not perform covariance or correlation matrix calculations.
#' @srrstatsNA {G3.1a} `GTFShift` does not perform covariance calculations.
#' @srrstatsNA {G4.0} File output parameters in `load_feed()`, `unify()`, `rt_collect_json()`, and `rt_collect_protobuf()` accept explicit user-specified file paths passed directly to `tidytransit::write_gtfs()` or `utils::write.table()` rather than auto-generating file suffixes.
#' @srrstatsNA {G5.4c} Correctness tests validate exact analytical and geometric properties directly rather than relying on stored values from external closed-source paper outputs.
#' @srrstatsNA {G5.5} All algorithms implemented in `GTFShift` are deterministic and do not involve stochastic or random number generation components.
#' @srrstatsNA {G5.6} `GTFShift` implements deterministic spatial, schedule, and network operations rather than statistical parameter estimation models.
#' @srrstatsNA {G5.6a} `GTFShift` does not estimate statistical model parameters.
#' @srrstatsNA {G5.6b} `GTFShift` does not contain stochastic data simulation or randomized estimation algorithms.
#' @srrstatsNA {G5.7} `GTFShift` implements deterministic geometric and tabular aggregations rather than iterative or asymptotic statistical estimators.
#' @srrstatsNA {G5.9} `GTFShift` algorithms are deterministic geometric, network matching, and schedule aggregation routines rather than stochastic estimators susceptible to numerical noise.
#' @srrstatsNA {G5.9a} Deterministic spatial and schedule aggregation outputs scale continuously with coordinate geometry and do not involve ill-conditioned matrix inversions or stochastic optimization.
#' @srrstatsNA {G5.9b} `GTFShift` algorithms are completely deterministic and do not depend on random seeds or stochastic initial conditions.
#' @srrstatsNA {G5.10} All package tests execute quickly within the standard `testthat` suite using bundled sample datasets in `inst/extdata/samples` without requiring separate extended test environment flags.
#' @srrstatsNA {G5.11} Extended tests requiring large external dataset downloads are not needed; all test fixtures are bundled in `inst/extdata/samples`.
#' @srrstatsNA {G5.11a} Tests do not download large external datasets; network-dependent API queries are mocked in unit tests.
#' @srrstatsNA {G5.12} No separate extended test suite is required beyond the standard `testthat` suite.
#' @srrstatsNA {EA2.0} `GTFShift` operates on standardized GTFS relational feeds (validated via `tidytransit` and `gtfstools` with specification-defined keys such as `trip_id`, `route_id`, `stop_id`, and `shape_id`) and OpenStreetMap `sf` tables rather than arbitrary user-supplied tables requiring custom index column attributes.
#' @srrstatsNA {EA2.1} Uniqueness and relational integrity of GTFS identifiers are enforced during feed loading via `gtfstools::remove_duplicates()` and `gtfstools::remove_unused_ids()` in `load_feed()` rather than a single-column custom index attribute.
#' @srrstatsNA {EA2.2} Tables follow the GTFS and OpenStreetMap specifications (`tidygtfs` and `sf` classes) rather than custom index-attributed table classes.
#' @srrstatsNA {EA2.2a} `GTFShift` uses `tidygtfs` and `sf` classes rather than a custom index-column class system.
#' @srrstatsNA {EA2.2b} `GTFShift` does not set custom `"index"` attributes on tables because keys are defined by the GTFS specification (`trip_id`, `route_id`, `shape_id`, `stop_id`) and OSM specification (`osm_id`, `way_osm_id`).
#' @srrstatsNA {EA2.4} `GTFShift` uses the domain-standard `tidygtfs` class from `tidytransit` to represent relational GTFS tables rather than `dm`.
#' @srrstatsNA {EA2.5} Individual tables within `tidygtfs` follow the GTFS specification relational schema rather than custom index column attributes.
#' @srrstatsNA {EA4.1} `GTFShift` returns standard R `data.frame`, `sf`, `tidygtfs`, and `list` objects for downstream analysis rather than custom print/precision-formatted objects.
#' @srrstatsNA {EA5.0} `GTFShift` does not implement custom static plotting functions or graphical themes; visual exploration in vignettes uses external interactive mapping libraries such as `mapview`.
#' @srrstatsNA {EA5.0a} `GTFShift` does not generate static plots or specify typeface sizes.
#' @srrstatsNA {EA5.0b} `GTFShift` does not define custom colour palettes or plotting routines.
#' @srrstatsNA {EA5.1} `GTFShift` does not override default typefaces in `graphics` or other plotting packages.
#' @srrstatsNA {EA5.2} `GTFShift` does not implement custom screen-based print formatting methods for numeric types.
#' @srrstatsNA {EA5.3} Summary functions (`get_prioritisation_stats()`, `get_trip_speed_profile()`) return domain-specific physical metrics (lengths in meters, speeds in km/h, lane counts) rather than generic schema-level column `storage.mode` summaries.
#' @srrstatsNA {EA5.4} `GTFShift` does not implement visualization or plotting routines.
#' @srrstatsNA {EA5.5} `GTFShift` does not implement visualization routines with plotted axes.
#' @srrstatsNA {EA5.6} `GTFShift` does not internally bundle any JavaScript or dynamic visualization libraries.
#' @srrstatsNA {EA6.1} `GTFShift` does not produce graphical output, so visual regression testing (e.g., via `vdiffr`) is not applicable.
#' @srrstatsNA {SP2.0a} `GTFShift` does not implement any new spatial classes; all spatial objects use `sf` and `sfc` classes from the `sf` package.
#' @srrstatsNA {SP2.5a} `GTFShift` does not implement new classes for spatial data.
#' @srrstatsNA {SP3.0} `GTFShift` analyses transit routes, stops, and road network segments (`LINESTRING`, `MULTILINESTRING`, `POINT`) and does not implement spatial neighbourhood or contiguity weight algorithms.
#' @srrstatsNA {SP3.0a} `GTFShift` does not operate on regular spatial grids or rook/queen neighbourhood structures.
#' @srrstatsNA {SP3.0b} `GTFShift` does not compute spatial neighbourhood graphs or k-nearest-neighbour weights.
#' @srrstatsNA {SP3.1} `GTFShift` does not compute spatially weighted neighbour contributions.
#' @srrstatsNA {SP3.2} `GTFShift` does not perform spatial statistical point sampling; 1D line discretization in `project_points_along_geometry()` uses a uniform step (`geometry_sample_meters`) to measure cumulative distance along a single route geometry.
#' @srrstatsNA {SP3.3} `GTFShift` is not spatial regression software.
#' @srrstatsNA {SP3.4} `GTFShift` is not spatial clustering software.
#' @srrstatsNA {SP3.5} `GTFShift` is not spatial machine learning software.
#' @srrstatsNA {SP3.6} `GTFShift` is not spatial machine learning software.
#' @srrstatsNA {SP5.0} `GTFShift` does not implement custom spatial classes; returned `sf` and `tidygtfs` objects inherit default `plot` methods from `sf` and `tidytransit`.
#' @srrstatsNA {SP5.1} `GTFShift` does not implement custom plotting functions.
#' @srrstatsNA {SP5.2} `GTFShift` does not implement custom plotting functions.
#' @srrstatsNA {SP5.3} `GTFShift` returns `sf` objects that can be passed directly to interactive HTML mapping packages such as `mapview` (as demonstrated in package vignettes) rather than re-implementing interactive visualization wrappers.
#' @srrstatsNA {SP6.1a} Spatial distance and length operations in `GTFShift` use spherical geometry (`s2`) by default on geographic coordinates and support user-specified projected CRS via `metric_crs`, avoiding inaccurate planar calculations on ellipsoidal data.
#' @srrstatsNA {SP6.3} `GTFShift` does not consider spatial neighbours.
#' @srrstatsNA {SP6.4} `GTFShift` does not consider spatial neighbours.
#' @srrstatsNA {SP6.5} `GTFShift` does not implement spatial clustering algorithms.
#' @srrstatsNA {SP6.6} `GTFShift` does not implement spatial machine learning algorithms.
#' @noRd
NULL
