library(sp)

islines <- function(g1, g2){
  ## return TRUE if geometries intersect as lines, not points
  inherits(gIntersection(g1,g2),"SpatialLines")
}

sections <- function(sl){
  ## union and merge and disaggregate to make a
  ## set of non-overlapping line segments
  disaggregate(gLineMerge(gUnion(sl,sl)))
}

aggit <- function(sldf, attr, fun=sum){
  ## simplify down to SpatialLines
  sl = as(sldf, "SpatialLines")
  ## get the line sections that make the network
  slu = sections(sl)
  ## overlay network with routes
  overs = over(slu, sl, returnList=TRUE)
  ## overlay is true if end points overlay, so filter them out:
  overs = lapply(1:length(overs), function(islu){
    Filter(function(isl){
      islines(sl[isl,],slu[islu,])
    }, overs[[islu]])
  })
  ## now aggregate the required attribute using fun():
  aggs = sapply(overs, function(os){fun(sldf[[attr]][os])})

  ## make a SLDF with the named attribute:
  sldf = SpatialLinesDataFrame(slu, data.frame(Z=aggs))
  names(sldf)=attr
  sldf
}

lineLabels <- function(sldf, attr){
  text(coordinates(gCentroid(sldf,byid=TRUE)),labels=sldf[[attr]])
}


merge <- aggit(sf::as_Spatial(routes_moraissoares), "frequency")
