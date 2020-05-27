

#' create a neighbourhood network from a shapefile
#' 
#' @param map - a sf object defining regions / geometries - e.g. shapefile
#' @param idVar - the unique identifier for each region in the shapefile
#' @import dplyr
#' @return an edgelist of ids with from and to columns
#' @export
createNeighbourNetwork = function(map, idVar) {
  idVar = ensym(idVar)
  map = map %>% mutate(tmp_id = row_number())
  graph = map %>% sf::st_intersects()
  edges = tibble(
    from_tmp_id = rep(1:length(graph),sapply( graph, length)),
    to_tmp_id = unlist(graph %>% purrr::flatten())
  )
  edges = edges %>% 
    left_join(map %>% as_tibble() %>% select(from_tmp_id = tmp_id, from = !!idVar), by="from_tmp_id") %>%
    left_join(map %>% as_tibble() %>% select(to_tmp_id = tmp_id, to = !!idVar), by="to_tmp_id") %>%
    filter(from != to) %>%
    select(-from_tmp_id, -to_tmp_id)
  return(edges)
}

#' create map a set of variables from one shapefile to another based on a uniform density across the input and output shapes
#' 
#' @param groupedDf - a grouped dataframe containing the statistic to be interpolated  
#' @param interpolateVar - the statistic, 
#' @param inputShape - an input map, 
#' @param inputIdVar - an id shared between the grouped data fram and the input map, 
#' @param outputShape - an output map which must be grouped by the desired output, 
#' @import dplyr
#' @return a dataframe continaing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
#' @export
interpolateByArea = function(groupedDf, interpolateVar, inputShape, inputIdVar, outputShape) {
  inputIdVar = ensym(inputIdVar)
  outputVars = outputShape %>% groups()
  interpolateVar = ensym(interpolateVar)
  grps = groupedDf %>% groups()
  
  if (as_label(inputIdVar) %in% sapply(grps, as_label) || 
    as_label(interpolateVar) %in% sapply(grps, as_label)) stop("input grouping contains one of input id, output id or interpolation")
  
  
  if(!(as_label(inputIdVar) %in% colnames(groupedDf))) stop("Join id column missing from groupedDf")
  if(!(as_label(inputIdVar) %in% colnames(inputShape))) stop("Join id column missing from inputShape")
  
  if(identical(outputVars,NULL)) stop("Output shape must be grouped by identifiers") 
  
  message("calculating area ....")
  inputShape$inputArea = inputShape %>% sf::st_area() %>% as.numeric()
  #outputShape$outputArea = outputShape %>% sf::st_area() 
  
  message("calculating intersection ....")
  intersection = inputShape %>% sf::st_intersection(outputShape)
  
  message("calculating intersection areas....")
  intersection$intersectionArea = intersection %>% sf::st_area() %>% as.numeric()
  
  intersection = intersection %>% as_tibble() %>% mutate(
    fracInput = intersectionArea/inputArea
  #  fracOutput = intersectionArea/outputArea
  ) 
  
  mapping = intersection %>% as_tibble() %>% select(!!inputIdVar, fracInput, !!!outputVars)
  
  mapping = suppressWarnings(mapping %>% inner_join(groupedDf, by=as_label(inputIdVar)))
  mapping = mapping %>% mutate(intersectionValue = !!interpolateVar * fracInput)
  mapping = mapping %>% group_by(!!!grps, !!!outputVars) %>% summarise(!!interpolateVar := sum(intersectionValue))
  
  return(mapping)
}

#' create map a set of variables from one shapefile to another based on a uniform density across the input and output shapes
#' 
#' @param sf - a shapefile
#' @param zipDir - a zip directory
#' @import dplyr
#' @return a dataframe continaing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
#' @export
saveShapefile = function(sf, zipFile, overwrite=FALSE) {
  zipDir = zipFile %>% stringr::str_remove(".zip")
  shpName = (zipDir %>% stringr::str_split("/"))[[1]] %>% tail(1)
  if (!dir.exists(zipDir)) dir.create(zipDir)
  oldwd = getwd()
  setwd(zipDir)
  suppressWarnings(sf::st_write(sf, paste0(shpName, ".shp"), driver="ESRI Shapefile"))
  setwd("..")
  zip(zipfile = zipFile,files=shpName)
  setwd(oldwd)
  unlink(zipDir, recursive=TRUE)
}
