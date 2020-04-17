

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