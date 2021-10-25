map_keyword_country <- function(keyword, time = "all") {
          require(sf)
          require(tidyverse)
          require(rnaturalearth)
          
          world_shp <- st_transform(
                    merge(
                              st_as_sf(ne_countries(), crs = "4236"),
                              gtrendsR::gtrends(
                                        keyword = keyword,
                                        low_search_volume = TRUE,
                                        time = time
                              ) %>%
                                        .$interest_by_country %>%
                                        mutate(hits = as.numeric(hits)) %>%
                                        select(name = location, hits)
                    ),
                    crs = "+proj=moll"
          )

          
          ggplot(world_shp) +
                    geom_sf(aes(fill = hits)) +
                    scale_fill_viridis_c(na.value = "white") +
                    coord_sf() +
                    theme_bw() +
                    labs(fill = "Google Index", title = keyword) +
                    theme(panel.border = element_blank(), 
                          legend.position = "bottom")
}
