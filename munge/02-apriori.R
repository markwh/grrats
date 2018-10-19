# SWOT a priori database (already processed in gis)

locs_ap <- st_read("data/gis/locs_ap_join.shp") %>% 
  as.data.frame() %>% 
  select(-loc, -n, -geometry)

cache("locs_ap")
