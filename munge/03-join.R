# Join width, height, and a priori data (currently just qwbm)


# a priori info: make a small subset for joining wbm.
locs_ap_tojoin <- locs_ap %>% 
  select(x_m, QWBM)

joindf <- heightdf %>% 
  mutate(W = widthdf$W,
         loc = indicize(x_m)) %>% 
  left_join(locs_ap_tojoin, by = "x_m")
cache("joindf")
