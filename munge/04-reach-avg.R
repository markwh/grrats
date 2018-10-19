# Take averages at 10-km scale. 


agfun <- function(x) median(x, na.rm = TRUE)
reachdf <- joindf %>% 
  filter(dx <= 3000) %>% # Don't trust dH if dx is too large
  group_by(segment) %>% 
  mutate(xadj = x_m - min(x_m),
         reach = indicize(ceiling((xadj + 1) / 10000))) %>% 
  group_by(segment, reach, date, timeind) %>% 
  summarize(x_m = agfun(x_m), QWBM = agfun(QWBM),
            H = agfun(H), W = agfun(W),
            S = agfun(dH) / agfun(dx), n = n()) %>% 
  ungroup() %>% 
  mutate(loc = indicize(reach + (100 * segment))) # No longer identical to reach
cache("reachdf")

agfun <- function(x) mean(x, na.rm = TRUE)
reachdf_mean <- joindf %>% 
  filter(dx <= 3000) %>% # Don't trust dH if dx is too large
  group_by(segment) %>% 
  mutate(xadj = x_m - min(x_m),
         reach = indicize(ceiling((xadj + 1) / 10000))) %>% 
  group_by(segment, reach, date, timeind) %>% 
  summarize(x_m = agfun(x_m), QWBM = agfun(QWBM),
            H = agfun(H), W = agfun(W),
            S = agfun(dH) / agfun(dx), n = n()) %>% 
  ungroup() %>% 
  mutate(loc = indicize(reach + (100 * segment))) # No longer identical to reach
cache("reachdf_mean")



