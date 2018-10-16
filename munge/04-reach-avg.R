# Take averages at 10-km scale. 


agfun <- function(x) median(x, na.rm = TRUE)
reachdf <- joindf %>% 
  filter(dx <= 3000) %>% 
  mutate(reach = round(x_m / 10000)) %>% 
  group_by(reach, date, timeind) %>% 
  summarize(x_m = agfun(x_m), H = agfun(H), W = agfun(W),
            S = agfun(dH) / agfun(dx), n = n()) %>% 
  ungroup() %>% 
  mutate(loc = indicize(reach))
cache("reachdf")

agfun <- function(x) mean(x, na.rm = TRUE)
reachdf_mean <- joindf %>% 
  filter(dx <= 3000) %>% 
  mutate(reach = round(x_m / 10000)) %>% 
  group_by(reach, date, timeind) %>% 
  summarize(x_m = agfun(x_m), H = agfun(H), W = agfun(W),
            S = agfun(dH) / agfun(dx), n = n()) %>% 
  ungroup() %>% 
  mutate(loc = indicize(reach))
cache("reachdf_mean")



