# Take averages at 10-km scale. 

agfun <- function(x) median(x, na.rm = TRUE)
reachdf <- joindf %>% 
  mutate(reach = round(x / 10000)) %>% 
  group_by(reach, date, time) %>% 
  summarize(x = agfun(x), H = agfun(H), W = agfun(W),
            S = agfun(dH) / agfun(dx), n = n()) %>% 
  ungroup()
cache("reachdf")

agfun <- function(x) mean(x, na.rm = TRUE)
reachdf_mean <- joindf %>% 
  mutate(reach = round(x / 10000)) %>% 
  group_by(reach, date, time) %>% 
  summarize(x = agfun(x), H = agfun(H), W = agfun(W),
            S = agfun(dH) / agfun(dx), n = n()) %>% 
  ungroup()
cache("reachdf_mean")

