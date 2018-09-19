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



# swotlists ---------------------------------------------------------------

nmin <- 5 # minimum number of observations to be averaged into a reach
wmat <- reachdf %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  select(W, time, loc) %>% 
  spread(key = time, value = W) %>% 
  select(-loc) %>% 
  as.matrix()

smat <- reachdf %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  select(S, time, loc) %>% 
  spread(key = time, value = S) %>% 
  select(-loc) %>% 
  as.matrix()

hmat <- reachdf %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  select(H, time, loc) %>% 
  spread(key = time, value = H) %>% 
  select(-loc) %>% 
  as.matrix()

xmat <- reachdf %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  select(x, time, loc) %>% 
  spread(key = time, value = x) %>% 
  select(-loc) %>% 
  as.matrix()

datemat <- reachdf %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  transmute(date = as.numeric(date), time, loc) %>% 
  spread(key = time, value = date) %>% 
  select(-loc) %>% 
  as.matrix()

dAmat <- calcdA_mat(wmat, hmat, "median")


reachcase <- list(W = wmat, S = smat, H = hmat, dA = dAmat, x = xmat, date = datemat)

cache("reachcase")
