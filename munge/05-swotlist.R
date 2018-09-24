
# swotlist ----------------------------------------------------------------

make_dawgmat <- function(swotdf, varname, timevar = "time", locvar= "loc") {
  out <- swotdf %>% 
    select(!!varname, !!timevar, !!locvar) %>% 
    spread(key = !!timevar, value = !!varname) %>% 
    select(-!!locvar) %>% 
    as.matrix()
}

make_swotlist <- function(swotdf) {
}


wmat <- joindf %>% 
  select(W, time, loc) %>% 
  spread(key = time, value = W) %>% 
  select(-loc) %>% 
  as.matrix()

smat <- joindf %>% 
  select(S, time, loc) %>% 
  spread(key = time, value = S) %>% 
  select(-loc) %>% 
  as.matrix()

hmat <- joindf %>% 
  select(H, time, loc) %>% 
  spread(key = time, value = H) %>% 
  select(-loc) %>% 
  as.matrix()

xmat <- joindf %>% 
  select(x, time, loc) %>% 
  spread(key = time, value = x) %>% 
  select(-loc) %>% 
  as.matrix()

datemat <- joindf %>% 
  transmute(date = as.numeric(date), time, loc) %>% 
  spread(key = time, value = date) %>% 
  select(-loc) %>% 
  as.matrix()

dAmat <- calcdA_mat(wmat, hmat, "median")


mscase <- list(W = wmat, S = smat, H = hmat, dA = dAmat, x = xmat, date = datemat)

msdf <- mscase %>% 
  swot_tidy()

cache("mscase")
cache("msdf")