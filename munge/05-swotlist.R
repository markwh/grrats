
# swotlist ----------------------------------------------------------------

make_dawgmat <- function(swotdf, varname, timevar = "timeind", locvar= "loc") {
  out <- swotdf %>% 
    mutate(value = as.numeric(.data[[varname]])) %>% # Gets around date formatting issue    
    select(value, !!timevar, !!locvar) %>% 
    spread(key = !!timevar, value = value) %>% 
    select(-!!locvar) %>% 
    as.matrix()
  out
}

make_swotlist <- function(swotdf, pos_slope = TRUE) {
  out <- list(
    W = make_dawgmat(swotdf, "W"),
    S = make_dawgmat(swotdf, "S"),
    H = make_dawgmat(swotdf, "H"),
    x = make_dawgmat(swotdf, "x_m"),
    date = make_dawgmat(swotdf, "date")
  )
  
  # Remove non-positive slopes
  if (pos_slope) { 
    out$S[is.na(out$S)] <- 0
    out$S[out$S <= 0] <- NA
  }
  
  out$dA <- calcdA_mat(w = out$W, h = out$H, zero = "minimum")
  out
}


# Node-level swotlist -----------------------------------------------------

mscase <- make_swotlist(joindf)

msdf <- mscase %>% 
  swot_tidy()

cache("mscase")
cache("msdf")

# Reach-level swotlists ---------------------------------------------------

nmin <- 5 # Minimum number of nodes to have aggregated over
reachcase <- reachdf %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  make_swotlist()

reachcase_mean <- reachdf_mean %>% 
  filter(n >= nmin) %>% 
  mutate(loc = reach) %>% 
  make_swotlist()

cache("reachcase")
cache("reachcase_mean")
