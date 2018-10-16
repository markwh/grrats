# Make bamdata, bampriors objects

load("cache/reachcases2.RData")
load("cache/reachcases2_mean.RData")
load("cache/nodecases2.RData")


# Subset to a reasonable temporal sampling. 
timeint <- 15 # time interval in days

ssamp_t <- function(swotlist, timeint) {
  keeptimes <- seq(1, ncol(swotlist$W), by = timeint)
  out <- swot_sset(swotlist, keeptimes = keeptimes)
  out
}

nodecases3 <- nodecases2 %>% 
  map(~ssamp_t(., timeint))
reachcases3 <- reachcases2 %>% 
  map(~ssamp_t(., timeint))
reachcases3_mean <- reachcases2_mean %>% 
  map(~ssamp_t(., timeint))

# Q estimates
qhats_node <- nodecases3 %>% 
  map(~exp(estimate_logQbar(.$W)))
qhats_reach <- reachcases3 %>% 
  map(~exp(estimate_logQbar(.$W)))
qhats_reach_mean <- map(reachcases3_mean, ~exp(estimate_logQbar(.$W)))


# bamdata objects
library(bamr)
bds_node <- map2(nodecases3, qhats_node, ~swot_bamdata(.x, Qhat = .y, max_xs = 99999))
bds_reach <- map2(reachcases3, qhats_reach, ~swot_bamdata(.x, Qhat = .y, max_xs = 99999))
bds_reach_mean <- map2(reachcases3_mean, qhats_reach_mean, 
                       ~swot_bamdata(.x, Qhat = .y, max_xs = 99999))


cache("bds_node")
cache("bds_reach")
cache("bds_reach_mean")
