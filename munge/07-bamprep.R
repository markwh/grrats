# Make bamdata, bampriors objects

load("cache/reachcases2.RData")
load("cache/reachcases2_mean.RData")
load("cache/nodecases2.RData")

# Q estimates
qhats_node <- map(nodecases2, ~exp(estimate_logQbar(.$W)))
qhats_reach <- map(reachcases2, ~exp(estimate_logQbar(.$W)))
qhats_reach_mean <- map(reachcases2_mean, ~exp(estimate_logQbar(.$W)))


# bamdata objects
library(bamr)
bds_node <- map2(nodecases2, qhats_node, ~swot_bamdata(.x, Qhat = .y, max_xs = 99999))
bds_reach <- map2(reachcases2, qhats_reach, ~swot_bamdata(.x, Qhat = .y, max_xs = 99999))
bds_reach_mean <- map2(reachcases2_mean, qhats_reach_mean, 
                       ~swot_bamdata(.x, Qhat = .y, max_xs = 99999))


cache("bds_node")
cache("bds_reach")
cache("bds_reach_mean")
