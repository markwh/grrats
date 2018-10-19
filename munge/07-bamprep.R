# Make bamdata, bampriors objects


# bamdata objects
library(bamr)
bds_node <- map(nodecases2, ~swot_bamdata(., max_xs = 99999))
bds_reach <- map(reachcases2, ~swot_bamdata(., max_xs = 99999))
bds_reach_mean <- map(reachcases2_mean, ~swot_bamdata(., max_xs = 99999))


cache("bds_node")
cache("bds_reach")
cache("bds_reach_mean")
