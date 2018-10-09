# BAM inversions

load("cache/bds_node.RData")
load("cache/bds_reach.RData")
load("cache/bds_reach_mean.RData")




tryest <- bam_estimate(bds_reach$`4`)

bam_hydrograph(tryest)



bps_reach <- map(bds_reach, ~bam_priors(., variant = "manning_amhg"))

map(bps_reach, ~.[["b_hat"]]) %>% 
  map_lgl(~sum(is.na(.)) == 0)
bamr:::estimate_b(bds_reach$`22`$Wobs)
bds_reach$`22`$Wobs %>% t()


ests_reach <- map(bds_reach, ~bam_estimate(., variant = "manning"))
qests_reach <- map(ests_reach, ~bam_qpred)


