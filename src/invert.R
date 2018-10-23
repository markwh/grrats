# BAM inversions

load("cache/bds_node.RData")
load("cache/bds_reach.RData")
load("cache/bds_reach_mean.RData")



trybp <- bam_priors(bds_reach$`0203`)
tryest <- bam_estimate(bds_reach$`0203`)

bam_hydrograph(tryest)
pripost_A0(tryest, bampriors = trybp)

stan_dens(tryest, pars = "A0")


bps_reach <- map(bds_reach, ~bam_priors(., variant = "manning_amhg"))

map(bps_reach, ~.[["b_hat"]]) %>% 
  map_lgl(~sum(is.na(.)) == 0)
bamr:::estimate_b(bds_reach$`22`$Wobs)
bds_reach$`22`$Wobs %>% t()


ests_reach <- map(bds_reach, ~bam_estimate(., variant = "manning"))
ests_reach_mean <- map(bds_reach_mean, ~bam_estimate(., variant = "manning"))
qests_reach <- map(ests_reach, ~bam_qpred)


cache("ests_reach_mean")


bps_reach_mean <- map(bds_reach_mean, ~bam_priors(., variant = "manning"))
ests_reach_mean$`0201` %>% pripost_q(bampriors = bps_reach_mean$`0201`, conf.level = 0.95)

ests_reach_mean$`0201` %>% pripost_A0(bampriors = bps_reach_mean$`0201`)

ests_reach_mean$`0201` %>% pripost_n(bampriors = bps_reach_mean$`0201`)
ests_reach_mean$`0201` %>% pripost_qbar(bamdata = bds_reach_mean$`0201`, 
                                        bampriors = bps_reach_mean$`0201`)
