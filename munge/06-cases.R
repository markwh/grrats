# Create cases from node and reach-averaged data, based on a given river segment length
# As of 10/17/2018: respect contributing area steps from gis.Rmd
# This means that now there will be 2 criteria for separating into sets of reaches:
#   1. similarity of contributing catchment area
#   2. length of river segment

seglen_node <- 50000 # maximum length of a "set" of nodes, in meters
nodelocs <- joindf %>% 
  filter(segment %% 2 == 0) %>% # odd segments are outside of boundaries
  group_by(segment, loc, x_m, QWBM) %>% 
  summarize(n = n()) %>% 
  group_by(segment) %>% 
  mutate(xadj = x_m - min(x_m),
         set = findInterval(xadj, vec = 0:ceiling(max(xadj) / seglen_node) * seglen_node),
         segset = paste0(sprintf("%02d", segment), 
                         sprintf("%02d", set))) %>% 
  ungroup()


nodelocgrps <- split(nodelocs$loc, f = nodelocs$segset)

qwbm_nodesets <- split(nodelocs$QWBM, f = nodelocs$segset) %>% 
  map(~median(., na.rm = TRUE))

nodecases <- nodelocgrps %>% 
  map(~swot_sset(mscase, keeplocs = .)) %>% 
  map2(qwbm_nodesets, function(x, y) {attr(x, "QWBM") <- y; x})



# Reach data --------------------------------------------------------------
seglen_reach <- 100000 # Same, for reach data

reachlocs <- reachdf %>% 
  filter(segment %% 2 == 0) %>% # odd segments are outside of boundaries
  group_by(segment, reach, loc, x_m, QWBM) %>% 
  summarize(n = n()) %>% 
  group_by(segment) %>% 
  mutate(xadj = x_m - min(x_m),
         set = findInterval(xadj, vec = 0:ceiling(max(xadj) / seglen_reach) * seglen_reach),
         segset = paste0(sprintf("%02d", segment), 
                         sprintf("%02d", set))) %>% 
  ungroup()

reachlocgrps <- split(reachlocs$loc, f = reachlocs$segset)
qwbm_reachsets <- split(reachlocs$QWBM, f = reachlocs$segset) %>% 
  map(~median(., na.rm = TRUE))

reachcases <- reachlocgrps %>% 
  map(~swot_sset(reachcase, keeplocs = .)) %>% 
  map2(qwbm_reachsets, function(x, y) {attr(x, "QWBM") <- y; x})
  

reachcases_mean <- reachlocgrps %>% 
  map(~swot_sset(reachcase_mean, keeplocs = .)) %>% 
  map2(qwbm_reachsets, function(x, y) {attr(x, "QWBM") <- y; x})


cache("nodecases")
cache("reachcases")
cache("reachcases_mean")

# Subset to a reasonable temporal sampling. -------------------------------
timeint <- 15 # time interval in days

ssamp_t <- function(swotlist, timeint) {
  keeptimes <- seq(1, ncol(swotlist$W), by = timeint)
  out <- swot_sset(swotlist, keeptimes = keeptimes)
  out
}

nodecases2 <- nodecases %>% 
  map(~ssamp_t(., timeint))
reachcases2 <- reachcases %>% 
  map(~ssamp_t(., timeint))
reachcases2_mean <- reachcases_mean %>% 
  map(~ssamp_t(., timeint))

cache("nodecases2")
cache("reachcases2")
cache("reachcases2_mean")
