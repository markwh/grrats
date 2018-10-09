# Create cases from node and reach-averaged data, based on a given river segment length

alldists <- apply(mscase$x, 1, min, na.rm = TRUE)
reldists <- alldists - alldists[1]


seglen_node <- 50000 # size of mass-conserved river segments for node data (meters)
seglen_reach <- 100000 # Same, for reach data

bindf <- data.frame(x = alldists, xadj = reldists) %>% 
  mutate(int = findInterval(xadj, 
                            vec = 0:ceiling(max(xadj) / seglen_node) * seglen_node),
         ind = 1:nrow(.))
bininds <- with(bindf, split(ind, f = int))         

nodecases <- bininds %>% 
  map(~swot_sset(mscase, keeplocs = .))


# Reach data --------------------------------------------------------------

alldists_r <- apply(reachcase$x, 1, min, na.rm = TRUE)
reldists_r <- alldists_r - alldists_r[1]

bindf_r <- data.frame(x = alldists_r, xadj = reldists_r) %>% 
  mutate(int = findInterval(xadj, 
                            vec = 0:ceiling(max(xadj) / seglen_reach) * seglen_reach),
         ind = 1:nrow(.))
bininds_r <- with(bindf_r, split(ind, f = int))
reachcases <- bininds_r %>% 
  map(~swot_sset(reachcase, keeplocs = .))

reachcases_mean <- bininds_r %>% 
  map(~swot_sset(reachcase_mean, keeplocs = .))



# Subset to maximize observations per time, location ----------------------

swot_hasdata <- function(swotlist) {
  mygt <- function(x, y) {
    out <- x > y
    out[is.na(out)] <- FALSE
    out
  }
  swotlist$S[!mygt(swotlist$S, 0)] <- NA

  out <- with(swotlist,  (!is.na(W)) & (!is.na(S)) & (!is.na(dA)))
  out
}

#' If a time and location doesn't have *all* data required for bam, replace *all* 
#' matrices' data with NA for that time and location.
allNAs <- function(swotlist) {
  hasdata <- swot_hasdata(swotlist)
  out <- map(swotlist, function(x) {x[!hasdata] <- NA; x})
  out
} 


# Prune sparse rows and columns, recursively until no more pruning takes place.
# Then prune cases with too few times and/or locations. 

minlocs_node <- 20 # All included times must have at least this many locations present
minlocs_reach <- 6 # All included times must have at least this many locations present
mintimes_node <- 30 # All cases must include at least this many times
mintimes_reach <- 20

keeptimes_node <- nodecases %>% 
  map(swot_hasdata) %>% 
  map(~which(apply(., 2, sum) >= minlocs_node))
ntimes_node <- map_dbl(keeptimes_node, ~length(unique(.)))
keepsects_node <- which(ntimes_node > mintimes_node) 

nodecases2 <- map2(nodecases[keepsects_node], keeptimes_node[keepsects_node],
                   ~swot_sset(.x, keeptimes = .y)) %>% 
  map(allNAs)

# Reaches now
keeptimes_reach <- reachcases %>% 
  map(swot_hasdata) %>% 
  map(~which(apply(., 2, sum) >= minlocs_reach))
ntimes_reach <- map_dbl(keeptimes_reach, ~length(unique(.)))
keepsects_reach <- which(ntimes_reach > mintimes_reach) 

reachcases2 <- map2(reachcases[keepsects_reach], 
                    keeptimes_reach[keepsects_reach],
                    ~swot_sset(.x, keeptimes = .y)) %>% 
  map(allNAs)

keeptimes_reach_mean <- reachcases_mean %>% 
  map(swot_hasdata) %>% 
  map(~which(apply(., 2, sum) >= minlocs_reach))
ntimes_reach_mean <- map_dbl(keeptimes_reach_mean, ~length(unique(.)))
keepsects_reach_mean <- which(ntimes_reach_mean > mintimes_reach) 

reachcases2_mean <- map2(reachcases_mean[keepsects_reach_mean], 
                         keeptimes_reach[keepsects_reach_mean],
                         ~swot_sset(.x, keeptimes = .y)) %>% 
  map(allNAs)

cache("nodecases2")
cache("reachcases2")
cache("reachcases2_mean")
