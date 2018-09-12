

# Checks on data munging--joining widths to heights, etc. -----------------


# Test plot of H on W 

hwtest <- joindf %>% 
  filter(x %in% sample(loctbl$x, 5)) %>% 
  ggplot(aes(x = W, y = H)) + 
  geom_point(aes(color = x))
hwtest + scale_x_log10()

rchsmpl <- sample(reachdf$reach, 10)
hwtest <- reachdf %>% 
  filter(reach %in% rchsmpl) %>% 
  ggplot(aes(x = W, y = S)) + 
  geom_point(aes(color = x))
hwtest + scale_x_log10()

hwtest <- reachdf_mean %>% 
  filter(reach %in% rchsmpl) %>%
  ggplot(aes(x = W, y = S)) + 
  geom_point(aes(color = x))
hwtest + scale_x_log10()

# calculated dA for a handful of locations
plot_DAWG(dAmat[sample(1:nrow(dAmat), 10), ]) + geom_point(aes(color = loc), size = 0.2)

# swot_plot for a handful of locations
mscase %>% 
  swot_sset(keeplocs = 1:13) %>% 
  swot_plot()