# Munge matlab files

missmat <- readMat("https://osu.box.com/shared/static/p590fahl5i03vhw75ljnlmmv40pklt40.mat")
str(missmat)


# Heights -----------------------------------------------------------------


terpmat <- readMat("https://osu.box.com/shared/static/anbvjbmczfkqb9gmuekcq7xa9zmdjo3s.mat")
str(terpmat)

terpdf <- melt(terpmat$TERP) %>% 
  mutate(x = rep(as.vector(terpmat$x), each = length(terpmat$y)),
         date = rep(as.vector(terpmat$y), length(terpmat$x))) %>% 
  transmute(H = value, x, date = as.Date(round(date), origin = "0000-01-01")) %>% #,
            # loc = as.integer(as.factor(x)), 
            # time = as.integer(as.factor(date))) 
  group_by(date, x) %>% 
  summarize(H = mean(H)) %>% 
  group_by(date) %>% 
  arrange(x) %>% 
  mutate(S = c(diff(H), NA) / c(diff(x), NA)) %>% 
  ungroup()

# hmat <- swot_untidy(terpdf)

# Widths ------------------------------------------------------------------

widthdf <- melt(missmat$Wgrid[[1]], na.rm = FALSE) %>% 
  mutate(time = rep(as.vector(missmat$Wgrid[[2]]), 
                    length(as.vector(missmat$Wgrid[[3]]))),
         FD = rep(as.vector(missmat$Wgrid[[3]]),
                  each = length(as.vector(missmat$Wgrid[[2]])))) %>% 
  transmute(W = value, date = as.Date(round(time), origin = "0000-01-01"), 
            x = terpmat$x[FD])


setdiff(terpdf$date, widthdf$date)
intersect(terpdf$date, widthdf$date)
intersect(terpdf$x, widthdf$x)


# swotlist ----------------------------------------------------------------

joindf <- inner_join(na.omit(terpdf), na.omit(widthdf), by = c("date", "x"))
loctbl <- data.frame(loc = 1:length(unique(joindf$x)),
                     x = sort(unique(joindf$x)))
timetbl <- data.frame(time = 1:length(unique(joindf$date)),
                      date = sort(unique(joindf$date)))

joindf <- joindf %>% 
  mutate(loc = loctbl$loc[match(x, loctbl$x)],
         time = timetbl$time[match(date, timetbl$date)]) %>% 
  filter(S > 0)
  
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

dAmat <- calcdA_mat(wmat, hmat, "median")
plot_DAWG(dAmat) + geom_point(aes(color = loc), size = 0.2)

joinlist <- list(W = wmat, S = smat, H = hmat, dA = dAmat)

joinlist %>% 
  swot_sset(keeplocs = foo) %>% 
  swot_tidy() %>% 
  group_by(time) %>% 
  mutate(n = n()) %>% 
  
  # swot_purge_nas() %>%
  swot_tidy() %>% 
  filter(!(is.na(W) & is.na(S) & is.na(dA)))
  glimpse()
  swot_plot()

joinlist %>% 
  swot_sset(keeplocs = 1:20) %>% 
  estA0()

terpdf %>% 
  filter(time == time[1]) %>% 
  plot(H ~ loc, .)

widthdf %>% 
  na.omit() %>% 
  str()

plot(W ~ FD, na.omit(widthdf))


terpraster <- terpdf %>% 
  filter(x < quantile(x, 0.1),
         y < quantile(y, 0.1)) %>%
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = H))

ggsave(terpraster, filename = "graphs/terpraster_1.jpg", dpi = 300)

tdf2 <- terpdf %>% 
  group_by(y) %>% 
  mutate(ismin = ifelse(H == min(H), 0, 1),
         ismax = ifelse(H == max(H), 0, 1)) %>% 
  ungroup()

tdf_min <- tdf2 %>% 
  filter(ismin == 0) 
tdf_max <- tdf2 %>% 
  filter(ismax == 0)

tdf_min %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()

tdf_max %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()

