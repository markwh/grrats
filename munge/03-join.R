# Join heights to widths


# Update width locations based on height location data --------------------

wdf2 <- widthdf %>% 
  transmute(W = value, date = as.Date(round(time), origin = "0000-01-01"), 
            x = heightmat$x[FD])


# Join widths to heights, calculate slope ---------------------------------


joindf <- inner_join(na.omit(heightdf), na.omit(wdf2), by = c("date", "x"))

loctbl <- data.frame(loc = 1:length(unique(joindf$x)),
                     x = sort(unique(joindf$x)))
timetbl <- data.frame(time = 1:length(unique(joindf$date)),
                      date = sort(unique(joindf$date)))

joindf <- joindf %>% 
  mutate(loc = loctbl$loc[match(x, loctbl$x)],
         time = timetbl$time[match(date, timetbl$date)])

# Add slopes
joindf <- joindf %>% 
  group_by(date) %>% 
  arrange(x) %>% 
  mutate(dH = c(diff(H), NA),
         dx = c(diff(x), NA),
         S = dH / dx) %>% 
  ungroup() %>% 
  filter(dx < 10000) # Slope estimate unreliable if dx is too large

cache("joindf")
