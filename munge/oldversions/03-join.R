# Join heights to widths


# Update width locations based on height location data --------------------

wdf2 <- widthdf %>% 
  transmute(W = value, date = as.Date(round(time), origin = "0000-01-01"), 
            x = heightmat$x[FD])


# Join widths to heights --------------------------------------------------

joindf0 <- inner_join(na.omit(heightdf), na.omit(wdf2), by = c("date", "x"))

# Tables to allow sequential numbering of dates and locations
loctbl <- data.frame(loc = 1:length(unique(joindf0$x)),
                     x = sort(unique(joindf0$x)))
timetbl <- data.frame(time = 1:length(unique(joindf0$date)),
                      date = sort(unique(joindf0$date)))

joindf <- joindf0 %>% 
  mutate(loc = loctbl$loc[match(x, loctbl$x)],
         time = timetbl$time[match(date, timetbl$date)])

cache("joindf")
