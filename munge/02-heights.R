# Read and process heights

# Heights -----------------------------------------------------------------
# This section crashes on my chromebook--requires a respectable amount of memory and cpu.

heightmat <- readMat("https://osu.box.com/shared/static/anbvjbmczfkqb9gmuekcq7xa9zmdjo3s.mat")
# str(heightmat)

heightdf0 <- melt(heightmat$TERP) 

heightdf1 <- heightdf0 %>% 
  mutate(x = rep(as.vector(heightmat$x), 
                 each = length(heightmat$y)),
         date = rep(as.vector(heightmat$y), 
                    length(heightmat$x))) %>% 
  transmute(H = value, x, date = as.Date(round(date), origin = "0000-01-01")) 
cache("heightdf1")


# Only keep the dates that have width data (cuts data size by factor of ~5)
hdates <- unique(heightdf1$date)
keepdates <- intersect(unique(widthdf$date), hdates)
heightdf2 <- heightdf1 %>%
  filter(date %in% keepdates)

# If more than 1 height per day, take daily average height
heightdf3 <- heightdf2 %>% 
  group_by(date, x) %>% 
  summarize(H = mean(H)) # TODO: How many are getting averaged?


# Add slopes
heightdf4 <- heightdf3 %>% 
  group_by(date) %>% 
  arrange(x) %>% 
  mutate(dH = c(diff(H), NA),
         dx = c(diff(x), NA),
         S = dH / dx) %>% 
  ungroup() %>% 
  filter(dx < 10000) # Slope estimate unreliable if dx is too large

heightdf <- heightdf4
cache("heightdf")


