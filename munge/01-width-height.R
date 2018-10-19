# Read data from .mat files


# New data version (as of 10/8/2018)
# As of 10/17/2018: Indicates river segments from gis.Rmd 

width0 <- readMat("data/MississippidailywidthGrid.mat")
width1 <- width0$Wpackage %>% 
  setNames(attr(., "dimnames")[[1]])

width2 <- melt(width1$w, na.rm = FALSE) %>% 
  mutate(lat = rep(as.vector(width1$lat), nrow(width1$w)),
         lon = rep(as.vector(width1$lon), nrow(width1$w)),
         lat = round(lat, digits = 4),
         lon = round(lon, digits = 4))

height1 <- width1$Hdexes %>% 
  setNames(attr(., "dimnames")[[1]])

height2 <- melt(height1$H) %>% 
  mutate(time = rep(as.vector(height1$T), 
                    length(as.vector(height1$X))),
         lat = rep(as.vector(height1$lat), 
                   length(as.vector(height1$T))),
         lon = rep(as.vector(height1$lon), 
                   length(as.vector(height1$T))),
         lat = round(lat, digits = 4),
         lon = round(lon, digits = 4),
         X = rep(as.vector(height1$X), 
                 length(as.vector(height1$T))),
         W = rep(as.vector(height1$W), 
                 length(as.vector(height1$T))))

# Unique locations, including unique-per-location string

wlocs <- width2 %>% 
  group_by(lat, lon) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(locstr = paste(lat, lon, sep = ";"))

hlocs <- height2 %>% 
  group_by(lat, lon, X) %>% 
  summarize(n = n()) %>% 
  group_by(lat, lon) %>% 
  summarize(X = X[1], n = n()) %>% 
  ungroup() %>% 
  filter(n == 1) %>% 
  mutate(locstr = paste(lat, lon, sep = ";"))

commonlocs <- inner_join(wlocs, hlocs, by = c("lat", "lon", "locstr"))

wlocs_orig <- paste(round(width1$lat, digits = 4), round(width1$lon, digits = 4), sep = ";")
keepinds_w <- wlocs_orig %in% commonlocs$locstr

hlocs_orig <- paste(round(height1$lat, digits = 4), round(height1$lon, digits = 4), sep = ";")
keepinds_h <- hlocs_orig %in% commonlocs$locstr

# Final-ish products
width3 <- width1 %>% 
  within(., {
    w = w[, keepinds_w]
    lat = lat[1, keepinds_w]
    lon = lon[1, keepinds_w]
  })
width4 <- melt(width3$w, na.rm = FALSE) %>% 
  mutate(lat = rep(as.vector(width3$lat), each = nrow(width3$w)),
         lon = rep(as.vector(width3$lon), each = nrow(width3$w))) %>% 
  select(-Var2) %>%
  rename(W = value, timeind = Var1)

height3 <- height1 %>% 
  within(., {
    H = H[, keepinds_h]
    X = X[1 , keepinds_h]
    lat = lat[1, keepinds_h]
    lon = lon[1, keepinds_h]
  })
height4 <- melt(height3$H, na.rm = FALSE) %>% 
  mutate(time = rep(as.vector(height3$T), 
                    length(as.vector(height3$X))),
         lat = rep(as.vector(height3$lat), 
                   each = length(as.vector(height3$T))),
         lon = rep(as.vector(height3$lon), 
                   each = length(as.vector(height3$T))),
         X = rep(as.vector(height3$X), 
                 each = length(as.vector(height3$T)))) %>% 
  transmute(date = as.Date(time, origin = "0000-01-01"), timeind = Var1,
            lat, lon, x_m = X, H = value)

# Add slopes
height5 <- height4 %>% 
  group_by(date) %>% 
  arrange(x_m) %>% 
  mutate(dH = c(diff(H), NA),
         dx = c(diff(x_m), NA),
         S = dH / dx) %>% 
  ungroup()

# Split into river segments based on gis.Rmd.
# Note that *even* intervals will be the good ones (inside the bounds of goodness)
breaks <- c(0, 105268.6, 429268.6, 430268.6, 887268.6, 926268.6, 
            1506268.6, 1628268.6, 1855268.6, 1897268.6, 2132268.6, 1e10)
height6 <- height5 %>% 
  mutate(segment = findInterval(x_m, breaks))

# Final products and cache
widthdf <- width4
heightdf <- height6


cache("widthdf")
cache("heightdf")

