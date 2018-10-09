# Read data from .mat files


# Widths ------------------------------------------------------------------

# missmat <- readMat("https://osu.box.com/shared/static/p590fahl5i03vhw75ljnlmmv40pklt40.mat")
missmat <- readMat("data/Mississippi_Widthgrid3.mat")
str(missmat, 1)

widthdf <- melt(missmat$Wgrid[[1]], na.rm = FALSE) %>% 
  mutate(time = rep(as.vector(missmat$Wgrid[[2]]), 
                    length(as.vector(missmat$Wgrid[[3]]))),
         FD = rep(as.vector(missmat$Wgrid[[3]]),
                  each = length(as.vector(missmat$Wgrid[[2]]))),
         X = rep(as.vector(missmat$Wgrid[[4]]),
                  each = length(as.vector(missmat$Wgrid[[2]]))),
         Y = rep(as.vector(missmat$Wgrid[[5]]),
                  each = length(as.vector(missmat$Wgrid[[2]]))),
         date = as.Date(time, origin = "0000-01-01"))

cache("widthdf")








