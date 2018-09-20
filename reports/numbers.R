
nmbrs = list(
  # width data
  nobstot_w = nrow(widthdf),
  ndates_w = length(unique(widthdf$date)),
  nlocs_w = length(unique(widthdf$x)),
  
  # height data
  nobstot_h = nrow(heightdf),
  ndates_h = length(unique(heightdf$date)),
  nlocs_h = length(unique(heightdf$x)),
  
  # Joined data
  nobstot_j = nrow(joindf),
  ndates_j = length(unique(joindf$date)),
  nlocs_j = length(unique(joindf$x)),
  
  
  
)
