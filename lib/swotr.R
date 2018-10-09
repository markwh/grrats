# Functions to be incorporated into swotr package


#' Fix swot_plot to coerce all variables to numeric
swot_plot <- function (swotlist, vars = "all") {
  if (!(length(vars) == 1 && vars == "all")) {
    swotlist <- swotlist[vars]
  }
  
  numvers <- vapply(swotlist, 
                    function(x) inherits(as.vector(x), "numeric"), 
                    logical(1))
  
  swotlist <- swotlist[numvars]
  
  plotdf <- swot_tidy(swotlist) %>% 
    gather(key = variable, value = value, -time, -loc) %>% 
    mutate(loc = as.factor(loc), value = value)
  out <- ggplot(plotdf, aes(x = time, y = value, color = loc)) + 
    geom_line() + geom_point() + facet_wrap(~variable, scales = "free_y")
  out
}
