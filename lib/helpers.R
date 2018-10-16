
# box_search("cache", type = "folder") # Search for id of grrats folder
# box_search("grrats", type = "folder")

cache_boxid <- 53075448928
# boxr::box_setwd(cache_boxid) # id of grrats folder

# Overwrite default function behavior for boxr
cache_push <- function(dir_id = cache_boxid, 
                       local_dir = file.path(getwd(), "cache"), 
                       ignore_dots = TRUE,
                       overwrite = FALSE, delete = FALSE) {
  boxr::box_push(dir_id = dir_id, local_dir = local_dir, 
                 ignore_dots = ignore_dots, overwrite = overwrite, 
                 delete = delete)
}

cache_fetch <- function(dir_id = cache_boxid,
                        local_dir = file.path(getwd(), "cache"),
                        recursive = TRUE,
                        overwrite = FALSE, delete = FALSE) {
  boxr::box_fetch(dir_id = dir_id, local_dir = local_dir, 
                  recursive = recursive, overwrite = overwrite, 
                  delete = delete)
}

#' A HydroSWOT-based estimator for mean(logQ) based on mean(logW)
#' 
#' Note: MSE (ocv) for this model is 0.551, meaning logQbar_sd should be 0.742. 
#' But really it could be somewhat lower, since different locations' estimates
#' are being aggregated. Their errors will be correlated an unknown but large 
#' amount, so 0.742 is a safe, conservative number here. 
estimate_logQbar <- function(Wobs) {
  lwbar <- apply(log(Wobs), 1, mean, na.rm = TRUE)
  logQbar <- -3.2184 + 1.7039 * lwbar
  median(logQbar)
}

#' Make index ordering for a variable x
#' 
#' Used for making `loc` column in joindf, etc.
#' 
#' @param x a sortable vector
indicize <- function(x) {
  unqx <- unique(x)
  out <- match(x, sort(unqx))
  out
}


#' Copied from markstats package, modified to work in subdirectory (e.g. notebook)
#' Fetch a single object from cache
#' 
#' Does the opposite of ProjectTemplate::cache -- given an object name (as a
#' character string), retrieves that object from the cache directory.
#' 
#' @param ...	the objects to be retrieved, as names (unquoted) or character strings (quoted).
#' @param list	a character vector naming objects to be retrieved.
#' @param envir the environment into which to retrieve the object.
#' @return None, used for side-effects only
#' @seealso  ProjectTemplate::cache
#' @export
fromcache <- function(..., list = character(), envir = parent.frame()) {
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) || 
                                  is.character(x), NA, USE.NAMES = FALSE))) 
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) 
    names <- character()
  list <- .Primitive("c")(list, names)
  
  cachedir <- "cache"
  if (!dir.exists("cache") && dir.exists("../cache"))
    cachedir <- "../cache"
  files <- file.path(cachedir, paste0(list, ".RData"))
  lapply(files, load, envir = envir)
}
