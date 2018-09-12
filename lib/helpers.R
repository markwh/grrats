
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
