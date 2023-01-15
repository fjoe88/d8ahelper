lapply_preserve_names <- function(list, fun){
  #' lapply wrapper, but preserve object names
  #'
  #' base lapply function give access to only the element of the vector but not other attributes
  #' such as name

  lapply(seq_along(list), function(i) {
    obj = list[i]
    names(obj) = names(list)[i]
    fun(obj)
  })
}

#use future
lapply_preserve_names_use_future <- function (list, fun)
{
  future.apply::future_lapply(
    X = seq_along(list),
    FUN = function(i) {
      obj = list[i]
      names(obj) = names(list)[i]
      fun(obj)
    },
    future.seed = TRUE
  )
}

list_files_fwd_slash <- function(path = readr::clipboard(), return_str = TRUE, ...) {
  #' wraper function list.files to allow coppied windows path format containing backward slashes

  path <- stringr::str_replace_all(path, "\\\\+", "/")

  if (return_str == TRUE){
    return(path)
  } else {
    list.files(path = path, ...)
  }
}

is_fct_or_chr <- function(x) {
  is.factor(x) | is.character(x)
}

get_dir_size <- function(path) {
  print(path)
  files <- list.files(path, full.names = T, recursive = T)
  vect_size <- sapply(files, function(x)
    file.size(x))
  if (length(vect_size) == 0) {
    print("Directory is empty.")
    df <- data.frame("folder" = path,
                     "size" = 0,
                     "unit" = "MB")
    return(df)
  }
  size_files <- sum(vect_size)
  print(glue::glue("path: {path} \n size: {size_files/10**6} MB"))
  df <- data.frame("folder" = path,
                   "size" = size_files / 10 ** 6,
                   "unit" = "MB")

  return(df)
}

list_dir_sizes <- function(root_dir) {
  folders <- list.files(root_dir, full.names = T, recursive = F)

  foo <- lapply(folders, get_dir_size)
  df <- do.call(rbind, foo)
  df1 <- df[order(df$size, decreasing = T),]
  print("top 10 folders by size")
  sapply(1:10, function(i) {
    print(i)
    print(tail(strsplit(df1$folder[i], "/")[[1]], n = 1))
    print(paste0(df1$size[i], "MB"))
    # print(glue::glue("{)}MB"))
  })
  return(df1)
}


map_preserve_names_use_future <- function (list, fun)
{
  furrr::future_map(seq_along(list), function(i) {
    obj = list[i]
    names(obj) = names(list)[i]
    fun(obj)
  })
}

map_preserve_names <- function (list, fun)
{
  purrr::map(seq_along(list), function(i) {
    obj = list[i]
    names(obj) = names(list)[i]
    fun(obj)
  })
}
