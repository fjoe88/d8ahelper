
#' lapply wrapper but preserve object names as the name suggests
#'
#' @param list
#' @param fun
#'
#' @return
#' @export
#'
#' @examples
lapply_preserve_names <- function(list, fun){

  lapply(seq_along(list), function(i) {
    obj = list[i]
    names(obj) = names(list)[i]
    fun(obj)
  })
}

# lapply wrapper but preserve object names, use future package to add parallel processing
#'
#' @param list
#' @param fun
#'
#' @return
#' @export
#'
#' @examples
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

#' wraper function list.files to allow coppied windows path format containing backward slashes
#'
#' @param path
#' @param return_str
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
list_files_fwd_slash <- function(path = readr::clipboard(), return_str = TRUE, ...) {

  path <- stringr::str_replace_all(path, "\\\\+", "/")

  if (return_str == TRUE){
    return(path)
  } else {
    list.files(path = path, ...)
  }
}

#' check if is factor or charactor
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_fct_or_chr <- function(x) {
  is.factor(x) | is.character(x)
}

#' get directory size
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
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

#' list directory sizes
#'
#' @param root_dir
#'
#' @return
#' @export
#'
#' @examples
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

#' map but preserve object names
#'
#' @param list
#' @param fun
#'
#' @return
#' @export
#'
#' @example
#' map_preserve_names(list(a=1, b=2), function(x)x+1)
#' map_preserve_names(list(a=c(1,2,3), b=c(2,3,4)), sum)
map_preserve_names <- function (list, fun)
{
  purrr::map(seq_along(list), function(i) {
    obj = list[i][[1]]
    return_obj <- fun(obj)
    names(return_obj) = names(list)[i]
    return(return_obj)
  })
}


#' map but preserve object names, use future to add parallel processing
#'
#' @param list
#' @param fun
#'
#' @return
#' @export
#'
#' @example
#' map_preserve_names_use_future(list(a=1, b=2), function(x)x+1)
#' map_preserve_names_use_future(list(a=c(1,2,3), b=c(2,3,4)), sum)
map_preserve_names_use_future <- function (list, fun)
{
  furrr::future_map(seq_along(list), function(i) {
    obj = list[i][[1]]
    return_obj <- fun(obj)
    names(return_obj) = names(list)[i]
    return(return_obj)
  })
}
