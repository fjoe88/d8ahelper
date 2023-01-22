

#' lapply wrapper but preserve object names as the name suggests
#'
#' @param list
#' @param fun
#'
#' @return
#' @export
#'
#' @examples
#' lapply_preserve_names(list(first.item=c(1,2,3), second.item=c(3,2,1)), function(x)paste0("each list member's name is accessible now! --> ", names(x)))
lapply_preserve_names <- function(list, fun) {
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
#' lapply_preserve_names_use_future(list(first.item=c(1,2,3), second.item=c(3,2,1)), function(x)paste0("each list member's name is accessible now! --> ", names(x)))
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
