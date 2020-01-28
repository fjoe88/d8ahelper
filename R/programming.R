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
