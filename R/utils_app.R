is_categorical <- function(x){
  is.factor(x) || is.logical(x) || is.character(x)
}

from_vars_to_choice_list <- function(x){
  if (purrr::is_empty(x)) return(list(NA))
  if (length(x) == 1) return(list(x[1]))
  x
}