is_categorical <- function(x){
  is.factor(x) || is.logical(x) || is.character(x)
}

from_vars_to_choice_list <- function(x){
  if (purrr::is_empty(x)) return(list(NA))
  if (length(x) == 1) return(list(x[1]))
  x
}

get_inst_file <- function(local_path, package = pkgload::pkg_name()){
  path <- system.file(local_path, package = package)
  
  if (nchar(path) == 0) { 
    rlang::abort(
      class = "error_inst_file_not_found",
      message = sprintf(
        "File `%s` not found in global path `%s`",
        local_path,
        system.file(package = package)
      )
    )
  }
  path
}

read_inst_rds <- function(local_path, package = pkgload::pkg_name()){
  file <- get_inst_file(local_path, package)
  readRDS(file)
}

