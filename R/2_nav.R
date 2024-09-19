#' @title pkg_dev_nav
#' @description Navigate to dev file in RStudio
#' @param name character string for the file name to be opened
#' @return file opens in RStudio
#' @export
pkg_dev_nav <- function(name = NULL){
  path <- dev_dir <- file.path(getwd(),"dev")
  if(is.null(name))print(list.files(dev_dir))
  allowed_names <- c("combined.R","setup.R","dev.R","test_dev.R","test_prod.R","test_dir")
  if(name%in%allowed_names)path <- file.path(path,name)
  RosyUtils::view_file(path = path)
}
#' @title pkg_dev_nav_combined
#' @description Navigate to combined.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_combined <- function(){
  pkg_dev_nav("combined.R")
}
#' @title pkg_dev_nav_setup
#' @description Navigate to setup.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_setup <- function(){
  pkg_dev_nav("setup.R")
}
#' @title pkg_dev_nav_dev
#' @description Navigate to dev.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_dev <- function(){
  pkg_dev_nav("dev.R")
}
