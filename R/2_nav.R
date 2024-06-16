#' @title pkg_dev_nav
#' @description Navigate to dev file in RStudio
#' @param name character string for the file name to be opened
#' @return file opens in RStudio
#' @export
pkg_dev_nav <- function(name = NULL){
  dev_dir <- file.path(getwd(),"dev")
  if(is.null(name))return(list.files(dev_dir))
  rstudioapi::navigateToFile(file.path(dev_dir,paste0(name,".R")))
}
#' @title pkg_dev_nav_combined
#' @description Navigate to combined.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_combined <- function(){
  pkg_dev_nav("combined")
}
#' @title pkg_dev_nav_setup
#' @description Navigate to setup.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_setup <- function(){
  pkg_dev_nav("setup")
}
#' @title pkg_dev_nav_dev
#' @description Navigate to dev.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_dev <- function(){
  pkg_dev_nav("dev")
}
