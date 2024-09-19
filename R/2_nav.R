#' @title pkg_dev_nav
#' @description Navigate to dev file in RStudio
#' @param name character string for the file name to be opened
#' @return file opens in RStudio
#' @export
pkg_dev_nav <- function(name = NULL,pane = T){
  path <- dev_dir <- file.path(getwd(),"dev")
  test_dir <- file.path(dev_dir,"test_dir")
  if(is.null(name))print(list.files(dev_dir))
  allowed_names <- c("combined.R","setup.R","dev.R","test_dev.R","test_prod.R","test_dir")
  if(name%in%allowed_names){
    path <- file.path(path,name)
    launch_dir <- dev_dir
    if(name%in%c("test_dev.R","test_prod.R")){
      launch_dir <- test_dir
    }
    if(pane)RosyUtils::view_file(launch_dir)
  }
  RosyUtils::view_file(path = path)
}
#' @title pkg_dev_nav_combined
#' @description Navigate to combined.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_combined <- function(pane = T){
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
#' @title pkg_dev_nav_test_dev
#' @description Navigate to dev.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_test_dev <- function(){
  pkg_dev_nav("test_dev.R")
}
#' @title pkg_dev_nav_test_dev
#' @description Navigate to dev.R dev file in RStudio
#' @return file opens in RStudio
#' @export
pkg_dev_nav_test_prod <- function(){
  pkg_dev_nav("test_prod.R")
}
