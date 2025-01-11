#' @title pkg_dev_nav
#' @description Navigate to dev file in RStudio
#' @param name character string for the file name to be opened
#' @return file opens in RStudio
#' @export
pkg_dev_nav <- function(name = NULL,pane = TRUE){
  path <- dev_dir <- file.path(getwd(),"dev")
  test_dir <- file.path(dev_dir,"test_dir")
  if(is.null(name))print(list.files(dev_dir))
  allowed_names <- c("combined.R","vignettes.Rmd", "tests.R","setup.R","dev.R","test_dev.R","test_prod.R","test_dir")
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
pkg_dev_nav_combined <- function(pane = TRUE){
  pkg_dev_nav("vignettes.Rmd",pane = FALSE)
  pkg_dev_nav("tests.R",pane = FALSE)
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
file_paths_dev <- function(){
  usethis:::check_is_package()
  pkg_dir <- getwd()
  pkg_name <- basename(pkg_dir)
  dev_dir <- file.path(pkg_dir,"dev")
  file_paths <- NULL
  for(file_path in c(file.path(dev_dir,"setup.R"),
                     file.path(pkg_dir,"README.Rmd"),
                     file.path(pkg_dir,"NEWS.md"),
                     file.path(dev_dir,"dev.R"),
                     file.path(dev_dir,"test_dev.R"),
                     file.path(dev_dir,"test_prod.R"),
                     file.path(dev_dir,"vignettes.Rmd"),
                     file.path(dev_dir,"tests.R"),
                     file.path(dev_dir,"combined.R"))
  ){
    if(file.exists(file_path))file_paths <- append(file_paths,file_path)
  }
  return(file_paths)
}
show_clickable_devs <- function(){
  bullet_in_console("Click below to open dev files...")
  for(file_path in file_paths_dev()){
    names(file_path) <- basename(file_path)
    bullet_in_console(file = file_path,bullet_type = ifelse(file.exists(file_path), ">","x"))
  }
}
launch_devs <- function(){
  for(file_path in file_paths_dev()){
    RosyUtils::view_file(file_path)
  }
}
