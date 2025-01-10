# refresh packages =============================================================
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()# remotes::install_github("brandonerose/Rosyverse")
RosyDev::dev_pull_and_update()
.rs.restartR()
# LOAD =========================================================================
RosyUtils::clear_env()
Rosyverse::load_all()
# Setup ========================================================================
pkg_dir <- getwd()
pkg_name <- basename(pkg_dir)
dev_dir <- file.path(pkg_dir,"dev")
test_dir <- file.path(dev_dir,"test_dir")
RosyUtils::view_file(pkg_dir)
# RosyUtils::view_file(dev_dir)
# RosyUtils::view_file(test_dir)
RosyDev::setup_RosyDev(
  overwrite = FALSE, # be careful will overwrite certain files
  use_golem = FALSE,
  launch_files = TRUE
)
# document =====================================================================
RosyDev::bump_version(which = "dev")
RosyDev::dev_document()
# update and push ==============================================================
RosyDev::dev_update()
RosyDev::fast_commit(
  message = "dev",
  push = TRUE
)
#other =========================================================================
devtools::test()
codetools::checkUsagePackage("your_package_here", suppressLocal = TRUE)
devtools::check_man()
covrpage::covrpage()
checkhelper::check_as_cran()
goodpractice::goodpractice()
# checkhelper::get_notes()
# checkhelper::print_globals()
covr::package_coverage(path = getwd())
covr::report()
x<-checkhelper::find_missing_tags()
data_check <- x$data
function_check <- x$functions
function_check <- function_check[which(function_check$test_has_export_and_return =="not_ok"|function_check$test_has_export_or_has_nord =="not_ok"),]
missing_export_and_return <- function_check$topic[which(function_check$test_has_export_and_return =="not_ok")]%>% unique()
missing_export_or_nord <- function_check$topic[which(function_check$test_has_export_or_has_nord =="not_ok")] %>% unique()
if(length(missing_export_and_return)>0)RosyUtils::vec_cat(missing_export_and_return)
if(length(missing_export_or_nord)>0)RosyUtils::vec_cat(missing_export_or_nord)

extract_function_calls <- function(file) {
  parsed <- parse(file)
  calls <- sapply(parsed, function(x) {
    if (is.call(x)) {
      all.names(x)
    } else {
      NULL
    }
  })
  unique(unlist(calls))
}
x<-extract_function_definitions("dev/combined.R")
x[dw(x)]
get_external_functions("your_package_here") %>% vec_cat("- ")

#other2 =========================================================================

devtools::build_readme()
usethis::use_version(which = "dev")
pkgdown::build_site_github_pages()
devtools::check()
devtools::build()
RosyDev::run_test_dev()
RosyDev::run_test_prod()

#open files ====================================================================

RosyUtils::view_file(pkg_dir)
RosyUtils::view_file(dev_dir)
RosyUtils::view_file(test_dir)
RosyDev::pkg_dev_nav_combined()
RosyDev::pkg_dev_nav_dev()
RosyDev::pkg_dev_nav_setup()
RosyDev::pkg_dev_nav_test_dev()
RosyDev::pkg_dev_nav_test_prod()
