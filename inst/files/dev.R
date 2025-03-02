# refresh packages  ============================================================
RosyUtils::clear_env()
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()# remotes::install_github("brandonerose/Rosyverse")
.rs.restartR()
# RESET DEV FILES ==============================================================
RosyDev::delete_combined()
# RosyDev::delete_dev()
RosyDev::dev_pull_and_update() # will delete dev and over write combined
# Setup ========================================================================
Rosyverse::load_all()
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
  launch_files = FALSE
)
RosyUtils::view_file(pkg_dir)
# RosyUtils::view_file(dev_dir)
RosyDev::pkg_dev_nav_combined()
# update =======================================================================
RosyDev::dev_update()
RosyDev::fast_commit()
# Style ========================================================================
lintr::lint(filename = "dev/combined.R")
lintr::all_linters() %>% names()
lintr::lint(filename = "dev/combined.R",linters = lintr::all_linters()[["object_name_linter"]])
styler::style_file(path = "dev/combined.R",strict = TRUE)
formatR::tidy_dir(file = "R",width.cutoff = 80)
styler::style_file(path = "dev/tests.R")
lintr::lint(filename = "dev/tests.R")
formatR::tidy_file(file = "dev/tests.R",width.cutoff = 80)
goodpractice::goodpractice()
RosyDev::wrap_roxygen_text()
RosyDev::remove_trailing_whitespace()
# document =====================================================================
RosyDev::bump_version(which = "dev")
 #usethis::use_version(which = "dev")
# quick checks =================================================================
devtools::test()
devtools::check_man()
checkhelper::check_as_cran()
checkhelper::find_missing_tags()
devtools::build_readme()
RosyDev::dev_document()
# update and push ==============================================================
RosyDev::dev_update()
RosyDev::fast_commit(message = "dev",push = TRUE)
# more checks ==================================================================
devtools::test()
codetools::checkUsagePackage("your_package_here", suppressLocal = TRUE)
devtools::check_man()
covrpage::covrpage()
pkgdown::build_site_github_pages()
RosyDev::run_test_dev()
RosyDev::run_test_prod()
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
#open files ====================================================================

RosyUtils::view_file(pkg_dir)
RosyUtils::view_file(dev_dir)
RosyUtils::view_file(test_dir)
RosyDev::pkg_dev_nav_combined()
RosyDev::pkg_dev_nav_dev()
RosyDev::pkg_dev_nav_setup()
RosyDev::pkg_dev_nav_test_dev()
RosyDev::pkg_dev_nav_test_prod()

# scrap ========================================================================


