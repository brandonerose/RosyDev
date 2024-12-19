# refresh packages =============================================================
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()# remotes::install_github("brandonerose/Rosyverse")
.rs.restartR()
# LOAD =========================================================================
RosyUtils::clear_env()
Rosyverse::load_all()
# pull first =========================
usethis:::git_pull()
RosyDev::delete_combined() # for when you pull a new update from github
# Setup ========================================================================
pkg_dir <- getwd()
pkg_name <- basename(pkg_dir)
dev_dir <- file.path(pkg_dir,"dev")
test_dir <- file.path(dev_dir,"test_dir")
RosyUtils::view_file(pkg_dir)
# RosyUtils::view_file(dev_dir)
# RosyUtils::view_file(test_dir)
RosyDev::setup_RosyDev(
  overwrite = F, # be careful will overwrite certain files
  use_golem = F,
  launch_files = T
)
# update and push ----------------------------------
RosyDev::dev_update(document = F)
RosyDev::fast_commit(
  message = "dev",
  push = T,
  ask = F,
  bump_version = F,
  which = "dev"
)
# update document and push ----------------------------------
RosyDev::dev_update()
RosyDev::fast_commit(
  message = "dev",
  push = T,
  ask = F,
  bump_version = F,
  which = "dev"
)
# bump version update and push  ================================================
RosyDev::fast_commit(
  message = "dev",
  push = T,
  ask = T,
  bump_version = T,
  which = "dev"
)
#other =========================================================================

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
