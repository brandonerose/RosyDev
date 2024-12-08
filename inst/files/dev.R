# .rs.restartR()
# remotes::install_github("brandonerose/Rosyverse")
# Rosyverse::update_all()
Rosyverse::load_all()
RosyUtils::clear_env()
# main =========================================================================
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
  launch_files = F
)
RosyDev::pkg_dev_nav_combined()
# RosyDev::delete_combined() # for when you pull a new update from github

# update and push ----------------------------------

RosyDev::dev_update()
RosyDev::fast_commit(
  message = "dev",
  push = T,
  ask = F,
  bump_version = F,
  which = "dev"
)

# refresh packages ======================

remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()
.rs.restartR()

#  ============================================

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
