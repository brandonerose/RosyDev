# .rs.restartR()
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()
Rosyverse::load_all()
RosyUtils::clear_env()
# main =========================================================================
(pkg_dir <- getwd())
(pkg_name <- basename(pkg_dir))
# devtools::load_all()

RosyDev::setup_RosyDev(
  overwrite = F, # be careful will overwrite certain files
  use_golem = F,
  launch_files = T
)

# delete_combined() # for when you pull a new update from github

RosyDev::dev_update()

RosyDev::fast_commit(
  message = "dev",
  push = T,
  ask = T,
  bump_version = T,
  which = "dev"
)

RosyDev::fast_commit(
  message = "dev",
  push = T,
  ask = F,
  bump_version = F,
  which = "dev"
)

#other =========================================================================

devtools::build_readme()

usethis::use_version(which = "dev")

devtools::check()

devtools::build()

#open files ====================================================================

rstudioapi::filesPaneNavigate(getwd())
RosyDev::pkg_dev_nav_combined()
RosyDev::pkg_dev_nav_dev()
RosyDev::pkg_dev_nav_setup()
