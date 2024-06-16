# .rs.restartR()
# remotes::install_github("brandonerose/Rosyverse")
# Rosyverse::update_all
(pkg_dir <- getwd())
(pkg_name <- basename(pkg_dir))
# devtools::load_all()

RosyDev::setup_RosyDev(overwrite = T) # be careful will overwrite certain files

RosyDev::dev_update()

usethis::use_version("minor")

dev_update()

devtools::build_readme()

RosyDev::fast_commit(comment = "dev")

devtools::check()

devtools::build()

#open files -----

RosyDev::pkg_dev_nav_combined()
RosyDev::pkg_dev_nav_dev()
RosyDev::pkg_dev_nav_setup()
