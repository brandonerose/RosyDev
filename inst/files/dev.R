# .rs.restartR()
library("RosyDev") # remotes::install_github("brandonerose/RosyDev")
(pkg_dir <- getwd())
(pkg_name <- basename(pkg_dir))
# devtools::load_all()

setup_RosyDev(overwrite = T) # be careful will overwrite certain files

dev_update()

usethis::use_version("minor")

dev_update()

devtools::build_readme()

fast_commit(comment = "dev")

devtools::check()

devtools::build()

#open files -----

pkg_dev_nav_combined()
pkg_dev_nav_dev()
pkg_dev_nav_setup()
