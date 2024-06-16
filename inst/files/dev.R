# .rs.restartR()
library("RosyDev") # remotes::install_github("brandonerose/RosyDev")
(pkg_dir <- getwd())
(pkg_name <- basename(pkg_dir))
pkg_title <- "Rosy Develpoment"
# devtools::load_all()

setup_RosyDev()

dev_update()

usethis::use_version("dev")

dev_update()

devtools::build_readme()

fast_commit(comment = "dev")

devtools::check()

devtools::build()

#open files -----

pkg_dev_nav_combined()
pkg_dev_nav_dev()
pkg_dev_nav_setup()
