# .rs.restartR()
library("RosyDev") # remotes::install_github("brandonerose/RosyDev")
(pkg_dir <- getwd())
(pkg_name <- basename(pkg_dir))
pkg_title <- "Rosy Develpoment"
# devtools::load_all()

setup_Rosydev()

dev_update()

usethis::use_version("dev")

dev_update()

devtools::build_readme()

fast_commit(comment = "dev")

devtools::check()

devtools::build()



