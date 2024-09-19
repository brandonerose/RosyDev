.rs.restartR()
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()
Rosyverse::load_all()
RosyUtils::clear_env()
# SETUP ########################################################################
pkg_dir <- getwd()
pkg_name <- basename(pkg_dir)
dev_dir <- file.path(pkg_dir,"dev")
test_dir <- file.path(dev_dir,"test_dir")
# RosyUtils::view_file(pkg_name)
# RosyUtils::view_file(dev_dir)
RosyUtils::view_file(test_dir)
# FUNCTIONS ####################################################################

# devtools::load_all() # for all functions internal and external
library("your_package_here")
# library('tidyverse')

# IMPORT #######################################################################



# TIDY #########################################################################



# TRANSFORM ####################################################################



# COMMUNICATE ##################################################################



# SAVE #########################################################################

# save_dir

# SCRAP ########################################################################



# END ##########################################################################
