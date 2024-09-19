.rs.restartR()
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all()
Rosyverse::load_all()
RosyUtils::clear_env()
# SETUP ########################################################################
(pkg_dir <- getwd())
(pkg_name <- basename(pkg_dir))
save_dir <- file.path(pkg_dir,"dev","test_dir")
dir.create(save_dir,showWarnings = F)
# FUNCTIONS ####################################################################

devtools::load_all() # for all functions internal and external
# library('tidyverse')

# IMPORT #######################################################################



# TIDY #########################################################################



# TRANSFORM ####################################################################



# COMMUNICATE ##################################################################



# SAVE #########################################################################

# save_dir

# SCRAP ########################################################################



# END ##########################################################################
