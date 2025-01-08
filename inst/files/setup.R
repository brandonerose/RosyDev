# SETUP ####
pkg_name <- basename(getwd())
golem::fill_desc(
  pkg_name = pkg_name, # The Name of the package containing the App
  pkg_title = "Rosy Dev", # The Title of the package containing the App
  pkg_description = "Rosy development for your R package.", # The Description of the package containing the App
  authors = person(
    given = "Brandon", # Your First Name
    family = "Rose", # Your Last Name
    role = "cre",
    email = "thecodingdocs@gmail.com" # Your Email
  ),
  repo_url = paste0("https://github.com/brandonerose/",pkg_name), # The URL of the GitHub Repo (optional),
  pkg_version = utils::packageVersion(pkg_name)
)
golem::set_golem_options()
usethis::use_mit_license("Brandon Rose") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "Brandon Rose")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_testthat()
usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("pr-commands")
usethis::use_pkgdown()
usethis::use_pkgdown_github_pages()
golem::remove_favicon() # Uncomment to remove the default favicon
logo_path <- "dev/logo.png"
file.copy(
  from = logo_path,
  to = "man/figures/logo.png",
  overwrite = T
)
file.copy(
  from = logo_path,
  to = "inst/app/www/logo.png",
  overwrite = T
)
golem::use_favicon("inst/app/www/logo.png") # path = "path/to/ico". Can be an online file.
# usethis::use_package("dplyr")
# usethis::use_dev_package("RosyDev")
usethis::use_git()
#system("git branch -m master main")
usethis::use_github(private = F)
RosyDev::setup_RosyDev(
  overwrite = T, # be careful will overwrite certain files
  use_golem = T,
  launch_files = T
)

usethis::use_pkgdown_github_pages()
