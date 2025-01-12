#' @title Package System
#' @description Find the file system of a package
#' @param silent logical for messages
#' @param launch_files logical for launching files
#' @param overwrite logical for overwrite
#' @param use_golem logical for using golem
#' @return path
#' @export
setup_RosyDev <- function(
    silent = FALSE,
    launch_files = FALSE,
    overwrite = FALSE,
    use_golem = FALSE,
    only_if_imported = TRUE) {
  usethis:::check_is_package()
  # usethis::use_pipe(export = FALSE)
  pkg_dir <- getwd()
  pkg_name <- basename(pkg_dir)
  dev_dir <- file.path(pkg_dir, "dev")
  test_dir <- file.path(dev_dir, "test_dir")
  if (!silent) message("pkg_dir: ", pkg_dir)
  dir.create(dev_dir, showWarnings = FALSE)
  dir.create(test_dir, showWarnings = FALSE)
  copy_these <- system.file(
    file.path(
      "files",
      c(
        "gitignore",
        "Rbuildignore",
        "setup.R",
        "dev.R",
        "test_dev.R",
        "test_prod.R",
        "README.Rmd"
      )
    ),
    package = "RosyDev"
  )
  paste_here <- c(
    file.path(pkg_dir, ".gitignore"),
    file.path(pkg_dir, ".Rbuildignore"),
    file.path(dev_dir, "setup.R"),
    file.path(dev_dir, "dev.R"),
    file.path(dev_dir, "test_dev.R"),
    file.path(dev_dir, "test_prod.R"),
    file.path(pkg_dir, "README.Rmd")
  )
  for (i in 1:length(copy_these)) {
    the_file_exisits <- file.exists(paste_here[i])
    if (the_file_exisits) {
      message("Already a file: ", paste_here[i])
      if (overwrite) message("overwritten!")
    }
    was_copied <- file.copy(
      from = copy_these[i],
      to = paste_here[i],
      overwrite = overwrite
    )
    if (was_copied) {
      if (basename(copy_these[i]) %in% c("dev.R", "README.Rmd", "test_prod.R")) {
        RosyUtils::replace_word_file(
          file = paste_here[i],
          pattern = "your_package_here",
          replace = basename(pkg_dir)
        )
      }
    }
  }
  dev_combine_split_R_files()
  if (launch_files) launch_devs()
  # copy_to <- file.path("man","figures")
  copy_to <- file.path("inst", "app", "www") # can fix this later
  if (use_golem) {
    copy_golem_to_wd(
      overwrite = overwrite,
      silent = silent
    )
    copy_to <- file.path("inst", "app", "www")
    copy_logos_to_package(copy_to = copy_to, only_if_imported = only_if_imported)
  }
  # if(use_pkgdown){ # would have to have github setup already... so hold for now
  #   do_it <- TRUE
  #   if(file.exists("pkgdown")){
  #     do_it <- utils::menu(choices = c("Yes", "No"),title = "You are about to run `usethis::use_pkgdown_github_pages()` but it already pkgdown folder already exists. Are you sure?")==1
  #     if(do_it){
  #       usethis::use_pkgdown_github_pages()
  #     }
  #   }
  # }
  show_clickable_devs()
}
