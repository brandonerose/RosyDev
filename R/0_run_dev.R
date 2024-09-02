#' @title Update Dev
#' @description Update package from combined.R by documenting and combining files again
#' @param silent logical for messages
#' @return message
#' @export
dev_update <- function(silent = F,use_internal_pkg = T){
  usethis:::check_is_package()
  pkg_dir <- getwd()
  if( ! silent) message("pkg_dir: ",pkg_dir)
  pkg_name <- basename(pkg_dir)
  if( ! silent) message("pkg_name: ",pkg_name)
  if(!file.exists(file.path(pkg_dir,"dev","combined.R")))combine_R_files()
  split_R_files()
  devtools::document()
  attachment::att_amend_desc()
  golem::detach_all_attached()
  # devtools::unload()
  devtools::load_all()
  combine_R_files()
  # rstudioapi::navigateToFile("dev/combined.R")
  if(use_internal_pkg){
    pkg_version <- as.character(utils::packageVersion(pkg_name))
    pkg_date <- Sys.Date()
    add_to_sysdata(pkg_name,pkg_version,pkg_date)
  }
}
#' @title add_to_sysdata
#' @description Load sysdata.rda if it exists and add objects in `...` to it.
#' @param silent logical for messages
#' @return message
#' @export
add_to_sysdata <- function(..., silent = F,overwrite = F){
  objs <- usethis:::get_objs_from_dots(usethis:::dots(...))
  usethis:::check_is_package()
  temp_env <- new.env()
  path <- "R/sysdata.rda"
  if(file.exists(path) && ! overwrite){
    load(path, envir = temp_env)
    if(!silent) message("RosyDev loaded: ",names(temp_env) %>% paste0(collapse = ", "))
  }
  for(object_name in objs){
    if(!silent) ifelse(object_name %in% objects(envir = temp_env),"Updated: ","Added: ") %>% message(object_name)
    assign(object_name, get(object_name, envir = parent.frame()), envir = temp_env)
  }
  mapply(
    save,
    list = list(names(temp_env)),
    file = path,
    MoreArgs = list(envir = temp_env, compress = "bzip2", version = 2)
  )
  if(!silent) message("RosyDev saved: ",names(temp_env) %>% paste0(collapse = ", "))
}
#' @title Package System
#' @description Find the file system of a package
#' @param silent logical for messages
#' @param launch_files logical for launching files
#' @param overwrite logical for overwrite
#' @return path
#' @export
setup_RosyDev <- function(silent = F,launch_files = T,overwrite = F){
  usethis:::check_is_package()
  usethis::use_pipe()
  pkg_dir <- getwd()
  if( ! silent) message("pkg_dir: ",pkg_dir)
  dev_dir <- file.path(pkg_dir,"dev")
  dir.create(dev_dir,showWarnings = F)
  copy_these <- system.file(file.path("files",c("gitignore","Rbuildignore","setup.R","dev.R")),package = "RosyDev")
  paste_here <- c(
    file.path(pkg_dir,".gitignore"),
    file.path(pkg_dir,".Rbuildignore"),
    file.path(dev_dir,"setup.R"),
    file.path(dev_dir,"dev.R")
  )
  for(i in 1:length(copy_these)){
    the_file_exisits <- file.exists(paste_here[i])
    if(the_file_exisits){
      message("Already a file: ",paste_here[i])
      if(overwrite)message("overwritten!")
    }
    file.copy(
      from = copy_these[i],
      to = paste_here[i],
      overwrite = overwrite
    )
  }
  if(overwrite||!file.exists(file.path(pkg_dir,"dev","combined.R")))combine_R_files()
  if(launch_files){
    file_paths <- c(
      file.path(pkg_dir,"README.Rmd"),
      file.path(pkg_dir,"NEWS.md"),
      file.path(dev_dir,"setup.R"),
      file.path(dev_dir,"dev.R"),
      file.path(dev_dir,"combined.R")
    )
    for(file_path in file_paths){
      if(file.exists(file_path)) rstudioapi::navigateToFile(file_path)
    }
  }
  message("RosyDev setup successful!")
}
#' @title Fast Commit
#' @description commit for git with one function
#' @param message character string for commit message
#' @param push logical for git push. Also see `ask`.
#' @param ask logical for asking before final git push
#' @param bump_version logical for bumping version. Will also run `dev_update` again. See `which.`
#' @inheritParams usethis::use_version
#' @return commited git
#' @export
fast_commit <- function(message = "dev", push = F,ask = T, bump_version = F, which = "dev"){
  usethis::use_git(message = message)
  if(bump_version){
    usethis::use_version(which = which)
    dev_update()
    usethis::use_git(message = message)
  }
  if(push){
    choice <- T
    if(ask){
      choice <- utils::menu(choices = c("Yes", "No"),title = "You are about to push to git based on what has been committed. Are you sure?")==1
    }
    if(choice) usethis:::git_push()
  }
}
