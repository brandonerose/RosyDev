#' @import RosyUtils
#' @title Update Dev
#' @description Update package from combined.R by documenting and combining files again
#' @param silent logical for messages
#' @return message
#' @export
dev_update <- function(
    silent = F,
    use_internal_pkg = T,
    is_production = F,
    overwrite = F
){
  dev_combine_split_R_files(choice = "both",silent = silent,overwrite = overwrite)
  devtools::document()
  attachment::att_amend_desc()
  golem::detach_all_attached()
  devtools::load_all()
  dev_combine_split_R_files(choice = "combine",silent = silent, overwrite = T)
  pkg_version <- as.character(utils::packageVersion(pkg_name))
  if(file.exists("inst/golem-config.yml")){
    copy_golem_to_wd()
    golem::amend_golem_config(key = "golem_name", value = pkg_name,talkative = F)
    golem::amend_golem_config(key = "golem_version", value = pkg_version,talkative = F)
    golem::amend_golem_config(key = "app_prod", is_production,talkative = F)
    options("golem.app.prod" = is_production)
  }
  check_for_update <- file.exists("dev/update_log.csv")
  update_log <- data.frame(file = character(0),mtime= character(0))
  if(check_for_update){
    update_log <-read.csv("dev/update_log.csv")
  }
  due_for_update <- F
  if(file.exists("README.Rmd")){
    ref_file <- "README.Rmd"
    do_it <- T
    if(check_for_update){
      if(any(update_log$file==ref_file)){
        do_it <- as.character(file.info(ref_file)$mtime)!=update_log$mtime[which(update_log$file==ref_file)]
      }
    }
    if(do_it){
      due_for_update <- T
    }
  }
  if(file.exists("vignettes")){
    ref_file <- "vignettes"
    test_for_vig <- list.files("vignettes") %>% tools::file_ext()
    test_for_vig <- "Rmd"%in% test_for_vig
    if(test_for_vig){
      do_it <- T
      if(check_for_update){
        if(any(update_log$file==ref_file)){
          do_it <- as.character(file.info(ref_file)$mtime)!=update_log$mtime[which(update_log$file==ref_file)]
        }
      }
      if(do_it){
        due_for_update <- T
      }
    }
  }
  if(use_internal_pkg){
    pkg_date <- Sys.Date()
    add_to_sysdata(pkg_name,pkg_version,pkg_date)
  }
  show_clickable_devs()
  write.csv(update_log,file = "dev/update_log.csv",row.names = F)
  if(due_for_update){
    bullet_in_console("Due for documentation update: `RosyDev::dev_document()`")
  }
}
#' @title dev_document
#' @description document
#' @return message
#' @export
dev_document <- function(pkgdown = F,force = F){
  check_for_update <- file.exists("dev/update_log.csv")
  update_log <- data.frame(file = character(0),mtime= character(0))
  if(check_for_update){
    update_log <-read.csv("dev/update_log.csv")
  }
  any_updates <- F
  if(file.exists("README.Rmd")){
    ref_file <- "README.Rmd"
    do_it <- T
    if(check_for_update){
      if(any(update_log$file==ref_file)){
        do_it <- as.character(file.info(ref_file)$mtime)!=update_log$mtime[which(update_log$file==ref_file)]
      }
    }
    if(do_it||force){
      devtools::build_readme()
      update_log <- update_log[which(update_log$file!=ref_file),] %>%
        rbind(
          data.frame(
            file = ref_file,
            mtime =file.info(ref_file)$mtime %>% as.character()
          )
        )
      any_updates <- T
    }
  }
  if(file.exists("vignettes")){
    ref_file <- "vignettes"
    test_for_vig <- list.files("vignettes") %>% tools::file_ext()
    test_for_vig <- "Rmd"%in% test_for_vig
    if(test_for_vig){
      do_it <- T
      if(check_for_update){
        if(any(update_log$file==ref_file)){
          do_it <- as.character(file.info(ref_file)$mtime)!=update_log$mtime[which(update_log$file==ref_file)]
        }
      }
      if(do_it||force){
        devtools::build_vignettes()
        update_log <- update_log[which(update_log$file!=ref_file),] %>%
          rbind(
            data.frame(
              file = ref_file,
              mtime =file.info(ref_file)$mtime %>% as.character()
            )
          )
        any_updates <- T
      }
    }
  }
  if(file.exists("pkgdown")&&pkgdown){
    ref_file <- "pkgdown"
    do_it <- T
    if(check_for_update){
      do_it <- any_updates
    }
    if(do_it||force){
      pkgdown::build_site_github_pages()
    }
  }
  if(any_updates||force){
    write.csv(update_log,"dev/update_log.csv",row.names = F)
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
#' @title Fast Commit
#' @description commit for git with one function
#' @inheritParams usethis::use_git
#' @inheritParams usethis::use_version
#' @return commited git
#' @export
fast_commit <- function(message = "dev", push = F){
  usethis::use_git(message = message)
  if(push){
    usethis:::git_push()
    url <- pkgload::pkg_desc()[["get"]]("URL") %>% strsplit(",") %>% unlist()
    url <- gsub("\n","",url) %>% trimws() %>% as.character()#affects link name
    if(length(url)>0){
      i <- 1
      blank <- "......................."
      mesge <- "Project info/code here:"
      for(u in url){
        bullet_in_console(ifelse(i==1,mesge,blank),url = u)
        i <- i+1
      }
    }else{
      bullet_in_console("Project info/code here:",file = getwd())
    }
  }
}
#' @title bump_version
#' @description bump version
#' @inheritParams usethis::use_version
#' @return bump
#' @export
bump_version <- function(which = "dev"){
  usethis::use_version(which = which)
  dev_update()
}
#' @title copy_golem_to_wd
#' @description copy minimum golem files to working directory
#' @inheritParams setup_RosyDev
#' @return files being copied if needed/wanted
copy_golem_to_wd <- function(overwrite = F, silent = T){
  if(!usethis:::is_package())stop("Your wd is not a package!")
  dir.create("inst",showWarnings = F)
  golem_files <- c(
    system.file("shinyexample","inst","golem-config.yml", package = "golem"),
    system.file("shinyexample","R","app_config.R", package = "golem")
  )
  for (golem_file in golem_files){
    bn <- basename(golem_file)
    dn <- basename(dirname(golem_file))
    path_to_new <- file.path(dn,bn)
    the_file_exisits <- file.exists(path_to_new)
    if(! the_file_exisits || overwrite){
      file.copy(
        from = golem_file,
        to = dn,
        overwrite = T
      )
      try({
        RosyUtils::replace_word_file(file = path_to_new, pattern = "shinyexample", replace = basename(getwd()))
      }, silent = TRUE)
    }
    if(!silent){
      if(the_file_exisits){
        message("Already a file: ",path_to_new)
        if(overwrite)message("overwritten!")
      }
    }
  }
}
#' @title delete_combined
#' @description delete combined.R file in dev for when you pull a new update from github
#' @return message
#' @export
delete_combined <- function(){
  if(!usethis:::is_package())stop("Your wd is not a package!")
  RosyUtils::delete_file(
    path = file.path(getwd(),"dev","combined.R")
  )
}
#' @title delete_combined_tests
#' @description delete tests.R file in dev for when you pull a new update from github
#' @return message
#' @export
delete_combined_tests <- function(){
  if(!usethis:::is_package())stop("Your wd is not a package!")
  RosyUtils::delete_file(
    path = file.path(getwd(),"dev","tests.R")
  )
}
#' @title delete_combined_vignettes
#' @description delete tests.R file in dev for when you pull a new update from github
#' @return message
#' @export
delete_combined_vignettes <- function(){
  if(!usethis:::is_package())stop("Your wd is not a package!")
  RosyUtils::delete_file(
    path = file.path(getwd(),"dev","vignettes.R")
  )
}
