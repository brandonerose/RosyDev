#' @title check_namespace_conflicts
#' @param pkgs character vector of pkgs
#' @export
check_namespace_conflicts<-function(pkgs){
  # pkgs <- pkgs %>% sort()
  x <- NULL
  for(pkg in pkgs){
    library(pkg, character.only = T)
    exported_functions <- ls(paste0("package:",pkg))
    internal_functions <-
      x <- x %>% rbind(
        data.frame(
          pkg = pkg,
          name = ls(paste0("package:",pkg)),
          type = "exported"
        )
      )
    x <- x %>% rbind(
      data.frame(
        pkg = pkg,
        name = setdiff(ls(envir = asNamespace(pkg), all.names = TRUE), exported_functions),
        type = "internal"
      )
    )
  }
  y <- data.frame(
    function_name = unique(x$name)
  )
  y$int_int_conflict <- y$function_name %>%  sapply(function(function_name){
    ROWS <- which(x$type%in%c("internal") & x$name == function_name)
    z <- x$pkg[ROWS] %>% unique()
    if(length(z)<2) return(NA)
    z %>% paste0(collapse = " | ") %>% return()
  })
  y$int_exp_conflict <- y$function_name %>%  sapply(function(function_name){
    ROWS <- which(x$type%in%c("internal","exported") & x$name == function_name)
    z <- x$pkg[ROWS] %>% unique()
    if(length(z)<2) return(NA)
    z %>% paste0(collapse = " | ") %>% return()
  })
  y$exp_exp_conflict <- y$function_name %>%  sapply(function(function_name){
    ROWS <- which(x$type%in%c("exported") & x$name == function_name)
    z <- x$pkg[ROWS] %>% unique()
    if(length(z)<2) return(NA)
    z %>% paste0(collapse = " | ") %>% return()
  })
  y <- y[order(y$int_int_conflict, decreasing = T),]
  y <- y[order(y$int_exp_conflict, decreasing = T),]
  y <- y[order(y$exp_exp_conflict, decreasing = T),]
  z<- y[which(!is.na(y$exp_exp_conflict)),]
  if(nrow(z)>0){
    z$function_name <- stringr::str_pad(
      string = z$function_name,
      width = (z$function_name %>% nchar() %>% max())+2,
      side = "right"
    )
    message("Below are some conflicts with exported names...\n",z$function_name %>% sapply(function(function_name){
      ROW <- which(z$function_name==function_name)
      return(paste0("   ",z$function_name[ROW],"-->  ",z$exp_exp_conflict[ROW]))
    }) %>% paste0(collapse = "\n"))
  }else{
    message("No major conflicts!")
  }
  return(y)
}
copy_logos_to_package <- function(copy_to = file.path("inst","app","www")){
  usethis:::check_is_package()
  pkg_dir <- getwd()
  pkg_name <- basename(pkg_dir)
  named_list <- Rosyverse::get_logo_paths(name_vec = c(pkg_name,"Rosyverse","TCD","TCDblack","TCDclear"))
  dir.create(copy_to,recursive = T,showWarnings = F)
  for(i in 1:length(named_list)){
    was_copied <- file.copy(
      from = named_list[[i]],
      to = file.path(copy_to,paste0(names(named_list[i]),".png")),
      overwrite = F
    )
  }
  logo_path <- file.path(copy_to,paste0(pkg_name,".png"))
  if(file.exists(logo_path)){
    file.copy(
      from = logo_path,
      to = file.path(copy_to,"logo.png"),
      overwrite = F
    )
  }
}
#' @title run_test_prod
#' @export
run_test_prod <- function(){
  source(file.path(getwd(),"dev","test_prod.R"))
}
#' @title run_test_dev
#' @export
run_test_dev <- function(){
  source(file.path(getwd(),"dev","test_dev.R"))
}
run_test_prod <- function(){
  source(file.path(getwd(),"dev","test_prod.R"))
}
