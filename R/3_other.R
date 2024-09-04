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
