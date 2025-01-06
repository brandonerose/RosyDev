#' @title combine_R_files
#' @param source_dir a file path for your source (such as R folder)
#' @param destination_dir a file path for your destination (such as dev folder)
#' @param filename a file name (ends in .R)
#' @param header_symbol single character for your header separator in combined file
#' @param max_new_lines integer for max number of new lines
#' @param new_lines character vector for new lines
#' @param overwrite logical for overwriting original file
#' @return message
#' @export
combine_R_files <- function(source_dir = file.path(getwd(),"R"), destination_dir=file.path(getwd(),"dev"),filename="combined.R",header_symbol = "=",max_new_lines=0,new_lines=character(0),overwrite = T) {
  # if(!file.exists(source_dir)){
  #   installed_packages <- installed.packages() %>% as.data.frame()
  #   pkgs <- installed_packages$Package %>% as.character()
  #   if(!source_dir%in%pkgs){
  #     stop("Your `source_dir` does not exist as a folder AND it is not the name of one of your installed packages!")
  #   }
  #   source_dir <- system.file("R",package = source_dir)
  # }
  # if(!file.exists(destination_dir)){
  #   the_wd <- getwd()
  #   message("`destination_dir` does not exist. Will default to your working directory... \nInstead of: '",destination_dir,"'\nWill Use: '",the_wd,"'")
  #   destination_dir <- the_wd
  # }
  file_list <- list.files(source_dir, pattern = "\\.R$", full.names = TRUE)
  combined_text <- character(0)
  for (file in file_list) {# file <- file_list %>% sample(1)
    file_name <- tools::file_path_sans_ext(basename(file))
    header <- paste0("# ", file_name, " ")
    header <- paste0(header,  paste0(rep(header_symbol,80-nchar(header)), collapse=""))
    combined_text <- c(combined_text, header,new_lines, readLines(file))
  }
  message(length(combined_text)," lines")
  combined_text <-paste(combined_text, collapse = "\n")
  combined_text <- gsub(paste0("\\n{",max_new_lines+2,",}"), "\n", combined_text)
  destination_file <- file.path(destination_dir, filename)
  if(!file.exists(destination_file)||overwrite){
    writeLines(combined_text, destination_file)
    bullet_in_console("Combined file saved to:",file = destination_file,bullet_type = "v")
  }
}
#' @title split_R_files
#' @inheritParams combine_R_files
#' @return message
#' @export
split_R_files <- function(source_dir= file.path(getwd(),"dev"), destination_dir=file.path(getwd(),"R"),filename = "combined.R",header_symbol = "=",new_lines=character(0)){
  file_content <- readLines(file.path(source_dir,filename))
  split_indices <- grep(paste0("^# .* ",paste0(rep(header_symbol,4),collapse=""), collapse=""), file_content)
  split_indices <- as.list(split_indices)
  scripts <- NULL
  while (length(split_indices)>0) {
    start_index <- split_indices[[1]]+1
    if(length(split_indices)==1){
      end_index <- length(file_content)
    }else{
      end_index <- split_indices[[2]]-1
    }
    if(start_index>end_index){
      out_lines <- ""
    }else{
      out_lines <- file_content[start_index:end_index]
    }
    scripts[[gsub(paste0("#| |",header_symbol), "", file_content[split_indices[[1]]])]] <- out_lines
    split_indices[[1]] <- NULL
  }
  for(i in seq_along(scripts)){
    output_file <- file.path(destination_dir, paste0(names(scripts)[i], ".R"))
    writeLines(
      new_lines %>% append(scripts[[i]]) %>%paste0(collapse = "\n"),
      con = output_file
    )
    cat("File saved:", output_file, "\n")
  }
}
execute_with_tryCatch <- function(expr) {
  result <- tryCatch(
    {
      eval(expr)  # Evaluate the expression
      TRUE        # If no error, return TRUE
    },
    error = function(e) {
      FALSE       # If error occurs, return FALSE
    }
  )
  return(result)
}
download_and_extract_source_R_files <- function(pkg) {
  temp_dir <- tempdir()
  installed_packages <- installed.packages() %>% as.data.frame()
  is_base <- pkg %in% installed_packages$Package[which(installed_packages$Priority =="base")]
  if(is_base){
    r_version <- R.Version()
    base_file_name <- paste0("R-", paste(r_version$major, r_version$minor, sep = "."))
    tarball_file_name <- paste0(base_file_name, ".tar.gz")
    tarball_url <- paste0("https://cran.r-project.org/src/base/R-", r_version$major, "/",tarball_file_name)
    temp_dir <- tempdir()
    tar_file <- file.path(temp_dir,tarball_file_name)
    was_downloaded <- tryCatch({
      download.file(tarball_url, tar_file)
      TRUE
    },error = function(e) {FALSE})
  }else{
    tar_file<-download.packages(pkg, destdir = temp_dir, type = "source")
    if(length(tar_file)>0){
      was_downloaded <- T
      tar_file <- tar_file[,2]
    }else{
      was_downloaded <- F
    }
  }
  if (was_downloaded) {
    untar(tarfile = tar_file, exdir = temp_dir)
  }
  return(was_downloaded)
}
#' @title pkg_combine_R_files
#' @param pkgs package name(s) as character string
#' @param filename_type character string of type filename: "name__version" or "name"
#' @return message
#' @export
pkg_combine_R_files <- function(pkgs, destination_dir=getwd(),filename_type="name__version",header_symbol = "=",max_new_lines=0,new_lines=character(0),overwrite = F,launch_file = 0) {
  installed_packages <- installed.packages() %>% as.data.frame()
  installed_packages$comparison_name <- installed_packages$Package
  if(filename_type=="name__version"){
    installed_packages$comparison_name <- paste0(installed_packages$Package,"__",gsub("\\.|\\-","_",installed_packages$Version))
  }
  installed_packages$file_name <- installed_packages$comparison_name %>% paste0(".R")
  if(!file.exists(destination_dir)){
    if(utils::menu(c("Yes","No and stop"),title = paste0("Your listed directory does not exist! Would you like to create? ",destination_dir))==2){
      stop("Stopped!")
    }
    dir.create(destination_dir,showWarnings = F)
  }
  pkgs_possible <- installed_packages$Package %>% as.character()
  BAD <- pkgs[which(!pkgs%in%pkgs_possible)]
  if(length(BAD)>0)stop("You have listed pkgs that are not installed: ",BAD %>% paste0(collapse = ", "))
  pkgs_df <- installed_packages[which(installed_packages$Package%in%pkgs),]
  downloaded_file_list <-
    destination_dir %>%
    list.files(pattern = "\\.R$", full.names = TRUE) %>%
    basename() %>%
    tools::file_path_sans_ext()
  THERE <- pkgs_df$comparison_name %in% downloaded_file_list
  pkgs_there <- pkgs_df$Package[which(THERE)]
  pkgs_missing <- pkgs_df$Package[which(!THERE)]
  if(length(pkgs_there)>0) message("Packages already there: ",pkgs_there %>% paste0(collapse = ", "))
  if(length(pkgs_missing)>0) message("Packages missing: ",pkgs_missing %>% paste0(collapse = ", "))
  if( ! overwrite){
    if(filename_type=="name__version"){
      downloaded_file_list <-
        downloaded_file_list %>%
        strsplit("__") %>%
        sapply(function(x){x[[1]]}) %>%
        unlist()
    }
    pkgs_missing <- pkgs_missing[which(!pkgs_missing%in%downloaded_file_list)]
  }
  if(length(pkgs_missing)==0)message("Nothing to be done at... ",destination_dir)
  # pb <- progress::progress_bar$new(
  #   format = "  getting packages [:bar] :percent ETA: :eta",
  #   total = length(pkgs_missing), clear = FALSE, width= 60)
  base_pkgs <- installed_packages$Package[which(installed_packages$Priority=="base")]
  r_version <- R.Version()
  base_file_name <- paste0("R-", paste(r_version$major, r_version$minor, sep = "."))
  tarball_file_name <- paste0(base_file_name, ".tar.gz")
  tarball_url <- paste0("https://cran.r-project.org/src/base/R-", r_version$major, "/",tarball_file_name)
  temp_dir <- tempdir()
  tarball_file_name <- file.path(temp_dir,tarball_file_name)
  source_dir_base <- file.path(temp_dir,base_file_name,"src","library")
  for(pkg in pkgs){
    ROW <- which(installed_packages$Package==pkg)
    is_base <- pkg %in% base_pkgs
    was_download <- F
    if(pkg %in% pkgs_missing){
      source_dir_root <- file.path(temp_dir,pkg)
      if(is_base){
        source_dir_root <- file.path(source_dir_base,pkg)
      }
      source_dir <- file.path(source_dir_root,"R")
      only_missing_R_folder <- file.exists(source_dir_root)&&!file.exists(source_dir)
      if((overwrite||!file.exists(source_dir))&&!only_missing_R_folder){
        was_download <- download_and_extract_source_R_files(pkg)
      }
      is_there <- file.exists(source_dir)
      if(was_download||is_there){
        if(is_there){
          combine_R_files(
            source_dir = source_dir,
            destination_dir = destination_dir,
            filename = installed_packages$file_name[ROW],
            header_symbol = header_symbol,
            max_new_lines = max_new_lines,
            new_lines = new_lines,
            overwrite = overwrite
          )
        }
      }
    }
    if(launch_file>0){
      file.path(destination_dir, installed_packages$file_name[ROW]) %>% rstudioapi::navigateToFile()
      launch_file <- launch_file - 1
    }
    # pb$tick()
  }
  pkgs_df <- installed_packages[which(installed_packages$Package%in%pkgs),]
  downloaded_file_list <-
    destination_dir %>%
    list.files(pattern = "\\.R$", full.names = TRUE) %>%
    basename() %>%
    tools::file_path_sans_ext()
  THERE <- pkgs_df$comparison_name %in% downloaded_file_list
  pkgs_there <- pkgs_df$Package[which(THERE)]
  pkgs_missing <- pkgs_df$Package[which(!THERE)]
  # if(length(pkgs_there)>0) message("Packages already there: ",pkgs_there %>% paste0(collapse = ", "))
  if(length(pkgs_missing)>0) message("Packages STILL missing: ",pkgs_missing %>% paste0(collapse = ", "))
}
#' @title pkg_combine_R_files_launch
#' @param pkgs package name(s) as character string
#' @param filename_type character string of type filename: "name__version" or "name"
#' @return message
#' @export
pkg_combine_R_files_launch <- function(pkg,destination_dir = tempdir(),filename_type = "name__version",header_symbol = "=",max_new_lines=0,new_lines=character(0),overwrite = F) {
  pkg_combine_R_files(
    pkgs = pkg,
    destination_dir = destination_dir,
    filename_type = "name__version",
    header_symbol = header_symbol,
    max_new_lines = max_new_lines,
    new_lines = new_lines,
    overwrite = overwrite,
    launch_file = 1
  )
}
