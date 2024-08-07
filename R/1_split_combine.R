#' @title combine_R_files
#' @param source_dir a file path for your source (such as R folder)
#' @param destination_dir a file path for your destination (such as dev folder)
#' @param filename a file name (ends in .R)
#' @param header_symbol single character for your header separator in combined file
#' @param max_new_lines integer for max number of new lines
#' @param new_lines character vector for new lines
#' @return message
#' @export
combine_R_files <- function(source_dir= file.path(getwd(),"R"), destination_dir=file.path(getwd(),"dev"),filename="combined.R",header_symbol = "=",max_new_lines=0,new_lines=character(0)) {
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
  writeLines(combined_text, destination_file)
  cat("Combined file saved to:", destination_file, "\n")
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
    scripts[[gsub(paste0("#| |",header_symbol), "", file_content[split_indices[[1]]])]] <- file_content[start_index:end_index]
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
#' @title Fast Commit
#' @description commit for git with one function
#' @param comment character string for commit message
#' @return commited git
#' @export
fast_commit <- function(comment = "dev"){
  usethis::use_git(comment)
}
#' @title pkg_combine_R_files
#' @inheritParams source combine_R_files
#' @param pkg package name as character string
#' @return message
#' @export
pkg_combine_R_files <- function(pkg, destination_dir=getwd(),filename="combined.R",header_symbol = "=",max_new_lines=0,new_lines=character(0)) {
  combine_R_files(
    source_dir = system.file("R", package = package),
    destination_dir = destination_dir,
    filename = filename,
    header_symbol = header_symbol,
    max_new_lines = max_new_lines,
    new_lines = new_lines
  )
}
#' @title split_R_files
#' @inheritParams combine_R_files
#' @return message
#' @export
