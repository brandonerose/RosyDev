dev_combine_split_R_files <- function(choice = "combine", silent = FALSE, overwrite = FALSE) {
  if (length(choice) > 1) stop("Choice must be length 1")
  choices <- c("split", "combine", "both")
  if (!choice %in% choices) stop("Choice must be one of... ", as_comma_string(choices))
  usethis:::check_is_package()
  pkg_dir <- getwd()
  if (!silent) message("pkg_dir: ", pkg_dir)
  pkg_name <- basename(pkg_dir)
  dev_dir <- file.path(pkg_dir, "dev")
  if (!silent) message("pkg_name: ", pkg_name)
  param_list <- list(
    # R
    list(
      source_dir = file.path(pkg_dir, "R"),
      file_name = "combined",
      file_ext = ".R"
    ),
    # tests
    list(
      source_dir = file.path(pkg_dir, "tests", "testthat"),
      file_name = "tests",
      file_ext = ".R"
    ),
    # vignettes
    list(
      source_dir = file.path(pkg_dir, "vignettes"),
      file_name = "vignettes",
      file_ext = ".Rmd"
    )
  )
  for (i in 1:length(param_list)) {
    source_dir <- param_list[[i]]$source_dir
    file_name <- param_list[[i]]$file_name
    file_ext <- param_list[[i]]$file_ext
    max_new_lines <- 0
    if (file_name == "vignettes") max_new_lines <- NULL
    if (choice %in% c("combine", "both")) {
      combine_R_files(
        source_dir = source_dir,
        file_name = file_name,
        file_ext = file_ext,
        silent = silent,
        overwrite = overwrite,
        max_new_lines = max_new_lines
      )
    }
    if (choice %in% c("split", "both")) {
      if (file.exists(source_dir)) {
        split_R_files(
          source_dir = dev_dir,
          destination_dir = source_dir,
          file_name = file_name,
          file_ext = file_ext,
          silent = silent
        )
      }
    }
  }
}
#' @title combine_R_files
#' @param source_dir a file path for your source (such as R folder)
#' @param destination_dir a file path for your destination (such as dev folder)
#' @param file_name a file name (ends in .R)
#' @param header_symbol single character for your header separator in combined file
#' @param max_new_lines integer for max number of new lines
#' @param new_lines character vector for new lines
#' @param overwrite logical for overwriting original file
#' @return message
#' @export
combine_R_files <- function(source_dir = file.path(getwd(), "R"), destination_dir = file.path(getwd(), "dev"), file_name = "combined", file_ext = ".R", header_symbol = "=", max_new_lines = 0, new_lines = character(0), silent = FALSE, overwrite = TRUE) {
  if (!file_ext %in% c(".R", ".Rmd")) stop("file_ext must be R or Rmd")
  expected_folder <- file.path(source_dir)
  if (!file.exists(expected_folder)) {
    if (!silent) bullet_in_console("No folder", file = expected_folder)
    return(invisible())
  }
  destination_file <- file.path(destination_dir, paste0(file_name, file_ext))
  if (!file.exists(destination_file) || overwrite) {
    dir.create(destination_dir, showWarnings = FALSE, recursive = TRUE)
    file_list <- list.files(source_dir, pattern = paste0("\\", file_ext, "$"), full.names = TRUE)
    combined_text <- character(0)
    for (file in file_list) { # file <- file_list %>% sample(1)
      file_name <- tools::file_path_sans_ext(basename(file))
      header <- paste0("# ", file_name, " ")
      header <- paste0(header, paste0(rep(header_symbol, 80 - nchar(header)), collapse = ""))
      combined_text <- c(combined_text, header, new_lines, readLines(file))
    }
    if (!silent) message(length(combined_text), " lines")
    combined_text <- paste(combined_text, collapse = "\n")
    if (!is.null(max_new_lines)) {
      combined_text <- gsub(paste0("\\n{", max_new_lines + 2, ",}"), "\n", combined_text)
    }
    writeLines(combined_text, destination_file)
    if (!silent) bullet_in_console("Combined file saved to:", file = destination_file, bullet_type = "v")
  }
}
#' @title split_R_files
#' @inheritParams combine_R_files
#' @return message
#' @export
split_R_files <- function(source_dir = file.path(getwd(), "dev"), destination_dir = file.path(getwd(), "R"), file_name = "combined", file_ext = ".R", header_symbol = "=", new_lines = character(0), silent = FALSE) {
  if (!file_ext %in% c(".R", ".Rmd")) stop("file_ext must be R or Rmd")
  expected_file <- file.path(source_dir, paste0(file_name, file_ext))
  if (!file.exists(expected_file)) {
    bullet_in_console("No file", file = expected_file)
    return(invisible())
  }
  dir.create(destination_dir, showWarnings = FALSE, recursive = TRUE)
  file_content <- readLines(expected_file)
  split_indices <- grep(paste0("^# .* ", paste0(rep(header_symbol, 4), collapse = ""), collapse = ""), file_content)
  script_names <- gsub(paste0("#| |", header_symbol), "", file_content[split_indices])
  if (anyDuplicated(script_names)) stop("You have duplicate script names! --> ", as_comma_string(RosyUtils::vec_which_duplicated(script_names)))
  split_indices <- as.list(split_indices)
  scripts <- NULL
  while (length(split_indices) > 0) {
    script_name <- gsub(paste0("#| |", header_symbol), "", file_content[split_indices[[1]]])
    start_index <- split_indices[[1]] + 1
    if (length(split_indices) == 1) {
      end_index <- length(file_content)
    } else {
      end_index <- split_indices[[2]] - 1
    }
    if (start_index > end_index) {
      out_lines <- ""
    } else {
      out_lines <- file_content[start_index:end_index]
    }
    scripts[[script_name]] <- out_lines
    split_indices[[1]] <- NULL
  }
  for (i in seq_along(scripts)) {
    output_file <- file.path(destination_dir, paste0(names(scripts)[i], file_ext))
    writeLines(
      new_lines %>% append(scripts[[i]]) %>% paste0(collapse = "\n"),
      con = output_file
    )
    cat("File saved:", output_file, "\n")
  }
}
execute_with_tryCatch <- function(expr) {
  result <- tryCatch(
    {
      eval(expr) # Evaluate the expression
      TRUE # If no error, return TRUE
    },
    error = function(e) {
      FALSE # If error occurs, return FALSE
    }
  )
  return(result)
}
download_and_extract_source_R_files <- function(pkg) {
  temp_dir <- tempdir()
  installed_packages <- installed.packages() %>% as.data.frame()
  is_base <- pkg %in% installed_packages$Package[which(installed_packages$Priority == "base")]
  if (is_base) {
    r_version <- R.Version()
    base_file_name <- paste0("R-", paste(r_version$major, r_version$minor, sep = "."))
    tarball_file_name <- paste0(base_file_name, ".tar.gz")
    tarball_url <- paste0("https://cran.r-project.org/src/base/R-", r_version$major, "/", tarball_file_name)
    temp_dir <- tempdir()
    tar_file <- file.path(temp_dir, tarball_file_name)
    was_downloaded <- tryCatch(
      {
        download.file(tarball_url, tar_file)
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
  } else {
    tar_file <- download.packages(pkg, destdir = temp_dir, type = "source")
    if (length(tar_file) > 0) {
      was_downloaded <- TRUE
      tar_file <- tar_file[, 2]
    } else {
      was_downloaded <- FALSE
    }
  }
  if (was_downloaded) {
    untar(tarfile = tar_file, exdir = temp_dir)
  }
  return(was_downloaded)
}
#' @title pkg_combine_R_files
#' @param pkgs package name(s) as character string
#' @param file_name_type character string of type file_name: "name__version" or "name"
#' @return message
#' @export
pkg_combine_R_files <- function(pkgs, destination_dir = getwd(), file_name_type = "name__version", header_symbol = "=", max_new_lines = 0, new_lines = character(0), overwrite = FALSE, launch_file = 0) {
  installed_packages <- installed.packages() %>% as.data.frame()
  installed_packages$comparison_name <- installed_packages$Package
  if (file_name_type == "name__version") {
    installed_packages$comparison_name <- paste0(installed_packages$Package, "__", gsub("\\.|\\-", "_", installed_packages$Version))
  }
  installed_packages$file_name <- installed_packages$comparison_name %>% paste0(".R")
  if (!file.exists(destination_dir)) {
    if (utils::menu(c("Yes", "No and stop"), title = paste0("Your listed directory does not exist! Would you like to create? ", destination_dir)) == 2) {
      stop("Stopped!")
    }
    dir.create(destination_dir, showWarnings = FALSE)
  }
  pkgs_possible <- installed_packages$Package %>% as.character()
  BAD <- pkgs[which(!pkgs %in% pkgs_possible)]
  if (length(BAD) > 0) stop("You have listed pkgs that are not installed: ", BAD %>% paste0(collapse = ", "))
  pkgs_df <- installed_packages[which(installed_packages$Package %in% pkgs), ]
  downloaded_file_list <-
    destination_dir %>%
    list.files(pattern = "\\.R$", full.names = TRUE) %>%
    basename() %>%
    tools::file_path_sans_ext()
  THERE <- pkgs_df$comparison_name %in% downloaded_file_list
  pkgs_there <- pkgs_df$Package[which(THERE)]
  pkgs_missing <- pkgs_df$Package[which(!THERE)]
  if (length(pkgs_there) > 0) message("Packages already there: ", pkgs_there %>% paste0(collapse = ", "))
  if (length(pkgs_missing) > 0) message("Packages missing: ", pkgs_missing %>% paste0(collapse = ", "))
  if (!overwrite) {
    if (file_name_type == "name__version") {
      downloaded_file_list <-
        downloaded_file_list %>%
        strsplit("__") %>%
        sapply(function(x) {
          x[[1]]
        }) %>%
        unlist()
    }
    pkgs_missing <- pkgs_missing[which(!pkgs_missing %in% downloaded_file_list)]
  }
  if (length(pkgs_missing) == 0) message("Nothing to be done at... ", destination_dir)
  # pb <- progress::progress_bar$new(
  #   format = "  getting packages [:bar] :percent ETA: :eta",
  #   total = length(pkgs_missing), clear = FALSE, width= 60)
  base_pkgs <- installed_packages$Package[which(installed_packages$Priority == "base")]
  r_version <- R.Version()
  base_file_name <- paste0("R-", paste(r_version$major, r_version$minor, sep = "."))
  tarball_file_name <- paste0(base_file_name, ".tar.gz")
  tarball_url <- paste0("https://cran.r-project.org/src/base/R-", r_version$major, "/", tarball_file_name)
  temp_dir <- tempdir()
  tarball_file_name <- file.path(temp_dir, tarball_file_name)
  source_dir_base <- file.path(temp_dir, base_file_name, "src", "library")
  for (pkg in pkgs) {
    ROW <- which(installed_packages$Package == pkg)
    is_base <- pkg %in% base_pkgs
    was_download <- FALSE
    if (pkg %in% pkgs_missing) {
      source_dir_root <- file.path(temp_dir, pkg)
      if (is_base) {
        source_dir_root <- file.path(source_dir_base, pkg)
      }
      source_dir <- file.path(source_dir_root, "R")
      only_missing_R_folder <- file.exists(source_dir_root) && !file.exists(source_dir)
      if ((overwrite || !file.exists(source_dir)) && !only_missing_R_folder) {
        was_download <- download_and_extract_source_R_files(pkg)
      }
      is_there <- file.exists(source_dir)
      if (was_download || is_there) {
        if (is_there) {
          combine_R_files(
            source_dir = source_dir,
            destination_dir = destination_dir,
            file_name = installed_packages$file_name[ROW],
            header_symbol = header_symbol,
            max_new_lines = max_new_lines,
            new_lines = new_lines,
            overwrite = overwrite
          )
        }
      }
    }
    if (launch_file > 0) {
      file.path(destination_dir, installed_packages$file_name[ROW]) %>% rstudioapi::navigateToFile()
      launch_file <- launch_file - 1
    }
    # pb$tick()
  }
  pkgs_df <- installed_packages[which(installed_packages$Package %in% pkgs), ]
  downloaded_file_list <-
    destination_dir %>%
    list.files(pattern = "\\.R$", full.names = TRUE) %>%
    basename() %>%
    tools::file_path_sans_ext()
  THERE <- pkgs_df$comparison_name %in% downloaded_file_list
  pkgs_there <- pkgs_df$Package[which(THERE)]
  pkgs_missing <- pkgs_df$Package[which(!THERE)]
  # if(length(pkgs_there)>0) message("Packages already there: ",pkgs_there %>% paste0(collapse = ", "))
  if (length(pkgs_missing) > 0) message("Packages STILL missing: ", pkgs_missing %>% paste0(collapse = ", "))
}
#' @title pkg_combine_R_files_launch
#' @param pkg package name as character string
#' @param file_name_type character string of type file_name: "name__version" or "name"
#' @return message
#' @export
pkg_combine_R_files_launch <- function(pkg, destination_dir = tempdir(), file_name_type = "name__version", header_symbol = "=", max_new_lines = 0, new_lines = character(0), overwrite = FALSE) {
  pkg_combine_R_files(
    pkgs = pkg,
    destination_dir = destination_dir,
    file_name_type = "name__version",
    header_symbol = header_symbol,
    max_new_lines = max_new_lines,
    new_lines = new_lines,
    overwrite = overwrite,
    launch_file = 1
  )
}
#' @title find_imported_functions
#' @return functions
#' @export
find_imported_functions <- function(file_path="dev/combined.R") {
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }
  lines <- readLines(file_path, warn = FALSE)
  written_functions <- lines %>%
    regmatches(gregexpr("[A-Za-z]+::[a-zA-Z_]+", lines, perl = TRUE)) %>%
    unlist() %>% unique() %>% sort()
  return(written_functions)
}
