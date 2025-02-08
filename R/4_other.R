#' @title check_namespace_conflicts
#' @param pkgs character vector of pkgs
#' @export
check_namespace_conflicts <- function(pkgs, ignores = c(
  "%>%",
  "pkg_date",
  # "pkg_version",
  "pkg_name",
  ".__NAMESPACE__.",
  ".__S3MethodsTable__.",
  ".packageName"
)) {
  # pkgs <- pkgs %>% sort()
  x <- NULL
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
    exported_functions <- ls(paste0("package:", pkg))
    internal_functions <-
      x <- x %>% rbind(
        data.frame(
          pkg = pkg,
          name = ls(paste0("package:", pkg)),
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
  if (is_something(ignores)) {
    y <- y[which(!y$function_name %in% ignores), ]
  }
  y$int_int_conflict <- y$function_name %>% sapply(function(function_name) {
    ROWS <- which(x$type %in% c("internal") & x$name == function_name)
    z <- x$pkg[ROWS] %>% unique()
    if (length(z) < 2) {
      return(NA)
    }
    z %>%
      paste0(collapse = " | ") %>%
      return()
  })
  y$int_exp_conflict <- y$function_name %>% sapply(function(function_name) {
    ROWS <- which(x$type %in% c("internal", "exported") & x$name == function_name)
    z <- x$pkg[ROWS] %>% unique()
    if (length(z) < 2) {
      return(NA)
    }
    z %>%
      paste0(collapse = " | ") %>%
      return()
  })
  y$exp_exp_conflict <- y$function_name %>% sapply(function(function_name) {
    ROWS <- which(x$type %in% c("exported") & x$name == function_name)
    z <- x$pkg[ROWS] %>% unique()
    if (length(z) < 2) {
      return(NA)
    }
    z %>%
      paste0(collapse = " | ") %>%
      return()
  })
  y <- y[order(y$int_int_conflict, decreasing = TRUE), ]
  y <- y[order(y$int_exp_conflict, decreasing = TRUE), ]
  y <- y[order(y$exp_exp_conflict, decreasing = TRUE), ]
  z <- y[which(!is.na(y$exp_exp_conflict)), ]
  if (nrow(z) > 0) {
    z$function_name <- stringr::str_pad(
      string = z$function_name,
      width = (z$function_name %>% nchar() %>% max()) + 2,
      side = "right"
    )
    message("Below are some conflicts with exported names...\n", z$function_name %>% sapply(function(function_name) {
      ROW <- which(z$function_name == function_name)
      return(paste0("   ", z$function_name[ROW], "-->  ", z$exp_exp_conflict[ROW]))
    }) %>% paste0(collapse = "\n"))
  } else {
    message("No major conflicts!")
  }
  return(y)
}
#' @title get_external_functions
#' @param pkg character vector of pkg
#' @export
get_external_functions <- function(pkg) {
  ls(paste0("package:", pkg))
}
#' @title get_all_functions
#' @param pkg character vector of pkg
#' @export
get_all_functions <- function(pkg) {
  ls(getNamespace(pkg))
}
get_logo_paths <- function() {
  logo_folder <- system.file("logos", package = "RosyDev")
  logo_files <- logo_folder %>% list.files(full.names = TRUE)
  logo_files <- logo_files[which(endsWith(logo_files, ".png"))]
  allowed_names <- logo_files %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    gsub("hex-", "", .)
  named_list <- as.list(logo_files)
  names(named_list) <- allowed_names
  return(named_list)
}
get_imported_packages <- function(pkg_name) {
  pkg_desc <- utils::packageDescription(pkg_name)
  imports <- pkg_desc$Imports
  if (!is.null(imports)) {
    imports_vector <- gsub("\\s*\\(.*?\\)", "", strsplit(imports, ",")[[1]])
    imports_vector <- trimws(imports_vector)
  } else {
    imports_vector <- character(0) # No imports
  }
  return(imports_vector)
}
copy_logos_to_package <- function(copy_to = file.path("inst", "app", "www"), only_if_imported = TRUE) {
  usethis:::check_is_package()
  pkg_dir <- getwd()
  pkg_name <- basename(pkg_dir)
  named_list <- get_logo_paths()
  if (only_if_imported) {
    named_list <- named_list[which(!startsWith(names(named_list), "Rosy") | (startsWith(names(named_list), "Rosy") & names(named_list) %in% get_imported_packages(pkg_name)))]
  }
  dir.create(copy_to, recursive = TRUE, showWarnings = FALSE)
  for (i in 1:length(named_list)) {
    was_copied <- file.copy(
      from = named_list[[i]],
      to = file.path(copy_to, paste0(names(named_list[i]), ".png")),
      overwrite = FALSE
    )
  }
  logo_path <- file.path(copy_to, paste0(pkg_name, ".png"))
  if (file.exists(logo_path)) {
    file.copy(
      from = logo_path,
      to = file.path(copy_to, "logo.png"),
      overwrite = FALSE
    )
  }
}
#' @title run_test_prod
#' @export
run_test_prod <- function() {
  source(file.path(getwd(), "dev", "test_prod.R"))
}
#' @title run_test_dev
#' @export
run_test_dev <- function() {
  source(file.path(getwd(), "dev", "test_dev.R"))
}
#' @title extract_function_definitions
#' @export
extract_function_definitions <- function(file) {
  parsed <- parse(file)
  names <- sapply(parsed, function(x) {
    if (is.call(x) && identical(x[[1]], as.name("<-"))) {
      as.character(x[[2]])
    } else {
      NULL
    }
  })
  return(unlist(names[!sapply(names, is.null)]))
}
#' @title dev_show_functions
#' @export
dev_show_functions <- function() {
  files <- "dev/combined.R"
  if (!file.exists(files)) {
    files <- list.files(file.path("R"), pattern = "\\.R$", full.names = TRUE)
  }
  all_functions <- unlist(lapply(files, extract_function_definitions))
  return(all_functions)
}
#' @title dev_show_duplicated_functions
#' @export
dev_show_duplicated_functions <- function() {
  all_functions <- dev_show_functions()
  all_functions <- all_functions[which(duplicated(all_functions))] %>%
    unique() %>%
    sort(decreasing = TRUE)
  if (length(all_functions) == 0) bullet_in_console("No duplicated functions", bullet_type = "v")
  return(all_functions)
}
#' @title dev_function_freq
#' @export
dev_function_freq <- function() {
  files <- "dev/combined.R"
  if (!file.exists(files)) {
    files <- list.files(file.path("R"), pattern = "\\.R$", full.names = TRUE)
  }
  get_function_freq <- function(file) {
    tmp <- getParseData(parse(file, keep.source = TRUE))
    tmp <- tmp %>% dplyr::filter(token == "SYMBOL_FUNCTION_CALL")
    return(tmp$text)
  }
  all_functions <- unlist(lapply(files, get_function_freq))
  all_functions %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    return()
}
