% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_split_combine.R
\name{pkg_combine_R_files_launch}
\alias{pkg_combine_R_files_launch}
\title{pkg_combine_R_files_launch}
\usage{
pkg_combine_R_files_launch(
  pkg,
  destination_dir = tempdir(),
  file_name_type = "name__version",
  header_symbol = "=",
  max_new_lines = 0,
  new_lines = character(0),
  overwrite = FALSE
)
}
\arguments{
\item{pkg}{package name as character string}

\item{file_name_type}{character string of type file_name: "name__version" or "name"}
}
\value{
message
}
\description{
pkg_combine_R_files_launch
}
