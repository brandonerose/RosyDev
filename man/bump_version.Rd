% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_run_dev.R
\name{bump_version}
\alias{bump_version}
\title{bump_version}
\usage{
bump_version(which = "dev")
}
\arguments{
\item{which}{A string specifying which level to increment, one of: "major",
"minor", "patch", "dev". If \code{NULL}, user can choose interactively.}

\item{message}{character string for commit message}
}
\value{
bump
}
\description{
bump version
}
