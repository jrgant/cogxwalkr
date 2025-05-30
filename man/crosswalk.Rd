% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosswalk.r
\name{crosswalk}
\alias{crosswalk}
\title{Crosswalk cognitive measures}
\usage{
crosswalk(
  cog1,
  cog2,
  data,
  num_iter = NULL,
  condition_by = NULL,
  condition_loop = FALSE,
  boot_control = list(...)
)
}
\arguments{
\item{cog1}{The name of the first cognitive measure column}

\item{cog2}{The name of the second cognitive measure column}

\item{data}{A data.table or data.frame containing the cognitive measure data}

\item{num_iter}{Number of split iterations to conduct}

\item{condition_by}{The name of a conditioning variable by which splits will be
conducted. If not conducted, the function will use unconditional splits.}

\item{condition_loop}{Whether to conduct conditional splitting sequentially. Defaults
to FALSE to maximize speed. Unused if \code{condition_by} is NULL. See documentation
for \code{make_conditional_splits()} for details.}

\item{boot_control}{A list of settings passed to \code{bootstrap_crosswalk()} and functions
that handle bootstrap confidence interval estimation. Also allows for some control
of final output.}
}
\description{
Crosswalk cognitive measures
}
