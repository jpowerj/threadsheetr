% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threader_combine.R
\name{threader_combine}
\alias{threader_combine}
\title{Step 2: Combine Parsed Data Files}
\usage{
threader_combine(
  parsed_path = NULL,
  combined_path = NULL,
  spec_fpath = NULL,
  verbose = FALSE
)
}
\arguments{
\item{parsed_path}{Path to the directory containing the parsed files generated by \code{threader_parse()}.}

\item{combined_path}{Path to the directory where you want the combined data saved}

\item{spec_fpath}{Path to the \code{.yaml}-format spec file.}

\item{verbose}{Optional: If \code{TRUE}, prints additional debugging information.}
}
\value{
Output: a giant csv file with a row for \emph{every} piece of data we have
}
\description{
Step 2: Combine Parsed Data Files
}
\examples{
combined_df <- threader_combine()
}
