% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/open_eaf.R
\name{open_eaf}
\alias{open_eaf}
\title{open_eaf Function}
\usage{
open_eaf(corpus = corpus_kpv, row = 1, program = FALSE)
}
\arguments{
\item{corpus}{To which data frame you want to apply the function}

\item{row}{The number of dataframe row you want to open}

\item{program}{The program with which you want to open ELAN file}
}
\description{
This function opens an ELAN file from R. If no row number is specified it is opened from the beginning. However, one can specify row number, and then the file is opened from that utterance. The row number means here the index number in the current local frame. Please note that operations like slice() impact the row numbering. Also the function demands that there is a row called `filename`, which contains complete path. The place where file is opened is determined by editing .psfx file, so that also has to be present (it is automatically generated after one opening).
}
\examples{
open_eaf(5)
}
\keyword{ELAN}
