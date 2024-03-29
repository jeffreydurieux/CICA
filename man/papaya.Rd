% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/papaya.R
\name{papaya}
\alias{papaya}
\title{View images with Papaya}
\usage{
papaya(images, outdir = NULL, ...)
}
\arguments{
\item{images}{character filenames or \code{nifti} objects to be viewed}

\item{outdir}{output directory for index and all to go}

\item{...}{Options to be passed to \code{\link{pass_papaya}}}
}
\value{
Output directory where index.html, js, and copied nii.gz files
}
\description{
Writes temporary images out from nifti objects or passes character filenames
of images to papaya JS viewer
}
\examples{
\dontrun{
library(neurobase)
x = nifti(img = array(rnorm(100^3), dim= rep(100, 3)), dim=rep(100, 3), datatype=16)
thresh = datatyper(x > 1)
index.file = papaya(list(x, thresh))
}
}
