% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed_papaya.R
\name{embed_papaya}
\alias{embed_papaya}
\title{Embed images with Papaya}
\usage{
embed_papaya(images, outdir = NULL)
}
\arguments{
\item{images}{character filenames or \code{nifti} objects to be viewed}

\item{outdir}{output directory for index and all to go}
}
\value{
Output html
}
\description{
Writes temporary images out from nifti objects or passes character filenames
of images to papaya JS viewer
}
