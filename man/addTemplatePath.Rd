% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quartoobject-methods.R
\docType{methods}
\name{addTemplatePath}
\alias{addTemplatePath}
\alias{addTemplatePath-generic}
\alias{addTemplatePath,QuartoCompoundObject-method}
\alias{addTemplatePath-QuartoCompoundObject}
\title{Add an Entry to the Template Search Path list}
\usage{
addTemplatePath(x, path, mustExist = FALSE, .after = NA)

\S4method{addTemplatePath}{QuartoCompoundObject}(x, path, mustExist = FALSE, .after = NA)
}
\arguments{
\item{x}{a QuartoCompoundObject object}

\item{path}{the path to be added}

\item{mustExist}{Boolean must the specified path exist?}

\item{.after}{At what position should the new path be added?  The default value, \code{NA}, adds the new path at the end of the existing list}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
