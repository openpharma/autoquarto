% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{isQuartoWebsite}
\alias{isQuartoWebsite}
\title{Determine if a path points to a Quarto website}
\usage{
isQuartoWebsite(path)
}
\arguments{
\item{path}{the path to be tested}
}
\value{
\code{TRUE} if the path is a valid Quarto book, \code{FALSE} otherwise
}
\description{
The path is a Quarto website if and only if:
\itemize{
\item It is a readable directory
\item It contains a readable file named \verb{_project.yml}
\item \verb{_project.yml} contains a \code{project: type: website} entry
}
}
