% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ymlthis.R
\name{yml_book}
\alias{yml_book}
\title{Create a YAML book Element}
\usage{
yml_book(
  .yml = ymlthis::yml_empty(),
  ...,
  validNames = c("title", "author", "date", "chapters", "part", "appendices")
)
}
\value{
a \code{yml} object
}
\description{
Create a YAML book Element
}
\examples{
ymlthis::yml_empty() \%>\% 
  yml_book(
      title="mybook", 
      author="Jane Doe",
      date="5/9/2021",
      chapters=list(
        "index.qmd",
        "intro.qmd",
        "summary.qmd",
        "references.qmd"
      )
  )
}