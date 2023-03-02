#' Create a YAML project Element
#' 
#' @returns a `yml` object
#' @param .yml the YAML object to amend
#' @param validNames a vector of valid names for children of `.yml`
#' @param ... other arguments defining the objects to be added as children of `.yml`
#' @returns a `yml` object
#' @export
yml_project <- function (.yml, ..., validNames=c("part")){
  checkmate::assertSubset(names(...), validNames)
  .yml %>% ymlthis::yml_toplevel(.yml, "project"=list(...))
}

#' Create a YAML book Element
#' 
#' @param .yml the YAML object to amend
#' @param validNames a vector of valid names for children of `.yml`
#' @param ... other arguments defining the objects to be added as children of `.yml`
#' @returns a `yml` object
#' @examples 
#' ymlthis::yml_empty() %>% 
#'   yml_book(
#'       title="mybook", 
#'       author="Jane Doe",
#'       date="5/9/2021",
#'       chapters=list(
#'         "index.qmd",
#'         "intro.qmd",
#'         "summary.qmd",
#'         "references.qmd"
#'       )
#'   )
#' @export
yml_book <- function (.yml=ymlthis::yml_empty(), ..., validNames=c("title", "author", "date", "chapters", "part", "appendices")){
  checkmate::assertSubset(names(list(...)), validNames)
  .yml %>% ymlthis::yml_toplevel(list(book=list(...)))
}

#' Convert a tibble f parameter definitions to YAML
#' 
#' @param d The parameter tibble
#' @returns a YML object
#' @section Usage Notes:
#' The input tibble should contain columns Parameter, Value, Mode, IsNULL and IsNA.
#' @export
parameterTibbleToYAML <- function(d) {
  checkmate::assertTibble(d)
  checkmate::assertSetEqual(names(d), c("Parameter", "Value", "Mode", "IsNULL", "IsNA"))
  rv <- d %>% 
          dplyr::rowwise() %>%
          dplyr::group_map(
            function(.x, .y) {
              if (.x$IsNULL) {
                rv <- NULL
              } else if (.x$IsNA) {
                rv <- NA
              } else if (.x$Mode == "literal") {
                rv <- .x$Value
              } else if (.x$Mode == "name") {
                rv <- .x$Value
              } else if (.x$Mode == "value") {
                if (.x$Value == "NA") {
                  rv <- NA
                } else if (.x$Value == "NULL") {
                  rv <- NULL
                } else {
                  if (exists(.x$Value)) {
                    rv <- as.list(get(.x$Value))
                  } else {
                    futile.logger::flog.warn(paste0("Unable to find object '", .x$Value, "'.  Returning a literal..."))
                    rv <- .x$Value
                  }
                }
              } else {
                stop(paste0("Unable to process ", .x$Value))
              }
              rv
            } 
          )
  names(rv) <- d %>% dplyr::pull(Parameter)
  rv
}
