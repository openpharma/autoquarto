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

#' Read the YAML Front Matter From a File
#' 
#' `rmarkdown::yaml_front_matter` does not work correctly when the parameter name
#' is `y`, `Y`, `Yes`, `n` and possibly other similar values.  `ymlthis::yml_pluck`
#' errors when the parameter value is an unquoted integer.  Both problems seem to
#' be related to the fact that `yaml::read_yaml` uses version 1.1 of the YAML spec 
#' rather than version 1.2.
#' 
#' So use this internal method as a way to avoid both issues.
#' @param path The path to the file whose front matter is to be read
#' @param item If not NA, the element of the front matter to be returned
#' @return a list containing the YAML front matter
readYamlFrontMatter <- function(path, item=NA) {
  # Validate
  checkmate::assertFileExists(path, access="rw")
  if (!is.na(item)) {
    checkmate::assertCharacter(item, max.len=1)
  }
  # Execute
  # 2023-Feb ymlthis::yml_pluck errors when the value is an integer, so let's roll our own
  x <- rmarkdown::yaml_front_matter(path)
  if (!is.na(item)) {
    x <- x[[item]]
  }
  y <- lapply(names(x), function(z) x[[z]])
  names(y) <- names(x)
  # Possible bug in rmarkdown::yaml_front_matter()
  # https://github.com/vubiostat/r-yaml/issues/122
  # https://stackoverflow.com/questions/75595253/
  names(y)[which(names(y) == "TRUE")] <- "y"
  names(y)[which(names(y) == "FALSE")] <- "n"
  y
}

#' Replace the params Element of a File's YAML Front Matter
#' 
#' @param path The path to the file whose front matter is to be updated
#' @param params The new parameter values
#' @param restrict How to restrict the updating of the parameters.  `"none"`
#' results in any parameter that is in either the front matter or the list to be
#' written to the edited file.  `"both"` means only those parameters that are in
#' both the original front matter and the list are included.  `"file"` means that
#' only parameter names in the original front matter are included and those only 
#' in the list are ignored.  `"list"` means that only those parameters in the
#' list are included and those only in the original front matter are ignored.
#' @export
replaceYamlParams <- function(path, params, restrict=c("none", "both", "file", "list")) {
  # Validate
  checkmate::assertFileExists(path, access="rw")
  checkmate::assertList(params, types=c("character", "numeric", "list"))
  restrict <- match.arg(restrict)
  checkmate::assertSubset(restrict, c("none", "both", "file", "list"))
  # Execute
  frontMatter <- readYamlFrontMatter(path)
  # print("Before...")
  # ymlthis::draw_yml_tree(frontMatter)
  currentParams <- ymlthis::yml_pluck("params")
  if (restrict == "none") {
    frontMatter <- ymlthis::yml_replace(frontMatter, params=params)
  } else {
    stop("Not yet implemented")
  }
  # print("After...")
  # ymlthis::draw_yml_tree(frontMatter)
  lines <- readLines(path)
  if (lines[1] != "---") {
    stop(paste0(path, " is malformed: front matter not found"))
  }
  endFrontMatter <- 2
  found <- FALSE
  while(!found & endFrontMatter < length(lines)) {
    if (lines[endFrontMatter] == "---") {
      found <- TRUE
    } else {
      endFrontMatter <- endFrontMatter + 1
    }
  }
  if (!endFrontMatter) {
    stop(paste0(path, " is malformed: front matter has no end"))
  }
  newFrontMatter <- stringr::str_split(yaml::as.yaml(frontMatter), "\\n")[[1]]
  newFrontMatter <- newFrontMatter[which(newFrontMatter != "\n")]
  writeLines(
    c(
      "---",
      newFrontMatter, 
      "---",
      lines[(endFrontMatter+1):length(lines)]
    ),
    path
  )
}
