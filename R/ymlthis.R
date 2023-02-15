#' Create a YAML project Element
#' 
#' @returns a `yml` object
#' @export
yml_project <- function (.project, ..., validNames=c("part")){
  checkmate::assertSubset(names(...), validNames)
  .yml %>% ymlthis::yml_toplevel(.yml, "project"=list(...))
}

#' Create a YAML book Element
#' 
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

# ymlthis::yml_empty() %>% 
#   yml_book(
#       title="mybook", 
#       author="Jane Doe",
#       date="5/9/2021",
#       chapters=list(
#         "index.qmd",
#         "intro.qmd",
#         "summary.qmd",
#         "references.qmd"
#       )
#   ) %>% 
#   ymlthis::draw_yml_tree()
# 
# ymlthis::yml_empty() %>% yml_book(badName = FALSE)
# 
# 
# ymlthis::yml_pluck(x, "book", "title")
# 
# ymlthis::yml_empty
