# rmarkdown::yaml_front_matter
# 
# 
# library(tidyverse)
# 
# x <- ymlthis::yml_empty() %>% ymlthis::yml_params(list("data"=ymlthis::as_yml(mtcars)))
# 
# ymlthis::draw_yml_tree(x)
# 
# ymlthis::has_field(x, "params")
# 
# ymlthis::yml_output

#' Create a YAML book element
#' @inheritParams yml_element
#' @returns a `yml` object
#' @export
yml_book <- function (.yml, ...){
  yml_element(.yml, "book", ...)
}

#' Capture, validate, and write YAML
#' 
#' `yml_element()` writes valid YAML for an arbitrary field of R Markdown YAML. 
#' `yml_element()` captures the actual field functions and translates them to YAML. 
#' This function accepts multiple output formats (separated by commas) and 
#' validates each by evaluating the function internally. 
#' The YAML fields under the parent element come from arguments in their 
#' respective R functions. If you wanted to see the available fields in 
#' `pdf_document()`, for instance, you would read the documentation for that 
#' function using `?pdf_document`.
#' @inheritParams ymlthis::yml_output
#' @param name the name of the YAML field being created
#' @returns a `yml` object
#' @examples 
#' identical(
#'   ymlthis::yml_empty() %>% yml_element("output", html_document()), 
#'   ymlthis::yml_empty() %>% ymlthis::yml_output(html_document()) # TRUE
#' )
#' @export
yml_element <- function(.yml, name, ...) {
  x <- rlang::enquos(...)
  validate_output_yml(x)
  args_list <- purrr::map(x, rlang::call_args)
  function_name_list <- purrr::map(x, rlang::call_name)
  function_namespaces <- purrr::map(x, rlang::call_ns)
  function_name_list <- purrr::map2(function_namespaces, function_name_list, 
                                    prepend_namespace)
  if (length(x) == 1) {
    .yml[[name]] <- parse_output_yml(args_list[[1]], function_name_list[[1]])
    return(.yml)
  }
  warn_if_duplicate_fields(.yml, list(book = ""))
  .yml$output <- purrr::map2(args_list, function_name_list, 
                             parse_output_yml, use_default = TRUE) %>% purrr::flatten()
  .yml
  
}

prepend_namespace <- function (function_namespace, function_name) {
  ifelse(is.null(function_namespace), function_name, paste0(function_namespace, 
                                                            "::", function_name))
}

parse_output_yml <- function (args, function_name, use_default = FALSE) {
  if (!rlang::has_length(args) && !use_default) {
    return(function_name)
  }
  if (!rlang::has_length(args) && use_default) {
    output_yml <- list("default")
    names(output_yml) <- function_name
    return(output_yml)
  }
  yml_list <- list(purrr::map_if(args, rlang::is_call, eval_with_rmarkdown, 
                                 check_type = FALSE))
  names(yml_list) <- function_name
  yml_list
}

validate_output_yml <- function (.function_calls) {
  purrr::walk(.function_calls, eval_tidy_yml)
}

warn_if_duplicate_fields <- function (yml_object, new_fields) 
{
  fields <- names(new_fields)
  duplicate_fields <- any(names(yml_object) %in% fields)
  if (duplicate_fields) {
    fields <- glue::glue("`{fields}`") %>% glue::glue_collapse(sep = ", ", 
                                                               last = " and ")
    msg <- glue::glue("Top-level fields {fields} already present. \\\n      Replacing the existing fields. \\\n      Use `yml_replace() if you want to replace fields without a warning.")
    warning(msg, call. = FALSE)
  }
  invisible(yml_object)
}

eval_tidy_yml <- function (x) {
  out <- rlang::catch_cnd(eval_with_rmarkdown(x), classes = "error")
  if (!is.null(out)) 
    stop_yml_eval(out, x)
  out
}

eval_with_rmarkdown <- function (x, check_type = TRUE) {
  msg <- "rmarkdown must be installed to use outputs"
  if (!requireNamespace("rmarkdown", quietly = TRUE)) 
    stop(msg, call. = FALSE)
  x <- withr::with_namespace("rmarkdown", rlang::eval_tidy(x))
  if (check_type && !inherits(x, "rmarkdown_output_format")) {
    stop("`output` must return object of class `rmarkdown_output_format`", 
         call. = FALSE)
  }
  x
}

stop_yml_eval <- function (e, x) {
  stop("Invalid argument in YAML output function ", rlang::quo_text(x), 
       "\n", as.character(e), call. = FALSE)
}