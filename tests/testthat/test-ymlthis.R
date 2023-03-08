test_that("yml_book works", {
  expect_error(
    ymlthis::yml_empty() %>% yml_book(badName = TRUE),
    ".* failed: Must be a subset of .*, but has additional elements \\{'badName'\\}."
  )

  expect_equal(
    ymlthis::yml_empty() %>% yml_book(title = "Some title"),
    ymlthis::as_yml(list(book = list(title = "Some title")))
  )
  expect_equal(
    ymlthis::yml_empty() %>% yml_book(title = "Some title", chapters = list("chapter1.qmd", "chapter2.qmd")),
    ymlthis::yml_empty() %>% ymlthis::yml_toplevel(list(book = list(title = "Some title", chapters = list("chapter1.qmd", "chapter2.qmd"))))
  )
})

test_that("yml_project works", {
  expect_error(yml_project(27, badName = TRUE), "Assertion on '.yml' failed: Must inherit from class 'yml', but has class 'numeric'.")
  expect_error(ymlthis::yml_empty() %>% yml_project(badName = TRUE))

  x <- ymlthis::yml_empty() %>% yml_project(type = "book", `output-dir` = getwd())
  expect_true(x %>% ymlthis::has_field("project"))
  y <- x %>% ymlthis::yml_pluck("project")
  expect_true(y %>% ymlthis::has_field("type"))
  expect_true(y %>% ymlthis::has_field("output-dir"))
  expect_equal(y %>% ymlthis::yml_pluck("type"), "book")
  expect_equal(y %>% ymlthis::yml_pluck("output-dir"), getwd())
})

test_that("parameterTibbleToYAML works", {
  expect_error(parameterTibbleToYAML(5), "Assertion on 'd' failed: Must be a tibble, not double.")
  expect_error(parameterTibbleToYAML(tibble::tibble(X = 1)))

  t <- tibble::tibble(
    Parameter = c("d", "x", "y"),
    Value = c("mtcars", "1", "This is a string"),
    Mode = c("name", "value", "literal"),
    IsNULL = FALSE,
    IsNA = FALSE
  )
  expect_equal(parameterTibbleToYAML(t), ymlthis::yml_empty() %>% ymlthis::yml_replace(d = "mtcars", x = "1", y = "This is a string"))
})
