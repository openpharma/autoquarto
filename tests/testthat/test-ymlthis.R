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

test_that(".readYamlFrontMatter fails gracefully with bad input", {
  expect_error(.readYamlFrontMatter(5), "Assertion on 'path' failed: No file provided.")
  expect_error(
    .readYamlFrontMatter(
      test_path("testData", "testBook", "intro.qmd"), 5
    ),
    "Assertion on 'item' failed: Must be of type 'character', not 'double'."
  )
})

test_that(".readYamlFrontMatter works", {
  expect_equal(
    .readYamlFrontMatter(test_path("testData", "testBook", "intro.qmd"), "params"),
    list(dataName = "mtcars", rowCount = 10, x = "wt", y = "mpg", g = "cyl", plotTitle = "This is a title for the plot")
  )
})

test_that("replaceYamlParams fails gracefully with bad input", {
  yaml <- c(
    "---", "title: Global intro", "format: html", "editor: visual",
    "params:", "    dataName: mtcars", "    rowCount: 10",
    "    x: wt", "    y: mpg", "    g: cyl",
    "    plotTitle: This is a title for the plot",
    "---"
  )

  expect_error(replaceYamlParams(5), "Assertion on 'path' failed: No file provided.")
  expect_error(
    replaceYamlParams(test_path("testData", "testBook", "intro.qmd"), 5),
    "Assertion on 'params' failed: Must be of type 'list', not 'double'."
  )
  expect_error(replaceYamlParams(test_path("testData", "testBook", "intro.qmd"), list(), "badReplace"))
  expect_error(replaceYamlParams(test_path("testData", "testBook", "intro.qmd"), list(), "both"), "Not yet implemented")
  expect_error(replaceYamlParams(test_path("testData", "testBook", "intro.qmd"), list(), "file"), "Not yet implemented")
  expect_error(replaceYamlParams(test_path("testData", "testBook", "intro.qmd"), list(), "list"), "Not yet implemented")
  withr::with_file("tmp.yml", {
    writeLines(yaml[2:length(yaml)], "tmp.yml")
    expect_error(replaceYamlParams("tmp.yml", list()), "tmp.yml is malformed: front matter not found")
  })
  withr::with_file("tmp.yml", {
    writeLines(yaml[1:(length(yaml) - 1)], "tmp.yml")
    expect_error(replaceYamlParams("tmp.yml", list()), "tmp.yml is malformed: front matter has no end")
  })
})

test_that("replaceYamlParams works", {
  yamlIn <- c(
    "---", "title: Global intro", "format: html", "editor: visual",
    "params:", "    dataName: NA", "    rowCount: NA",
    "    x: NA", "    y: NA", "    g: NA",
    "    plotTitle: NA",
    "---",
    "Blah blah blah"
  )
  yamlOut <- c(
    "---", "title: Global intro", "format: html", "editor: visual",
    "params:", "  dataName: mtcars", "  rowCount: 10.0",
    "  x: wt", "  'y': mpg", "  g: cyl",
    "  plotTitle: This is a title for the plot",
    "---",
    "Blah blah blah"
  )

  withr::with_file("tmp.yml", {
    writeLines(yamlIn, "tmp.yml")
    replaceYamlParams(
      "tmp.yml",
      list(
        dataName = "mtcars",
        rowCount = 10,
        x = "wt",
        y = "mpg",
        g = "cyl",
        plotTitle = "This is a title for the plot"
      )
    )
    expect_equal(readLines("tmp.yml"), yamlOut)
  })
})
