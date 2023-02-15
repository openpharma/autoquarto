test_that("yml_book works", {
  expect_error(
    ymlthis::yml_empty() %>% yml_book(badName=TRUE),
    ".* failed: Must be a subset of .*, but has additional elements \\{'badName'\\}."
  )
  
  expect_equal(
    ymlthis::yml_empty() %>% yml_book(title="Some title"),
    ymlthis::as_yml(list(book=list(title="Some title")))
  )
  expect_equal(
    ymlthis::yml_empty() %>% yml_book(title="Some title", chapters=list("chapter1.qmd", "chapter2.qmd")),
    ymlthis::yml_empty() %>% ymlthis::yml_toplevel(list(book=list(title="Some title", chapters=list("chapter1.qmd", "chapter2.qmd"))))
  )
})
