bad <- testthat::test_path("testData", "badpath")
book <- testthat::test_path("testData", "testBook")
doc <- testthat::test_path("testData", "testDocument", "custom_intro.qmd")
website <- testthat::test_path("testData", "testWebsite")

test_that("isQuartoBook works", {
  expect_false(isQuartoBook(99))
  expect_false(isQuartoBook(bad))
  expect_true(isQuartoBook(book))
  expect_false(isQuartoBook(doc))
  expect_false(isQuartoBook(website))
})

test_that("isQuartoDocument works", {
  expect_false(isQuartoDocument(99))
  expect_false(isQuartoDocument(bad))
  expect_false(isQuartoDocument(book))
  expect_true(isQuartoDocument(doc))
  expect_false(isQuartoDocument(website))
})

test_that("isQuartoWebsite works", {
  expect_false(isQuartoWebsite(99))
  expect_false(isQuartoWebsite(bad))
  expect_false(isQuartoWebsite(book))
  expect_false(isQuartoWebsite(doc))
  expect_true(isQuartoWebsite(website))
})
