test_that("addTemplatePath fails elegantly with bad inputs", {
  x <- QuartoWebsite()

  expect_error(addTemplatePath(x, 1), "Assertion on 'path' failed: Must be of type 'character', not 'double'.")
  expect_error(addTemplatePath(x, "temp", 1), "Assertion on 'mustExist' failed: Must be of type 'logical', not 'double'.")
  expect_error(addTemplatePath(x, "badFolder", TRUE), "Assertion on 'path' failed: Directory 'badFolder' does not exist.")
  expect_error(addTemplatePath(x, "badFolder", FALSE), NA)
  expect_error(addTemplatePath(x, "temp", .after=99), "Assertion on '.after' failed: Element 1 is not <= 1.")
  expect_error(addTemplatePath(x, "temp", .after=1:3), "Assertion on '.after' failed: Must have length 1.")
})

test_that("addTemplatePath works correctly", {
  x <- QuartoWebsite()
  x <- addTemplatePath(x, "folder1")
  expect_equal(x, QuartoWebsite(templateSearchPath=list(".", "folder1")))
  x <- addTemplatePath(x, "folder2", .after=1)
  expect_equal(x, QuartoWebsite(templateSearchPath=list(".", "folder2", "folder1")))
})


test_that("removeTemplatePath fails elegantly with bad inputs", {
  x <- QuartoWebsite()
  
  expect_error(removeTemplatePath(x, path="badPath", pos=1), "Only one of `path` and `pos` should be given")
  expect_error(removeTemplatePath(x, path=5), "Assertion on 'path' failed: Must be of type 'character', not 'double'.")
  expect_error(removeTemplatePath(x, pos=5), "Assertion on 'pos' failed: Element 1 is not <= 1.")
})

test_that("removeTemplatePath works correctly", {
  x <- QuartoWebsite()
  expect_equal(removeTemplatePath(x, "."), QuartoWebsite(templateSearchPath=list()))
  x <- QuartoWebsite()
  expect_equal(removeTemplatePath(x, "notInPath"), QuartoWebsite())
  
  y <- QuartoWebsite(templateSearchPath=list(".", "temp"))
  expect_equal(removeTemplatePath(y, "."), QuartoWebsite(templateSearchPath=list("temp")))
  y <- QuartoWebsite(templateSearchPath=list(".", "temp"))
  expect_equal(removeTemplatePath(y, "notInPath"), y)
})