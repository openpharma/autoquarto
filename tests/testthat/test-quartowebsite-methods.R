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

test_that("locateTemplate fails elegantly with bad inputs", {
  x <- QuartoWebsite()
  
  expect_error(locateTemplate(x, fileName=5), "Assertion on 'fileName' failed: Must be of type 'character', not 'double'.")
  expect_error(locateTemplate(x, fileName="myTemplate.qmd", strict="YES"), "Assertion on 'strict' failed: Must be of type 'logical', not 'character'.")
  expect_error(locateTemplate(x, fileName=c("badPath", "goodPath")), "Assertion on 'fileName' failed: Must have length 1.")
  expect_error(locateTemplate(x, fileName="goodFile.qmd", strict=c(TRUE, FALSE)), "Assertion on 'strict' failed: Must have length 1.")
})

test_that("locateTemplate works", {
  x <- QuartoWebsite(templateSearchPath=list("../testData/projectA", "../testData/global"))
  y <- QuartoWebsite(templateSearchPath=list("../testData/projectB", "../testData/global"))
  expect_equal(locateTemplate(x, fileName="intro.qmd"), "../testData/projectA/intro.qmd")
  expect_equal(locateTemplate(y, fileName="intro.qmd"), "../testData/global/intro.qmd")
  
  x <- QuartoWebsite(templateSearchPath=list("../testData/projectA/study1", "../testData/projectA", "../testData/global"))
  y <- QuartoWebsite(templateSearchPath=list("../testData/projectB/study1", "../testData/projectB", "../testData/global"))
  expect_equal(locateTemplate(x, fileName="intro.qmd"), "../testData/projectA/study1/intro.qmd")
  expect_equal(locateTemplate(y, fileName="intro.qmd"), "../testData/global/intro.qmd")
  
  x <- QuartoWebsite(templateSearchPath=list("../testData/projectA/study2", "../testData/projectA", "../testData/global"))
  y <- QuartoWebsite(templateSearchPath=list("../testData/projectB/study1", "../testData/projectB", "../testData/global"))
  expect_equal(locateTemplate(x, fileName="intro.qmd"), "../testData/projectA/intro.qmd")
  expect_equal(locateTemplate(y, fileName="intro.qmd"), "../testData/global/intro.qmd")
  
  expect_equal(locateTemplate(x, fileName="../testData/custom_intro.qmd"), "../testData/custom_intro.qmd")
  expect_error(locateTemplate(x, fileName="../testData/bad_custom_intro.qmd"), "Assertion on 'foundFile' failed: File does not exist: '../testData/bad_custom_intro.qmd'")
  
})
