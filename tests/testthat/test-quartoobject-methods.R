test_that("addTemplatePath fails elegantly with bad inputs", {
  checkBadInputs <- function(x) {
    expect_error(addTemplatePath(x, 1), "Assertion on 'path' failed: Must be of type 'character', not 'double'.")
    expect_error(addTemplatePath(x, "temp", 1), "Assertion on 'mustExist' failed: Must be of type 'logical', not 'double'.")
    expect_error(addTemplatePath(x, "badFolder", TRUE), "Assertion on 'path' failed: Directory 'badFolder' does not exist.")
    expect_error(addTemplatePath(x, "badFolder", FALSE), NA)
    expect_error(addTemplatePath(x, "temp", .after=99), "Assertion on '.after' failed: Element 1 is not <= 1.")
    expect_error(addTemplatePath(x, "temp", .after=1:3), "Assertion on '.after' failed: Must have length 1.")
  }

  checkBadInputs(QuartoCompoundObject())
  checkBadInputs(QuartoBook())
  checkBadInputs(QuartoWebsite())
  
  expect_error(addTemplatePath(QuartoObject()))
  expect_error(addTemplatePath(QuartoDocument()))
})



test_that("addTemplatePath works correctly", {
  correctFunctionality <- function(cls) {
    x <- new (cls)
    x <- addTemplatePath(x, "folder1")
    expect_equal(x, new (cls, templateSearchPath=list(".", "folder1")))
    x <- addTemplatePath(x, "folder2", .after=1)
    expect_equal(x, new (cls, templateSearchPath=list(".", "folder2", "folder1")))
  }
  
  correctFunctionality("QuartoCompoundObject")
  correctFunctionality("QuartoBook")
  correctFunctionality("QuartoWebsite")
})


test_that("removeTemplatePath fails elegantly with bad inputs", {
  checkBadInputs <- function(x) {
    expect_error(removeTemplatePath(x, path="badPath", pos=1), "Only one of `path` and `pos` should be given")
    expect_error(removeTemplatePath(x, path=5), "Assertion on 'path' failed: Must be of type 'character', not 'double'.")
    expect_error(removeTemplatePath(x, pos=5), "Assertion on 'pos' failed: Element 1 is not <= 1.")
  }

  checkBadInputs(QuartoCompoundObject())
  checkBadInputs(QuartoBook())
  checkBadInputs(QuartoWebsite())
  
  expect_error(removeTemplatePath(QuartoObject()))
  expect_error(removeTemplatePath(QuartoDocument()))
})

test_that("removeTemplatePath works correctly", {
  correctFunctionality <- function(cls) {
    x <- new(cls)
    expect_equal(removeTemplatePath(x, "."), new(cls, templateSearchPath=list()))
    x <- new(cls)
    expect_equal(removeTemplatePath(x, "notInPath"), x)

    y <- QuartoWebsite(templateSearchPath=list(".", "temp"))
    expect_equal(removeTemplatePath(y, "."), QuartoWebsite(templateSearchPath=list("temp")))
    y <- QuartoWebsite(templateSearchPath=list(".", "temp"))
    expect_equal(removeTemplatePath(y, "notInPath"), y)
  }

  correctFunctionality("QuartoCompoundObject")
  correctFunctionality("QuartoBook")
  correctFunctionality("QuartoWebsite")
})

test_that("locateTemplate fails elegantly with bad inputs", {
  checkBadInputs <- function(x) {
    expect_error(locateTemplate(x, fileName=5), "Assertion on 'fileName' failed: Must be of type 'character', not 'double'.")
    expect_error(locateTemplate(x, fileName="myTemplate.qmd", strict="YES"), "Assertion on 'strict' failed: Must be of type 'logical', not 'character'.")
    expect_error(locateTemplate(x, fileName=c("badPath", "goodPath")), "Assertion on 'fileName' failed: Must have length 1.")
    expect_error(locateTemplate(x, fileName="goodFile.qmd", strict=c(TRUE, FALSE)), "Assertion on 'strict' failed: Must have length 1.")
  }
  
  checkBadInputs(QuartoCompoundObject())
  checkBadInputs(QuartoBook())
  checkBadInputs(QuartoWebsite())
  
  expect_error(locateTemplate(QuartoObject(), "someFile.qmd"))
  expect_error(locateTemplate(QuartoDocument(), "someFile.qmd"))
})

test_that("locateTemplate works", {
  correctFunctionality <- function(cls) {
    
    x <- new(cls, templateSearchPath=list(test_path("testData/projectA"), test_path("testData/global")))
    y <- new(cls, templateSearchPath=list(test_path("testData/projectB"), test_path("testData/global")))
    expect_equal(locateTemplate(x, fileName="intro.qmd"), R.utils::getAbsolutePath(test_path("testData/projectA/intro.qmd")))
    expect_equal(locateTemplate(y, fileName="intro.qmd"), R.utils::getAbsolutePath(test_path("testData/global/intro.qmd")))

    x <- new(cls, templateSearchPath=list(test_path("testData/projectA/study1"), test_path("testData/projectA"), test_path("testData/global")))
    y <- new(cls, templateSearchPath=list(test_path("testData/projectB/study1"), test_path("testData/projectB"), test_path("testData/global")))
    expect_equal(locateTemplate(x, fileName="intro.qmd"), R.utils::getAbsolutePath(test_path("testData/projectA/intro.qmd")))
    expect_equal(locateTemplate(y, fileName="intro.qmd"), R.utils::getAbsolutePath(test_path("testData/global/intro.qmd")))

    x <- new(cls, templateSearchPath=list(test_path("testData/projectA/study2"), test_path("testData/projectA"), test_path("testData/global")))
    y <- new(cls, templateSearchPath=list(test_path("testData/projectB/study1"), test_path("testData/projectB"), test_path("testData/global")))
    expect_equal(locateTemplate(x, fileName="intro.qmd"), R.utils::getAbsolutePath(test_path("testData/projectA/intro.qmd")))
    expect_equal(locateTemplate(y, fileName="intro.qmd"), R.utils::getAbsolutePath(test_path("testData/global/intro.qmd")))

    expect_equal(locateTemplate(x, fileName=test_path("testData/custom_intro.qmd")), R.utils::getAbsolutePath(test_path("testData/custom_intro.qmd")))
    expect_error(
      locateTemplate(x, fileName=test_path("testData/bad_custom_intro.qmd")),
      paste0(
        "Assertion on 'foundFile' failed: File does not exist: '",
        R.utils::getAbsolutePath(test_path("testData/bad_custom_intro.qmd")),
        "'"
      )
    )
  }
  
  correctFunctionality("QuartoCompoundObject")
  correctFunctionality("QuartoBook")
  correctFunctionality("QuartoWebsite")
})

test_that("addChapter fails elegantly with bad inputs", {
  checkBadInputs <- function(x) {
    expect_error(addChapter(x, file=5), "Assertion on 'file' failed: Must be of type 'character', not 'double'.")
    expect_error(addChapter(x, file="myTemplate.qmd", "one"), "Assertion on '.after' failed: Must be of type 'number', not 'character'.")
    expect_error(addChapter(x, file=c("badPath", "goodPath")), "Assertion on 'file' failed: Must have length <= 1, but has length 2.")
    expect_error(addChapter(x, file="goodFile.qmd", .after=1:2), "Assertion on '.after' failed: Must have length 1.")
    expect_error(addChapter(x, file="goodFile.qmd", .after=-1), "Assertion on '.after' failed: Element 1 is not >= 1.")
  }
  
  checkBadInputs(QuartoCompoundObject())
  checkBadInputs(QuartoBook())
  checkBadInputs(QuartoWebsite())
  
  expect_error(addChapter(QuartoObject(), "someFile.qmd"))
  expect_error(addChapter(QuartoDocument(), "someFile.qmd"))
})

test_that("addChapter works", {
  correctFunctionality <- function(cls, ...) {
    x <- new(cls, ...)
    
    x <- addChapter(x, "someFile.qmd")
    expect_equal(x@chapters, list("someFile.qmd"))
    x <- addChapter(x, "anotherFile.qmd")
    expect_equal(x@chapters, list("someFile.qmd", "anotherFile.qmd"))
    x <- addChapter(x, "thirdFile.qmd", .after = 1)
    expect_equal(x@chapters, list("someFile.qmd", "thirdFile.qmd", "anotherFile.qmd"))
  }
  
  correctFunctionality("QuartoCompoundObject")
  correctFunctionality("QuartoBook")
  correctFunctionality("QuartoWebsite")
})

test_that("removeChapter fails elegantly with bad inputs", {
  checkBadInputs <- function(x) {
    expect_error(removeChapter(x, file = 5), "Assertion on 'file' failed: Must be of type 'character', not 'double'.")
    expect_error(removeChapter(x, file = "myTemplate.qmd", pos = 3), "Only one of `path` and `pos` should be given")
    expect_error(removeChapter(x, file = c("someFile.qmd", "anotherFile.qmd")), "Assertion on 'file' failed: Must have length <= 1, but has length 2.")
    expect_error(removeChapter(x, pos = c(0, 0)), "Assertion on 'pos' failed: Must have length 1.")
    expect_error(removeChapter(x, pos = 1), "Assertion on 'pos' failed: Element 1 is not <= 0.")
  }
  
  checkBadInputs(QuartoCompoundObject())
  checkBadInputs(QuartoBook())
  checkBadInputs(QuartoWebsite())
  
  expect_error(addChapter(QuartoObject(), "someFile.qmd"))
  expect_error(addChapter(QuartoDocument(), "someFile.qmd"))
})

test_that("removeChapter works", {
  correctFunctionality <- function(cls, ...) {
    x <- new(cls, ...)
    
    x <- addChapter(x, "someFile.qmd")
    y <- removeChapter(x, "someFile.qmd")
    expect_equal(y@chapters, list())
    y <- removeChapter(x, pos=1)
    expect_equal(y@chapters, list())
    
    x <- addChapter(x, "anotherFile.qmd")
    y <- removeChapter(x, "someFile.qmd")
    expect_equal(y@chapters, list("anotherFile.qmd"))
    y <- removeChapter(x, pos=1)
    expect_equal(y@chapters, list("anotherFile.qmd"))
    
    x <- addChapter(x, "thirdFile.qmd")
    y <- removeChapter(x, "anotherFile.qmd")
    expect_equal(y@chapters, list("someFile.qmd", "thirdFile.qmd"))
    y <- removeChapter(x, pos=2)
    expect_equal(y@chapters, list("someFile.qmd", "thirdFile.qmd"))
  }
  
  correctFunctionality("QuartoCompoundObject")
  correctFunctionality("QuartoBook")
  correctFunctionality("QuartoWebsite")
})

test_that("publish fails gracefully with bad input", {
  checkBadInputs <- function(x) {
    outFile <- withr::local_file(test_path("testData", "_work", "outFile"))
    workDir <-test_path("testData", "_work")
    
    expect_error(publish(x, 5), "Assertion on 'outFile' failed: No path provided.")
    # expect_error(
    #   publish(x, test_path("badFolder", "outputFile")), 
    #   "Assertion on 'outFile' failed: Path to file \\(dirname\\) does not exist: .+"
    # )
    # expect_error(publish(x, outFile, 5))
    # expect_error(
    #   publish(x, outFile, list("a", "b")),
    #   "Assertion on 'params' failed: Must have names."
    # )
    # expect_error(
    #   publish(x, outFile, list(), 5), 
    #   "Assertion on 'workDir' failed: Must be of type 'character', not 'double'."
    # )
    # expect_error(
    #   publish(x, outFile, list(), workDir, "false"), 
    #   "Assertion on 'tidyUp' failed: Must be of type 'logical', not 'character'."
    # )
  }    
    
  checkBadInputs(QuartoCompoundObject())
  checkBadInputs(QuartoBook())
  checkBadInputs(QuartoWebsite())
    
  expect_error(publish(QuartoObject(), "someFile.qmd"))
  expect_error(publish(QuartoDocument(), "someFile.qmd")) 
})

# test_that("publish works", {
#   correctFunctionality <- function(x, ...) {
#     outFile <- test_path("testDir", "_work", "outFile")
#     withr::local_file("outputFile", {
#       publish(
#         x,
#         outFile,
#         ...
#       )
#     })
#     
#     QuartoBook(
#       templateSearchPath = list(testthat::test_path("testData", "global")),
#       chapters = list(
#         "index.qmd",
#         testthat::test_path("testData", "testParams", "introParams.qmd")
#       )
#     )
#     params <- list(
#       dataName = "mtcars",
#       rowCount = 10,
#       x = "wt",
#       y = "mpg",
#       g = "cyl",
#       plotTitle = "A title for the plot"
#     )
#     
#     expect_no_error(publish(book, params))
#     expect_output_file()
#   }
#   
# })
