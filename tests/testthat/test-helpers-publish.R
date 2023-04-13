test_that(".prepareToPublish fails gracefully with bad input", {
  file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE))
  expect_error(
    .prepareToPublish(1),
    "Assertion on 'workDir' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    .prepareToPublish(
      c(tempdir(), tempdir())
    ),
    "Assertion on 'workDir' failed: Must have length 1, but has length 2."
  )
  expect_error(
    .prepareToPublish(tempdir(), 14),
    "Assertion on 'outFile' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    .prepareToPublish(tempdir(), c("outFile", "outFile")),
    "Assertion on 'outFile' failed: Must have length 1, but has length 2."
  )
  expect_error(
    .prepareToPublish(tempdir(), "outFile", 5),
    "Assertion on 'logFile' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    .prepareToPublish(tempdir(), "outFile", c("logFle", "logFile")),
    "Assertion on 'logFile' failed: Must have length 1, but has length 2."
  )
  skip_on_ci()
  expect_error(
    .prepareToPublish(tempdir(), "outFile", "/badPath"),
    stringr::fixed("Assertion on 'logFile' failed: '/' not writeable.")
  )
})

test_that(".prepareToPublish works", {
  expect_message(
    .prepareToPublish(tempdir(), "outFile", NA),
    "logFile is .+\\/outFile_\\d{4,4}_\\d{2,2}[A-Z][a-z]{2,2}_\\d{2,2}_\\d{6,6}\\.log"
  )
  expect_no_message(
    .prepareToPublish(tempdir(), "outFile", file.path(tempdir(), "myLogFile.log"))
  )
  expect_equal(.prepareToPublish(tempdir(), "outFile", NA), tempdir())

  withr::local_file("temp.txt", {
    con <- file(file.path(tempdir(), "temp.txt"))
    writeLines(c("Hello", "World"), con)
    close.connection(con)
    expect_message(
      .prepareToPublish(tempdir(), "outFile", file.path(tempdir(), "myLogFile.log")),
      ".+ Deleting current contents.+"
    )
  })

  withr::local_file("logFie.log", {
    expect_equal(
      .prepareToPublish(NULL, "outFile", "myLogFile.log"),
      tempdir()
    )
  })
})

test_that("processProjectYAML fails gracefully with bad input", {
  expect_error(
    .processProjectYAML(QuartoDocument("myFile.qmd")),
    "Assertion on 'x' failed: Must inherit from class 'QuartoCompoundObject', but has class 'QuartoDocument'."
  )
  expect_error(
    .processProjectYAML(QuartoBook(), 5),
    "Assertion on 'workDir' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    .processProjectYAML(QuartoBook(), c("myFolder", "myFolder")),
    "Assertion on 'workDir' failed: Must have length 1, but has length 2."
  )
  expect_error(
    .processProjectYAML(QuartoBook(), tempdir(), 5),
    "Assertion on 'outFile' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    .processProjectYAML(QuartoBook(), tempdir(), c("myLog", "myLog")),
    "Assertion on 'outFile' failed: Must have length 1, but has length 2."
  )
  expect_error(
    .processProjectYAML(QuartoBook(), tempdir(), file.path(tempdir(), "myLogFile.log"), 5),
    "Assertion on 'params' failed: Must be of type 'list', not 'double'."
  )
  expect_error(
    .processProjectYAML(QuartoBook(), tempdir(), file.path(tempdir(), "myLogFile.log"), list(1)),
    "Assertion on 'params' failed: Must have names."
  )
})

test_that("processProjectYAML works", {
  workDir <- file.path(tempdir(), "abc")
  dir.create(workDir)
  withr::defer(unlink(workDir))

  .processProjectYAML(
    QuartoBook() %>% addChapter("index.qmd") %>% addChapter("body.qmd"),
    workDir,
    "outFile",
    list(a = 1, b = "two")
  )
  expect_true(file.exists(file.path(workDir, "_quarto.yml")))
  yml <- yaml::read_yaml(file.path(workDir, "_quarto.yml"))
  expect_equal(
    as.list(yml %>% ymlthis::yml_pluck("book", "chapters")),
    list("index.qmd", "body.qmd")
  )
})

test_that(".renderQuartoCompoundObject works", {
  # workDir <- file.path(tempdir(), "abc")
  # dir.create(workDir)
  # withr::defer(unlink(workDir))
  #
  # projectFiles <- list.files(test_path("testData", "testBook"), full.names = TRUE)
  # file.copy(projectFiles, workDir)
  #
  # expect_no_error(.renderQuartoCompoundObject(workDir))
})
