#' Prepare to Publish a Quarto Object
#' 
#' Check the working folder is accessible, set up the log file
#' @param workDir The working folder
#' @param outFile The name of the output file
#' @param logFile The requested log file 
#' @returns The path to the working directory
.prepareToPublish <- function(workDir, outFile, logFile) {
  if (!is.null(workDir)) {
    checkmate::assertCharacter(workDir, len=1)
    checkmate::assertDirectoryExists(workDir, access="rwx")
  }
  if (is.na(logFile)) {
    logFile <- file.path(
      here::here(),
      paste0(
        tools::file_path_sans_ext(basename(outFile)),
        "_",
        format(lubridate::now(), "%Y_%m%b_%d_%H%M%S"),
        ".log"
      )
    )
    print(paste0("logFile is ", logFile))
  }
  futile.logger::flog.layout(futile.logger::layout.format("~t ~l [~n:~f]: ~m"))
  futile.logger::flog.appender(futile.logger::appender.file(logFile))
  if (is.null(workDir)) {
    workDir <- tempdir()
    # futile.logger::flog.info(paste0("Using `", workDir, "' as a working folder"))
    checkmate::assertDirectoryExists(workDir, access="rwx")
  }
  workFiles <- list.files(workDir, full.names=TRUE)
  if (length(workFiles) > 0) {
    futile.logger::flog.info(paste0("workDir [", workDir, "] is not empty.  Deleting current contents..."))
    file.remove(workFiles)
  }
  workDir
}

#' Process a project YAML
#' 
#' @param x The QuartoObject that defines the project YAML
#' @param workDir The working directory 
#' @param outFile The path to the output file
#' @param params A list of parameters to write to the project YAML
.processProjectYAML <- function(x, workDir, outFile, params) {
  futile.logger::flog.info("Processing _quarto.yml...")
  # Define output file
  qYML <- quartoYML(x) 
  y <- qYML %>% 
    ymlthis::yml_pluck(x@type) %>% 
    ymlthis::yml_replace("output-file"=tools::file_path_sans_ext(basename(outFile)))
  zz <- list()
  zz[[x@type]] <- y
  qYML <- qYML %>% ymlthis::yml_replace(zz)
  # Define output folder
  proj <- qYML %>%
    ymlthis::yml_pluck("project") %>%
    ymlthis::yml_replace("output-dir"=R.utils::getAbsolutePath(dirname(outFile)))
  qYML <- qYML %>% ymlthis::yml_replace("project"=proj)
  # Write _quarto.yml
  futile.logger::flog.info("Writing _quarto.yml")
  qYML %>% ymlthis::use_yml_file(R.utils::getAbsolutePath(file.path(workDir, "_quarto.yml")))
  futile.logger::flog.debug("_quarto.yml is:")
  invisible(
    lapply(
      readr::read_lines(R.utils::getAbsolutePath(file.path(workDir, "_quarto.yml"))), 
      futile.logger::flog.debug
    )
  )
}

#' Render a QuartoCompoundObject
#' 
#' @param workDir The working directory containing the _quarto.yaml file
.renderQuartoCompoundObject <- function(workDir) {
  futile.logger::flog.info("Rendering report...")
  quarto::quarto_render(workDir, as_job=FALSE)
  futile.logger::flog.info("Done")
}