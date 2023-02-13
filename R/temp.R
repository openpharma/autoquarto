

# book <- QuartoBook(
#           templateSearchPath=list(
#             "./tests/testData/projectA/study1",
#             "./tests/testData/projectA",
#             "./tests/testData/global"
#           ),
#           chapters=list(
#             "index.qmd",  # Required by Quarto.  See https://github.com/quarto-dev/quarto-cli/discussions/4024
#             "intro.qmd",
#             "body.qmd",
#             "conclusion.qmd",
#             "environment.qmd"
#           )
#         )
# 
# futile.logger::flog.threshold(futile.logger::DEBUG)
# futile.logger::flog.layout(futile.logger::layout.format("~t ~l ~n ~f: ~m"))
# futile.logger::flog.appender(futile.logger::appender.console())
# publish(book, "/home/kirkpatj/Packages/autoquarto/tests/testOutput/testReport.html", workDir="/home/kirkpatj/Packages/autoquarto/temp")
# 
# 
# 
# 
# 
# 
# 
# 
# website <- QuartoWebsite(
#   templateSearchPath=list(
#     "./tests/testData/projectA/study1", 
#     "./tests/testData/projectA", 
#     "./tests/testData/global"
#   ),
#   chapters=list(
#     "intro.qmd",
#     "body.qmd",
#     "conclusion.qmd",
#     "environment.qmd"
#   )
# )
# 
# 
# publish(website, "./testReport.qmd")
# 
# # 
# # library(ymlthis)
# # 
# # format(lubridate::now(), "%Y_%m%b_%d_%H%M%S")
