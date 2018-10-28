library(testthat)
library(TAPChunks)



singleTest  <- function (nameTest) {
  fileTest  <- list.files("tests/list_test", pattern = nameTest)
  file.copy(file.path("tests/list_test", fileTest), "tests/testthat")

  newName  <- paste0("test_", fileTest)

  if (!file.exists(file.path("tests/testthat"))) {

    dir.create(file.path("tests/testthat"))
  }

  file.rename(file.path("tests/testthat", fileTest),
              file.path("tests/testthat", newName))


  cat(TAPChunks:::BlueText(TAPChunks:::BoldText(TAPChunks:::UnderLineText(
    paste(
      "\n============== Test(Start):",
      nameTest,
      "==============\n\n"
    )
  ))))
  devtools::test()
  unlink(file.path("tests/testthat", newName))
  cat(TAPChunks::BlueText(TAPChunks::BoldText(TAPChunks::UnderLineText(
    paste(
      "\n============== Test(End):",
      nameTest,
      "==============\n\n"
    )
  ))))

}


singleTest("Example_Configuration")
singleTest("Example_CacheControls")
singleTest("Example_DataLake")
singleTest("Example_InternalUtilities")
singleTest("Example_Functions")
singleTest("Example_Weighting")
singleTest("Example_EmailTracker")

