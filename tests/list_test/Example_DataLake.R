

test_that("CacheControls:\n", {

  OptionsMessage(
    Error = T,    Warning = T,
    Info = F ,    Success = F
  )

  #
  context("Testing ListADLDirectory...\n")
  expect_s3_class(ListADLDirectory("Source_Data/SW"), "data.table")
  expect_equal(nrow(ListADLDirectory("Source_Data/SW/Email")), 3)
  #
  context("Testing ReadADLFile with Rdata...\n")
  Test <- ListADLDirectory("Source_Data/SW/Email/rdata")[[1]][1]
  assign(substr(Test, 1, gregexpr("\\.", Test)[[1]][1] - 1), ReadADLFile(
    "Source_Data/SW/Email/rdata",
    ListADLDirectory("Source_Data/SW/Email/rdata")[[1]][1],
    SaveChunk = F,
    ShowInfo = F
  ))

  fileName <-
    gsub(".Rdata",
         "",
         ListADLDirectory("Source_Data/SW/Email/rdata")[[1]][1])
  expect_s3_class(get(fileName), "data.table")
  remove(list = fileName)
  #
  context("Testing ReadADLFile with CSV...\n")
  Test <- ListADLDirectory("Source_Data/SW/Email/csv")[[1]][1]
  assign(substr(Test, 1, gregexpr("\\.", Test)[[1]][1] - 1), ReadADLFile(
    "Source_Data/SW/Email/csv",
    ListADLDirectory("Source_Data/SW/Email/csv")[[1]][1],
    SaveChunk = F,
    ShowInfo = F
  ))
  fileName <-
    gsub(".csv", "", ListADLDirectory("Source_Data/SW/Email/csv")[[1]][1])
  expect_s3_class(get(fileName), "data.table")
  remove(list = fileName)
  #
  context("Testing ReadADLChunk with Rdata...\n")
  test <-
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M02",
      FileExtension = "rdata",
      Directory = F
    )
  expect_s3_class(test, "data.table")
  expect_equal(as.factor(unique(test$timestamp)), as.factor(c("2016M01", "2016M02")))
  #
  context("Testing ReadADLChunk with CSV...\n")
  test <-
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M02",
      FileExtension = "csv",
      Directory = F
    )
  expect_s3_class(test, "data.table")
  expect_equal(as.factor(unique(test$timestamp)), as.factor(c("2016M01", "2016M02")))

  #
  context("Testing ReadABSFile...\n")
  test <- ReadABSFile(FileSpecific = "2017_12/msft_deliver_email_2017_12.zip")
  expect_s3_class(test, "data.table")

  test <- ReadABSFile(FileSpecific = "2017_12/msft_deliver_social_data_2017_12.csv")
  expect_s3_class(test, "data.table")

  remove(test)
})

