


test_that("Message:\n", {
  OptionsMessage()
  expect_equal(Configuration_env$Message$Configuration$SuperUser, FALSE)

  context("Testing Info messages...\n")
  catInfo("Test 'catInfo'", "catInfo")
  expect_equal(Configuration_env$Message$Configuration$StatusMessage,
               "info")

  context("Testing Success messages...\n")
  catSuccess("Test 'catSuccess'", "catSuccess")
  expect_equal(Configuration_env$Message$Configuration$StatusMessage,
               "success")

  context("Testing SuperUse messages without print...\n")
  catSuperUser("Test 'catSuperUser'", "catSuperUser")
  expect_equal(Configuration_env$Message$Configuration$StatusMessage,
               "success")

  context("Testing Warning messages...\n")
  expect_warning(catWarning("Test 'catWarning'", "catWarning"),
                 "Test 'catWarning'")
  expect_equal(Configuration_env$Message$Configuration$StatusMessage,
               "warn")

  context("Testing Error messages...\n")
  expect_error(catError("Test 'catError'", "catError"), "Test 'catError'")
  expect_equal(Configuration_env$Message$Configuration$StatusMessage,
               "error")

  # context("Checking the version of R...\n")
  if (Configuration_env$Message$Configuration$RStudioVersion < 1.1)
    catWarning("Old version of Rstudio")


})





