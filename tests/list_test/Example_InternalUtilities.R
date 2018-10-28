test_that("Message:\n", {
  OptionsMessage(Info = F, Success = F)
  context("Testing CheckDemongDB and LoadDemogDB...")
  remove(TAP_env, envir = .GlobalEnv)
  expect_equal(CheckDemogDB(), TRUE)
  expect_equal(CheckDemogDB(), TRUE)
  expect_equal(length(TAP_env), 3)
  expect_s3_class(TAP_env$demog_helper, "data.table")
  expect_s3_class(TAP_env$target_helper, "data.table")


  context("Testing ShowADL..")
  expect_equal(ShowADLPrefix("email"), "SWEmail")
  expect_equal(ShowADLPath("email")[1], "Source_Data/SW/Email/rdata")
  expect_equal(
    HttpCode(404),
    "Not Found - The requested resource could not be found but may be available in the future."
  )

  context("Testing Open Connection functions..")
  expect_error(ConnectToADL(1, 1, 1),
               "The keys to open the ADL connection are invalid.")
  expect_equal(class(ConnectToADL()), "character")
  # expect_error(OpenSqlConnection("test", "test"), "Connection failed")
  # expect_equal(class(OpenSqlConnection("CAPENGSQL2\\TAP,51567", "MDSDB")), "RODBC")


  expect_equal(class(ValidateChunk(TestCCMCostChunk)), "list")
  expect_equal(ValidateChunk(TestClientChunk)$valid, T)
  expect_equal(ValidateChunk(iris)$valid, F)

  expect_equal(class(GetMapping("H"))[1], "data.table")
  expect_equal(class(GetMapping("P"))[1], "data.table")
  expect_equal(class(GetMapping("G"))[1], "data.table")


  expect_equal(CheckRequirements(TAP_env$InfoChunk[!is.na(ram_bytes)]$file_name_rdata[1:6]), "success")
})
