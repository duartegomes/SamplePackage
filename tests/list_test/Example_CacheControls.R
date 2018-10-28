

context("Test script configuration functions:")

test_that("CacheControls:\n", {

  OptionsMessage(Error = T, Warning = T, Info = F, Success = F)
  context("Checks if cache is empty, cache files have been deleted...\n")
  DeleteCache("**", ForceDelete = T)
  expect_equal(0, length(list.files(ShowCachePath())))

  context("Testing when reading two email files...\n")
  if(!exists("TAP_env")){
    Test <<- ReadADLChunk("Email", "2017M06", "2017M07")
    expect_equal(4, length(list.files(ShowCachePath())))
    DeleteCache("*helper*", ForceDelete = T)
    expect_equal(2, length(list.files(ShowCachePath())))
  }else{
    Test <<- ReadADLChunk("Email", "2017M06", "2017M07")
    expect_equal(2, length(list.files(ShowCachePath())))
    expect_equal(ListCacheDirectory()[["File Name"]],
                 c("SWEmail2017M06.Rdata", "SWEmail2017M07.Rdata"))
    expect_equal(data.table::is.data.table(ShowCacheStatus("Email")), T)}

  context("Testing a set of functions (CachePath, Protect, CleanMemory, Unprotect)...\n")
  expect_warning(CachePath(),"This function has been renamed to ShowCachePath.")
  Protect(Test);CleanMemory()
  expect_equal(intersect(ls(envir = .GlobalEnv), "Test"), "Test")
  Unprotect(Test);CleanMemory()
  expect_equal(intersect(ls(envir = .GlobalEnv), "Test"), character(0))

  context("Testing DeleteCache...\n")
  DeleteCache("*email*2017M06*", T)
  expect_equal(ListCacheDirectory()[["File Name"]], "SWEmail2017M07.Rdata")

  DeleteCache("*email*2017M07*", T)
  expect_equal(ListCacheDirectory(), NULL)

})
