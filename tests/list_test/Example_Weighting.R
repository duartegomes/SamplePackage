library(TAPChunks)

context("Weighting Record Counts")

test_that("The ApplyWeightCalc function returns the
          same number of records as it received", {
  expect_equal(nrow(TestEmailChunk),
               nrow(ApplyWeightCalc(TestEmailChunk)))

  expect_equal(nrow(TestClientChunk),
                            nrow(ApplyWeightCalc(TestClientChunk)))

  expect_equal(nrow(TestCCMCostChunk),
               nrow(ApplyWeightCalc(TestCCMCostChunk)))

  expect_equal(nrow(TestCCMUsageChunk),
               nrow(ApplyWeightCalc(TestCCMUsageChunk)))

  expect_equal(nrow(TestNetworkhvChunk),
               nrow(ApplyWeightCalc(TestNetworkhvChunk)))

  expect_equal(nrow(TestWorkloadDBChunk),
               nrow(ApplyWeightCalc(TestWorkloadDBChunk)))
})


context("Weighting Granularity")

test_that("The weighting functions require the granularity to have GS or V", {
  expect_error(ApplyWeightCalc(TestEmailChunk, "x"))
  expect_error(ApplyWeightCalc(TestEmailChunk, "GSx"))
})

test_that("The weighting functions give the required granularity", {
  # Make sure that the number of weights generated remains consistent
  expect_equal(ShowWeightCalc(TestEmailChunk, "GSV")[, data.table::uniqueN(`Shape Weight`)], 182)
  expect_equal(ShowWeightCalc(TestEmailChunk, "GS" )[, data.table::uniqueN(`Shape Weight`)], 115)
  expect_equal(ShowWeightCalc(TestEmailChunk, "GV" )[, data.table::uniqueN(`Shape Weight`)], 107)
  expect_equal(ShowWeightCalc(TestEmailChunk, "G"  )[, data.table::uniqueN(`Shape Weight`)],  33)
  expect_equal(ShowWeightCalc(TestEmailChunk, "SV" )[, data.table::uniqueN(`Shape Weight`)],  30)
  expect_equal(ShowWeightCalc(TestEmailChunk, "S"  )[, data.table::uniqueN(`Shape Weight`)],   9)
  expect_equal(ShowWeightCalc(TestEmailChunk, "V"  )[, data.table::uniqueN(`Shape Weight`)],   8)
  # Make sure the show weight calculation and the apply weight generates the same number of weights

  expect_equal(ApplyWeightCalc(TestClientChunk, "gsv")[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "gsv")[, data.table::uniqueN(`Shape Weight`)])
  expect_equal(ApplyWeightCalc(TestClientChunk, "gs" )[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "gs" )[, data.table::uniqueN(`Shape Weight`)])
  expect_equal(ApplyWeightCalc(TestClientChunk, "gv" )[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "gv" )[, data.table::uniqueN(`Shape Weight`)])
  expect_equal(ApplyWeightCalc(TestClientChunk, "sv" )[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "sv" )[, data.table::uniqueN(`Shape Weight`)])
  expect_equal(ApplyWeightCalc(TestClientChunk, "s"  )[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "s"  )[, data.table::uniqueN(`Shape Weight`)])
  expect_equal(ApplyWeightCalc(TestClientChunk, "v"  )[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "v"  )[, data.table::uniqueN(`Shape Weight`)])
  expect_equal(ApplyWeightCalc(TestClientChunk, "g"  )[, data.table::uniqueN(weight)],
               ShowWeightCalc (TestClientChunk, "g"  )[, data.table::uniqueN(`Shape Weight`)])


  expect_equal(ApplyWeightCalc(TestEmailChunk[timestamp == "2015M01"])[, 1, by = weight][, sum(weight)],
               ShowWeightCalc (TestEmailChunk)[, sum(`Shape Weight`)], tolerance = 0.5)


})


