


library(TAPChunks)
OptionsMessage()

test_that("Functions:\n", {
  context("Testing ReadADLChunk...\n")

  expect_s3_class(
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M02",
      FileExtension = "rdata",
      Directory = F
    ),
    "data.table"
  )
  expect_s3_class(
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M01",
      FileExtension = "rdata",
      Directory = F
    ),
    "data.table"
  )
  expect_s3_class(
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M02",
      FileExtension = "csv"  ,
      Directory = F
    ),
    "data.table"
  )
  expect_s3_class(
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M01",
      FileExtension = "RData",
      Directory = F
    ),
    "data.table"
  )
  expect_s3_class(
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M02",
      FileExtension = "Csv"  ,
      Directory = F
    ),
    "data.table"
  )
  expect_equal(unique(
    ReadADLChunk(
      "Email",
      From = "2016M01",
      To = "2016M03",
      FileExtension = "rdata",
      Directory = F
    )$timestamp
  ),
  as.factor(c("2016M01", "2016M02", "2016M03")))



  context("Testing ReadADLFile...\n")

  assign("SWEmail2016M09", ReadADLFile(
    "Source_Data/SW/Email/rdata",
    "SWEmail2016M09.Rdata",
    SaveChunk = T,
    ShowInfo = T
  ))
  expect_s3_class(SWEmail2016M09, "data.table")


  context("Testing AddDimension...\n")

  expect_s3_class(AddProductHierarchy(TestEmailChunk, SelectHierarchy = "Product")         ,
                  "data.table")
  expect_s3_class(
    AddProductHierarchy(TestEmailChunk, SelectHierarchy = "Product Category"),
    "data.table"
  )
  expect_s3_class(
    AddProductHierarchy(TestEmailChunk, SelectHierarchy = "Product Type")    ,
    "data.table"
  )

  expect_equal(
    names(
      AddProductHierarchy(TestEmailChunk, SelectHierarchy = "Product")
    ),
    c(
      "row_id"      ,
      "uuid"    ,
      "timestamp",
      "Product2"      ,
      "Product1"    ,
      "Product0",
      "sw_flag" ,
      "email_delivery",
      "email_server",
      "host"    ,
      "variable",
      "value"
    )
  )

  expect_equal(
    names(
      AddProductHierarchy(TestEmailChunk, SelectHierarchy = "ProductCategory")
    ),
    c(
      "row_id"         ,
      "uuid"        ,
      "timestamp",
      "ProductCategory",
      "Product0"    ,
      "sw_flag" ,
      "email_delivery" ,
      "email_server",
      "host"    ,
      "variable"       ,
      "value"
    )
  )

  expect_equal(
    names(
      AddProductHierarchy(TestEmailChunk, SelectHierarchy = "Product Type")
    ),
    c(
      "row_id"        ,
      "uuid"       ,
      "timestamp",
      "ProductType"   ,
      "Product0"   ,
      "sw_flag"  ,
      "email_delivery",
      "email_server",
      "host"     ,
      "variable"      ,
      "value"
    )
  )

  expect_s3_class(AddGeoHierarchy(TestEmailChunk, SelectHierarchy = "Org Geo"),
                  "data.table")
  expect_equal(
    names(AddGeoHierarchy(TestEmailChunk, SelectHierarchy = "Org Geo")),
    c(
      "row_id"      ,
      "uuid"    ,
      "timestamp" ,
      "Area"          ,
      "Region"      ,
      "SubReg"  ,
      "Subsidiary",
      "Country",
      "sw_flag"   ,
      "email_delivery",
      "email_server",
      "host"    ,
      "clean"     ,
      "variable"      ,
      "value"
    )
  )

  expect_s3_class(
    AddSegmentHierarchy(TestEmailChunk, SelectHierarchy = "Org Segment"),
    "data.table"
  )

  expect_equal(
    names(
      AddSegmentHierarchy(TestEmailChunk, SelectHierarchy = "Org Segment")
    ),
    c(
      "row_id"  ,
      "uuid"     ,
      "timestamp"     ,
      "Sector"  ,
      "SubSector",
      "Segment"       ,
      "Segment_Code",
      "sw_flag"  ,
      "email_delivery",
      "email_server",
      "host"    ,
      "clean"    ,
      "variable"      ,
      "value"
    )
  )


  context("Testing AddFirmographics...\n")

  expect_equal(
    names(AddFirmographics(TestEmailChunk)),
    c(
      "row_id",
      "uuid",
      "timestamp",
      "Country",
      "Segment_Code",
      "Vertical",
      "sw_flag",
      "email_delivery",
      "email_server",
      "host",
      "clean",
      "variable",
      "value"
    )
  )


  context("Testing CheckDemogDB...\n")

  expect_equal(CheckDemogDB(), T)


  context("Testing LoadDemogDB...\n")

  expect_equal(LoadDemogDB(), T)

  context("Testing ShapeChunk...\n")

  expect_s3_class(ShapeChunk(TestEmailChunk, Variable = "variable", Value = "value", Operation= "Sum"), "data.table")

})
