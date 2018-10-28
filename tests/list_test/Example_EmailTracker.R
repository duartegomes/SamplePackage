


test_that("Email Tracker:\n", {
  OptionsMessage()
  context("Testing ReadADLChunk...\n")

  test <- ReadADLChunk(Source = "Email", "2017M01", "2017M03")

  expect_s3_class(test, "data.table")
  expect_equal(as.factor(unique(test$timestamp)), as.factor(c("2017M01", "2017M02", "2017M03")))
  expect_equal(as.factor(names(test)), as.factor(
    c(
      "row_id",
      "uuid",
      "sw_flag",
      "email_delivery",
      "email_server",
      "host",
      "timestamp",
      "variable",
      "value"
    )
  ))


  context("Testing FilterChunk...\n")

  test <- FilterChunk(Data = test , email_delivery != "antispam")
  expect_s3_class(test, "data.table")
  expect_equal(as.character(unique(test$email_delivery)), c("hosted", "onsite"))


  context("Testing CalculateColumn...\n")

  test   <- CalculateColumn(Data = test,
                            Calculation = NoProd := ifelse(variable == "NoProd", 0, 1))
  expect_s3_class(test, "data.table")
  expect_equal(as.factor(names(test)), as.factor(
    c(
      "row_id",
      "uuid",
      "sw_flag",
      "email_delivery",
      "email_server",
      "host",
      "timestamp",
      "variable",
      "value",
      "NoProd"
    )
  ))


  test   <- CalculateColumn(test,
                            Filter      = "365|[M|m]icrosoft|ms" %matches% host,
                            Calculation = host2 := "Exchange Online")

  test   <- CalculateColumn(test,
                            Filter      = "google" %matches% host,
                            Calculation = host2 := "Google Online")

  test   <- CalculateColumn(
    test,
    Filter      = "onsite" %matches% host |
      "on site" %matches% host | host == "Unclassified",
    Calculation = host2 := "NHO"
  ) #No Host Org

  test   <- CalculateColumn(test,
                            Filter      = is.na(host2),
                            Calculation = host2 := "Other Online")
  expect_s3_class(test, "data.table")
  expect_equal(as.factor(names(test)), as.factor(
    c(
      "row_id",
      "uuid",
      "sw_flag",
      "email_delivery",
      "email_server",
      "host",
      "timestamp",
      "variable",
      "value",
      "NoProd",
      "host2"
    )
  ))
  expect_equal(as.character(unique(test$host2)), c(
   "Other Online",  "NHO", "Google Online", "Exchange Online"
  ))



  context("Testing AggregateChunk...\n")

  orgs   <- AggregateChunk  (
    test,
    Aggregation = .(
      service = unique(host2),
      sw_flag = unique(sw_flag),
      OnPrem  = max(NoProd)
    ),
    By          = c("uuid", "timestamp")
  )
  expect_s3_class(orgs, "data.table")
  expect_equal(as.factor(names(orgs)), as.factor(c(
    "uuid", "timestamp", "service", "sw_flag", "OnPrem"
  )))
  expect_equal(as.character(unique(orgs$service)), c(
    "Other Online",  "NHO", "Google Online", "Exchange Online"
  ))


  context("Testing CalculateColumn...\n")

  orgs   <-
    CalculateColumn(orgs, Calculation = Hosted := ifelse(service == "NHO", F, T))
  expect_s3_class(orgs, "data.table")
  expect_equal(as.factor(names(orgs)), as.factor(
    c("uuid", "timestamp", "service", "sw_flag", "OnPrem", "Hosted")
  ))

  orgs   <-
    CalculateColumn(orgs,
                    Filter = OnPrem == 0 & !Hosted,
                    Calculation = Delivery := "None")
  orgs   <-
    CalculateColumn(orgs,
                    Filter = OnPrem == 1 & !Hosted,
                    Calculation = Delivery := "OnPrem")
  orgs   <-
    CalculateColumn(orgs,
                    Filter = OnPrem == 0 &  Hosted,
                    Calculation = Delivery := "Hosted")
  orgs   <-
    CalculateColumn(orgs,
                    Filter = OnPrem == 1 &  Hosted,
                    Calculation = Delivery := "Mixed")

  expect_s3_class(orgs, "data.table")
  expect_equal(as.factor(names(orgs)), as.factor(
    c(
      "uuid",
      "timestamp",
      "service",
      "sw_flag",
      "OnPrem",
      "Hosted",
      "Delivery"
    )
  ))
  expect_equal(as.factor(unique(orgs$Delivery)), as.factor(c("Mixed", "OnPrem", "Hosted", "None")))

  context("Testing AggregateChunk...\n")

  service_apps <- AggregateChunk(
    orgs,
    Filter = Delivery != "None" &
      service != "NHO",
    Aggregation = .(
      uuid,
      sw_flag,
      timestamp,
      variable = service,
      value = 1
    )
  )
  expect_s3_class(service_apps, "data.table")
  expect_equal(as.factor(names(service_apps)), as.factor(c(
    "uuid", "sw_flag", "timestamp", "variable", "value"
  )))
  expect_equal(as.character(unique(service_apps$variable)), c(
    "Other Online", "Google Online","Exchange Online"
  ))

  server_apps  <- AggregateChunk(
    test,
    Filter      =  variable != "NoProd",
    Aggregation =   .(uuid, sw_flag, timestamp,
                      variable, value = 1)
  )
  expect_s3_class(server_apps, "data.table")
  expect_equal(as.factor(names(server_apps)), as.factor(c(
    "uuid", "sw_flag", "timestamp", "variable", "value"
  )))

  context("Testing JoinChunks...\n")

  out <- JoinChunks(service_apps, server_apps)
  expect_s3_class(out, "data.table")
  expect_equal(nrow(service_apps) + nrow(server_apps), nrow(out))
  expect_equal(as.factor(unique(out$timestamp)), as.factor(c("2017M01", "2017M02", "2017M03")))

  out <-
    merge(out, orgs[, .(Delivery, timestamp, uuid)], by = c("uuid", "timestamp"))
  expect_s3_class(out, "data.table")
  expect_equal(as.factor(names(out)), as.factor(
    c(
      "uuid",
      "timestamp",
      "sw_flag",
      "variable",
      "value",
      "Delivery"
    )
  ))

  context("Testing CleanChunk...\n")

  out <- CleanChunk(out)
  test2 <- out[uuid %in% TAP_env$demog_helper$uuid]
  test2[!(uuid %in% TAP_env$unclean)]
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), nrow(test2))


  context("Testing AddFirmographics...\n")

  out <- AddFirmographics(out)
  expect_s3_class(out, "data.table")
  expect_equal(as.factor(names(out)), as.factor(
    c(
      "uuid",
      "timestamp",
      "Country",
      "Segment_Code",
      "Vertical" ,
      "sw_flag",
      "Delivery",
      "clean",
      "variable",
      "value"
    )
  ))

  out <- FilterChunk(out, Filter = Segment_Code != "LSB")


  context("Testing ApplyWeightCalc...\n")

  out <- ApplyWeightCalc(out)
  expect_s3_class(out, "data.table")
  expect_equal(as.factor(names(out)), as.factor(
    c(
      "uuid",
      "timestamp",
      "Country",
      "Segment_Code",
      "Vertical" ,
      "sw_flag",
      "Delivery",
      "clean",
      "variable",
      "value",
      "gsv",
      "WClean",
      "weight"
    )
  ))


  out <- AddHierarchy(out, "GSP", "Org Geo, Org Segment, Product")
  expect_s3_class(out, "data.table")
  expect_equal(as.factor(names(out)), as.factor(
    c(
      "uuid",
      "timestamp",
      "Area",
      "Region",
      "SubReg",
      "Subsidiary",
      "Country",
      "Sector",
      "SubSector",
      "Segment",
      "Segment_Code",
      "Vertical",
      "Product2",
      "Product1",
      "Product0",
      "sw_flag",
      "Delivery",
      "gsv",
      "WClean",
      "weight",
      "clean",
      "variable",
      "value"
    )
  ))


  out   <- CalculateColumn(out, Calculation = Brand := Product2)
  out   <- CalculateColumn(out,
                           Filter = "Microsoft" %matches% Product2,
                           Calculation = Brand := "Exchange Net")


  Sample   <-
    ShowSampleSize(out,
                   By = c("Area", "Region", "SubReg", "Sector", "SubSector", "Segment"))
  expect_s3_class(Sample, "data.table")
  expect_equal(as.factor(names(Sample)), as.factor(
    c(
      "timestamp",
      "Area",
      "Region",
      "SubReg",
      "Sector",
      "SubSector",
      "Segment",
      "weighted_sample",
      "weighted_total",
      "weighted_pc",
      "unweighted_sample",
      "unweighted_total",
      "unweighted_pc"
    )
  ))


  Delivery <-
    ShowSampleSize(out,
                   By = c(
                     "Area",
                     "Region",
                     "SubReg",
                     "Sector",
                     "SubSector",
                     "Segment",
                     "Delivery"
                   ))
  expect_s3_class(Delivery, "data.table")
  expect_equal(as.factor(names(Delivery)), as.factor(
    c(
      "timestamp",
      "Area",
      "Region",
      "SubReg",
      "Sector",
      "SubSector",
      "Segment",
      "Delivery",
      "weighted_sample",
      "weighted_total",
      "weighted_pc",
      "unweighted_sample",
      "unweighted_total",
      "unweighted_pc"
    )
  ))


  Brand    <-
    ShowSampleSize(
      out,
      By = c(
        "Area",
        "Region",
        "SubReg",
        "Sector",
        "SubSector",
        "Segment",
        "Delivery",
        "Brand"
      )
    )
  expect_s3_class(Brand, "data.table")
  expect_equal(as.factor(names(Brand)), as.factor(
    c(
      "timestamp",
      "Area",
      "Region",
      "SubReg",
      "Sector",
      "SubSector",
      "Segment",
      "Delivery",
      "Brand",
      "weighted_sample",
      "weighted_total",
      "weighted_pc",
      "unweighted_sample",
      "unweighted_total",
      "unweighted_pc"
    )
  ))

  context("Testing ApplyPenetration...\n")

  Product0 <- ApplyPenetration(
    Data = out,
    Dimension = c(
      "Area",
      "Region",
      "SubReg",
      "Sector",
      "SubSector",
      "Segment",
      "Delivery"
    ),
    PenetrationDimension = c("Product2", "Product1", "Product0")
  )

  expect_s3_class(Product0, "data.table")
  expect_equal(as.factor(names(Product0)), as.factor(
    c(
      "Segment",
      "SubReg",
      "Delivery",
      "timestamp",
      "Product0",
      "Product1",
      "Product2",
      "Region",
      "Area",
      "SubSector",
      "Sector",
      "weighted_sample_N",
      "weighted_total_N",
      "weighted_pc_N",
      "unweighted_sample_N",
      "unweighted_total_N",
      "unweighted_pc_N",
      "weighted_sample_D",
      "weighted_total_D",
      "weighted_pc_D",
      "unweighted_sample_D",
      "unweighted_total_D",
      "unweighted_pc_D"
    )
  ))



  Product1 <- ApplyPenetration(
    Data = out,
    Dimension = c(
      "Area",
      "Region",
      "SubReg",
      "Sector",
      "SubSector",
      "Segment",
      "Delivery"
    ),
    PenetrationDimension = c("Product2", "Product1")
  )
  expect_s3_class(Product1, "data.table")
  expect_equal(as.factor(names(Product1)), as.factor(
    c(
      "Segment",
      "SubReg",
      "Delivery",
      "timestamp",
      "Product1",
      "Product2",
      "Region",
      "Area",
      "SubSector",
      "Sector",
      "weighted_sample_N",
      "weighted_total_N",
      "weighted_pc_N",
      "unweighted_sample_N",
      "unweighted_total_N",
      "unweighted_pc_N",
      "weighted_sample_D",
      "weighted_total_D",
      "weighted_pc_D",
      "unweighted_sample_D",
      "unweighted_total_D",
      "unweighted_pc_D"
    )
  ))


  Product2 <- ApplyPenetration(
    Data = out,
    Dimension = c(
      "Area",
      "Region",
      "SubReg",
      "Sector",
      "SubSector",
      "Segment",
      "Delivery"
    ),
    PenetrationDimension = c("Product2")
  )

  expect_s3_class(Product2, "data.table")
  expect_equal(as.factor(names(Product2)), as.factor(
    c(
      "Segment",
      "SubReg",
      "Delivery",
      "timestamp",
      "Product2",
      "Region",
      "Area",
      "SubSector",
      "Sector",
      "weighted_sample_N",
      "weighted_total_N",
      "weighted_pc_N",
      "unweighted_sample_N",
      "unweighted_total_N",
      "unweighted_pc_N",
      "weighted_sample_D",
      "weighted_total_D",
      "weighted_pc_D",
      "unweighted_sample_D",
      "unweighted_total_D",
      "unweighted_pc_D"
    )
  ))

})
