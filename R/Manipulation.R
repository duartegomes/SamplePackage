#' @title Stripping Spiceworks chunks of unclean records
#'
#' @description When we load Spiceworks demographics data we allocate taxonomy data
#' for Geography, Segment and Vertical.  When an org identifier (UUID) has all three
#' of these it is marked as being clean.  This function allows us to easily remove
#' all the orgs that are not clean from a data set.
#'
#' @details
#' The function has a dependency in that the demographics data base needs to be loaded
#' to the R environment. A user may check this by running CheckDemogDB().  The demographics
#' data base is established by running LoadDemogDB ().
#'
#' When this function is called it first validates that it has been sent a valid chunk.
#' Next it uses the uuid column to locate all rows in the input file that have an org identifier
#' that is listed as not being clean in the demographics database.  These rows are
#' filtered from the data before it is returned.
#' @examples  email <- CleanChunk(TestEmailChunk)
#' @author Jonathan Tooley Associados Lda
#' @param Data Chunk to be cleaned
#' @return Returns a data table containing only the rows in the original file that are
#' associated with clean organizations. If the routine was not able to run then the
#' original data set is returned unaltered.
#' @export
#' @family Weighting and cleaning functions
#' @seealso \code{\link{TAPChunks}}
# Backlog #126
CleanChunk <- function(Data) {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  if (CheckDemogDB()){

    if (ValidateChunk(Data)$valid) {

      Data   <- subset(Data, uuid %in% TAP_env$demog_helper$uuid) #remove uuid not available in Demogs
      Data   <- subset(Data, !(uuid %in% TAP_env$unclean))
      Data   <- droplevels(Data)
      return(Data)
    } else
      return(Data)
  } else
    return(Data)
}

#' @title Reviewing a weight calculation
#' @description
#' This function shows the weight calculation resulting from the submitted data.
#' @details
#' The weight calculation is designed to alter the distribution of the sample to match
#' the distribution of the population data that we get from the Market Model.  The size of the
#' unweighted sample is normally unchanged by the operation.  If the user sets the scale parameter
#' this default weighting style may be changed so that the weighted sample size becomes equal to
#' the population.\cr\cr
#' The calculation is performed over a variety of dimensions, with the default being GSV
#' (by all combinations of Geography, Segment and Vertical).  Using the Granularity parameter
#' this may be changed to any combination of G S and V.\cr
#'
#' @section Example Weighting Calculation:
#' Once the Granularity parameter is set there will be a weight for each of the distinct
#' values.  In this example we assume Granularity = "S" which results in 9 different categories.
#' If you wish to repeat this example you may run the command shown in the examples section. \cr
#' \strong{Step 1: Separate Unclean Orgs} \cr
#' Unclean orgs will not be counted and so Target Orgs, Target Distribution, Expected Orgs and the final
#' Weight will all be zero. \cr
#' \strong{Step 2: Count the orgs in the sample} \cr
#' The sample submitted to the function via the data parameter is counted to give the number of
#' orgs in each category or \emph{Sample Orgs}. \cr
#' \strong{Step 3: Count the orgs in the population} \cr
#' The population from the market model is counted to give orgs in each category or \emph{Population Orgs} \cr
#' \strong{Step 4: Establish target orgs} \cr
#' We establish targets \emph{Target Orgs}.  These are usually equal to the population except where our sample is zero
#' in which case it is also zero.  If we don't make this adjustment the total weighted sample will
#' not equal the total unweighted sample because there is no possible weight that can scale a zero to a value. \cr
#' \strong{Step 5: Express the \emph{Target Orgs} as ratios that sum to unity} \cr
#' The \emph{Target Distribution} is the value of \emph{Target Orgs} divided by the total \emph{Target Orgs}. \cr
#' \strong{Step 6: Calculate the number of orgs expected to give the population distribution} \cr
#' The expected sample size \emph{Expected Orgs} that will respect the proportions in the population are
#' calculated by multiplying the number of clean \emph{Sample Orgs} by the \emph{Target Distribution}. \cr
#' \strong{Step 7: Calculate the weight} \cr
#' The system calculates two types of weight:
#' The \emph{Shape Weight} is the \emph{Expected Orgs} divided by the \emph{Sample Orgs}.
#' The \emph{Scale Weight} is the \emph{Target Orgs}   divided by the \emph{Sample Orgs}.
#' If the sample orgs is zero then the either weight is also zero. \cr
#'
#' @section Example Calculation:
#' \tabular{llrrrrrrr}{
#' Segment \tab Clean \tab Sample Orgs \tab Population Orgs \tab Target Orgs \tab Target Distribution \tab Expected Orgs \tab Scale Weight \tab Shape Weight\cr
#' LSB \tab Y \tab 87 \tab 44,289,935 \tab  44,289,935 \tab  0.73233\tab  298.061\tab 509,079 \tab  3.4344\cr
#' CSB \tab Y \tab 80 \tab 10,273,280 \tab  10,273,280 \tab  0.16986\tab   69.136\tab 128,416 \tab  0.8663\cr
#' LMM \tab Y \tab 72 \tab    841,880 \tab     841,880 \tab  0.01392\tab    5.665\tab  11,692 \tab  0.0788\cr
#' CMM \tab Y \tab 85 \tab    573,335 \tab     573,335 \tab  0.00948\tab    3.858\tab   6,745 \tab  0.0455\cr
#' UMM \tab Y \tab 13 \tab    199,535 \tab     199,535 \tab  0.00329\tab    1.342\tab  15,348 \tab  0.1035\cr
#' ENT \tab Y \tab  2 \tab    111,200 \tab     111,200 \tab  0.00183\tab    0.748\tab  55,600 \tab  0.3750\cr
#' Edu \tab Y \tab 41 \tab  3,011,555 \tab   3,011,555 \tab  0.04979\tab   20.267\tab  73,452 \tab  0.4955\cr
#' Gov \tab Y \tab 28 \tab  1,176,720 \tab   1,176,720 \tab  0.01945\tab    7.919\tab  42,025 \tab  0.2835\cr
#' NoPC\tab Y \tab  0 \tab 54,034,370 \tab           0 \tab  0.00000\tab    0.000\tab       0 \tab  0.0000\cr
#' }
#' @export
#' @param Data Chunk to be weighted
#' @param Granularity Dimensions to weight. Default being GSV (by all combinations of Geography, Segment and Vertical)
#' @examples ShowWeightCalc(TestEmailChunk, Granularity = "S")
#' @author Jonathan Tooley Associados Lda
#' @family Weighting and cleaning functions
#' @return Returns a data table with the calculation detailed.
#' @seealso \code{\link{TAPChunks}}
# Backlog #93
ShowWeightCalc <- function(Data, Granularity = "gsv") {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  if (CheckDemogDB()){
    # Define the granularity for weighting
    Weight_levl <- c()

    choices <-
      grep(
        paste0("[", Granularity, "]"),
        c("G", "S", "V"),
        ignore.case = T,
        value = T
      )
    if (length(choices) != nchar(Granularity))
      catInfo("Granularity parameter should only contain the characters G, S or V.")

    if (length(intersect(choices, "G")) == 1) {
      Weight_levl <- c(Weight_levl, "WCountry")
    }
    if (length(intersect(choices, "S")) == 1) {
      Weight_levl <- c(Weight_levl, "Segment")
    }
    if (length(intersect(choices, "V")) == 1) {
      Weight_levl <- c(Weight_levl, "Vertical")
    }

    if (length(Weight_levl) != 0){

      if (length(unique(Data$timestamp)) > 1) {
        choice <-  min(as.character(unique(Data$timestamp)))
        catInfo("The data submitted has more than one timestamp.")
        catInfo("Weights are calculated independently for each timestamp.")
        catInfo("For the purposes of this illustration we have chosen the first.")
        catInfo(paste("period in your data:", choice))
        Data <- Data[timestamp == choice]
      }

      # Get the set of uuid's from the data sent to us
      sample_set  <- unique(Data$uuid)

      sample_meta <-
        TAP_env$demog_helper  [uuid %in% sample_set, c("uuid",
                                                       "WeightingChoice_Code",
                                                       "Dim_OrgSeg_3Segment_Code",
                                                       "Sector_Code",
                                                       "clean"), with = F]

      sample_meta <-
        sample_meta[,
                    .(uuid, WCountry = WeightingChoice_Code,
                      Segment = Dim_OrgSeg_3Segment_Code, Vertical = Sector_Code, clean)]

      sample_dist <-
        sample_meta   [,
                       .(`Sample Orgs` = data.table::uniqueN(uuid)),
                       by = c(Weight_levl, "clean")]

      target      <- TAP_env$target_helper[,
                                           .(Vertical = vertical_sector, Segment = segment_code,
                                             WCountry = w_country, entity_count)]

      populn_dist <- target [, .(`Population Orgs` = sum(entity_count),
                                 clean = "Y"), by = Weight_levl]

      Weight_Calc <-
        merge(
          sample_dist,
          populn_dist,
          all = T,
          by = c(Weight_levl, "clean")
        )

      Weight_Calc[is.na(`Sample Orgs`), `Sample Orgs` := 0]
      total_clean_sample <- Weight_Calc[clean == "Y", sum(`Sample Orgs`, na.rm = T)]
      Weight_Calc[is.na(`Population Orgs`), `Population Orgs` := 0]
      Weight_Calc[, `Target Orgs`         := ifelse(`Sample Orgs` == 0, 0, `Population Orgs`)]
      Weight_Calc[, `Target Distribution` := `Target Orgs` / sum(`Target Orgs`, na.rm = T)]
      Weight_Calc[, `Expected Orgs` := `Target Distribution` * total_clean_sample]
      Weight_Calc[, `Shape Weight`  := ifelse(`Sample Orgs` == 0, 0, `Expected Orgs` / `Sample Orgs`)]
      Weight_Calc[, `Scale Weight`  := ifelse(`Sample Orgs` == 0, 0, `Target Orgs`   / `Sample Orgs`)]

      data.table::setkeyv(Weight_Calc, c("clean", Weight_levl))

      Weight_Calc[clean == "N", c(Weight_levl) := "***"]
      Weight_Calc <- Weight_Calc[, .(
        `Sample Orgs`         = sum(`Sample Orgs`),
        `Population Orgs`     = sum(`Population Orgs`),
        `Target Orgs`         = sum(`Target Orgs`),
        `Target Distribution` = sum(`Target Distribution`),
        `Expected Orgs`       = sum(`Expected Orgs`),
        `Scale Weight`        = sum(`Scale Weight`),
        `Shape Weight`        = sum(`Shape Weight`)
      ),
      by = c(Weight_levl, "clean")]

      new   <- gsub("segment_code", "Segment",
                    gsub("vertical_sector", "Vertical",
                         gsub("WCountry", "Country", Weight_levl)))

      data.table::setnames(Weight_Calc, old = Weight_levl, new = new)

      return (Weight_Calc[`Sample Orgs` != 0])
    }
    catInfo("No granularity selected.")
  }
  catInfo("This function requires both demogs and targets to be loaded to the environment.")
}

#' @title Reviewing a weight calculation
#' @details DEPRECATED: See \code{\link{ShowWeightCalc}}
#' @export
#' @keywords internal
WeightCalculation <- function(Data, Granularity = "gsv") {
  catWarning("This function has been renamed to ShowWeightCalc and is deprecated.")
  ShowWeightCalc(Data, Granularity)
}

#' @title Joining two data chunks to form one
#' @description
#' This function joins two chunks of data together to form one appended table.
#' @details
#' This function performs a full join of two data chunks by appending the two chunks
#' together. The resulting data chunk has all the data from both submitted chunks and
#' so is a full outer join. If the two submitted chunks have the same column format then
#' the resulting chunk will also retain this format. If either of the submitted chunks
#' has a column not present in the other then this column will be retained in the output
#' but.  In order to do this the missing column is created in the input chunk that doesn't
#' have the column.  This created chunk is filled with the value NA.\cr\cr
#' The data is not reweighted after join.
#'
#' @family Chunk Manipulators
#' @param i A data chunk
#' @param j A data chunk
#' @export
#' @author Jonathan Tooley Associados Lda
#' @return Returns a data table with the two submitted tables appended together
#' @seealso \code{\link{TAPChunks}}
#'
JoinChunks <- function(...) {
  tables <- list(...)
  names(tables) <- match.call(expand.dots = FALSE)$...

  if (length(tables)!=2)
    catError("Only works with two tables.")

  if (class(tables[[1]])[1] != "data.table" |
      class(tables[[2]])[1] != "data.table")
    catError("Parameters must be tables.")

  missing.columns.2 <-
    setdiff(names(tables[[1]]), names(tables[[2]]))
  missing.columns.1 <-
    setdiff(names(tables[[2]]), names(tables[[1]]))

  if (length(missing.columns.1) == 0 & length(missing.columns.2) == 0) {
    data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)
  } else{
    catInfo("Unmatched Columns:")
    if (length(missing.columns.1) != 0 & length(missing.columns.2) == 0)
      catInfo(
        sprintf(
          "%s: %s\n", names(tables)[1], paste(missing.columns.1, collapse = ", ")
        )
      )
    if (length(missing.columns.1) == 0 & length(missing.columns.2) != 0)
      catInfo(
        sprintf(
          "%s: %s\n", names(tables)[2], paste(missing.columns.2, collapse = ", ")
        )
      )
    if (length(missing.columns.1) != 0 & length(missing.columns.2) != 0) {
      catInfo(
        sprintf(
          "%s: %s\n", names(tables)[1], paste(missing.columns.1, collapse = ", ")
        )
      )
      catInfo(
        sprintf(
          "%s: %s\n", names(tables)[2], paste(missing.columns.2, collapse = ", ")
        )
      )
    }
    stop()
  }
}

#' @title Matching text patterns to data in chunks
#' @description
#' This is a function that can match a string pattern to data in a chunk
#' @details
#' A feature that is typical to SpiceWorks chunks is a series of variables that
#' we wish to group together into one. To see an example of this consider the various
#' variables that are used to describe versions of Microsoft Exchange: \cr\cr
#' exchange_other \cr
#' exchange_2003 \cr
#' exchange_2007 \cr
#' exchange_2010 \cr
#' exchange_2000 \cr
#' exchange_2013 and \cr
#' exchange_2007_685.25 \cr\cr
#' This function allows the user to easily select all of these by matching these
#' variables to a string pattern. For example \emph{"Exchange" \%matches\% variable} is a filter condition
#' that will extract all rows where the string \emph{Exchange} occurs in the variable.  The test is not case
#' sensitive.
#'
#' @param Pattern A string that contains a regular expression.
#' @param Y A string or a vector of strings.
#' @examples "Exchange" %matches% TestEmailChunk$variable
#' @export
#' @author Jonathan Tooley Associados lda
#' @seealso \code{\link{TAPChunks}}

`%matches%`  <- function(Pattern, Y) {
  if (length(Pattern) == 1)
    grepl(Pattern, Y, ignore.case = TRUE)
}



#' @title Aggregate chunks to create smaller chunks
#' @description Aggregate chunks into a smaller chunks by the metadata selected by the user. (Review)
#' @export
#' @param Data Chunk to be aggregated
#' @param Filter  Filter to be applyied in the chunk
#' @param Aggregation Aggregate function and column to be summarized.
#' @param By columns where the chunk will be grouped by.
#' @param ... Error handelling
#' @author Jonathan Tooley Associados Lda
#' @family Chunk Manipulators
#' @seealso \code{\link{TAPChunks}}
# Backlog #93
AggregateChunk  <- function(Data, Filter = 1 == 1, Aggregation = .N, By = NULL, ...){

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  f1  <- substitute(Filter)
  if (typeof (f1) != "language")
    catError("Syntax error in filter definition.")

  c1  <- substitute(Aggregation)
  if (typeof (c1) != "language" | grepl(":=", deparse(c1)))
    catError("Syntax error in aggregation definition.")

  e <- try(tb <- typeof(By), silent = T)
  e <- strsplit(e, ":")[[1]][2]
  e <- gsub("\n", "", e)
  if (!exists("tb")) catError(paste0("Syntax error in the group by definition:", e, "."))
  if (!(tb == "character" | tb == "NULL")) catError("Syntax error in the group by definition.")

  By <- gsub(" ", "", By)

  e <-  try({
    ret  <- Data[eval(f1), eval(c1), By][]
    ret  <- droplevels(ret)
    return(ret)
  }
  , silent = T)

  e <- strsplit(e, ":")[[1]][2]
  e <- gsub("\n", "", e)

  catError(paste0("Syntax error:", e, "."))
}

#' @title Filtering data chunks
#' @description
#' This function allows users to apply a filter to a data chunk to receive
#' a subset of the input data.
#' @family Chunk Manipulators
#' @import data.table
#' @export
#' @author Jonathan Tooley Associados Lda
#' @param Data A data chunk
#' @param Filter A string defining the filter condition
#' @param ... (TODO)
#' @examples FilterChunk(TestEmailChunk, filter = "variable = 'exchange_2010'")
#' @seealso \code{\link{TAPChunks}}
# Backlog #93
FilterChunk  <- function(Data, Filter = 1 == 1, ...){

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  f1  <- substitute(Filter)
  if (typeof (f1) != "language") catError("Syntax error in filter definition.")
  if (deparse(f1) == "1 == 1")   catError("Missing or incomplete filter definition")
  e <-  try({
    ret <- Data[eval(f1)]
    ret <- droplevels(ret)
    return(ret)
  }
  , silent = T)

  e <- strsplit(e, ":")[[1]][2]

  catError(e)
}

#' @title Add Product hierarchy to a data chunk
#' @description This function allows you to add a Product hierarchy to the chunk.
#' @section Product Hierarchy:
#' The raw data that we receive from SpiceWorks contains a great deal of product
#' information.  This is typically shown in the many product columns that we see in a typical
#' raw file and that gets transposed when written as a chunk.
#' The product descriptions are detailed and so to summarize our finindings it is necessary
#' to add a product hierarchy.
#' @section Hierarchies available:
#' \tabular{llll}{
#' \strong{Hierarchy Name}   \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3}    \cr
#' \strong{Product (default)}          \tab Product0        \tab Product1        \tab Product2\cr
#' \strong{Product Type}     \tab Product0        \tab ProductType     \cr
#' \strong{Product Category} \tab Product0        \tab ProductCategory}
#' @family RDR Integration Tools
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @export
#' @import data.table
#' @inheritParams AddGeoHierarchy
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @examples AddProductHierarchy(TestEmailChunk, "ProductType")
#' @seealso \code{\link{TAPChunks}}
#' Issue #135
AddProductHierarchy  <- function(Data, SelectHierarchy = NULL){

  if(is.null(SelectHierarchy)) {
    SelectHierarchy = "Product"
  }
  AddDimension(Data = Data, Dimension = "P", SelectHierarchy)
}

#' @title Add geography hierarchy to a data chunk
#' @description This function allows you to add a Geo hierarchy to the chunk.
#' @section Geo Hierarchy:
#' The function accesses the RDR and retrieves the standard geography
#' hierarchy that is matched to the country names that the file has.
#' @section Hierarchies available:
#' \tabular{llllll}{
#' \strong{Hierarchy Name} \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3} \tab \strong{Level4} \tab \strong{Level5} \cr
#' \strong{Org Geo (default)}        \tab Country         \tab Subsidiary      \tab SubReg          \tab Region          \tab Area \cr
#' \strong{DSST Geo}       \tab DSST Country    \tab Area }
#' @section Columns fields description:
#' \tabular{ll}{
#' \strong{Area}   \tab The MS Sales area for the country\cr
#' \strong{Region} \tab The MS Sales region\cr
#' \strong{SubReg} \tab The MS Sales sub-region\cr
#' \strong{Subsidiary} \tab The MS Sales subsidiary\cr
#' \strong{Country} \tab The country value attributed by the data provider}
#'
#' @section Reference Data Repository:
#' The Reference Data Repository (RDR) allows stores master data relating to
#' our taxonomy.  It can be used to add taxonomy hierarchies to chunks that have had
#' firmographic data added (See \code{\link{AddFirmographics}}). Firmographic data is
#' established by our data load processes (which refer to the RDR to allocate firmographics)
#' and is stored in a separate database.
#'
#' Typically functions using the RDR will try and use firmographic data in the chunk
#' submitted but if they can't detect firmographic data they will attempt to add this by calling
#' AddFirmographics for you.
#'
#' Functions that access the RDR require an internet connection which has access to
#' MS Corp net.
#'
#' @family RDR Integration Tools
#' @export
#' @import data.table
#' @author Jonathan Tooley Associados Lda
#' @param Data This is a required field and is usually a TAP chunk but it can be any item of class "data.table" or "data.frame".
#' If the user submits a data frame then this will first be converted to a data table before the calculated column is added.
#' This is because data tables are far more efficient in R than a data frame.
#' The resulting table that is returned by this function will also be a data table even if a data frame was submitted.
#' @param SelectHierarchy Name of the Hierarchy to be added in the Chunk
#' @keywords internal
#' @return A data chunk with the additional hierarchy columns added
#' @examples AddGeoHierarchy(TestEmailChunk)
#' @seealso \code{\link{TAPChunks}}
#' Issue #
AddGeoHierarchy  <- function(Data, SelectHierarchy = NULL){
  if( !"Country" %in% names(Data)) Data <- AddFirmographics(Data, Dimension = "G" )

  if(is.null(SelectHierarchy)){SelectHierarchy = "Org Geo"}
  Data <- AddDimension(Data = Data, Dimension = "G", SelectHierarchy = SelectHierarchy)
  return(Data)

}

#' @title Add Segment hierarchy to a data chunk
#' @description This function allows you to add a Segment hierarchy to the chunk.
#' @section Segment Hierarchy:
#' The function accesses the RDR and retrieves the standard geography
#' hierarchy that is matched to the segment names that the file has.
#' @section Hierarchy available:
#' \tabular{llll}{
#' \strong{Hierarchy Name}   \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3} \tab \strong{Level4}\cr
#' \strong{Org Segment (default)}      \tab Sector          \tab SubSector       \tab Segment         \tab Segment_Code}
#' @section Columns fields description:
#' \tabular{ll}{
#' \strong{Sector}      \tab Consumer, Commercial or Public\cr
#' \strong{SubSector}   \tab An aggregation of the segment\cr
#' \strong{Segment}     \tab The segment name\cr
#' \strong{Segment_Code}\tab A three character mnemonic}
#' @section AddGeoHierarchy Reference Data Repository
#' @inheritParams AddGeoHierarchy
#' @family RDR Integration Tools
#' @export
#' @import data.table
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @examples AddSegmentHierarchy(TestEmailChunk)
#' @seealso \code{\link{TAPChunks}}
#' Issue #
AddSegmentHierarchy  <- function(Data, SelectHierarchy = NULL){
  if( !"Segment_Code" %in% names(Data)) Data <- AddFirmographics(Data, Dimension = "S" )

  if(is.null(SelectHierarchy)){SelectHierarchy = "OrgSegment"}
  AddDimension(Data = Data, Dimension = "S", SelectHierarchy = SelectHierarchy)
}

#' @title RMarkdown
#' @description
#' Create a html file that is saved in ShowCachePath()
#' @details TODO
#' @family Chunk Manipulators
#' @param Source A data chunk
#' @param MappingTable The column name to map. If none, it has as default 'device_os' or 'os_name'
#' @param Format 'Full' to show all OS mappings | 'Unknown' to show only OS that are unknown | 'NULL' to not create a report.
#' @export
#' @author Jonathan Tooley Associados Lda
#' @return Returns a html file that is saved in ShowCachePath()
#' @seealso \code{\link{TAPChunks}}
#' Issue #
RMarkdown <- function(Source, MappingTable = source.os, Format, FileName){

  if(!missing(Format)){

    timestamp     <- Source[, unique(timestamp)]
    source.null   <- MappingTable[, !c("os", "N"), with = F]
    source.os.UNK <- source.null[OS == 'Unknown']
    objsave       <- c("source.null", "source.os.UNK", "timestamp")
    Path          <- ShowCachePath()

    if (!file.exists(file.path(Path, "Logs"))) {

      dir.create(file.path(Path, "Logs"))
    }

    if (!file.exists(file.path(Path, "Logs/OSMappings"))) {

      dir.create(file.path(Path, "Logs/OSMappings"))
    }

    save(list = objsave, file = paste0(Path, "/Logs/OSMappings/OSMapping.Rdata"))

    Format <- tolower(Format)

    if( Format == "all"){

      suppressMessages(capture.output(rmarkdown::render("LogsDefinition/OSMapping.Rmd", output_dir = paste0(Path, "/Logs/OSMappings"), output_file = paste0("OSMapping", FileName, ".html"))))

      catInfo(paste("Log page is available here:", paste0(Path, "/Logs/OSMappings/", "OSMapping", FileName, ".html")))}

    if( Format == "unknown"){

      suppressMessages(capture.output(rmarkdown::render("LogsDefinition/OSMappingUnknown.Rmd", output_dir = paste0(Path, "/Logs/OSMappings"), output_file = paste0("OSMappingUnknown", FileName, ".html"))))

      catInfo(paste("Log page is available here: ", paste0(Path, "/Logs/OSMappings/", "OSMappingUnknown", FileName, ".html")))}

  }
}


#' @title Add OS Mappings to a data chunk
#' @description
#' This function reads Operating System mappings from the RDR. The RDR typically
#' stores text patterns which are used to match to the os field in the raw data.
#'
#' This makes it easy for the user to deal with inconsistencies and to get a simple
#' and consistent hierarchy for OS.
#' This function is mainly used for Workload and PCclient data.
#' @section OS Hierarchy:
#' The function will read the OS name from the data chunk and will match to the pattern
#' that is stored in the RDR.  After replacing all the names matched it will show the
#' percentage of OS values that were not able to be recognised. The original column
#' denoting operating system is not returned and the following columns are added:
#' \tabular{ll}{
#' \strong{Platform} \tab Server or Client \cr
#' \strong{Architecture} \tab \cr
#' \strong{Ecosystem} \tab \cr
#' \strong{OS} \tab }
#' @family Chunk Manipulators
#' @section AddGeoHierarchy Reference Data Repository
#' @family RDR Integration Tools
#' @inheritParams AddGeoHierarchy
#' @param OSColumn The column name to be mapped. If none, it has as default 'device_os' or 'os_name'
#' @param Format 'Full' to show all OS mappings | 'Unknown' to show only OS that are unknown | 'NULL' to not create a report.
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @export
#' @examples AddOSHierarchy(TestClientChunk)
#' @keywords internal
#' @author Jonathan Tooley Associados Lda
#' @return Returns a data table without the original column of the os and a new one named 'os.map'
#' @seealso \code{\link{TAPChunks}}
#' Issue #116
AddOSHierarchy <- function(Data, OSColumn, Format){

  FileName <- deparse(substitute(Data))

  if (missing(Data)){

    catError("Please input a valid Chunk")

  }
  if (missing(OSColumn)){

    OSColumn  <- names(Data)[match(c("os_name", "device_os"), names(Data), nomatch = F)]
    if (length(OSColumn) == 0) catError("Please select the OS column to map.")

    catInfo(sprintf("%s from %s will be mapped", OSColumn, deparse(substitute(Data))))

  }else{

    if (length(names(Data)[match(OSColumn, names(Data), nomatch = F)]) == 0){
      catError("Please select a valid os column to map.")
    }
  }

  Data     <- data.table::data.table(Data)
  Data.os  <- Data[,
                       .N,
                       by = OSColumn]
  data.table::setkeyv(Data.os, OSColumn)
  data.table::setkeyv(Data   , OSColumn)

  Data.os[, OSColumn := eval(parse(text = OSColumn))]

  #remove special characters
  Data.os[, OSColumn := gsub("[^0-9A-Za-z-]", " ", OSColumn)]
  Data.os[, OSColumn := gsub("\\s+"         , " ", OSColumn)]

  #creating a higher hierarchy level to the os
  Data.os[, OS := "NA"]

  os.mapping <- GetMapping("O")

  #Pattern Matching of os
  for (i in 1: nrow(os.mapping)){

    Data.os[OSColumn %in% grep(os.mapping[i]$MatchPattern,
                           OSColumn,
                           ignore.case = T,
                           value = T)
              & OS == "NA",
              OS := os.mapping[i]$OS]
  }


  Data.os[OS == "NA", OS := "Unknown"]

  if(!missing(Format)){

    Format <- tolower(Format)

    if( Format == "unknown" | Format == "all"){

      RMarkdown(Data, Data.os, Format, FileName)}

    Path   <- ShowCachePath()

    cache  <-  data.table(list.files(paste0(Path, "/Logs/OSMappings")))[, grep(".Rdata", V1, value = TRUE)]

    unlink(paste0(Path, "/Logs/OSMappings","/", cache))}



  mapped    <- Data.os[OS != "Unknown", sum(N)]
  total     <- Data.os[, sum(N)]
  mapped    <- round(mapped / total * 100, 2)

  unmapped  <- Data.os[OS == "Unknown", data.table::uniqueN(OSColumn)]

  catInfo(sprintf("%s%% OSs were mapped.\nThere are %s of Unknown OSs",
                  mapped, unmapped))

  Data  <- Data[Data.os]
  Data  <- Data[,
                    !c("os", OSColumn, "N"),
                    with = F]

  os.mapping     <- os.mapping[, !"MatchPattern"]
  os.mapping     <- unique(os.mapping[OS %in% Data[, unique(OS)]])
  Data         <- merge(Data, os.mapping, by = "OS", all.x = T)

  Data <- Data[,
                   OrderColumn(Data),
                   with = F]

  return(Data)
}

#' @title Shape chunk - Pivot and Unpivot
#' @description
#' This function will reshape a data table, it will pivot a variable to several columns.
#' @details
#' This function takes a data table, a variable and value to reshape. The unique values
#' of the variable will become new columns and the value will fill those columns. NAs will
#' be replaced by 0.
#' @family Chunk Manipulators
#' @param Data A data table
#' @param Variable The column name to reshape
#' @param Value The column name to fill the columns
#' @param Horizontal Boolean: TRUE as default
#' @export
#' @author Jonathan Tooley Associados Lda
#' @return Returns a data table with shaped as user requested
#' @seealso \code{\link{TAPChunks}}
#' Issue #162
ShapeChunk <- function(Data, Variable = NULL, Value = NULL, Horizontal = T, Operation= "default"){

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  if (is.null(Variable)){

    catError("Please fill variable.")
  }

  if (length(intersect(Variable, names(Data))) == 0){

    catError("Please select a valid variable.")
  }

  if (length(intersect(Variable, names(Data))) >= 2 & Horizontal){

    Variable <- Variable[1]
    catWarning(sprintf("You must select only one variable, by default is select: %s", Variable))
  }

  if (is.null(Value) & Horizontal){

    catError("Please fill value.")
  }

  if(Horizontal){

    if (length(intersect(Variable, names(Data))) == 0){

      catError("Please select a valid value.")
    }

  }

  if (Horizontal){


    columns      <- setdiff(names(Data), c(Variable, Value))
    columns      <- paste(columns, collapse = "+")

    if(! Operation %in% c("Sum", "Count", "Average")) {
      catWarning("The Operation parameter is not valid.", "Operation")
      catWarning("The Operation availabe are:\n\t\t- Sum\n\t\t- Average\n\t\t- Count","Operation availabe")
      catWarning("For default, Sum was used to group the value"); Operation= "Sum"}

    if (Operation == "Sum") {
      shape.chunk  <- data.table::dcast.data.table(data.table::data.table(Data),
                                                   as.formula(paste(columns, "~", Variable)),
                                                   value.var = Value , fun.aggregate =sum)
    } else{
      if (Operation == "Average") {
        shape.chunk  <- data.table::dcast.data.table(data.table::data.table(Data),
                                                     as.formula(paste(columns, "~", Variable)),
                                                     value.var = Value , fun.aggregate = mean)
      } else{
        if (Operation == "Count")
          shape.chunk  <- data.table::dcast.data.table(data.table::data.table(Data),
                                                       as.formula(paste(columns, "~", Variable)),
                                                       value.var = Value , fun.aggregate = length)
      }
    }



  }else{

    columns      <- setdiff(names(Data), Variable)
    shape.chunk  <- data.table::melt.data.table(data.table::data.table(Data),
                                                id.vars      = columns,
                                                measure.vars = Variable)
  }

  shape.chunk[is.na(shape.chunk)]  <- 0

  return(shape.chunk)
}

#' @title Add Firmographics
#' @description A standard telemetry chunk contains an org identifier
#' called the uuid (Universally Unique Identifier).  As a separate process
#' we analize the uuids and attribute certain firmographics to them.  Examples include
#' the country name, segment and vertical market. These attributes are stored in a
#' separate database having all the demographic information.  This function
#' helps user to easily attach the firmographic attributes to a chunk.
#' @details
#' The function checks to see if the demographics database has been loaded
#' to the environment.If not it will first load the demographics database.
#'
#' The function joins the submitted chunk to the database using the uuid and returns the
#' original data with the following extra columns added:
#' \tabular{ll}{\strong{Column}\tab\strong{Description}\cr
#' \strong{Clean}   \tab The clean flag to show if the org has a full set of firmographics. \cr
#' \strong{Country} \tab The country name as defined by the source provider.\cr
#' \strong{Segment} \tab The segment as calculated by our load process (typically from PC and employee sizes).\cr
#' \strong{Vertical}\tab The vertical market as defined by SIC codes.\cr}
#' @family Chunk Manipulators
#' @import data.table
#' @export
#' @author Jonathan Tooley Associados Lda
#' @param Data A data chunk
#' @return A data chunk with additional columns
#' @examples AddFirmographics(TestServerRolesChunk)
#' @seealso \code{\link{TAPChunks}}
#' Issue #136
AddFirmographics <- function(Data, Dimension = "All") {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  if (TestNet() == F) {

    catError("Off-Line")
  }

  CheckDemogDB()

  catInfo (sprintf("Firmographics has been loaded: \n\t- Number of Organizations: %s",
                   nrow(TAP_env$demog_helper)))

  Firmographics <-
    TAP_env$demog_helper[,
                         .( uuid, Country = ParentCountry_Name,
                            Vertical = Sector_Code, Segment_Code = Dim_OrgSeg_3Segment_Code, clean)]


  if(Dimension != "All"){
    if(!(Dimension %in% c("G", "S", "V")) ) catError("The dimension is unknown")

    Firmographics <- switch(Dimension,
                            "G" = Firmographics[, .(uuid, Country, clean)],
                            "S" = Firmographics[, .(uuid, Segment_Code, clean)],
                            "V" = Firmographics[, .(uuid, Vertical, clean)])
  }

  cols <- setdiff(intersect(names(Data), names(Firmographics)), "uuid")

  if (length(cols) > 0){

    Data <- Data[, setdiff(names(Data), cols), with = F]
  }

  Data <- merge(Data, Firmographics, all.x = T, by = "uuid")

  if (length (Data[is.na(clean), uuid]) != 0) {

    numberRow <- length(Data[is.na(clean), uuid])
    catWarning(sprintf("There are %s orgs without firmographic info. They will be removed.", numberRow),
               numberRow)
    Data <- Data[!is.na(clean)]
  }

  Data <- Data[,
               OrderColumn(Data),
               with = F]

  return(Data)
}

#' @title Add Hierarchy
#' @description
#' This function allows adding all hierarchies at same time. The user can define which dimension wants to add to the dataset.
#' @family RDR Integration Tools
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @inheritSection AddGeoHierarchy Geo Hierarchy
#' @inheritSection AddSegmentHierarchy Segment Hierarchy
#' @inheritSection AddProductHierarchy Product Hierarchy
#' @import data.table
#' @export
#' @author Jonathan Tooley Associados Lda
#' @param Data A data chunk
#' @param Dimension Dimension to be added to the dataset. i.e "G" "S" "O" "P"
#' @param Hierarchy TODO
#' @seealso \code{\link{TAPChunks}}
AddHierarchy <- function(Data,
                         Dimension = NULL,
                         Hierarchy = NULL) {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  if(!is.null(Hierarchy)) Hierarchy <- gsub("\\s","",strsplit(Hierarchy,",")[[1]])
  Data <- AddFirmographics(Data)

  if (!is.null(Dimension)) {
    Dimension <- toupper(Dimension)
    order <- NULL
    for (i in 1:nchar(Dimension)) {
      order <- c(order, substr(Dimension, i, i))
    }
    if (length(unique(order)) != nchar(Dimension))
      catError("Invalid format:\n\tDimensions were repeated.")

    order <- gregexpr("G|S|P|O|V", Dimension)[[1]]
    if (length(order) != nchar(Dimension))
      catError("Invalid format:\n\tThere are unknown dimensions.")

    answer <- NULL
    for (i in c("G", "S", "O", "P")) {
      new.answer <-
        substr(Dimension,
               gregexpr(i, Dimension)[[1]],
               gregexpr(i, Dimension)[[1]])

      if (new.answer != "")
        answer <- c(answer, new.answer)
    }
  }

  if (is.null(Dimension)) {
    DimAvailable <- c("Geo", "Segment", "OS", "Product", "Vertical")
    Question <-
      paste(1:length(DimAvailable),
            DimAvailable,
            sep = "-",
            collapse = "\n\t")
    format_right <- F
    while (format_right == F) {
      answer <-
        readline(cat(
          paste0(
            "Choose Hierarchy: please, insert numbers separated by commas\n\t",
            paste0(Question, "\n")
          )
        ))
      if (is.na(sum(as.numeric(strsplit(answer, ",")[[1]])))) {
        catWarning("You should put number separate by comas.")
        format_right <- F
      } else {
        answer <- as.numeric(strsplit(answer, ",")[[1]])
        format_right <- T
      }
    }
    answer <- substr(DimAvailable[answer], 1, 1)
  }
  H <-
    unique(GetMapping("H")[, .(dim = substr(DimensionCategory, 1, 1), FriendlyName)])
  H <- H[dim %in% answer ]
  H$FriendlyName <- gsub("\\s", "", H$FriendlyName)
  Hierarchy <- gsub("\\s", "", Hierarchy)

  checkHierarchy <- setdiff(Hierarchy, H$FriendlyName)
  if(length(checkHierarchy) != 0) catWarning(sprintf("The following hierarchies weren't detected:\n\t\t-%s", paste(checkHierarchy, collapse = "\n\t\t-")))

  Hierarchy <- intersect(Hierarchy, H$FriendlyName)
  Hierarchy <- unique(Hierarchy)

  for (i in answer) {
    Data <- switch(
      i,
      # "Firmographics" = AddFirmographics(Data),
      "G"           = AddGeoHierarchy(Data,
                                      if (nrow(H[dim == "G" &
                                                 FriendlyName %in% Hierarchy]) == 0) {
                                        NULL
                                      } else{
                                        paste0(H[dim == "G" & FriendlyName %in% Hierarchy]$FriendlyName, collapse = ", ")
                                      })
      ,
      "S"           = AddSegmentHierarchy(Data,
                                          if (nrow(H[dim == "S" &
                                                     FriendlyName %in% Hierarchy]) == 0) {
                                            NULL
                                          } else{
                                            paste0(H[dim == "S" & FriendlyName %in% Hierarchy]$FriendlyName, collapse = ", ")
                                          }),
      "O"           = AddOSHierarchy(Data),
      "P"           = AddProductHierarchy(Data,
                                          if (nrow(H[dim == "P" &
                                                     FriendlyName %in% Hierarchy]) == 0) {
                                            NULL
                                          } else{
                                            paste0(H[dim == "P" & FriendlyName %in% Hierarchy]$FriendlyName, collapse = ", ")
                                          }),
      "V"           = Data
    )
  }
  Data <- Data[, OrderColumn(Data), with = F]

  return(Data)
}

#' @title  Merge Chunks Objects
#' @description This function merges 2 datasets/ chunks.
#' @examples MergeChunks(TestEmailChunk, TestClientChunk, by = "uuid, timestamp")
#' @param x,y Tables to be merged
#' @param ... by Column names in x and y to merge on or
#' by.x, by.y Vectors of column names in x and y to merge on
#' @param join "inner", "outer", "left" or "right"
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
# Backlog #180
MergeChunks <- function(x, y, ..., join = "inner") {

  if (!is.data.frame(x))
    catError(sprintf("\nError: %s is not a table", deparse(substitute(x))))
  if (!is.data.frame(y))
    catError(sprintf("\nError: %s is not a table", deparse(substitute(y))))

  suffixes.x <- paste0(".", deparse(substitute(x)))
  suffixes.y <- paste0(".", deparse(substitute(y)))

  parameters <- c("by.x", "by.y", "by")

  list.parameters <- list(...)
  if (length(setdiff(names(list.parameters), parameters)) != 0) {

    catError("\nThe parameters are not well defined.")
  }

  parameters.list <- NULL

  for (i in parameters) {

    parameters.list <- c(parameters.list, sum(i == names(list.parameters)))
  }

  joinlist <- c("inner", "outer", "left", "right")

  if(!(join %in% joinlist)) catError(sprintf("The join command needs to be one of the following %s", paste0(joinlist, collapse = ", ")))

  join <- switch (tolower(join),
                  "inner" = "inner",
                  "outer" = "all",
                  "left"  = "all.x",
                  "right" = "all.y")

  if (sum(parameters.list == c(1, 1, 0)) == 3) {

    key.x <- gsub("\\s", "", (list.parameters$by.x))
    key.x <- strsplit(key.x, ",")[[1]]

    if (length(setdiff(key.x, names(x))) != 0){

      catError("\nColumn name error.")
    }

    key.y <- gsub("\\s", "", (list.parameters$by.y))
    key.y <- strsplit(key.y, ",")[[1]]

    if (length(setdiff(key.y, names(y))) != 0) {

      catError("\nColumn name error.")
    }

    return (eval(parse(text = sprintf("merge(
      x, by.x = key.x,
      y, by.y = key.y,
      suffixes = c(suffixes.x, suffixes.y),
      allow.cartesian = T, %s = TRUE)", join))))

  } else{

    if (sum(parameters.list == c(0, 0, 1)) == 3) {

      key <- gsub("\\s", "", (list.parameters$by))
      key <- strsplit(key, ",")[[1]]

      if (length(setdiff(key, names(x))) != 0) {

        catError("\nColumn name error.")
      }

      if (length(setdiff(key, names(y))) != 0) {

        catError("\nColumn name error.")
      }

      return (eval(parse(text = sprintf("merge(
                                        x, y, by = key,
                                        suffixes = c(suffixes.x, suffixes.y),
                                        allow.cartesian = T, %s = TRUE)", join))))
    }else{
      if (sum(parameters.list == c(0, 0, 0)) == 3) {

        key <-
          intersect(names(x)[!sapply(x, is.integer)], names(y)[!sapply(y, is.integer)])
        key <- setdiff(key, "variable")

        if (length(key) == 0) {

          catError("There isn't connection between the tables.")
        }

        catInfo(sprintf("Default key is:\n\t-%s", paste(key, collapse = "\n\t-")))

        return (eval(parse(text = sprintf("merge(
                                        x, y, by = key,
                                        suffixes = c(suffixes.x, suffixes.y),
                                        allow.cartesian = T, %s = TRUE)", join))))

      }else {

        catError("\nEither select the 'by' or select 'by.x' and the 'by.y'.")
      }
    }
  }
}

#' @title  Select Columns in Chunk
#' @description This function allows to select which columns from the chunk to extract
#' @examples SelectColumns(Source = Chunk, Columns = c("uuid", "variable", "value"))
#' @param Source chunk
#' @param Columns List of name columns to extract
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
# Backlog #179
SelectColumns <- function(Source = NULL, Columns = NULL) {
  Columns <- gsub("\\s", "", Columns)

  if (is.null(Source)) {

    catError("\nPlease select a valid Source.")
  }

  if (is.null(Columns)) {

    catError("\nPlease select the list of columns to extract.")
  }

  if (length(intersect(names(Source), Columns)) == length(Columns)) {

    Source <- data.table::data.table(Source)
    chunk  <- Source[, Columns, with = F]
  }else{
    diff.columns  <- setdiff(Columns, names(Source))
    catError(sprintf("\n Column(s) not found: %s", diff.columns))
  }

  return(chunk)
}


#' @title  Save Objects
#' @description This function allows to save R environment objects into local machine. Formats available csv and rdata
#' @examples SaveLocal(TestClientChunk, TestEmailChunk, path = getwd());
#' SaveLocal("TestClientChunk", "TestEmailChunk", FileExtension = "rdata", path = getwd())
#' @param ... Objects to be saved
#' @param FileExtension A string with the required file extension. This can be one of two types, csv or rdata.
#' @param Path Destination of the files with information of the R session objects
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
SaveLocal <- function(..., FileExtension = "csv", Path = NULL) {

  FileExtension <- tolower(FileExtension)

  if (FileExtension != c("csv") & FileExtension != c("rdata")) {

    catError("The file extension is unknown.")
  }

  if (is.null(Path)) {

    Path <- ShowCachePath()
  }

  if (!file.exists(Path)) {

    catError("The path doesn't exist.")
  }

  list.obj         <- match.call(expand.dots = FALSE)$...
  list.data        <- list(...)
  names(list.data) <- file.path(Path, paste0(list.obj, paste0(".", FileExtension)))
  class_obj        <- sapply(list.data, class)

  for (i in 1:length(list.data)) {

    if (class_obj[i] == "character") {

      list.data[[i]] <- get(list.data[[i]])
    }
  }

  n <- 1
  for (file.name in names(list.data)) {

    if (FileExtension == "csv") {
      data.table::fwrite(list.data[[file.name]], file = file.name)
    }
    if (FileExtension == "rdata") {
      assign(paste(list.obj[[n]]), list.data[[file.name]])
      save(list = paste(list.obj[[n]]), file = file.name)
    }
    n <- n + 1
  }
  catInfo(paste("Saved successfully. Path:", Path))
}


#' @title Export to SQL
#' @description This function inserts a R table into SQL
#' @param Connection OpenSqlConnection(Connection, DataBase) TODO
#' @param InputData class = Data.frame | Data.table, table in r
#' @param DbSchema class = string, name of schema in SQL
#' @param DbTableName class = string, name for table in SQl
#' @param ServerPath path where the FMT and TSV file
#' @param Append default = FALSE, When Append = F it creates a new table. If not, adds new information to an exist table in SQL
#' @return None
#' @export
ExportToSQL <- function(Connection, InputData, DbSchema, DbTableName, ServerPath = ".", Append = FALSE){

  if (missing(Connection) || class(Connection) != "RODBC") {

    catError("Argument 'Connection' is missing, with no default or is not a RODBC Connection.")

  }else if (missing(InputData)) {

    catError("Argument 'InputData' is missing, with no default.")

  }else if (missing(DbSchema)) {

    catError("Argument 'DbSchema' is missing, with no default.")

  }else if (missing(DbTableName)) {

    catError("Argument 'DbTableName' is missing, with no default.")
  }

  start.time  <- Sys.time()

  if (ServerPath == ".") {

    ServerPath <- ShowCachePath()
  }
  WriteFmt(InputData, ServerPath)
  name.file <- sprintf(paste(ServerPath, "WasteFile/%s.tsv", sep = "/"), "tsv_file")

  write.table(InputData, file = name.file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = T, na = "")

  #data.table::fwrite(InputData, file = name.file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = T, na = "")

  DbSchema     <- gsub("\\[|\\]", "", DbSchema)   ; DbSchema     <- sprintf("[%s]", DbSchema)
  DbTableName  <- gsub("\\[|\\]", "", DbTableName); DbTableName  <- sprintf("[%s]", DbTableName)


  if (Append == FALSE) {

    CreateTable(Connection, InputData, DbSchema, DbTableName)
    bulk.query <- paste0(paste("BULK INSERT", paste(DbSchema, DbTableName, sep = ".")),
                         paste(" FROM", paste0("'", name.file, "'")),
                         paste(" WITH (CODEPAGE = 1252, FORMATFILE = ",
                               paste0("'", paste(gsub("tsv", "fmt", name.file), sep = "/"), "'"), ","),
                         paste(" FIELDTERMINATOR = '\t',  FIRSTROW = 2, ROWTERMINATOR = '\n', TABLOCK ,KEEPNULLS)"))

    result <- RODBC::sqlQuery(Connection, bulk.query)
    if (length(result) == 2 ){

      catInfo(sprintf("ERROR-> %s", result[1])); catError("Bulk ERROR")
    }
  }else{

    CreateTable(Connection, InputData, DbSchema = "temporary", DbTableName = "tab")
    bulk.query <- paste0("BULK INSERT temporary.tab",
                         paste(" FROM", paste0("'", name.file, "'")),
                         paste(" WITH (CODEPAGE = 1252, FORMATFILE = ",
                               paste0("'", paste(gsub("tsv", "fmt", name.file), sep = "/"), "'"), ","),
                         paste(" FIELDTERMINATOR = '\t',  FIRSTROW = 2, ROWTERMINATOR = '\n', TABLOCK,KEEPNULLS )"))

    result <- RODBC::sqlQuery(Connection, bulk.query)
    if (length(result) == 2){
      catInfo(sprintf("ERROR-> %s", result[1])); catError("Bulk ERROR")
    }

    query <- sprintf("SELECT COUNT(*) as Rows FROM sys.tables T INNER JOIN sys.schemas S ON T.schema_id=S.schema_id WHERE S.name='%s' and T.name='%s'",
                     gsub("\\[|\\]", "", DbSchema), gsub("\\[|\\]", "", DbTableName))

    exist.table <- data.table::data.table(RODBC::sqlQuery(Connection, query))

    if (exist.table$Rows == 0) {
      CreateTable(Connection, InputData, DbSchema, DbTableName)
      catInfo(sprintf("The table %s was created because was not found in the DB.", DbTableName))
    }

    columns       <- sprintf("[%s]", paste(names(InputData), collapse = "],["))
    append_query  <- sprintf("INSERT INTO %s.%s (%s) SELECT %s FROM [temporary].[tab]",
                             DbSchema, DbTableName, columns, columns )
    result <- RODBC::sqlQuery(Connection, append_query)
    if (length(result) == 2){
      catInfo(sprintf("ERROR-> %s", result[1])); catError("ERROR inserting data")
    }

    result <- RODBC::sqlQuery(Connection, "DROP TABLE [temporary].[tab]")
    if (length(result) == 2){
      catInfo(sprintf("ERROR-> %s", result[1])); catError("ERROR Dropping data")
    }
  }
  unlink(list.files(paste(ServerPath, "WasteFile", sep = "/"), full.names = T, include.dirs = F, ignore.case = T))
  catSuccess(sprintf("Success! Processed in: %s ", ConvertTime(difftime(Sys.time(), start.time, units = "secs"))))
}


#' @title  Append CSV
#' @description This function allows to append a set of csv files into one.
#' @examples TODO
#' @param ... files to appen
#' @param Folder Folder where files are, if missing: CachePath will be called
#' @param Name Name to save CSV
#' @param Remove boolean: if true all files will be deleted after
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
# Issue #131
AppendCSV <- function(..., Folder, Name, Remove = F){


  if (missing(Folder)){

    catInfo("\nFolder not selected. It will look for files in TAPChunks.")
    Folder <- ShowCachePath()

  }else{

    Folder <- file.path(path.expand("~"), Folder)
    if (!file.exists(Folder)) {

      dir.create(Folder)

    }
  }

  if (missing(Name)){

    catInfo("\nName of file not defined. Name will be 'All.csv'.")
    Name   <- "All.csv"

  }else{

    if (length(grep(".csv", Name)) == 0) Name <- paste0(Name, ".csv")

  }

  files <- paste(match.call(expand.dots = FALSE)$...)

  if (length(files) == 0){

    catError("\nPlease select files to append.")

  }

  new.files = list()
  names.rows <- data.table::data.table(1)
  for (file in files){

    if (exists(file)){
      data.table::fwrite(get(file), file = file.path(Folder, paste0(file, ".csv")))
      names.rows  <- suppressWarnings(cbind(names.rows, names(get(file))))
      new.files   <- c(paste0(file, ".csv"), new.files)
    }else{
      if (length(grep(".csv", file)) == 1){
        new.files   <- c(file, new.files)
        names.rows  <- suppressWarnings(cbind(names.rows, names(data.table::fread(file, nrow = 1L))))
      }
    }

  }

  names(names.rows) <- paste(1:ncol(names.rows))
  names.rows[,1:=NULL]
  for (i in 2:ncol(names.rows)){

    if (length(setdiff(names.rows[["1"]], names.rows[[paste(i)]])) != 0){

      catError("\nSources to append don't have the same columns.")

    }
  }

  files.append  <- list.files(Folder,
                              pattern = ".csv")

  if(length(setdiff(new.files, files.append)) != 0){

    catError(sprintf("\n %s not found", setdiff(new.files, files.append)))
  }

  if(length(new.files) > 1){

    new.files   <- paste(new.files, collapse = "+")

  }

  shell(sprintf("cd %s & copy %s %s",          #go to dir of saved partition and copy every csv files
                Folder, new.files, Name,       #path to save melted file
                translate = T                  #path can be read with "\"
  ))

  if(Remove){

    new.files <- setdiff(new.files, Name)    #get all files except Appended one
    do.call(unlink, list(new.files))         #remove list of csv files
  }
}

#' @title Applying weights to data
#' @description
#' This function adds dynamically calculated weights to the submitted data.
#' @details
#' The weight calculation is designed to alter the distribution of the sample to match
#' the distribution of the population data that we get from the Market Model. The function offers two
#' types of weighting which can be chosen using the WType parameter. The weighting types are as follows:
#' \tabular{ll}{
#' \strong{Shape}\tab This is the default weighting type. The size of the weighted sample is normally
#' unchanged by the operation so the weighted and unweighted sample sizes of the entire table are unchanged.
#' Each individual org will be weighted so that the distribution of the sample orgs has the same shape as
#' the distribution of the population orgs.  \cr
#' \strong{Scale}\tab If the user sets the scale parameter the weighted sample size becomes equal to
#' the population as set in the market model.\cr}
#' \cr
#' The calculation is performed over a variety of dimensions, with the default being GSV
#' (by all combinations of Geography, Segment and Vertical).  Using the Granularity parameter
#' this may be changed to any combination of G S and V.\cr
#'
#' @inheritSection ShowWeightCalc Example Weighting Calculation
#' @inheritSection ShowWeightCalc Example Calculation
#' @export
#' @param Data Chunk to be weighted
#' @param Granularity Dimensions to weight. Default being GSV (by all combinations of Geography, Segment and Vertical)
#' @param WType Define the type of weighting as either \emph{Shape} (which is the default option) or \emph{Scale}
#' @examples ApplyWeightCalc(TestEmailChunk, Granularity = "S")
#' @author Jonathan Tooley Associados Lda
#' @family Weighting and cleaning functions
#' @return Returns the original data table with an additional column \emph{weight}.
#' @seealso \code{\link{TAPChunks}}
ApplyWeightCalc <- function(Data, Granularity = "gsv", WType = "Shape") {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  if (CheckDemogDB()){
    # Define the granularity for weighting
    Weight_levl <- 0L

    choices <-
      grep(
        paste0("[", Granularity, "]"),
        c("G", "S", "V"),
        ignore.case = T,
        value = T
      )
    if (length(choices) != nchar(Granularity))
      catError ("Granularity parameter should only contain the characters G, S or V")

    # According to the selection of weight type we construct a bit mask that will extract only
    # the meta data that we require

    if (length(intersect(choices, "G")) == 1) {
      Weight_levl <- bitwOr(Weight_levl, 0x003F)
    }
    if (length(intersect(choices, "S")) == 1) {
      Weight_levl <- bitwOr(Weight_levl, 0x03C0)
    }
    if (length(intersect(choices, "V")) == 1) {
      Weight_levl <- bitwOr(Weight_levl, 0x3C00)
    }

    if (Weight_levl != 0L){
      cols <- names(Data)

      # Some sources have many repeating rows because they list devices.  This is not relevant
      # to weighting schemas so we first summarise the input to just have orgs

      orgs <- Data[, .N, by = c("uuid", "timestamp")][, N := NULL]


      firmogs <- TAP_env$demog_helper[, .(uuid,
                                          gsv,
                                          WClean    = clean)]

      orgs <- firmogs[orgs, on = "uuid"]

      orgs[, gsv := bitwAnd(gsv, Weight_levl)]

      sample_dist <-
        orgs[, .(orgSample = data.table::uniqueN(uuid)), by = c("gsv", "WClean", "timestamp")]


      target      <- TAP_env$target_helper[, .(gsv, entity_count)]

      target[, gsv := bitwAnd(gsv, Weight_levl)]

      populn_dist <- target [, .(orgPop = sum(entity_count),
                                 WClean = "Y"), by = gsv]
      Weight_Calc <-
        merge(
          sample_dist,
          populn_dist,
          all.x = T,
          by = c("gsv", "WClean")
        )

      Weight_Calc[is.na(orgSample),  orgSample := 0]
      Weight_Calc[is.na(orgPop)   ,  orgPop    := 0]

      Weight_Calc[WClean == "Y", cs := sum(orgSample, na.rm = T), by = "timestamp"]
      Weight_Calc[, orgTarget  := ifelse(orgSample == 0, 0, orgPop)]
      Weight_Calc[, distTarget := orgTarget / sum(orgTarget, na.rm = T), by = "timestamp"]
      Weight_Calc[, orgExpect  := distTarget * cs]
      Weight_Calc[is.na(orgExpect), orgExpect := 0]
      Weight_Calc[, shapeW     := ifelse(orgSample == 0, 0, orgExpect / orgSample)]
      Weight_Calc[, scaleW     := ifelse(orgSample == 0, 0, orgTarget / orgSample)]

      if(WType == "Shape"){
        Weight_Calc[, weight     := shapeW]
      } else {
        if(WType == "Scale"){
          Weight_Calc[, weight     := scaleW]
        } else {
          catWarning("Unknown WeightType.")
          Weight_Calc[, weight     := 0]
        }
      }

      orgs<-orgs[Weight_Calc[, c("gsv", "WClean", "timestamp", "weight")], on = c("gsv", "WClean", "timestamp")]

      return(Data[orgs, on = c("uuid", "timestamp")])
    }
    catInfo("No granularity selected")
  }
  catInfo("This function requires both demogs and targets to be loaded to the environment")
}

#' @title Summarizing a file to see the sample size
#' @description This function will tell the user the sample size in a file.
#' The sample size is defined as the number of distinct orgs in the data and is
#' split between the number of unclean orgs, unweighted clean orgs and weighted clean
#' orgs.
#' @param Data TODO
#' @param By TODO
#' @export
#' @examples ShowSampleSize(TestEmailChunk)
#' ShowSampleSize(ApplyWeightCalc(TestEmailChunk))
#' ShowSampleSize(ApplyWeightCalc(TestEmailChunk), By = "email_delivery")
#' @author JTA Associados Lda

ShowSampleSize <- function(Data, By = NULL){

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  data_profile <- ValidateChunk(Data)
  if(data_profile$valid)
  {
    Data [, None := 1]  # We need a default column to use to join when there are no other columns selected
    byv     <- c(By, "None") # We automatically add the default column to the by
    tov     <- "None"
    # We load the demog database if it is not available so we can see the unclean orgs
    if (!CheckDemogDB()) LoadDemogDB()

    option <- bitwAnd(data_profile$status, 0x03) #Extract the weight and timestamp status
    if (option == 2 | option == 3) {
      if (length(intersect(By, "timestamp")) == 0) {
        byv <- c("timestamp", byv)
        tov <- c("timestamp", tov)   # Having a timestamp we add it to the by unless the user did
      }
    }

    # The dirty sample size (dss) and unweighted sample size (uss) can now be calculated
    dss <- Data[ (uuid %in% TAP_env$unclean), .(dirty_sample      = data.table::uniqueN(uuid)), by = byv]
    uss <- Data[!(uuid %in% TAP_env$unclean), .(unweighted_sample = data.table::uniqueN(uuid)), by = byv]

    # We also add the dirty sample total and unweighted sample total
    dst <- Data[ (uuid %in% TAP_env$unclean), .(dst               = data.table::uniqueN(uuid)), by = tov]
    ust <- Data[!(uuid %in% TAP_env$unclean), .(ust               = data.table::uniqueN(uuid)), by = tov]

    # Weighted sample size may now be run
    if (option == 0 | option == 2) { # No weight so we just prepare a blank column
      wss <- Data[                            , .(weighted_sample   = "***")        , by = byv]
    }

    if (option == 1 | option == 3) { # There is a weight so we need to calculate the wss.
      # A two step calculation where we count orgs but by weight
      # We then prepare a weighted average
      wss <- Data[, .(uss = data.table::uniqueN(uuid)), by = c(byv, "weight")][, #Run the first stage where we get raw org counts by weight
                                                                               .(weighted_sample   = round(sum(uss * weight), 1)), by = byv]

      wst <- Data[, .(uss = data.table::uniqueN(uuid)), by = c(tov, "weight")][, #Run the first stage where we get raw org counts by weight
                                                                               .(wst               = round(sum(uss * weight), 1)), by = tov]
      if(!is.null(By)){
        wss <- merge(wss, wst, by = tov)
        wss[, weighted_pc := round(  weighted_sample / wst, 6)]
        data.table::setnames(wss, old = "wst", new = "weighted_total")
      }
    }

    if(!is.null(By)){
      uss <- merge(uss, ust, by = tov)
      uss[, unweighted_pc := round(unweighted_sample / ust, 6)]
      data.table::setnames(uss, old = "ust", new = "unweighted_total")
    }

    if (nrow(dss) == 0)
      return(      merge(wss,uss,by = byv)                        [, -c("None")])
    else return(merge(merge(wss,uss, by = byv, sort = TRUE), dss, all = T, by = byv, sort = TRUE)[, -c("None")])
  }
}

#' @title Show Sample Size
#' @keywords internal
#' @export
#' @details This has been renamed to ShowSampleSize
SampleSize <- function(Data, By = NULL){
  ShowSampleSize(Data = Data, By = By)
  catWarning("This function has been renamed as ShowSampleSize.")
}


#' @title UseAsSlicer
#' @description TODO
#' @details TODO
#' @family Chunk Manipulators
#' @param Data A data table
#' @param ReportName TODO
#' @export
#' @author Jonathan Tooley Associados Lda
#' @return TODO
#' @seealso \code{\link{TAPChunks}}
#' Issue #129
UseAsSlicer <- function(Data, ReportName = "test"){

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  temp.folder <- ShowCachePath()
  if (length(list.files(temp.folder)) != 0) {
    if (sum(list.files(temp.folder) == ReportName) == 1) {
      answer <-
        menu(c("Yes", "No"), title = "Is there a report with the same name, do you want to continue?\n")
      if (answer == 1) {
        unlink(file.path(temp.folder, ReportName),
               recursive = T ,
               force = T)

        dir.create(file.path(temp.folder, ReportName))

        catInfo("The folder has been replaced.")

      } else{
        catError("The operation was interrupted.")
      }
    } else{
      dir.create(file.path(temp.folder, ReportName))
    }
  } else{
    dir.create(file.path(temp.folder, ReportName))
  }

  test.prod <-
    intersect(names(Data), gsub(
      "_Name|Parent",
      "",
      c(Product_Hierarchy[Dimension == "Product"]$ViewColumn, "variable")
    ))
  test.geo  <-
    intersect(names(Data), gsub(
      "_Name|Parent",
      "",
      c(Product_Hierarchy[Dimension == "Geography"]$ViewColumn, "WCountry")
    ))
  test.seg  <-
    intersect(names(Data), gsub(
      "_Name|Parent",
      "",
      c(Product_Hierarchy[Dimension == "Segment"]$ViewColumn, "Segment_Code")
    ))
  test.os   <-
    intersect(names(Data), c("Platform", "Architecture", "Ecosystem", "OS"))

  if(length(test.prod) > 1) {
    map_prod <- unique(Data[, test.prod, with = F])[, ID_Prod := .I]
    Data <- merge(Data, map_prod, by = test.prod)[, !test.prod, with = F]
    catInfo("Product map was successfully created.")
    SaveLocal(map_prod, Path = file.path(temp.folder, ReportName))
  } else{
    catInfo("No Product dimensions found.")
  }

  if (length(test.geo) > 1) {
    map_geo <- unique(Data[, c(test.geo), with = F])[, ID_Geo := .I]
    Data <- merge(Data, map_geo, by = test.geo)[, !test.geo, with = F]
    catInfo("Geography map was successfully created.")
    SaveLocal(map_geo, Path = file.path(temp.folder, ReportName))
  } else {
    catInfo("No Geography dimensions found.")
  }

  if (length(test.seg) > 1) {
    map_seg <- unique(Data[, test.seg, with = F])[, ID_Seg := .I]
    Data <- merge(Data, map_seg, by = test.seg)[, !test.seg, with = F]
    catInfo("Segment map was successfully created.")
    SaveLocal(map_seg, Path = file.path(temp.folder, ReportName))
  } else{
    catInfo("No Segment dimensions found.")
  }

  if (length(test.os) > 1) {
    map_OS <- unique(Data[, test.os, with = F])[, ID_OS := .I]
    Data <- merge(Data, map_OS, by = test.os)[, !test.os, with = F]
    catInfo("OS map was successfully created.")
    SaveLocal(map_OS, Path = file.path(temp.folder, ReportName))
  } else{
    catInfo("No OS dimensions found.")
  }
  fact_table <- Data
  SaveLocal(fact_table, Path = file.path(temp.folder,ReportName))
}


#' @title Show the Size of a Longitudinal Set
#' @export
#' @param Data TODO
#' @examples ShowLongitudinalSize(TestEmailChunk)
#' @author Jonathan Tooley Associated Lda
ShowLongitudinalSize <- function(Data){

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  data_profile <- ValidateChunk(Data)
  if(data_profile$valid)
  {
    if (!CheckDemogDB()) LoadDemogDB()

    ls <- Data[!uuid %in% TAP_env$unclean, data.table::uniqueN(timestamp),
               by = uuid][V1 == max(V1),
                          data.table::uniqueN(uuid)]
    ds <- Data[ uuid %in% TAP_env$unclean, data.table::uniqueN(timestamp),
                by = uuid][V1 == max(V1),
                           data.table::uniqueN(uuid)]
    catInfo(paste("Clean == 'Y' organizations in longitudinal:", ls))
    catInfo(paste("Clean == 'N' organizations in longitudinal:", ds))
    return(ls)
  } else catError("This function needs to have a valid chunk passed.")
}


#' @title Forming the Longitudinal Data Set
#' @export
#' @param Data TODO
#' @import data.table
#' @author Jonathan Tooley Associados Lda
#'
ApplyLongitudinalSet <- function(Data) {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  profile <- ValidateChunk(Data)
  if (profile$valid) {
    if (bitwAnd(profile$status, 0x02) == 0x02) {
      max_periods <- Data[, data.table::uniqueN(timestamp)]
      long_set    <-
        Data[, .(periods = data.table::uniqueN(timestamp)), by = "uuid"][periods == max_periods]$uuid
      orig_set    <- Data[, data.table::uniqueN(uuid)]
      catInfo(paste(
        "Submitted data set has ",
        orig_set,
        " organizations covering ",
        max_periods,
        " periods."
      ))
      catInfo(paste(
        "The longitudinal set has ",
        length(long_set),
        " oranizations which appear in all periods."
      ))
      catInfo("This new data should be reweighted.")
      return (Data[uuid %in% long_set])
    } else
      catInfo("No timestamp detected so we can't form the longitudinal set.")
  } else
    catInfo("Longitudinal process not run due to previous validation error.")
}




#' @title ADD Dimension
#' @description TODO
#' @details TODO
#' @inheritParams GetMapping
#' @examples AddDimension(Data = Data, Dimension = "P")
#' @author JTA Associados Lda
#' @return TODO
#' @param Data TODO
#' @param Dimension TODO
#' @param SelectHierarchy TODO
#' @export
#' @family TODO
AddDimension <- function(Data, Dimension = "G", SelectHierarchy = NULL) {

  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) catError("Data file not specified.")

  ExpDimension <- switch(Dimension,
                         "G" = "Geography",
                         "P" = "Product",
                         "S" = "Segment")


  MapHierarchy <-
    GetMapping("H")[DimensionCategory == ExpDimension,-"Name", with = F]


  ##TODO1 isto tem que vir correcto do SQL-tabela de hieraquia se nao for do SQL do getMapping
  if (ExpDimension == "Product") {
    MapHierarchy <- rbind(MapHierarchy[HierarchyName == "DimH_Product"],
                          MapHierarchy[HierarchyName != "DimH_Product",

                                       rbind(
                                         data.table::data.table(
                                           DisplayName = "Product0_Variant",
                                           Level_ID = 1,
                                           IsVariant = "Y"
                                         )
                                         ,
                                         .SD[, .(DisplayName, Level_ID = Level_ID + 1, IsVariant)]
                                       )
                                       , by = c("DimensionCategory", "HierarchyName", "FriendlyName")])
    ##TODO1

    ##TODO4 na função AddFirmographics definir a variavel "variable" como "Product0_Variant"
    MapHierarchy[DisplayName == "Product0_Variant", DisplayName := "variable"]
    ##TODO4

  }


  ##TODO2 isto tem que vir correcto do SQL-tabela de hieraquia- se nao for do SQL do getMapping
  if (ExpDimension == "Segment") {
    MapHierarchy$Level_ID  <- MapHierarchy$Level_ID + 1

    MapHierarchy  <- rbind(MapHierarchy,
                           MapHierarchy[, data.table::data.table(DisplayName = "Segment_Code",
                                                                 Level_ID = 1,
                                                                 IsVariant = "Y"), by = c("DimensionCategory", "HierarchyName", "FriendlyName")])

  }
  ##TODO2
  if (ExpDimension == "Geography") {

    MapHierarchy[DisplayName=="Dim_OrgGeo_5Country",  DisplayName:="Country"]
    MapHierarchy[HierarchyName=="DimH_DSSTGeo" & DisplayName=="Area",  DisplayName:="Area_DSST"]

  }



  ##TODO3 tem que se retirar "parent" do SQL-tabela de hieraquia
  MapHierarchy$DisplayName  <-
    gsub("Parent", "", MapHierarchy$DisplayName)
  ##TODO3

  if(length(intersect(MapHierarchy$DisplayName, names(Data))) == 0) Data <- AddFirmographics(Data)


  MapHierarchyDIM <-
    MapHierarchy[DisplayName %in% intersect(MapHierarchy$DisplayName, names(Data)), .SD[Level_ID ==
                                                                                          min(Level_ID)], by = c("DimensionCategory", "HierarchyName")]


  MapHierarchyDIM[, DimInData := "Y"]

  MapHierarchy  <-
    merge(MapHierarchyDIM,
          MapHierarchy,
          by = names(MapHierarchy),
          all = T)
  MapHierarchy[order(Level_ID), DimLevel := .SD[DimInData == "Y", Level_ID], by = c("DimensionCategory", "HierarchyName")]
  MapHierarchy  <-
    MapHierarchy[!is.na(DimLevel) & DimLevel <= Level_ID]


  MAP <- GetMapping(Dimension)

  ##TODO4 na função AddFirmographics definir a variavel "variable" como "Product0_Variant"
  if (Dimension == "P") {
    data.table::setnames(MAP, "Product0_Variant", "variable")
  }
  ##TODO4

  ##TODO5 tem que se corrigir a tabela SQL dos segmentos
  if (Dimension == "S") {
    MAP$Segment_Code  <- gsub("^\\d\\d\\d_", "", MAP$Segment_Code)
  }
  ##TODO5


  ## Create Question for several Hierarchy
  if (is.null(SelectHierarchy)) {
    Question  <-
      MapHierarchy[IsVariant == "N", .(FriendlyName, DisplayName, DimInData)]
    Question[DimInData == "Y", DisplayName := paste0(DisplayName)]

    Question  <-
      Question[, .(string = paste0(FriendlyName, sprintf(
        "(%s)", paste(DisplayName, collapse = ", ")
      ))),
      by = "FriendlyName"]$string

    Question  <- c(Question, "Select the columns")

    Question  <-
      paste(1:length(Question),
            Question,
            sep = "-",
            collapse = "\n\t")
    format_right <- F
    while (format_right == F) {
      answer <-
        readline(cat(
          paste0(
            "Choose Hierarchy: please, insert numbers separated by commas or press 0 to cancel the process: \n\t",
            Question
          )
        ))

      if (answer == 0) {
        catError("No source selected.")
      }

      if (is.na(sum(as.numeric(strsplit(answer, ",")[[1]])))) {
        catWarning("You should put number separate by commas.")
        format_right <- F
      } else {
        answer <- as.numeric(strsplit(answer, ",")[[1]])
        format_right <- T
      }
    }
  } else {
    SelectHierarchy  <- gsub("\\s", "", SelectHierarchy)

    SelectHierarchy  <- strsplit(SelectHierarchy, ",")[[1]]
    selectOptions    <-
      gsub("\\s", "", unique(MapHierarchy$FriendlyName))


    answer  <- NULL
    for (i in selectOptions) {
      answer  <- c(answer, i %in% SelectHierarchy)

    }

    answer  <- which(answer == T)

    if (length(answer) != length(SelectHierarchy))
      catError("Required hierarchies don't exist. ")

  }

  select_column <- NULL
  if (length(intersect(answer, length(unique(MapHierarchy$HierarchyName)) + 1)) == 1) {
    columns   <-
      sort(unique(MapHierarchy[IsVariant != "Y"]$DisplayName))
    Question  <-
      paste(1:length(columns),
            columns,
            sep = "-",
            collapse = "\n\t")

    format_right <- F
    while (format_right == F) {
      answer2 <-
        readline(cat(
          paste0("Choose the columns, separated by commas:\n\t", Question)
        ))

      if (is.na(sum(as.numeric(strsplit(answer2, ",")[[1]])))) {
        catWarning("You should put number separate by comas.")
        format_right <- F
      } else {
        answer2 <- as.numeric(strsplit(answer2, ",")[[1]])
        format_right <- T
      }
    }

    select_column <- c(select_column, paste(columns[answer2]))
    answer <-
      setdiff(answer, length(unique(MapHierarchy$HierarchyName)) + 1)

  }

  select_column <-
    c(select_column, paste(unique(MapHierarchy[FriendlyName %in% unique(MapHierarchy$FriendlyName)[answer], DisplayName])))
  select_column <- unique(select_column)

  # select_column <- setdiff(select_column, names(Data))

  if (length(setdiff(select_column, names(Data))) == 0) {
    catInfo("Already exist dimension.")
    return(Data)
  }

  MapHierarchy     <- merge(
    MapHierarchy,
    data.table::data.table(DisplayName = select_column, request =
                             "Y"),
    all = T,
    by = "DisplayName"
  )

  MapHierarchyADD  <-
    MapHierarchy[is.na(DimInData) &
                   request == "Y", .SD[1], by = "DisplayName"]


  for (i in  unique(MapHierarchy$HierarchyName)) {
    temp.Product   <- MapHierarchy[HierarchyName == i]

    temp.Product   <-
      temp.Product[DimInData == "Y" |
                     DisplayName %in% MapHierarchyADD[HierarchyName == i, DisplayName]]

    MapHierarchy[HierarchyName == i, DisplayName]

    temp.map       <-
      unique(MAP[, temp.Product[!(is.na(DimInData) &
                                    is.na(request))]$DisplayName, with = F])

    Data  <-
      merge(Data, unique(temp.map), by = temp.Product[DimInData == "Y", DisplayName])

    if (nrow(temp.map) != nrow(unique(temp.map))) {

      doubles <- temp.map[, .N, by = "Product0_Variant"][N > 1]
      doubles <- doubles[!Product0_Variant %in% c("[SW CCM-Cost multi-label product mapping]", "[SW CCM-Usage multi-label product mapping]")][, I := .I]
      text    <- NULL
      for(i in doubles$I){ text <- c(text, paste(doubles[i]$Product0_Variant, doubles[i]$N, "\n"))}
      catError(paste("Need check the mappings, have doubles:\n", text, collapse = "\t- "))

    }

  }

  removeCol  <- unique(MapHierarchy[is.na(request), DisplayName])
  removeCol  <- intersect(removeCol, names(Data))

  if (length(removeCol) != 0) {
    Data  <- Data[, .SD , .SDcols = !removeCol]
  }

  Data <- Data[, OrderColumn(Data), with = F]
  return(Data)
}

#' @title Apply/Calculate penetration in Data Set
#' @param Data Dataset where the penetration will be applied
#' @param Dimension Dimensions to be included in the final output
#' @param PenetrationDimension Dimension that is the Penetration target
#' @export
#' @import data.table
#' @author Jonathan Tooley Associados Lda
#'
ApplyPenetration <-
  function(Data, Dimension, PenetrationDimension) {

    if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
    if (is.null(Data)) catError("Data file not specified.")

    if (sum(Dimension %in% PenetrationDimension) != 0) {
      catError(
        "The same dimension can not appear simultaneously in the parameters dimension and penFill."
      )

    }
    if (sum(c(Dimension, PenetrationDimension) %in% names(Data)) != length(c(Dimension, PenetrationDimension))) {
      catError(
        "There is no match between the source attributes and the required dimensions."
      )

    }

    HierarchyMAP <- GetMapping("H")[IsVariant != "Y"]
    ##TODO-temp solution
    HierarchyMAP$DisplayName <-
      gsub("Parent", "", HierarchyMAP$DisplayName)

    HierarchyMAP <-
      HierarchyMAP[(!(Level_ID == 2 & DisplayName == "Product0")) & HierarchyName != "DimH_DSSTGeo"]

    HierarchyDIM <-
      unique(HierarchyMAP[DisplayName %in% Dimension, .(DimensionCategory, DisplayName, Level_ID)])
    metadataDIM <-
      HierarchyDIM[, .SD[Level_ID == min(Level_ID)] , by = "DimensionCategory"]$DisplayName

    if (sum(Dimension %in% HierarchyMAP$DisplayName) != length(Dimension)) {
      extraCols <- setdiff(Dimension, HierarchyMAP$DisplayName)

      metadataDIM <- c(metadataDIM, extraCols)

      HierarchyDIM <- rbind(
        HierarchyDIM,
        data.table::data.table(
          DimensionCategory = paste0("cat", 1:length(extraCols)),
          DisplayName = extraCols,
          Level_ID = 1
        )
      )
    }

    message <-
      unique(HierarchyDIM[order(Level_ID), .(DimensionCategory, DisplayName)])
    message[DisplayName %in% metadataDIM, DisplayName := paste0( DisplayName)]
    message <-
      message[, paste0(DimensionCategory,
                       "(",
                       paste0(DisplayName, collapse = ", "),
                       ")"), by = DimensionCategory]$V1

    catInfo(sprintf("Dimension: \n\t-%s", paste(message, collapse = "\n\t-")))

    HierarchyPEN <-
      unique(HierarchyMAP[DisplayName %in% PenetrationDimension , .(DimensionCategory, DisplayName, Level_ID)])

    if (sum(PenetrationDimension  %in% HierarchyMAP$DisplayName) != length(PenetrationDimension)) {
      extraCols <- setdiff(PenetrationDimension, HierarchyPEN$DisplayName)

      HierarchyPEN <- rbind(
        HierarchyPEN,
        data.table::data.table(
          DimensionCategory = paste0("cat", 1:length(extraCols)),
          DisplayName = extraCols,
          Level_ID = 1
        )
      )
    }

    metadataPEN <-
      HierarchyPEN[, .SD[Level_ID == min(Level_ID)] , by = "DimensionCategory"]$DisplayName

    message <-
      unique(HierarchyPEN[order(Level_ID), .(DimensionCategory, DisplayName)])
    message[DisplayName %in% metadataPEN, DisplayName := paste0( DisplayName)]
    message <-
      message[, paste0(DimensionCategory,
                       "(",
                       paste0(DisplayName, collapse = ", "),
                       ")"), by = DimensionCategory]$V1

    catInfo(sprintf(
      "Penetration filters: \n\t-%s",
      paste(message, collapse = "\n\t-")
    ))

    dataPenFill <- ShowSampleSize(Data, c(metadataDIM, metadataPEN))
    if ("dirty_sample" %in% names(dataPenFill)) catError("The Data isn't clean")

    dataDimension  <- ShowSampleSize(Data, c(metadataDIM))

    longMap <- NULL
    for (i in c(metadataDIM, metadataPEN)) {
      if (is.null(longMap)) {
        longMap <- dataPenFill[, .(unique(get(i)))]
        data.table::setnames(longMap, "V1", i)

      } else{
        longMap <- longMap[, unique(dataPenFill[[i]]), by = names(longMap)]
        data.table::setnames(longMap, "V1", i)
      }

    }
    timestamp <- unique(Data$timestamp)
    longMap   <- longMap[, timestamp, by = names(longMap)]


    HierarchyDIM <- rbind(HierarchyDIM, HierarchyPEN)

    for (i in HierarchyDIM$DimensionCategory) {
      if (nrow(HierarchyDIM[DimensionCategory == i]) > 1) {
        map.temp <-
          unique(Data[, HierarchyDIM[DimensionCategory == i, DisplayName], with =
                        F])
        longMap <- merge(longMap,
                         map.temp,
                         all.x = T,
                         by = intersect(names(longMap), names(map.temp)))

      }

    }
    longMap <- merge(
      longMap,
      dataPenFill,
      by = c(metadataDIM, metadataPEN, "timestamp"),
      all.x = T
    )
    longMap[, unweighted_total := unique(.SD[!is.na(unweighted_total), unique(unweighted_total)]), by =
              "timestamp"]
    uniqueCol <- c("timestamp")
    longMap[is.na(longMap)] <- 0
    longMap <-
      merge(
        longMap,
        dataDimension,
        by = c(metadataDIM, uniqueCol),
        suffixes = c("_N", "_D"),
        all.x = T
      )
    return(longMap)
  }

#' @title Save and open Excel file with data
#' @param Data Dataset to be saved in Excel
#' @export
#' @import data.table
#' @author Jonathan Tooley Associados Lda
#'
Xcel <- function(Data) {

  tFile <-

    tempfile(fileext = paste0(substitute(Data), ".tsv"), tmpdir = ShowCachePath())

  write.table(Data,
              tFile,
              row.names = F,
              sep = "\t",
              quote = F)

  shell(paste('start excel', tFile))

}




#' @title Add Vertical hierarchy to a data chunk
#' @description This function allows you to add a Vertical hierarchy to the chunk.
#' @section Vertical Hierarchy:
#' The function accesses the RDR and retrieves the standard vertical
#' hierarchy that is matched to the vertical names that the file has.
#' @section Hierarchy available:
#' \tabular{llll}{
#' \strong{Hierarchy Name}         \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3}    \cr
#' \strong{Org Vertical (default)} \tab Vertical        \tab Industry        \tab IndustrySector}
#' @family RDR Integration Tools
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @export
#' @import data.table
#' @inheritParams AddGeoHierarchy
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @examples AddVerticalHierarchy(TestEmailChunk)
#' @seealso \code{\link{TAPChunks}}
#' Issue #
AddVerticalHierarchy  <- function(Data, SelectHierarchy = NULL){
  if( !"Vertical" %in% names(Data)) Data <- AddFirmographics(Data, Dimension = "V" )
  Data
}

#'AddGSVHierarchy
#'
#' @description TODO
#' @param Data TODO
#' @export
#'
AddGSVHierarchy<- function(Data){

  Data <- AddFirmographics(Data)
  Data <-
    AddGeoHierarchy(
      AddSegmentHierarchy(
        AddVerticalHierarchy(Data)
      )
    )
  Data
}
