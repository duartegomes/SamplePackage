#' @title Preparing data for IPF
#' @description The mathematics of running an Iterative Proportional Fit are not hard
#' to do but the problem needs to be properly set up.  The data that is sent to IPF tools
#' such as \code{\link{RunIPF}} has many strict requirements and this function ensures that
#' data is properly formatted for IPF.
#' @param Data This can be a data table, a data frame or an array.  When the user presents a data table or data frame the system returns
#' an array in a format that is ready for the IPF function.  When the user presents an array the system returns a data table. This means
#' that this function can be used twice in a typical IPF workflow.  The first time converts a chunk of data
#' to a multi-dimensional array for the IPF function.  The IPF returns an array which can be submitted to
#' this function a second time to return the data to a chunk format.
#' @param Dimensions A vector of dimension names to include in the analysis. If this is not supplied the function
#' will look in the file to see which columns can be used for dimensions and will include them all.
#' @param Metric A calculation to be employed to generate the values which will be submitted to the analysis. If
#' the user does not provide this information the system will take the first numeric column that it finds
#' as the metric to be used in the IPF.
#' @param Order If this is set to True the function will print a list of members in each dimension in the
#' order in which it will appear in the array.
#' @details The underlying premise of this function is to convert from a data table format to an array
#' and back again. When we run an IPF both the seed and the targets are presented as multi-dimensional
#' arrays but we typically get the data from a data table.  The function will perform this transformation
#' for you. It will also transform a multidimensional array back into a data table.
#' @section Introduction:
#' This function has the following objectives: \cr
#' \itemize{
#' \item It takes a table of data and analyses it for suitability for IPF. It checks
#' that there are columns of data that can form the dimensions of the IPF and it checks
#' for numerical data that can form the seed.
#' \item It allows the user to define both the dimensions and the metric that will form
#' the seed.  These definitions are validated and then applied.
#' \item It aggregates the data down to the chosen dimensionality so that we don't
#' get duplicate items in the seed.
#' \item It then checks that every coordinate in the dimension space is represented in
#' the data or fills blank coordinates.
#' \item It creates an n dimensional array of values that can be sent to the IPF function
#' \code{\link{RunIPF}}.
#' \item It reports all the above activity in a series of messages.
#' \item It can also take the n dimensional array format and reconvert this into a
#' data table.  This is used to return a completed IPF calculation back into a Chunk
#' format.
#' }
#' @section Data Format:
#' As we have mentioned above the function can convert from a tabular format to an array or from an array to a tabular format. The typical
#' workflow in preparing an IPF is to take data in a table, either as a TAP Chunk or as a data table, and to prepare
#' multidimensional arrays to present to the Iterative Proportional Fit ("IPF") tool \code{\link{RunIPF}}. The output from the IPF tool is
#' another multidimensional array which can be submitted to this function to return the output back into a table. \cr\cr
#' \strong{CONVERTING FROM A TABLE TO AN ARRAY}
#' @section Analysing Dimensions:
#' The first step in converting from a table to an array is an analysis of the dimensions that appear to be available
#' in the submitted table. The function will consider all the columns that are in the submitted table. Any columns that
#' contain either characters or factors will be considered to be appropriate for use as a dimension. If the data does not
#' have any character or factor columns the function will end with an error message.  During the analysis
#' the function will note if columns containing factors have a custom sort order defined. Ordered factors will retain
#' their custom sort.  Non ordered factors will be sorted alphabetically as will character columns. \cr\cr
#' Here are some examples that you can run to see the dimension analysis phase: \cr\cr
#' \emph{Data with ordered factors} \cr
#' \preformatted{
#' a <- PrepareIPF(esoph)
#' - Data frame submitted.
#' - Analysing Dimensions:
#'   - Discovered 3 possible dimensions.
#' -    : { agegp,alcgp,tobgp }
#' -    : { 6,4,4 }
#' - The number of possible array coordinates is 96
#' }
#' \emph{Data with unordered factors} \cr
#' \preformatted{
#' a <- PrepareIPF(TestClientChunk)
#' - Data table submitted.
#' - Analysing Dimensions:
#' - Discovered 5 possible dimensions.
#' -    : { oem,form_factor,os_name,timestamp,variable }
#' -    : { 7,2,4,1,29 }
#' - The number of possible array coordinates is 1624
#' - Discovered 5 factors that are not ordered.
#' -    : { oem,form_factor,os_name,timestamp,variable }
#' - These will be ordered for you.
#' }
#' @section Defining Dimensions:
#' The previous step found the set of columns that could be used to define dimensions. You may not
#' wish to submit all of the dimensions in the table to the IPF and so a user may define the dimensions
#' that they wish to have included by using the Dimensions parameter. The parameter should be passed a
#' character vector with the names of the columns to include. Note that the array produced by this
#' function will respect the order that you select the dimensions. This will not effect the calculation
#' but can make the array easier to view by putting dimensions with fewer members in columns. \cr\cr
#' Here are some examples that you can run to see dimension definition phase. \cr
#' \preformatted{
#' PrepareIPF(esoph, Dimensions = c("alcgp", "tobgp"))
#' - Data frame submitted.
#' - Analysing Dimensions:
#' - Discovered 3 possible dimensions.
#' -    : { agegp,alcgp,tobgp }
#' -    : { 6,4,4 }
#' - The number of possible array coordinates is 96
#' - Filtered to 2 chosen dimensions.
#' -    : { alcgp,tobgp }
#'
#'            tobgp
#' alcgp       0-9g/day 10-19 20-29 30+
#'   0-39g/day        9    10     5   5
#'   40-79           34    17    15   9
#'   80-119          19    19     6   7
#'   120+            16    12     7  10
#'
#' PrepareIPF(esoph, Dimensions = c("tobgp", "alcgp"))
#' ....
#'           alcgp
#' tobgp      0-39g/day 40-79 80-119 120+
#'   0-9g/day         9    34     19   16
#'   10-19           10    17     19   12
#'   20-29            5    15      6    7
#'   30+              5     9      7   10
#' }
#' @section Aggregating Data:
#' Once the dimensions have been defined, either automatically or from the Dimensions parameter, the data
#' is aggregated to have one entry for each of the members of the chosen dimensions. If the data file has any
#' numeric columns these are summed during the aggregation. \cr\cr
#' Here is an example that you can run to see the aggregation phase: \cr
#' \preformatted{
#' PrepareIPF(esoph, Dimensions = "agegp")
#' 	 - Data frame submitted.
#'   - Analysing Dimensions:
#'   - Discovered 3 possible dimensions.
#'   -    : { agegp,alcgp,tobgp }
#'   -    : { 6,4,4 }
#'   - The number of possible array coordinates is 96
#'   - Filtered to 1 chosen dimensions.
#'   -    : { agegp }
#'   - Aggregating Dimensions:
#'   - Duplicate records found so we combined cells using the sum() function.
#'
#' agegp
#' 25-34 35-44 45-54 55-64 65-74   75+
#'   1     9    46    76    55    13
#' }
#' If you repeat the above example with Dimensions = "tobgp" you will be able to
#' see that the sum of the metric generated is always the same because of this data
#' aggregation phase.
#' @section Analysing Metrics:
#' In the examples that we showed above you can see that the resulting array has been populated with a numeric
#' value. The function analyses the data that is submitted noting any columns that have numerical data.
#' If there are no columns having numeric data the function returns a unitary seed.  This is an array with
#' the selected dimensions having the value 1 in each of the array's cells. Here is an example: \cr
#' \preformatted{
#' PrepareIPF(esoph[c("alcgp", "tobgp")])
#' - Data frame submitted.
#' - Analysing Dimensions:
#' - Discovered 2 possible dimensions.
#' -    : { alcgp,tobgp }
#' -    : { 4,4 }
#' - The number of possible array coordinates is 16
#' - No dimension filter was supplied.
#' - The file has no numeric columns so a unitary seed will be returned.
#' - Producing array output.
#' alcgp
#' tobgp      0-39g/day 40-79 80-119 120+
#'   0-9g/day         1     1      1    1
#'   10-19            1     1      1    1
#'   20-29            1     1      1    1
#'   30+              1     1      1    1
#' }
#' If the file has numerical columns then the default behaviour is for the function to choose the
#' first column (reading from left to right) as the metric.
#' @section Defining Metrics:
#' If the default behaviour of unitary seed or first column is not what you want then this
#' can be overridden using the Metric parameter. The user can either give the name of a different
#' column, or indeed any valid calculation. \cr\cr
#' Here are some examples that you can run to see the defining metrics phase: \cr
#' \preformatted{
#' PrepareIPF(esoph, Metric = ncontrols)           #Select another column
#' PrepareIPF(esoph, Metric = ncases / ncontrols)  #Select a calculation
#' PrepareIPF(esoph, Metric = 1)                   #Force a unitary seed
#' }
#' @section Filling Coordinates:
#' Once all metric has been defined the system is almost ready to produce the array but first
#' it must ensure that all of the coordinates of the array have a value.  In order to do this the
#' system takes all of the possible values listed in each of the chosen dimensions and creates
#' a set of all possible combinations, the cartesian product of the dimensions. If the input data
#' has a value for each member of the resulting set then no more processing is required. If the data
#' has gaps then additional records are inserted into the file with NA as the value of the metric.
#' This is done to allow the user to distinguish between values that were submitted as zero and
#' values which were missing from the data. \cr\cr
#' Here is an example that you can run to see the filling coordinate phase: \cr
#' \preformatted{
#' PrepareIPF(esoph)
#' ...
#' ...
#' , , agegp = 75+
#'
#' alcgp
#' tobgp      0-39g/day 40-79 80-119 120+
#'   0-9g/day         1     2      1    2
#'   10-19            2     1      1    1
#'   20-29           NA     0     NA   NA
#'   30+              1     1     NA   NA
#' }
#' Here you can see a zero in \code{{tobgp = 20-29, alcgp = 40-79, agegp = 75+}} which existed
#' in the original file and you can see five NAs which were added during the filling coordinate
#' phase.
#' @section Creating an Array:
#' The final array is then filled.
#' \itemize{
#' \item The dimensions will be as defined above
#' \item The order of each dimension's members will be as per the original data when that data
#' has ordered factors or an alphanumeric sort.
#' \item The metric will be as defined above or a unitary seed.
#' }
#' The order of the dimensions in the array can be changed if the dimensions are defined using
#' the Dimensions parameter. \cr
#' The array built by the function makes full use of names. Every dimension has a name (e.g. alcgp, tobgp)
#' and the members of each dimension are also named (e.g. 10-19, 20-20, 30+). This is required by the
#' \code{\link{RunIPF}} function which will use these names to analyse how to apply targets and also
#' to identify inconsistencies between seed and targets and inconsistencies between targets.\cr\cr
#' \strong{CONVERTING FROM AN ARRAY TO A TABLE}
#' When the user submits an array to this function a data table will be created.
#' \itemize{
#' \item A column will be produced for each dimension.  These columns will be ordered factors.
#' \item A column called "value" will be prepared to hold the numbers in the cells of the array.
#' \item There will be a row for each of the array's coordinates. The row count should, therefore,
#' always be the cartesian product of the size of each dimension.
#' }
#' Here is an example to show how the function converts from an array back to a table: \cr
#' \preformatted{
#' array <- PrepareIPF(esoph)
#' PrepareIPF(array)
#'       tobgp     alcgp agegp value
#' 1: 0-9g/day 0-39g/day 25-34     0
#' 2:    10-19 0-39g/day 25-34     0
#' 3:    20-29 0-39g/day 25-34     0
#' 4:      30+ 0-39g/day 25-34     0
#' }
#' Note that applying PrepareIPF twice can be a useful technique.  The file esoph has 88 rows of data but
#' the file produced using \code{PrepareIPF(PrepareIPF(esoph))} has the same data but it has 96 rows
#' of data. 96 is the product of 6 times 4 times 4 which are the sizes of the dimensions.
#' @family Modeling Tools
#' @author JTA - The Data Scientists
#' @export
#' @seealso \code{\link{TAPChunks}}
PrepareIPF <-
  function(Data,
             Dimensions,
             Metric,
             Order = FALSE) {
    # CHECK PARAMETERS
    if (missing(Data)) {
      TAPChunks:::catError("This functions requires the Data parameter to be filled.")
    }
    if (missing(Dimensions)) Dimensions <- NULL
    if (missing(Metric)) Metric <- NULL
    # CHECK SUBMITTED DATA FORMAT
    # If the user submits a data frame then we should transfer it to a data table
    if (length(intersect(class(Data), "data.table")) == 1) {
      TAPChunks:::catInfo("Data table submitted. ")
    }
    if (length(intersect(class(Data), "data.frame")) == 1 &
      length(intersect(class(Data), "data.table")) == 0) {
      Data <- data.table::as.data.table(Data)
      TAPChunks:::catInfo("Data frame submitted. ")
    }
    # We now check that we have a data table so that we can continue
    if (length(intersect(class(Data), "data.table")) == 1) {
      # We have a data table to work from so we start to analyse it
      # CREATE A DATA DICTIONARY
      columns <- names(Data)
      classes <- NULL
      for (i in columns) {
        classes <- c(classes, paste(class(Data[[i]]), collapse = " "))
      }
      # For convenience we store the result in a data table
      data.dictionary <- data.table::data.table(columns, classes)
      data.dictionary$char <- sapply(Data, is.character)
      data.dictionary$fact <- sapply(Data, is.factor)
      data.dictionary$ordd <- sapply(Data, is.ordered)
      # IDENTIFY COLUMNS WHICH COULD BE DIMENSIONS AND CONSTRUCT DIMENSION SET
      # Possible dimensions will be those columns of class factor or char
      TAPChunks:::catInfo("Analysing Dimensions:", "Dimensions")

      poss.dimensions <-
        data.dictionary[(char | fact) & !(columns == "uuid")]$columns
      coun.dimensions <- length(poss.dimensions)

      # We now want to get the size of each dimension and the values in each dimension
      # For the values we establish an empty list and we will use an index to concatenate new values to the list
      dimension.names <- list()
      index <- 1
      for (i in poss.dimensions) {
        # We add the dimension values to the list
        dimension.names[[index]] <- unique(as.character(Data[[i]]))
        index <- index + 1
        # And we calculate the size of each dimension, recording the result in out data dictionary
        dim.size <- length(unique(Data[[i]]))
        data.dictionary[columns == i, n := dim.size]
      }

      # It helps users to attach the name of the original column to the sets of values
      names(dimension.names) <- poss.dimensions

      TAPChunks:::catInfo(sprintf("Discovered %i possible dimensions.", coun.dimensions))
      if (coun.dimensions == 0) {
        TAPChunks:::catError("The data must have at least one column with character strings or factors.")
      }
      TAPChunks:::catInfo(paste("   : {", paste(
        as.character(poss.dimensions),
        collapse = ","
      ), "}"))
      TAPChunks:::catInfo(paste("   : {", paste(
        as.character(data.dictionary[columns %in% poss.dimensions]$n),
        collapse = ","
      ), "}"))
      array.size <- prod(data.dictionary[columns %in% poss.dimensions]$n)
      TAPChunks:::catInfo(sprintf("The number of possible array coordinates is %i", array.size))

      # We want our dimensional data to be stored as an ordered factor
      # so we identify columns that are character only which will need to
      # be converted to a factor that is ordered and we identify factors
      # which have not yet been ordered.

      char.columns <- data.dictionary[char == T]$columns
      coun.char <- length(char.columns)
      fact.columns <- data.dictionary[fact == T & ordd == F & !(columns == "uuid")]$columns
      coun.fact <- length(fact.columns)

      if (coun.char > 0) {
        TAPChunks:::catInfo(sprintf("Discovered %i character columns that are not ordered.", coun.char))
        TAPChunks:::catInfo(paste("   : {", paste(
          as.character(char.columns),
          collapse = ","
        ), "}"))
        Data[, char.columns] <-
          lapply(Data[, char.columns, with = FALSE], as.ordered)
      }
      if (coun.fact > 0) {
        TAPChunks:::catInfo(sprintf("Discovered %i factors that are not ordered.", coun.fact))
        TAPChunks:::catInfo(paste("   : {", paste(
          as.character(fact.columns),
          collapse = ","
        ), "}"))
        Data[, fact.columns] <-
          lapply(Data[, fact.columns, with = FALSE], as.ordered)
      }
      if (coun.fact + coun.char > 0) {
        TAPChunks:::catInfo("These will be ordered for you. ")
      }

      # Refresh the data dictionary
      data.dictionary$char <- sapply(Data, is.character)
      data.dictionary$fact <- sapply(Data, is.factor)
      data.dictionary$ordd <- sapply(Data, is.ordered)

      # Now we have identified all possible dimensions we can apply the user's dimension list
      if (!is.null(Dimensions)) {
        if (length(intersect(Dimensions, poss.dimensions)) == 0) {
          TAPChunks:::catError("Items in the Dimensions parameter could not be found in the data.")
        }
        if (length(setdiff(Dimensions, poss.dimensions)) > 0) {
          TAPChunks:::catWarning("Items listed in the dimensions parameter not found in the data file.")
        }
        # Apply filter
        poss.dimensions <- intersect(poss.dimensions, Dimensions)
        Dimensions <- intersect(Dimensions, poss.dimensions)
        coun.dimensions <- length(poss.dimensions)
        array.size <- prod(data.dictionary[columns %in% poss.dimensions]$n)
        TAPChunks:::catInfo(sprintf("Filtered to %i chosen dimensions.", coun.dimensions))
        TAPChunks:::catInfo(paste("   : {", paste(
          as.character(poss.dimensions),
          collapse = ","
        ), "}"))
      } else {
        TAPChunks:::catInfo("No dimension filter was supplied.")
      }
      if (Order) {
        for (i in poss.dimensions) {
          TAPChunks:::catInfo(paste(i, ": Has the following order:"), i)
          TAPChunks:::catInfo(paste(dimension.names[[i]], collapse = " < "))
        }
      }
      rows <- nrow(Data)
      Metric <- substitute(Metric)

      TAPChunks:::catInfo("Aggregating Dimensions:", "Dimensions")

      # It is important to aggregate the data to the dimensionality before
      # we apply the metric definition. This is in case the definition
      # is a ratio which can no longer be aggregated.
      # Get all numeric columns
      cols <- sapply(Data, is.numeric)
      cols <- names(cols)[cols]
      if (length(cols) != 0) {
        Data <- Data[, lapply(.SD, sum), .SDcols = cols, keyby = poss.dimensions]
      }
      if (nrow(Data) < rows) {
        TAPChunks:::catInfo("Duplicate records found so we combined cells using the sum() function.")
      }

      # DECIDE ON THE METRIC
      TAPChunks:::catInfo("Analysing Metrics:", "Metrics")

      if (is.null(Metric)) {
        # Possible metrics are numeric values
        poss.metrics <-
          data.dictionary[grepl("numeric|integer", classes)]$columns
        coun.metrics <- length(poss.metrics)
        TAPChunks:::catInfo(sprintf("Discovered %i possible metrics.", coun.metrics))
        TAPChunks:::catInfo(paste("   : {", paste(
          as.character(poss.metrics),
          collapse = ","
        ), "}"))
        if (length(poss.metrics) == 0) {
          TAPChunks:::catInfo("The file has no numeric columns so a unitary seed will be returned."
                               )
          Data <-
            Data[, .(ipf_Met = 1), keyby = poss.dimensions]
        } else {
          TAPChunks:::catInfo(sprintf("The first one: %s will be used.", poss.metrics[1]),
                              Highlight = poss.metrics[1]
                              )
          Data <-
            Data[, .(ipf_Met = sum(get(poss.metrics[1]))), keyby = poss.dimensions]
        }
      } else {
        e <- try({
          metric <- call("sum", Metric)
          Data <-
            Data[, .(ipf_Met = eval(Metric)), keyby = poss.dimensions]
        }
        ,
        silent = T
        )

        if (length(intersect(class(e), "data.table")) != 1) {
          e <- strsplit(e, ":")[[1]][2]
          TAPChunks:::catError(paste("Syntax error:", e, "."))
        } else {
          TAPChunks:::catInfo("Metric definition applied.")
        }
      }

      if (array.size < 500E3) {


        if (nrow(Data[ipf_Met < 0]) == 0) {
          # CREATE CROSS JOIN FULL SIZE TABLE
          # By now we have a data table that has the sum of the metric grouped by
          # the selected dimensions. Before we fill the n-dimensional array structure
          # we need to create the cross-join of the dimensions and expand the data to fill all points

          dimension.names <- dimension.names[poss.dimensions]

          for (i in names(dimension.names)) {
            dimension.names[[i]] <-
              factor(dimension.names[[i]], levels = dimension.names[[i]])
          }

          crossJoin <-
            do.call(data.table::CJ, dimension.names)

          Data <- Data[crossJoin]

          colnames <- intersect(names(Data), poss.dimensions)
          poss.dimensions <- colnames[length(colnames):1]

          dimension.names <- dimension.names[length(colnames):1]

          if (nrow(Data) > rows) {
            TAPChunks:::catInfo(
              "Data was not supplied for all combinations of the dimensions so we filled the array with NAs. "
            )
          }
        }

        dimdata <- unlist(lapply(dimension.names, length))

        TAPChunks:::catInfo("Producing array output.")

        setindexv(Data, names(dimension.names))

        # TRANSFORM INTO ARRAY FORM
        M <- array(data = Data$ipf_Met, dim = dimdata, dimension.names)
        if (!is.null(Dimensions)) {
          M <- aperm(M, Dimensions)
        }
        return(M)
      } else {
        TAPChunks:::catError("There are over 500k coordinates. Restrict dimensions.  ")
      }
      TAPChunks:::catError("The data contains negative values.")
    } else {
      if (class(Data) == "array" || class(Data) == "matrix") {
        table <- data.table::setDT(data.table::melt(Data))
        for (i in names(dimnames(Data))) {
          table[[i]] <- factor(table[[i]], levels = dimnames(Data)[[i]], ordered = TRUE)
        }

        return(table)
      } else {
        TAPChunks:::catError("Data submitted is neither a data table nor data frame nor an array.")
      }
    }
  }
