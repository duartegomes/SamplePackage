#' @title Preparing data for IPF
#' @description The mathematics of running an Iterative Proportional Fit are not hard
#' to do but the problem needs to be properly set up.  The data that is sent to IPF tools
#' such as \code{\link{RunIPF}} has many strict requirements and this function ensures that
#' data is properly formatted for IPF.
#' @param data This can be a data table, a data frame or an array.  When the user presents a data table or data frame the system returns
#' an array in a format that is ready for the IPF function.  When the user presents an array the system returns a data table.
#' @param dimensions A vector of dimension names to include in the analysis
#' @param metric A calculation to be employed to generate the values which will be submitted to the analysis
#' @return When the user submits a data frame or a data table the function returns an array.  When the user submits an array the system
#' returns a data table.
#' @details
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
#' @section Analysing Dimensions:
#' @section Defining Dimensions:
#' @section Analysing Metrics:
#' @section Defining Metrics:
#' @section Aggregating Data:
#' @section Filling Coordinates:
#' @section Creating an Array:
#'
#' @family Iterative Proportional Fitting Tools
#' @author JTA - The Data Scientists
#' @export
#' @seealso \code{\link{TAPChunks}}
PrepareIPF <-
  function(data       = NULL,
           dimensions = NULL,
           metric     = NULL,
           order      = FALSE) {
    # CHECK SUBMITTED DATA FORMAT
    # If the user submits a data frame then we should transfer it to a data table
    if (length(intersect(class(data), "data.table")) == 1) {
      TAPChunks::catInfo("Data table submitted. ")
    }
    if (length(intersect(class(data), "data.frame")) == 1 &
        length(intersect(class(data), "data.table")) == 0) {
      TAPChunks::catInfo("Data frame submitted. ")
      data <- data.table::as.data.table(data)
    }
    # We now check that we have a data table so that we can continue
    if (length(intersect(class(data), "data.table")) == 1) {
      # We have a data table to work from so we start to analyse it
      # CREATE A DATA DICTIONARY
      columns <- names(data)
      classes <- NULL
      for (i in columns) {
        classes <- c(classes, paste(class(data[[i]]), collapse = " "))
      }
      # For convenience we store the result in a data table
      data.dictionary      <- data.table::data.table(columns, classes)
      data.dictionary$char <- sapply(data, is.character)
      data.dictionary$fact <- sapply(data, is.factor)
      data.dictionary$ordd <- sapply(data, is.ordered)
      # IDENTIFY COLUMNS WHICH COULD BE DIMENSIONS AND CONSTRUCT DIMENSION SET
      # Possible dimensions will be those columns of class factor or char
      TAPChunks::catInfo("Analysing Dimensions:", "Dimensions")

      poss.dimensions <-
        data.dictionary[(char | fact) & !(columns == "uuid")]$columns
      coun.dimensions <- length(poss.dimensions)

      # We must have at least two dimensions to continue
      if (coun.dimensions < 2) {
        TAPChunks::catError("Need at least two dimensions to continue.")
      }
      # We now want to get the size of each dimension and the values in each dimension
      # For the values we establish an empty list and we will use an index to concatenate new values to the list
      dimension.names <- list()
      index <- 1
      for (i in poss.dimensions) {
        # We add the dimension values to the list
        dimension.names[[index]] <- unique(as.character(data[[i]]))
        index <- index + 1
        # And we calculate the size of each dimension, recording the result in out data dictionary
        dim.size <- length(unique(data[[i]]))
        data.dictionary[columns == i, n := dim.size]
      }

      # It helps users to attach the name of the original column to the sets of values
      names(dimension.names) <- poss.dimensions

      TAPChunks::catInfo(sprintf("Discovered %i possible dimensions.", coun.dimensions))
      TAPChunks::catInfo(paste("   : {", paste(
        as.character(poss.dimensions), collapse = ","
      ), "}"))
      TAPChunks::catInfo(paste("   : {", paste(
        as.character(data.dictionary[columns %in% poss.dimensions]$n), collapse = ","
      ), "}"))
      array.size <- prod(data.dictionary[columns %in% poss.dimensions]$n)
      TAPChunks::catInfo(sprintf("The number of possible array coordinates is %i", array.size))

      # We want our dimensional data to be stored as an ordered factor
      # so we identify columns that are character only which will need to
      # be converted to a factor that is ordered and we identify factors
      # which have not yet been ordered.

      char.columns <- data.dictionary[char == T]$columns
      coun.char    <- length(char.columns)
      fact.columns <- data.dictionary[fact == T & ordd == F & !(columns == "uuid")]$columns
      coun.fact    <- length(fact.columns)

      if(coun.char > 0){
        TAPChunks::catInfo(sprintf("Discovered %i character columns that are not ordered.", coun.char))
        TAPChunks::catInfo(paste("   : {", paste(
          as.character(char.columns), collapse = ","
        ), "}"))
        data[, char.columns] <-
          lapply(data[,char.columns, with = FALSE], as.ordered)
      }
      if(coun.fact > 0){
        TAPChunks::catInfo(sprintf("Discovered %i factors that are not ordered.", coun.fact))
        TAPChunks::catInfo(paste("   : {", paste(
          as.character(fact.columns), collapse = ","
        ), "}"))
        data[, fact.columns] <-
          lapply(data[,fact.columns, with = FALSE], as.ordered)

      }
      if(coun.fact+coun.char > 0){
        TAPChunks::catInfo("These will be ordered for you. ")
      }

      # Refresh the data dictionary
      data.dictionary$char <- sapply(data, is.character)
      data.dictionary$fact <- sapply(data, is.factor)
      data.dictionary$ordd <- sapply(data, is.ordered)

      # Now we have identified all possible dimensions we can apply the user's dimension list
      if (!is.null(dimensions)) {
        if (length(setdiff(dimensions, poss.dimensions)) > 0)
          TAPChunks::catWarning("Items listed in the dimensions parameter not found in the data file.")
        # Apply filter
        poss.dimensions <- intersect(poss.dimensions, dimensions)
        coun.dimensions <- length(poss.dimensions)
        array.size      <- prod(data.dictionary[columns %in% poss.dimensions]$n)
        TAPChunks:::catInfo(sprintf("Filtered to %i chosen dimensions.", coun.dimensions))
        TAPChunks::catInfo(paste("   : {", paste(
          as.character(poss.dimensions), collapse = ","
        ), "}"))
      } else TAPChunks::catInfo("No dimension filter was supplied.")

      # We must have at least two dimensions to continue
      if (coun.dimensions < 2) {
        TAPChunks::catError("Need at least two dimensions to continue.")
      }

      if (order) {
        TAPChunks::catInfo("Ordering Dimensions:", "Dimensions")

        for (i in poss.dimensions) {
          TAPChunks::catInfo(paste(i, ": Has the following order:"), i)
          TAPChunks::catInfo(paste(dimension.names[[i]], collapse = " < "))
        }
      }

      # DECIDE ON THE METRIC AND SUMMARISE THE METRIC TO REQUIRED DIMENSIONS
      TAPChunks::catInfo("Analysing Metrics:", "Metrics")

      rows <- nrow(data)
      metric <- substitute(metric)

      # It is important to aggregate the data to the dimensionality before
      # we apply the metric definition. This is in case the definition
      # is a ratio which can no longer be aggregated.
      # Get all numeric columns
      cols <- sapply(data, is.numeric)
      cols <- names(cols)[cols]
      data <- data[, lapply(.SD, sum), .SDcols = cols, keyby = poss.dimensions]

      if (is.null(metric)) {
        # Possible metrics are numeric values
        poss.metrics    <-
          data.dictionary[grepl("numeric|integer", classes)]$columns
        coun.metrics    <- length(poss.metrics)
        TAPChunks:::catInfo(sprintf("Discovered %i possible metrics.", coun.metrics))
        TAPChunks::catInfo(paste("   : {", paste(
          as.character(poss.metrics), collapse = ","
        ), "}"))
        TAPChunks:::catInfo(sprintf("The first one: %s will be used." , poss.metrics[1]),
                            Highlight = poss.metrics[1])
        data <-
          data[, .(ipf_Met = sum(get(poss.metrics[1]))), keyby = poss.dimensions]
      } else {
        e <-  try({
          metric <- call("sum", metric)
          data <-
            data[, .(ipf_Met = eval(metric)), keyby = poss.dimensions]
        }
        , silent = T)

        if (length(intersect(class(e), "data.table")) != 1) {
          e <- strsplit(e, ":")[[1]][2]
          TAPChunks::catError(paste("Syntax error:", e, "."))
        } else TAPChunks::catInfo("Metric definition applied.")
      }


      if (array.size < 500E3) {
        TAPChunks::catInfo("Analysing Structure:", "Structure")
        if (nrow(data) < rows)
          TAPChunks::catInfo("Duplicate records found so we combined cells using the sum() function.")

        if (nrow(data[ipf_Met < 0]) == 0) {
          # CREATE CROSS JOIN FULL SIZE TABLE
          # By now we have a data table that has the sum of the metric grouped by
          # the selected dimensions. Before we fill the n-dimensional array structure
          # we need to create the cross-join of the dimensions and expand the data to fill all points

          dimension.names <- dimension.names[poss.dimensions]

          crossJoin <-
            do.call(data.table::CJ, dimension.names[poss.dimensions])
          data      <- data[crossJoin]

          if (nrow(data) > rows)
            TAPChunks::catInfo(
              "Data was not supplied for all combinations of the dimensions so we filled the array with NAs. "
            )

          dimdata   <- data.dictionary[columns %in% poss.dimensions]$n

          # TRANSFORM INTO ARRAY FORM
          M <-
            array(data = data$ipf_Met, dim = dimdata, dimension.names)

          return(M)
        } else TAPChunks::catError("The data contains negative values.")

      } else
        TAPChunks::catError("There are over 500k coordinates. Restrict dimensions.  ")

    } else {
      if (class(data) == "array" || class(data) == "matrix") {
        table <-
          do.call(data.table::CJ, dimnames(data))[, .(value = eval(as.call(lapply(
            c("[", "data", names(dimnames(data))), as.symbol
          )))), , keyby = c(names(dimnames(data)))]

        for(i in names(dimnames(data))){
          table[[i]] <- factor(table[[i]], levels = dimnames(data)[[i]], ordered = TRUE)
        }

        return(table)

      } else
        TAPChunks:::catError("Data submitted is neither a data table nor data frame nor an array.")
    }

  }

