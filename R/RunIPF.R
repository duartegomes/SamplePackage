#' @title Run an Iterative Proportional Fit
#' @description Function to take an n-dimensional array and fit it to a set of
#' target vectors.
#' @param M A multidimensional array (or matrix) containing the seed for the IPF.
#' @param target A set of targets, one for each dimension, that the resulting array
#' will conform to.
#' @param maxiter The maximum number of iterations that the function will attempt
#' before stopping work.  If this is not specified the default is 25.
#' @param tolerance How close the function needs to get to the targets before it
#' declares that convergence has happened.
#' @family Iterative Proportional Fitting Tools
#' @export
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}


RunIPF <- function(M,
                        target,
                        maxiter    = 25,
                        tolerance  = 1E-6) {
  if (class(M) == "array" | class(M) == "matrix") {
    dimdata    <- dim(M)
    size       <- length(dimdata)
    dimensions <- names(dimnames(M))
    dimtarget  <- NULL
    for (j in 1:length(target)) {
      dimtarget <- c(dimtarget, length(target[[j]]))
    }

    TAPChunks::catInfo(sprintf("%i dimensional array submitted.", size))
    TAPChunks::catInfo(paste("   : {", paste(as.character(dimensions), collapse = ","), "}"))

    if (size > 1) {
      # CHECK ON THE TARGETS
      if (all(names(target) == dimensions)) {
        if (all(dimdata == dimtarget)) {
          Converged <- FALSE

          # Now we run the IPF first we set up a loop
          # which has a maximum iteration limit
          N     <- M
          tol   <- round(-log10(tolerance))
          fmt   <- paste0("%1.",as.character(tol),"f")
          count <- 0
          while (!Converged & maxiter > count) {
            count <- count + 1
            # Calculate an update factor
            Test <- NULL
            for (k in 1:size){
              moveFactor <-
                target[[k]] / apply(N,
                                    MARGIN = k,
                                    FUN    = sum,
                                    na.rm  = TRUE)
              # We apply the factor to create a new iteration of the matrix
              N          <- sweep(N, k, moveFactor, FUN = "*")
              Test <- c(Test, round(sqrt(sum(moveFactor - 1) ^ 2 / size), tol))
            }
            TAPChunks::catInfo(sprintf(paste("Root mean square adjustments [Iteration: %3i] {", paste(sprintf(fmt, Test), collapse = ","),"}"), count))

            E <- M - N
            M <- N

            # Now we need to test for convergence and we will use n + 1
            # tests.  One is that the size of E, the error matrix, is
            # small. Sometimes the error matrix can be small but the
            # system oscillates when adjusting the dimensions. The
            # best way to test this is to examine the RMS of the adjustment vector - 1
            # As we get closer to convergenge these vectors
            # should not need to transform the data anymore and so they contain
            # all 1s, making the RMS zero.

              Test1     <- round(max(E, na.rm = T), tol) == 0
              Test2     <- all(Test == 0)

              Converged <- Test1 & Test2
          }
          if (count == maxiter) TAPChunks::catWarning("Convergence was not reached") else
            TAPChunks::catSuccess(sprintf("converged after %3i iterations.", count))
          return(M)


        } else {
          TAPChunks::catInfo (paste(
            "Dimension size per the data   : {",
            paste(as.character(dimdata), collapse = ","),
            "}"
          ))
          TAPChunks::catInfo (paste(
            "Dimension size per the targets: {",
            paste(as.character(dimtarget), collapse = ","),
            "}"
          ))
          TAPChunks::catError("The targets don't have the same size as the data dimensions.")
        }
      } else {
        TAPChunks::catError(
          "The targets don't have the same names, or the same order, as the dimensions of the IPF."
        )
      }
    } else TAPChunks::catError("There must be more than one dimension to the array.  ")
  } else       TAPChunks::catError("This function required the data to be submitted as an array. The function FormatKruithof will do this.")
}






