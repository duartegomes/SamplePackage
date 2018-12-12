#' @title Run an Iterative Proportional Fit
#' @description Function to take an n-dimensional array and fit it to a set of
#' target arrays.
#' @param Data A multidimensional array containing the seed for the IPF. This multidimensional
#' array should be produced using the \code{\link{PrepareIPF}} function.  It is important that the array
#' has names. Each individual dimension must have a name and the members of the dimensions must also
#' have names.
#' @param ... In addition to the seed the IPF will need to receive targets which are passed to the
#' function as a series of parameters.  These targets must also be arrays and should also be prepared using
#' \code{\link{PrepareIPF}} . It is common to supply a one dimensional target array for each of the
#' dimensions of the IPF but it is not necessary to restict targets to one dimension.
#' Targets can also be multidimensional arrays.  The user can submit any number of targets.
#' @param ReplaceZeros This parameter defines the value that will be used to replace
#' and empty or zero values found in the seed. If this is not specified the default is zero.
#' @param Maxiter The maximum number of iterations that the function will attempt (without convergence)
#' before stopping work.  If this is not specified the default is 25.
#' @param Tolerance This function is used in a variety of places to control the IPF:
#' \itemize{
#' \item The function checks that the values in each target must be similar in value. Specifically each
#' target is compared to all the other submitted targets. The absolute value of each of the differences
#' between targets must be below the value defined in Tolerance.
#' \item Targets can have overlapping dimensions and when this happens the function will look at
#' all of the cell coordinates in the overlap and compare them.  The absolute value of the differences
#' between cells in the target overlap must be below the value defined in Tolerance.
#' \item Tolerance is also used in the definition of convergence.  Convergence criteria are discussed in
#' more detail below.
#' }
#' If the user does not supply a value for Tolerance the default is 0.000001.
#' @details
#' Iterative Proportional Fitting ("IPF"), sometimes referred to as \emph{Raking}, is a procedure for
#' adjusting a table of data cells such that they add up to selected totals for both the columns
#' and rows (in the two-dimensional case) of the table. The procedure will also scale to any number of
#' dimensions. The unadjusted data cells may be referred to as the \emph{seed} cells, and the selected totals
#' may be referred to as the \emph{target} (or Marginal) values.
#'
#' \if{html}{\figure{IPF.jpg}{options: width="30\%" alt="Figure: IPF.jpg"}}
#' \if{latex}{\figure{IPF.pdf}{options: width="3in" alt="Figure: IPF.pdf"}}
#'
#' @section Validate the seed format:
#' The first operation undertaken by the function is to validate the seed array passed to the Data
#' parameter. These are the requirements checked: \itemize{
#' \item The data must be submitted as an array.
#' \item The data must have more than one dimension.
#' \item The dimensions in the data must have more than one member. For example, a two dimensional
#' array of size n by m could also be submitted as an object of three dimensions of size n by m by 1.
#' This extra dimension is clearly redundant as it does not change the data at all and so any dimensions
#' of size 1 will be rejected.
#' \item The dimensions of the array must all be named so the system can match up the dimensions in
#' the seed and the targets.
#' \item The members of each dimension must all be named so the system can check that dimensions are
#' consistent from seed to target and are all complete.
#' \item The cell values must be positive numbers or empty cells (Marked with NA by the system).}
#' @section Validate the target formats:
#' The targets submitted are also validated by the system: \itemize{
#' \item The targets must also be arrays.
#' \item There must be at least two targets submitted to the function.
#' \item The targets' values must be positive numbers.
#' \item Unlike the seed, the targets must not have any empty cells.}
#' @section Check targets reconcile:
#' Each target can have its cells summed to a total and this total must be the same for each target.
#' Because of inaccuracy in number representation the system will compare each target with every other
#' target and compare the differences with the Tolerance parameter.  All the differences between the
#' targets must be below Tolerance for the targets to be accepted.
#' @section Check seed dimensions are represented in the targets:
#' The seed will have a set of dimensions and all of these will need a target.  The targets together
#' must represent every dimension in the seed at least once. Targets can overlap and it is not a problem
#' to have a dimension represented in more than one target, although this will trigger further consistency tests. \cr\cr
#' The system will also test that the targets don't have dimensions that are not in the seed.
#' @section Check all members of target dimensions are present:
#' Having checked that the union of the targets' dimensions is the same as the seed's dimensions the system will
#' validate the members of those dimensions.  The members of each dimension, as listed in the seed, are compared
#' to the members of the same dimension, as listed in the targets. Every member in the seed must be in the
#' targets' dimension and, conversely, the targets' dimensions must not refer to a member that is not in the seed.
#' @section Ensure target dimension members are aligned with the seed:
#' The IPF calculations are optimized for speed by removing the need to constantly link seed cells to target cells
#' by name. The mechanism for doing this is to sort the target data into exactly the same order as the seed data.
#' This allows the system to superimpose dataset on top of one another and make direct calculations.
#' @section Analyse targets for overlaps:
#' Targets may be completely orthogonal; this means that an n dimensional seed will have n targets of one dimension, but
#' this is not necessarily the case. Targets may overlap across one or many dimensions. Consider this example: \cr\cr
#' \preformatted{
#' Seed Dimensions:      {Geography, Operating System, Product, Segment}
#' Target One Dimensions:{Geography, Operating System,          Segment}
#' Target Two Dimensions:{Geography,                   Product, Segment}
#' }
#' In this case we have a 4D seed and two 3D targets. Because the union of the target sets is the same 4D
#' set as the seed this will converge (assuming the seed data is not scarce) providing the targets don't
#' contradict. To test this the function will consider the intersection of the targets which is a 2D plane
#' with Geography and Segment data. In order to check consistency, the data in the 3D targets is projected
#' onto the 2D plane and the resulting two planes are compared. The function ensures that every cell in the
#' 2D overlap plane is the same in the projections from both targets.
#' @section Data cleaning:
#' The seed may contain blank cells but these will now be replaced by zero numbers. This is done so that the
#' seed is homogenous and only has positive numbers or zeros. The function has a parameter \emph{ReplaceZeros}
#' and if the user has passed a positive number to this parameter the value is used to replace all the zeros in
#' the seed.
#' @section Report on data scarcity:
#' If the user has not opted to replace zeros then the data in the seed may be scarce. In order to guide the
#' user the function will now report on the scarcity. It does this by projecting the seed onto each dimension
#' in turn and it reports three metrics: \itemize{
#' \item It will report how many of the dimension's members have at least one zero or empty cell.
#' \item It then calculates, for each member of the dimension, the percentage of zero cells and reports the
#' minimum and maximum percentages found on the dimension.}
#' Here is an example: \cr
#' \preformatted{
#' - Data Dimension database has zero or blank values in 9 of 9 members.
#' - The incidence of zero or blank members ranges from 0.46 % to 28.70 %
#' - Data Dimension OS.id has zero or blank values in 8 of 8 members.
#' - The incidence of zero or blank members ranges from 4.94 % to 20.58 %
#' - Data Dimension variable has zero or blank values in 3 of 3 members.
#' - The incidence of zero or blank members ranges from 15.28 % to 15.28 %
#' - Data Dimension country.id has zero or blank values in 27 of 27 members.
#' - The incidence of zero or blank members ranges from 1.39 % to 50.00 %
#' }
#' @section Run the IPF:
#' The system will now start to iterate the seed to fit the data to the targets in turn.
#' As the iterations happen the system will report the status with reducing frequency. It
#' reports the first ten iterations and then reports every tenth iteration until iteration 100 when
#' it reports every hundredth and so on. \cr
#' Here is an example of a 4D seed being fitted to 2 targets. The report shows, for each iteration,
#' a number for each target submitted. The value shown is formed by summing the seed over the target's
#' dimensions and calculating the difference between summed seed and the target.  This difference is
#' itself a multidimensional array and, in order to present the difference as one number, the cells are squared,
#' averaged and then the square root taken.
#' \preformatted{
#'	- 4 dimensional array submitted.
#' -    : { database,OS.id,variable,country.id }
#' - Root mean square error [Iteration:     1] { 309027.031,263745.041 }
#' - Root mean square error [Iteration:     2] { 25555.726,18885.891 }
#' - Root mean square error [Iteration:     3] { 3154.598,3211.195 }
#' - Root mean square error [Iteration:     4] { 632.098,734.602 }
#' - Root mean square error [Iteration:     5] { 159.844,201.340 }
#' - Root mean square error [Iteration:     6] { 46.948,62.417 }
#' - Root mean square error [Iteration:     7] { 15.245,21.053 }
#' - Root mean square error [Iteration:     8] { 5.297,7.526 }
#' - Root mean square error [Iteration:     9] { 1.927,2.803 }
#' - Root mean square error [Iteration:    10] { 0.725,1.076 }
#' - Root mean square error [Iteration:    20] { 0.000,0.000 }
#' Success:
#'   - converged after  24 iterations.
#' }
#' @section Convergence criteria:
#' In the above report we saw that the system calculates the Root Mean Squared ("RMS") error when the
#' seed is compared to each target after every iteration. This calculation forms one part of
#' convergence testing. Here are the two criteria: \itemize{
#' \item The RMS for each target must be less than the value in Tolerance. This ensures that the
#' IPF has found solutions where the seed's margin agrees to each target. This is not quite sufficient
#' to claim convergence, however. Each iteration comprises a set of sub-iterations, one for each target.
#' A situation can arise where the seed oscillates between different states between sub-iterations but
#' returns to the same state at the end of the iteration. This can make all the RMS values
#' (which are calculated at sub-iteration ends) acceptable but there may not be one single unique solution.
#' Instead there are n solutions, where n is the number of targets.
#' \item To detect oscillations the system measures the change in the seed from one sub-iteration to the next
#' and convergence is only claimed when there is no movement between sub-iterations as well as between iterations.
#' }
#' @section Non Convergence:
#' If the function completes the same number of iterations as MaxIter then convergence has not happened and the
#' system will stop and will warn about non-convergence. When this happens the function returns the result as
#' at the last iteration.  This will allow you to continue further iterations without having to repeat what
#' was done before. Simply return the output back to the RunIPF function as the next seed with the same targets.
#' @family Modeling Tools
#' @export
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
RunIPF <- function(Data,
                   ...,
                   ReplaceZeros = 0,
                   Maxiter = 25,
                   Tolerance = 1E-6) {
  if (missing(Data)) {
    TAPChunks:::catError("This function requires an array to be submitted to the Data parameter.")
  }
  if (class(ReplaceZeros) != "numeric") {
    TAPChunks:::catError("The parameter ReplaceZeros must be numeric.", "ReplaceZeros")
  }
  if (sign(ReplaceZeros) == -1) {
    TAPChunks:::catError("The parameter ReplaceZeros must be positive.", "ReplaceZeros")
  }
  if (class(Maxiter) != "numeric") {
    TAPChunks:::catError("The parameter Maxiter must be numeric.", "Maxiter")
  }
  Maxiter <- as.integer(Maxiter)

  if (class(Tolerance) != "numeric") {
    TAPChunks:::catError("The parameter Tolerance must be numeric.", "Tolerance")
  }
  if (Tolerance < 0) {
    TAPChunks:::catError("The parameter Tolerance must be positive.", "Tolerance")
  }

  # We will check that the data submitted to the function is a data array.
  # A matrix in R is equivalent to a two dimensional array so we can also allow this
  # class.
  if (!(class(Data) == "array" | class(Data) == "matrix")) {
    TAPChunks:::catError("This function requires the data to be submitted as an array or a matrix.")
  }
  if (any(s[!is.na(s)] < 0)) {
    TAPChunks:::catError("The seed cannot contain negative numbers. ")
  }
  dimdata <- dim(Data)
  size <- length(dimdata)
  # It is possible to submit a one dimensional seed and so we must test that
  # the data has at least two dimensions.
  if (size < 2) {
    TAPChunks:::catError("The seed array must have more than one dimension.")
  }
  # It also doesn't make sense to submit
  # a multidimensional array where one of the dimensions has only one member
  # (a 5 x 1 array may LOOK multidimensional BUT it is equivalent to a vector, which isn't!)
  if (any(dimdata == 1)) {
    TAPChunks:::catError("The seed array must not contain dimensions with one element.")
  }
  dimensions <- names(dimnames(Data))
  # We also record the number of members in each dimension, the number of
  # dimensions and the names of those dimensions, checking that names were supplied.
  if (is.null(dimensions)) {
    TAPChunks:::catError("The seed array's dimensions must be named.")
  }
  if (any(dimensions == "")) {
    TAPChunks:::catError("Not all of the array's dimensions are named.")
  }
  # We also test that names have been given to all of the members of each dimension
  for (i in 1:size) {
    if (any(dimnames(Data)[[i]] == "")) {
      TAPChunks:::catError("All of the members of the seed array's dimensions must be named.")
    }
  }
  # The targets are passed using ellipsis so a user is not limited to sending any
  # particular number of targets. Targets do need to be carefully validated against
  # the data and also against each other to ensure that we have data consistency.
  # The first set of tests make sure that the targets contain only positive numeric objects
  Target <- list(...)
  numTargets <- length(Target)

  if (any(!unlist(lapply(Target, class)) %in% c("matrix", "array"))) {
    TAPChunks:::catError("All the targets must be of class array or matrix.  ")
  }
  if (numTargets < 2) {
    TAPChunks:::catError("At least two target arrays must be submitted. ")
  }
  if (!all(unlist(lapply(Target, is.numeric)))) {
    TAPChunks:::catError("The target arrays can only contain numbers.")
  }
  if (any(unlist(lapply(Target, is.na)))) {
    TAPChunks:::catError("The target arrays can not have NAs.")
  }
  if (any(unlist(lapply(lapply(Target, sign), min)) == -1)) {
    TAPChunks:::catError("The target arrays must not have negative numbers.")
  }

  # The next set of tests ensure that the targets sum to the same value.  Because
  # of limitations in floating point arithmetic we must consider that is not possible
  # to provide values that test as exactly equal. Instead the function has a tolerance
  # parameter and we test that no two targets differ by more than the tolerance set. We
  # start by creating a vector of the targets' sums.
  v <- unlist(lapply(Target, sum))
  # We now compare the differences in the totals over all the combinations of targets
  # This results in n(n-1)/2 distinct tests
  TEST <- FALSE
  for (i in 1:(numTargets - 1)) {
    for (j in (i + 1):numTargets) {
      if (abs(v[j] - v[i]) >= Tolerance) {
        TEST <- TRUE
        TAPChunks:::catInfo(sprintf("The sum of target %i is %4.2f", i, v[i]))
        TAPChunks:::catInfo(sprintf("The sum of target %i is %4.2f", j, v[j]))
      }
    }
  }
  # If the test fails then we report this and stop
  if (TEST) {
    TAPChunks:::catWarning("The totals of the target arrays are not numerically consistent.")
    TAPChunks:::catWarning("Specifically, at least one pair of targets have totals that differ")
    TAPChunks:::catWarning("by a value exceding the value of the Tolerance parameter.")
    TAPChunks:::catError("Reconciliation Failed.")
  }

  # Now we test some aspects of data consistency.  We start with the names of dimensions that exist
  # in the data and we must ensure that every one of them is represented (at least once) in the
  # targets.
  for (i in dimensions) {
    dimension.found <- FALSE
    for (j in 1:numTargets) {
      if (length(intersect(i, names(dimnames(Target[[j]])))) == 1) {
        # We have found the dimension represented in one of the targets
        dimension.found <- TRUE
      }
    }
    if (!dimension.found) {
      TAPChunks:::catError(sprintf("The dimension %s exists in the data but was not found in any of the targets.", i))
    }
  }
  # We must also consider the converse: No target can contain a dimension that is not named in the seed
  for (j in 1:numTargets) {
    dimension.error <- FALSE
    extra <- setdiff(names(dimnames(Target[[j]])), dimensions)
    if (length(extra) != 0) {
      TAPChunks:::catError(sprintf("Unknown dimension found in target %i:%s", j, extra))
    }
  }
  # We must also test for targets with single member dimensions
  for (j in 1:numTargets) {
    if (any(dim(Target[[j]]) == 1)) {
      TAPChunks:::catError("Targets cannot contain dimensions with one element.")
    }
  }
  # If any target has the same number of dimensions as the seed then you don't need an IPF!
  for (j in 1:numTargets) {
    if (length(dim(Target[[j]])) == size) {
      TAPChunks:::catError("Found a target with the same number of dimensions as the seed. This renders the IPF trivial.")
    }
  }
  # The targets contain dimensions and these dimensions have members. We must now consider each target
  # and each dimension inside that target and compare the list of members present with our seed.
  # All the members in the target's dimension must exist in the seed and, conversely, all the members
  # in the seed's dimension must exist in the target.
  dimension.consistency <- FALSE
  for (i in 1:numTargets) {
    target.dimensions <- names(dimnames(Target[[i]]))
    for (j in target.dimensions) {
      targ.members <- dimnames(Target[[i]])[[j]]
      seed.members <- attributes(Data)[["dimnames"]][[j]]
      noti.targ <- setdiff(seed.members, targ.members)
      noti.seed <- setdiff(targ.members, seed.members)
      if (length(noti.targ) != 0) {
        TAPChunks:::catWarning(sprintf("Target %i is missing members from dimension %s:", i, j))
        TAPChunks:::catWarning(noti.targ)
        dimension.consistency <- TRUE
      }
      if (length(noti.seed) != 0) {
        TAPChunks:::catWarning(sprintf("Target %i has unknown members in dimension %s:", i, j))
        TAPChunks:::catWarning(noti.seed)
        dimension.consistency <- TRUE
      }
    }
  }
  if (dimension.consistency) {
    TAPChunks:::catError("Comparison of the target's dimensions with the seed failed. ")
  }

  # At this stage we know that all the dimensions in the seed are represented in the targets
  # and all the dimensions in the targets also exist in the seed.  We still need to make a
  # check on the order that the elements of the dimensions are placed in the seed and the
  # targets.  This order must be the same in both seed and target.  For example, if the seed
  # array has three columns with names UK, USA and France we must be sure that the target
  # values are presented in the same order. So for each target we consider each dimension within it
  # and sort the data using names in the seed as a reference.
  for (i in 1:numTargets) {
    target.dimensions <- names(dimnames(Target[[i]]))
    targsize <- length(target.dimensions)
    for (j in 1:targsize) {
      # We look at the members of this dimension in the seed as our sort reference
      seed.members <- dimnames(Data)[[target.dimensions[j]]]
      # We look at the members of this dimension in the target also
      targ.members <- dimnames(Target[[i]])[[target.dimensions[j]]]
      # We produce a sort order by applying a factor to the target using the seed as reference
      sort <- order(factor(targ.members, levels = seed.members))
      # The tricky thing now is that the syntax of the command differs according to
      # the total number of dimensions in the target and the position that the particular
      # dimension has in the target. Because of this we build the code to sort dynamically.
      # The basic code to sort is Target <- Target[sort] and this will work for a one dimensional target.
      # If the target has n dimensions then then the code is Target <- Target[sort,,,,]
      # where the total number of commas is (n-1).
      # If we are sorting a dimension that is not the first then the code is
      # Target <- Target[,,sort,,] where the number of commas preceding the sort depends on the
      # position of the dimension in the target
      pre <- paste0(rep(",", j - 1), collapse = "")
      pst <- paste0(rep(",", targsize - j), collapse = "")
      srt <- paste0(as.character(sort), collapse = ",")
      sort.code <- paste0("Target[[",i,"]][", pre, "c(", srt, ")", pst, "]")
      # TAPChunks:::catInfo(sort.code) # Use this for debugging the dynamic code
      Target[[i]] <- eval(parse(text = sort.code))
    }
  }

  # We now have some thoughts on target overlaps.
  # Targets can be completely distinct or may overlap in which case we must have consistency
  # so we analyse the overlaps to make sure that the targets don't contradict.
  contradict.test <- FALSE
  for (i in 1:(numTargets - 1)) {
    i.names <- names(dimnames(Target[[i]]))
    for (j in (i + 1):numTargets) {
      j.names <- names(dimnames(Target[[j]]))
      ijconnection <- intersect(i.names, j.names)
      if (length(ijconnection) != 0) {
        TAPChunks:::catInfo(
          sprintf("The dimension %s appears in both targets %i and %i.", ijconnection, i, j)
        )
        # Now that we have identified that the same dimension appears in more than one
        # target we must ensure that the values for that dimension are the same
        i.total <- apply(Target[[i]], MARGIN = ijconnection, FUN = sum)
        j.total <- apply(Target[[j]], MARGIN = ijconnection, FUN = sum)
        if (length(ijconnection) == 1){
          # To aid the error messages we compare each member in turn
          # We only do this when the overlap is one dimensional as otherwise
          # we could generate far too many error messages
          for (k in dimnames(Data)[[ijconnection]]) {
            if (abs(i.total[k] - j.total[k]) > Tolerance) {
              contradict.test <- TRUE
              TAPChunks:::catInfo(
                sprintf("The target for %s in dimension %s is inconsistent between targets", k, ijconnection)
              )
              TAPChunks:::catInfo(
                sprintf("The value in target %i is %4.3f", i, i.total[k])
              )
              TAPChunks:::catInfo(
                sprintf("The value in target %i is %4.3f", j, j.total[k])
              )
            }
          }
        } else {
          if (any(abs(i.total - j.total) > Tolerance)){
            mes <- paste0(ijconnection, collapse = ",")
            TAPChunks:::catInfo(
              sprintf("Targets %i and %j overlap across {%s} but the values are not consistent",
                      i, j, mes)
            )
            contradict.test <- TRUE
          }
        }

      }
    }
  }

  if (contradict.test) {TAPChunks:::catError("Targets are not consistent over repeated dimensions.")}

  scarcity <- function(v) {
    sum(ifelse(v == 0, 1, 0)) / length(v)
  }

  # We now perform any data cleaning that we need
  if (any(unlist(lapply(Data, is.na))) & ReplaceZeros == 0) {
    TAPChunks:::catInfo("The seed array contains empty values.  These will be replaced")
    TAPChunks:::catInfo("by zeros. This will not alter the calculation.")
    Data[is.na(Data)] <- 0
  }

  if (any(unlist(lapply(Data, is.na))) & ReplaceZeros != 0) {
    TAPChunks:::catInfo("The seed array contains empty values.  These will be replaced")
    TAPChunks:::catInfo(sprintf("by the value you specified in the parameter ReplaceZeros (%4.3f),", ReplaceZeros))
    TAPChunks:::catInfo("any other zero values in the submitted data will also be replaced.")
    Data[is.na(Data)] <- ReplaceZeros
    Data[Data == 0] <- ReplaceZeros
  }

  for (i in dimensions) {
    dimscarcity <- apply(Data, MARGIN = i, FUN = scarcity)
    if (sum(as.numeric(dimscarcity == 0)) == length(dimscarcity)) {
      TAPChunks:::catInfo(sprintf("Data Dimension %s has no zero or blank members.", i), i)
    } else {
      TAPChunks:::catInfo(
        sprintf(
          "Data Dimension %s has zero or blank values in %i of %i members.", i,
          sum(as.numeric(dimscarcity != 0)),
          length(dimscarcity)
        ), i
      )
      TAPChunks:::catInfo(
        sprintf(
          "The incidence of zero or blank members ranges from %4.2f %% to %4.2f %%",
          100 * min(dimscarcity[dimscarcity != 0]),
          100 * max(dimscarcity), i
        )
      )
    }
  }

  TAPChunks:::catInfo(sprintf("%i dimensional array submitted.", size))
  TAPChunks:::catInfo(paste("   : {", paste(as.character(dimensions), collapse = ","), "}"))

  Report <- function() {
    TAPChunks:::catInfo(sprintf(
      paste(
        "Root mean square error [Iteration: %5i] {",
        paste(sprintf(fmt, Test), collapse = ",")
        , "}"
      ),
      count
    ))
  }

  Converged <- FALSE
  # Now we run the IPF first we set up a loop
  # which has a maximum iteration limit
  N <- Data
  M <- Data
  tol <- round(-log10(Tolerance))
  fmt <- paste0("%1.", as.character(tol), "f")
  count <- 0
  while (!Converged & Maxiter > count) {
    count <- count + 1
    # Calculate an update factor
    Test <- NULL
    for (k in 1:numTargets) {
      #  We get the names of the dimensions that are present in the various targets
      marginC <- names(dimnames(Target[[k]]))
      # The apply function will accept margins as a character vector with names
      # but the sweep function won't.  For this reason we must convert the names
      # into dimension numbers
      marginN <- unlist(lapply(names(dimnames(Target[[k]])), grep, dimensions))
      SumData <- apply(N, MARGIN = marginC, FUN = sum, na.rm = TRUE)
      SumTarg <- apply(Target[[k]], MARGIN = marginC, FUN = sum, na.rm = TRUE)

      moveFactor <- SumTarg / SumData

      moveFactor[!is.finite(moveFactor)] <- 0
      # We apply the factor to create a new iteration of the matrix
      N <- sweep(N, marginN, moveFactor, FUN = "*")
      Test <- c(Test, round(sqrt(sum((SumTarg - SumData)^2) / length(SumData)), tol))
    }

    # We report progress with reducing frequency
    if (count < 10) Report()
    if (count < 100 & count %% 10 == 0) Report()
    if (count < 1000 & count %% 100 == 0) Report()
    if (count <= 10000 & count %% 1000 == 0) Report()
    if (count > 10000 & count %% 10000 == 0) Report()

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

    Test1 <- all(round(E, tol) == 0)
    Test2 <- all(Test == 0)

    Converged <- Test1 & Test2
  }
  if (count == Maxiter) {
    TAPChunks:::catWarning("Convergence was not reached. ")
    return(N)
  } else {
    Report()
    TAPChunks:::catSuccess(sprintf("converged after %3i iterations.", count))
    return(N)
  }
}
