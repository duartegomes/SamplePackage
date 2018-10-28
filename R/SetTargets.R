#' @title Preparing a set of targets that will be used in a Iterative Proportional Fit algorithm
#' @param ... A list of named vectors (one for each dimension) with the set of targets
#' @param tolerance A value (defaults to 1E-6 if not provided) that will be used to weaken the test that each set of targets sums to the same value
#' @export
#' @description A way to enter sets of targets to be used by the IPF function.
#' @details
#' @section Introduction:
#' The function \code{\link{RunIPF}} requires some seed data and a series of target sets.  This function allows users to easily type target sets or reference
#' pre-calculated targets.
#' @section Error Checking:
#' The function performs some error checking on the values to make sure that the IPF will function well.  Tests include: \cr
#' \itemize{
#' \item At least two sets of targets must be submitted.
#' \item The target sets must all be named so that the IPF can link each target set to the correct dimension.
#' \item All the values in the target sets are numeric data types.
#' \item All the numeric values must be positive.
#' \item All of the target sets must sum to a series of value that are within tolerance of each other.
#' }
#' @section Using the function:
#' Using the function is very easy: Supply a list of vectors and give each one a name that matches your dimension. The
#' vectors can be referenced using the \emph{set} syntax \code{set(10,20,30,40)} and the name is
#' given using the = character to assign the set to the dimension.  The full syntax is then
#' \code{Segment = set(10,20,30,40)}. The complete command would then be:\cr
#' \code{SetTargets(Segment = set(10,20,30,40), Vertical = set(25,25,50)), ...} \cr
#' The function can accept vectors calculated elsewhere like this:\cr
#' \code{SetTargets(Segment = seg.vect, Vertical = vert.vect, ...)}\cr
#' @section Ordering:
#' When we give a set of numbers in each vector, these numbers will refer to the
#' desired total for a particular member of the dimension.  In order that the syntax
#' not get over complicated the particular member is not mentioned.  Instead the system
#' applies the numbers of the vector to the members of the dimension in a particular order.
#' Most dimensions in TAPChunks (such as geography and segment) have a specified order
#' that the system will use.  Data that has other dimensions without a predefined order
#' will be sorted into alphabetical order before the IPF is run.  \cr
#' The function \emph{PrepareIPF} will tell you the order in which the values of
#' each dimension will be used.  \cr
#
#' @return A list of named target vectors that will be used by the \emph{RunIPF} function.
#' This function will perform some extra checks to ensure that the dimension names given can be seen in the data
#' and that the number of values in each set matches the unique count of values in the dimension
#' as presented by the data.
#' @family Iterative Proportional Fitting Tools
#' @seealso \code{\link{TAPChunks}}
#' @author JTA - The Data Scientists
#' @examples SetTargets(geo = set(1, 2), vertical = set(1, 1, 0.999), segment = set(1, 1, 1.001), tolerance = 2E-3)
#'
SetTargets <- function(..., tolerance = 1E-6) {
  t <- list(...)
  if (length(t) < 2)
    TAPChunks::catError("At least two sets of targets must be submitted. ")
  if (!all(lapply(t, is.numeric)))
    TAPChunks::catError("The sets can only contain numbers.")
  if (any(lapply(t, function(i) {if (min(i) < 0) return(TRUE) else return(FALSE)})))
    TAPChunks::catError("Negative values are not allowed,")

  v <- unlist(lapply(t, sum))

  TEST <- FALSE
  for (j in 2:length(v)) if (abs(v[j]-v[j-1]) >= tolerance) TEST <- TRUE

  if (TEST) {
    TAPChunks::catWarning("All the target sets must sum to similar values.")
    TAPChunks::catWarning("Specifically the difference between any sum and ")
    TAPChunks::catWarning("all the other sums must be less than the value ")
    TAPChunks::catWarning("supplied in the tolerance parameter.  ")
    TAPChunks::catError  ("Reconciliation Failed.")
  }
  if (is.null(names(t)) || any(names(t)==""))   TAPChunks::catError("The target sets should be named.")
  return(t)
}

#' @title An alias for c
#' @export
#' @description This provides an easy alias for the c() function to help our TAP users
#' @author JTA - The Data Scientists
set <- c
