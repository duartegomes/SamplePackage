#' @title Recreate ReadADLChunk expressions for the Iterate Wrapper
#' @description This is an internal function that is used by the iteration wrapper to
#' edit a user's script replacing any periods with the period that is required by the wrapper function.
#' The wrapper function is IterateScript().
#' @param Exp This is an R expression that will have any periods replaced by the Period
#' @param Period This is a string with a standard TAP period in the format "2018M01" that
#' will be used to replace any existing periods passed in the expression.
#' @return An object of class expression which, when run, should perform the same
#' as the expression send in the Exp parameter but for the Period defined in the Period parameter.
#' @examples TAPChunks:::RecreateReadADLChunkExpression(Exp = parse(text = "Object <- ReadADLChunk(Source = 'Email', From = '2017M01')"), Period = "2018M02")
#' @author JTA: The Data Scientists
#' @family Internal Utilities
#' @keywords internal
RecreateReadADLChunkExpression <- function(Exp, Period) {
  block <- as.data.table(getParseData(parse(text = as.character(Exp))))

  # Parameter of function
  parameters <- names(as.list(args(ReadADLChunk)))
  parameters <- parameters[-length(parameters)]

  parameters <- data.table::data.table(order = 1:length(parameters), parameters)

  # Defined parameters
  parametersDefine <-
    block[, .(token,
      parameters = text,
      value = shift(text, 2, type = "lead")
    )][token %in% "SYMBOL_SUB"]
  parameters <-
    merge(parameters, parametersDefine[, .(parameters, value)], all.x = T)[order(order)]
  # No Defined parameters
  parametersNonDefine <-
    block[, .(token, value = text, parameters = shift(token, 2))][token %in% c("STR_CONST", "SYMBOL") &
      parameters != "SYMBOL_SUB"]
  parametersNonDefine$order <-
    parameters[is.na(value)]$order[1:nrow(parametersNonDefine)]


  # Detect parameters
  parameters <-
    merge(
      parameters,
      parametersNonDefine[, .(order, value)],
      all.x = T,
      by = "order",
      suffixes = c("", ".nonP")
    )
  parameters <-
    parameters[is.na(value), value := value.nonP][, .SD, .SDcols = !"value.nonP"]

  parameters[parameters %in% c("To", "From"), value := sprintf("\"%s\"", Period)]
  parameters <- parameters[!is.na(value) & parameters != "Iterate"]


  TAPChunks:::ShowADLPath(parse(text = parameters[parameters == "Source", value]))


  result <- paste(
    paste(block[1:grep("ReadADLChunk", text)]$text, collapse = ""),
    "(",
    paste(
      parameters$parameters,
      parameters$value,
      sep = " = ",
      collapse = ", "
    ),
    ")"
  )


  return(parse(text = result))
}
