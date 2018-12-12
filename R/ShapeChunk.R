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
#' @param Operation Sum, Count or Average
#' @examples ShapeChunk(TestEmailChunk, Variable = "variable", Value = "value")
#' @export
#' @author JTA - The Data Scientists
#' @return Returns a data table with shaped as user requested
#' @seealso \code{\link{TAPChunks}}
ShapeChunk <- function(Data, Variable = NULL, Value = NULL, Horizontal = T, Operation = "default") {

  if (missing(Data)) TAPChunks:::catError("Chunk not provided.")
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)

  if (is.null(Variable)) {
    TAPChunks:::catError("Please fill variable.")
  }

  if (length(intersect(Variable, names(Data))) == 0) {
    TAPChunks:::catError("Please provide a valid variable.")
  }

  if (length(intersect(Variable, names(Data))) >= 2 & Horizontal) {
    Variable <- Variable[1]
    TAPChunks:::catWarning(sprintf("You must select only one variable, by default is select: %s", Variable))
  }

  if (is.null(Value) & Horizontal) {
    TAPChunks:::catError("Please fill value.")
  }

  if (Horizontal) {
    if (length(intersect(Variable, names(Data))) == 0) {
      TAPChunks:::catError("Please select a valid value.")
    }

    columns <- setdiff(names(Data), c(Variable, Value))
    columns <- paste(columns, collapse = "+")

    Operation <- tolower(Operation)

    if (!Operation %in% c("sum", "count", "average")) {
      TAPChunks:::catWarning("The Operation parameter is not valid.", "Operation")
      TAPChunks:::catWarning("The Operation available are:\n\t\t- sum\n\t\t- average\n\t\t- count", "Operation available")
      TAPChunks:::catWarning("For default, sum was used to group the value")
      Operation <- "sum"
    }

    if (Operation == "sum") {
      shape.chunk <- data.table::dcast.data.table(data.table::data.table(Data),
                                                  as.formula(paste(columns, "~", Variable)),
                                                  value.var = Value, fun.aggregate = sum
      )
    } else {
      if (Operation == "average") {
        shape.chunk <- data.table::dcast.data.table(data.table::data.table(Data),
                                                    as.formula(paste(columns, "~", Variable)),
                                                    value.var = Value, fun.aggregate = mean
        )
      } else {
        if (Operation == "count") {
          shape.chunk <- data.table::dcast.data.table(data.table::data.table(Data),
                                                      as.formula(paste(columns, "~", Variable)),
                                                      value.var = Value, fun.aggregate = length
          )
        }
      }
    }
  } else {
    columns <- setdiff(names(Data), Variable)
    shape.chunk <- data.table::melt.data.table(data.table::data.table(Data),
                                               id.vars = columns,
                                               measure.vars = Variable
    )
  }

  shape.chunk[is.na(shape.chunk)] <- 0

  return(shape.chunk)
}
