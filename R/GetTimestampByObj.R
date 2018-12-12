#' @title GetTimestampByObj
#' @description Get the timestamp that already exists in the dataset
#' @param Exp Expression that has PublishADL
#' @author JTA - The Data Scientists
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
GetTimestampByObj <- function(Exp, obj) {

    # Align Parameters
    block <- as.data.table(getParseData(parse(text = as.character(Exp))))
    parameters <- as.list(args(PublishADL))

    parameters <- parameters[-length(parameters)]
    default <-sapply(parameters, function(x) if(x==""){NA}else{if(is.character(x)){paste0("\"",x, "\"")}else{as.character(x)}})

    parameters <- names(parameters)

    parameters <- data.table::data.table(order = 1:length(parameters), parameters,default)
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
    parameters <-
      parameters[is.na(value), value := default][, .SD, .SDcols = !"default"]

    DataParameters <- parameters[parameters == "Data", value]

    parameters[ is.na(value) & parameters == "ChunkName", value := DataParameters]
    # parameters[parameters== "Data", parameters := "ObjName" ]
    parameters <- parameters[parameters != "Replace"]
    parameters[ grep("\"", value, invert = T), value := paste0("\"", value, "\"") ]

    Iterate<-parameters[parameters=="Data", gsub("\"","",value)] %in% obj

    parameters <- rbind(parameters, data.table(order=5,parameters="Iterate",value=Iterate))

    InitializeEnvText <- paste(
      "GetTimestampByObjInternal",
      "(",
      paste(
        parameters[parameters != "Replace"]$parameters,
        parameters[parameters != "Replace"]$value,
        sep = " = ",
        collapse = ", "
      ),
      ")"
    )
    try(eval(parse(text = InitializeEnvText)), silent = T)
  }
