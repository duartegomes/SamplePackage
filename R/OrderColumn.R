#' @title Sort a data chunk's columns to a defined order
#' @description Sort the columns in the data chunk
#' @param DT Chunk where the sort order will be applied
#' @family Internal Utilities
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
OrderColumn <- function(DT) {
  DT <- names(DT)

  begin <- c("row_id", "uuid", "server_roles_uuid", "timestamp")

  dimGeo <- c(
    "Area", "Region", "SubReg",
    "Subsidiary", "Country", "WCountry"
  )

  dimSeg <- c(
    "Sector", "SubSector", "Segment",
    "Segment_Code", "Vertical"
  )

  dimOS <- c(
    "Platform", "Architecture", "Ecosystem",
    "OS", "OS_Name", "MatchPattern_Name"
  )

  dimProd <- c(
    "ProductCategory", "ProductType", "CloudProductDetail",
    "Product2", "Product1", "Product0"
  )

  end <- c("variable", "value")

  all.dim <- c(dimGeo, dimSeg, dimOS, dimProd)
  part1 <- intersect(c(begin, all.dim), DT)
  part2 <- setdiff(DT, c(c(begin, all.dim, end)))
  part3 <- intersect(end, DT)

  return(c(part1, part2, part3))
}
