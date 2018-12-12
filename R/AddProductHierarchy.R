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
#' @author JTA - The Data Scientists
#' @examples AddProductHierarchy(TestEmailChunk, "ProductType")
#' @seealso \code{\link{TAPChunks}}
AddProductHierarchy <- function(Data, SelectHierarchy = NULL) {
  if (is.null(SelectHierarchy)) {
    SelectHierarchy <- "Product"
  }
  TAPChunks:::AddDimension(Data = Data, Dimension = "P", SelectHierarchy)
}
