#' @title Pass Data to an Excel file
#' @description This function takes the data chunk that is passed and
#' saves its content in \emph{Tab Separated Values format} in a temporary file stored in
#' the Cache. The system then asks the operating system to open the file in Excel. If you add
#' any logic to your excel you will need to save your work in .xlsx format.
#' Excel must be installed on your system.
#' @param Data Dataset to be opened in Excel
#' @export
#' @examples Xcel(TestEmailChunk)
#' @import data.table
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
#' @family Utilities
Xcel <- function(Data) {
  tFile <-

    tempfile(fileext = paste0(substitute(Data), ".tsv"), tmpdir = ShowCachePath())

  write.table(Data,
    tFile,
    row.names = F,
    sep = "\t",
    quote = F
  )

  shell(paste("start excel", tFile))
}
