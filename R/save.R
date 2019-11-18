#' Save file Function
#'
#' Used to save data to a file in the working directory using UTF-8 encoding.
#' @param data The data to be saved
#' @param filename A character string representing the name of the file.
#' @keywords write file
#' @export
#' @examples
#' # Write a JSON string to a UTF-8 encoded file
#'
#' data <- "[{msg: 'Hello R users!', type: 'example'}]"
#' j_save(data, 'data.json')

j_save <- function(data, filename) {

  if( class(filename) != 'character' ) {
    err <- 'Invalid filename. It must be a character string'
    stop( err, call. = FALSE )
  }

  tempFile <- file(filename, encoding = "UTF-8")
  write(data, file = tempFile)
  close(tempFile)
}
