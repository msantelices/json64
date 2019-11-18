#' Encoding Function
#'
#' Used to encode a data.frame or list. By default, the output will be a base64 encoded JSON.
#' @param data A list or data.frame to encode.
#' @param json Defaults to TRUE. If TRUE, the output will be a base64 encoded JSON, else, the output will be an encoded string.
#' @keywords encode
#' @export
#' @examples
#' # Transform a data.frame to an encoded JSON string
#' df <- iris
#' encoded <- j_encode(df, json = TRUE)
#'

j_encode <- function( data, json = TRUE ) {

  if( class(data) == "list" ) {
    data <- as.data.frame.list(data)
  }

  str <- as.character( jsonlite::toJSON(data) )

  if(!json) {
    str <- substring(str, 3, nchar(str) - 2 )
  }

  res <- jsonlite::base64_enc(str)
  res <- gsub("\n", "", res)
  res

}
