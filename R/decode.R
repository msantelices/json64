#' Decoding Function
#'
#' Used to decode a base64 string. By default the function expects an encoded json.
#' @param str The string to be decoded.
#' @param json Defaults to TRUE. If TRUE, the function expects str to be an encoded json and will return a data.frame or list, depending on JSON structure. If FALSE, the function will return an string.
#' @keywords decode
#' @export
#' @examples
#' # Decode an encoded string:
#'
#' str <- "SGVsbG8gV29ybGQh"
#' j_decode(str, json = FALSE)
#'
#' # Decode an encoded json:
#'
#' encoded_json <- "W3sibXNnIjogIkhlbGxvIFdvcmxkISIsICJqc29uIjogdHJ1ZX1d"
#' j_decode(encoded_json)

j_decode <- function( str, json = TRUE ) {

  data <- rawToChar( jsonlite::base64_dec(str) )
  Encoding(data) <- "UTF-8"

  if(json) {
      res <- tryCatch(
        {
          res <- jsonlite::fromJSON(data)
        },
        error = function(e) {
          err <- "The input is not a valid encoded JSON. Set json argument to FALSE if you want to decode a string"
          stop( err, call. = FALSE )
        }
      )

  } else {
    res <- data
  }

  res

}
