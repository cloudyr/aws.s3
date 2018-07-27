#' @title Delete object
#' @description Deletes one or more objects from an S3 bucket.
#'
#' @template object
#' @template bucket
#' @param quiet A logical indicating whether (when \code{object} is a list of multiple objects), to run the operation in \dQuote{quiet} mode. Ignored otherwise. See API documentation for details.
#' @template dots
#' @details \code{object} can be a single object key, an object of class \dQuote{s3_object}, or a list of either.
#'
#' @return \code{TRUE} if successful, otherwise an object of class aws_error details if not.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html}{API Documentation}
#' @seealso \code{\link{put_object}}
#' @importFrom digest digest
#' @importFrom base64enc base64encode
#' @importFrom xml2 read_xml write_xml xml_add_child
#' @export
delete_object <- function(object, bucket, quiet = TRUE, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    regionname <- get_region(bucket)
    object <- get_objectkey(object)
    if (length(object) == 1) {
        r <- s3HTTP(verb = "DELETE", 
                    bucket = bucket,
                    path = paste0("/", object),
                    ...)
        return(TRUE)
    } else {
        xml <- xml2::read_xml(paste0('<?xml version="1.0" encoding="UTF-8"?><Delete><Quiet>', tolower(quiet),'</Quiet></Delete>'))
        for (i in seq_along(object)) {
            xml2::xml_add_child(xml, xml2::read_xml(paste0("<Object><Key>", get_objectkey(object[[i]]), "</Key></Object>")))
        }
        tmpfile <- tempfile()
        on.exit(unlink(tmpfile))
        xml2::write_xml(xml, tmpfile)
        md <- base64enc::base64encode(digest::digest(file = tmpfile, raw = TRUE))
        r <- s3HTTP(verb = "POST", 
                    bucket = bucket,
                    query = list(delete = ""),
                    request_body = tmpfile,
                    headers = list(`Content-Length` = file.size(tmpfile), 
                                   `Content-MD5` = md), 
                    ...)
        return(TRUE)
    }
}
