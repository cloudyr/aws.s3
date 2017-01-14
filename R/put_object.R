#' @title Put object
#' @description Puts an object into an S3 bucket
#'
#' @param file A character string containing the filename (or full path) of 
#' the file you want to upload to S3.
#' @template bucket
#' @param object A character string containing the name the object should 
#' have in S3 (i.e., its "object key"). If missing, the filename is used.
#' @template dots
#' @param headers List of request headers for the REST call.   
#'
#' @return If successful, \code{TRUE}, otherwise an aws_error object.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' @seealso \code{\link{delete_object}}
#' @export
put_object <- function(file, object, bucket, headers = list(), ...) {
    if (missing(object)) {
        object <- basename(file)
    } else {
        object <- get_objectkey(object)
    }
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = paste0('/', object),
                headers = c(headers, list(
                  `Content-Length` = ifelse(is.character(file) && file.exists(file), 
                                                       file.size(file), length(file))
                  )), 
                request_body = file,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

post_object <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "POST", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}
