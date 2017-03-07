#' @title Put object
#' @description Puts an object into an S3 bucket
#' @param file A character string containing the filename (or full path) of the file you want to upload to S3. Alternatively, an raw vector containing the file can be passed directly, in which case \code{object} needs to be specified explicitly.
#' @template bucket
#' @param object A character string containing the name the object should have in S3 (i.e., its "object key"). If missing, the filename is used.
#' @template dots
#' @param headers List of request headers for the REST call.   
#' @details This provide a generic interface for sending files (or serialized, in-memory representations thereof) to S3. Some convenience wrappers are provided for common tasks: \code{\link{s3save}} and \code{\link{s3saveRDS}}.
#' 
#' @return If successful, \code{TRUE}.
#' @examples
#' \dontrun{
#'   library("datasets")
#' 
#'   # write file to S3
#'   tmp <- tempfile()
#'   on.exit(unlink(tmp))
#'   utils::write.csv(mtcars, file = tmp)
#'   put_object(tmp, object = "mtcars.csv", bucket = "myexamplebucket")
#' 
#'   # write serialized, in-memory object to S3
#'   x <- rawConnection(raw(0), "w")
#'   utils::write.csv(mtcars, x)
#'   put_object(rawConnectionValue(x), object = "mtcars.csv", bucket = "myexamplebucketname")
#' 
#'   # alternative "S3 URI" syntax:
#'   put_object(rawConnectionValue(x), object = "s3://myexamplebucketname/mtcars.csv")
#'   close(x)
#' 
#'   # read the object back from S3
#'   read.csv(text = rawToChar(get_object(object = "s3://myexamplebucketname/mtcars.csv")))
#' }
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' @seealso \code{\link{put_bucket}}, \code{\link{get_object}}, \code{\link{delete_object}}
#' @export
put_object <- function(file, object, bucket, headers = list(), ...) {
    if (missing(object) && is.character(file)) {
        object <- basename(file)
    } else {
        if (missing(bucket)) {
            bucket <- get_bucketname(object)
        }
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
    return(TRUE)
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
    structure(r, class = "s3_object")
}
