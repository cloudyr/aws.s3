#' @rdname getobject
#' @title Get object
#' @description Retrieve an object from an S3 bucket
#' @template object
#' @template bucket
#' @param headers List of request headers for the REST call.
#' @template dots
#' @details \code{getobject} retrieves an object into memory. \code{saveobject} saves an object to a local file.
#' @examples
#' \dontrun{
#' # get an object in memory
#' ## create bucket
#' b <- putbucket("myexamplebucket")
#' ## save a dataset to the bucket
#' s3save(mtcars, bucket = b, object = "mtcars")
#' obj <- getbucket(b)
#' ## get the object in memory
#' x <- getobject(obj[[1]])
#' load(rawConnection(x))
#' "mtcars" %in% ls()
#'
#' # save an object locally
#' y <- getobject(obj[[1]], file = object[[1]][["Key"]])
#' y %in% dir()
#' 
#' # return parts of an object
#' ## use 'Range' header to specify bytes
#' getobject(object = obj[[1]], headers = list('Range' = 'bytes=1-120'))
#' }
#' @return If \code{file = NULL}, a raw object. Otherwise, a character string containing the file name that the object is saved to.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation}
#' @seealso \code{\link{getbucket}}
#' @export
getobject <- function(object, bucket, headers = list(), file = NULL, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } else {
        bucket <- get_bucketname(bucket)
    }
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        cont <- content(r, as = "raw")
        return(cont)
    }
}

#' @rdname getobject
#' @export
saveobject <- function(object, bucket, file, headers = list(), ...) {
    if (missing(file)) {
        stop('argument "file" is missing, with no default')
    }
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } else {
        bucket <- get_bucketname(bucket)
    }
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        writeBin(content(r, as = "raw"), con = file)
        return(file)
    }
}
