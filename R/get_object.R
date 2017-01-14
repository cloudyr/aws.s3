#' @rdname getobject
#' @title Get object
#' @description Retrieve an object from an S3 bucket
#' @template object
#' @template bucket
#' @param file An R connection, or file name specifying the local file to save the object into.
#' @param headers List of request headers for the REST call.
#' @param parse_response Passed through to \code{\link{s3HTTP}}, as this function requires a non-default setting. There is probably no reason to ever change this.
#' @template dots
#' @details \code{get_object} retrieves an object into memory. \code{save_object} saves an object to a local file. \code{head_object} checks whether an object exists by executing an HTTP HEAD request; this can be useful for checking object headers such as \dQuote{content-length} or \dQuote{content-type}.
#' @examples
#' \dontrun{
#' # get an object in memory
#' ## create bucket
#' b <- put_bucket("myexamplebucket")
#' ## save a dataset to the bucket
#' s3save(mtcars, bucket = b, object = "mtcars")
#' obj <- get_bucket(b)
#' ## get the object in memory
#' x <- get_object(obj[[1]])
#' load(rawConnection(x))
#' "mtcars" %in% ls()
#'
#' # save an object locally
#' y <- get_object(obj[[1]], file = object[[1]][["Key"]])
#' y %in% dir()
#' 
#' # return object using s3:// syntax
#' get_object("s3://myexamplebucket/mtcars")
#' 
#' # return parts of an object
#' ## use 'Range' header to specify bytes
#' get_object(object = obj[[1]], headers = list('Range' = 'bytes=1-120'))
#' }
#' @return If \code{file = NULL}, a raw object. Otherwise, a character string containing the file name that the object is saved to.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation: GET Object}
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectHEAD.html}{API Document: HEAD Object}
#' @seealso \code{\link{get_bucket}}, \code{\link{s3curl}}
#' @export
get_object <- function(object, bucket, headers = list(), parse_response = FALSE, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                parse_response = parse_response,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        cont <- httr::content(r, as = "raw")
        attributes(cont) <- httr::headers(r)
        return(cont)
    }
}

#' @rdname getobject
#' @export
save_object <- function(object, bucket, file, headers = list(), ...) {
    if (missing(file)) {
        stop('argument "file" is missing, with no default')
    }
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
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
        writeBin(httr::content(r, as = "raw"), con = file)
        return(file)
    }
}

#' @rdname getobject
#' @export
head_object <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "HEAD", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "HEAD")
    }
}

#' @title Get object torrent
#' @description Retrieves a Bencoded dictionary (BitTorrent) for an object from an S3 bucket.
#' 
#' @template object
#' @template bucket
#' @template dots
#'
#' @return Something.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETtorrent.html}{API Documentation}
#' @export
get_torrent <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object, "?torrent"),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
