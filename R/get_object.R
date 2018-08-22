#' @rdname get_object
#' @title Get object
#' @description Retrieve an object from an S3 bucket. To check if an object exists, see \code{\link{head_object}}
#' @template object
#' @template bucket
#' @param file An R connection, or file name specifying the local file to save the object into.
#' @param request_body For \code{select_object}, an XML request body as described in the \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html}{SELECT API documentation}.
#' @param headers List of request headers for the REST call.
#' @param parse_response Passed through to \code{\link{s3HTTP}}, as this function requires a non-default setting. There is probably no reason to ever change this.
#' @param as Passed through to \code{\link{httr::content}}. 
#' @template dots
#' @details \code{get_object} retrieves an object into memory as a raw vector. This page describes \code{get_object} and several wrappers that provide additional useful functionality.
#' 
#' \code{save_object} saves an object to a local file without bringing it into memory.
#' 
#' \code{s3connection} provides a \code{\link[base]{connection}} interface to an S3 object.
#' 
#' \code{select_object} uses the \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html}{SELECT API} to select part of a CSV or JSON object. This requires constructing and passing a fairly tedious request body, which users will have to construct themselves according to the documentation.
#'
#' Some users may find the raw vector response format of \code{get_object} unfamiliar. The object will also carry attributes, including \dQuote{content-type}, which may be useful for deciding how to subsequently process the vector. Two common strategies are as follows. For text content types, running \code{\link[base]{charToRaw}} may be the most useful first step to make the response human-readable. Alternatively, converting the raw vector into a connection using \code{\link[base]{rawConnection}} may also be useful, as that can often then be passed to parsing functions just like a file connection would be.
#'
#' Higher-level functions
#' 
#' @examples
#' \dontrun{
#'   # get an object in memory
#'   ## create bucket
#'   b <- put_bucket("myexamplebucket")
#'   
#'   ## save a dataset to the bucket
#'   s3save(mtcars, bucket = b, object = "mtcars")
#'   obj <- get_bucket(b)
#'   ## get the object in memory
#'   x <- get_object(obj[[1]])
#'   load(rawConnection(x))
#'   "mtcars" %in% ls()
#'
#'   # save an object locally
#'   y <- save_object(obj[[1]], file = object[[1]][["Key"]])
#'   y %in% dir()
#' 
#'   # return object using 'S3 URI' syntax, with progress bar
#'   get_object("s3://myexamplebucket/mtcars", show_progress = TRUE)
#' 
#'   # return parts of an object
#'   ## use 'Range' header to specify bytes
#'   get_object(object = obj[[1]], headers = list('Range' = 'bytes=1-120'))
#'  
#'   # example of streaming connection
#'   ## setup a bucket and object
#'   b <- put_bucket("myexamplebucket")
#'   s3write_using(mtcars, bucket = b, object = "mtcars.csv", FUN = utils::write.csv)
#'   
#'   ## setup the connection
#'   con <- s3connection("mtcars.csv", bucket = b)
#'   
#'   ## line-by-line read
#'   while(length(x <- readLines(con, n = 1L))) {
#'     print(x)
#'   }
#' 
#'   ## use data.table::fread without saving object to file
#'   library(data.table)
#'   fread(get_object("s3://myexamplebucket/mtcars", as = "text"))
#' 
#'   ## cleanup
#'   close(con)
#'   delete_bucket("myexamplebucket")
#' }
#' @return If \code{file = NULL}, a raw object. Otherwise, a character string containing the file name that the object is saved to.
#' @references
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation: GET Object}
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation: GET Object torrent}
#'  \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html}{API Documentation: SELECT Object}
#' @seealso \code{\link{get_bucket}}, \code{\link{object_exists}}, \code{\link{head_object}}, \code{\link{put_object}}, \code{\link{delete_object}}
#' @export
get_object <- 
function(object, 
         bucket, 
         headers = list(), 
         parse_response = FALSE, 
		 as = "raw",
         ...) {
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
    cont <- httr::content(r, as = as)
    return(cont)
}

#' @rdname get_object
#' @param overwrite A logical indicating whether to overwrite \code{file}. Passed to \code{\link[httr]{write_disk}}. Default is \code{TRUE}.
#' @export
save_object <- 
function(object, 
         bucket, 
         file = basename(object),
         headers = list(),
         overwrite = TRUE,
         ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    
    # create dir() if missing
    d <- dirname(file)
    if (!file.exists(d)) {
        dir.create(d, recursive = TRUE)
    }
    
    # use httr::write_disk() to write directly to disk
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                write_disk = httr::write_disk(path = file, overwrite = overwrite),
                ...)
    return(file)
}

#' @rdname get_object
#' @export
select_object <- 
function(
  object,
  bucket,
  request_body,
  headers = list(),
  parse_response = FALSE,
  ...
) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    
    r <- s3HTTP(verb = "POST", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                query = list(select = "", "select-type" = "2"),
                request_body = request_body,
                parse_response = parse_response,
                ...)
    cont <- httr::content(r, as = "raw")
    return(cont)
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
                path = paste0("/", object),
                query = list(torrent =""),
                ...)
    return(content(r, "raw"))
}

#' @rdname get_object
#' @export
s3connection <-
function(object,
         bucket,
         headers = list(),
         ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "connection",
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                ...)
    return(r)
}
