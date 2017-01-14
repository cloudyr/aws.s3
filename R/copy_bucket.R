#' @rdname copyobject
#' @title Copy Objects
#' @description Copy objects between S3 buckets
#' @details \code{copy_object} copies an object from one bucket to another without bringing it into local memory. For \code{copy_bucket}, all objects from one bucket are copied to another (limit 1000 objects). The same keys are used in the old bucket as in the new bucket.
#'
#' @param from_bucket A character string containing the name of the bucket you want to copy from.
#' @param to_bucket A character string containing the name of the bucket you want to copy into.
#' @param from_object A character string containing the name the object you want to copy.
#' @param to_object A character string containing the name the object should have in the new bucket.
#' @param headers List of request headers for the REST call.   
#' @template dots
#'
#' @return Something...
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectCOPY.html}{API Documentation}
#' @export
copy_object <- function(from_object, to_object = from_object, from_bucket, to_bucket, headers = list(), ...) {
    from_bucket <- get_bucketname(from_bucket)
    to_bucket <- get_bucketname(to_bucket)
    from_object <- get_objectkey(from_object)
    to_object <- get_objectkey(to_object)
    r <- s3HTTP(verb = "PUT", 
                bucket = to_bucket,
                path = paste0("/", to_object),
                headers = c(headers, 
                            `x-amz-copy-source` = paste0("/",from_bucket,"/",from_object)), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname copyobject
#' @export
copy_bucket <- function(from_bucket, to_bucket, ...) {
    from_bucket <- get_bucketname(from_bucket)
    to_bucket <- get_bucketname(to_bucket)
    if (!to_bucket %in% sapply(bucketlist(...), `[[`, "Name")) { 
        n <- putbucket(to_bucket, ...)
    }
    b <- getbucket(from_bucket, ...)
    # need to create a list of all objects (`getbucket` will return only 1000
    lapply(b, function(x) {
        copyobject(from_object = x, to_object = get_objectkey(x), from_bucket = from_bucket, to_bucket = to_bucket, ...)
    })
}
