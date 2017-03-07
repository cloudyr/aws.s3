#' @rdname acceleration
#' @title Bucket Acceleration
#' @description Get/Put acceleration settings or retrieve acceleration status of a bucket.
#' @details Transfer acceleration is a AWS feature that enables potentially faster file transfers to and from S3, particularly when making cross-border transfers (such as from a European client location to the \samp{us-east-1} S3 region). Acceleration must be enabled before it can be used. Once enabled, \code{accelerate = TRUE} can be passed to any aws.s3 function via \code{\link{s3HTTP}}. \code{get_acceleration} returns the acceleration status of a bucket; \code{put_acceleration} enables or suspends acceleration.
#'
#' @template bucket
#' @param status Character string specifying whether acceleration should be \dQuote{Enabled} or \dQuote{Suspended}.
#' @template dots
#'
#' @return For \code{get_acceleration}: If acceleration has never been enabled or suspend, the value is \code{NULL}. Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}). For \code{put_acceleration}: If acceleration has never been enabled or suspend, the value is \code{NULL}.
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTaccelerate.html}{API Documentation: PUT Bucket accelerate}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETaccelerate.html}{API Documentation: GET Bucket accelerate}
#' @examples
#' \dontrun{
#' b <- bucketlist()
#' get_acceleration(b[[1]])
#' put_acceleration(b[[1]], "Enabled")
#' get_acceleration(b[[1]])
#' put_acceleration(b[[1]], "Suspended")
#' }
#' @export
get_acceleration <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(accelerate = ""),
                ...)
    attributes(r) <- NULL
    if (identical(r, list())) {
        return(NULL)
    } else {
        return(r)
    }
}

#' @rdname acceleration
#' @export
put_acceleration <- function(bucket, status = c("Enabled", "Suspended"), ...){
    b <- paste0(
'<AccelerateConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"> 
  <Status>',match.arg(status),'</Status> 
</AccelerateConfiguration>')
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(accelerate = ""),
                request_body = b,
                ...)
    attributes(r) <- NULL
    if (identical(r, list())) {
        return(NULL)
    } else {
        return(r)
    }
}
