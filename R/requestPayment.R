#' @rdname requestpayment
#' @title requestPayment
#' @description Get/Put the requestPayment subresource for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the requestPayment information, if set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTrequestPaymentGET.html}{API Documentation}
#' @export
get_requestpayment <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?requestPayment",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}

#' @rdname requestpayment
#' @export
put_requestpayment <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?requestPayment",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}
