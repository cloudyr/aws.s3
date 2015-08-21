#' @title List buckets for authenticated user
#'
#' @description bucketlist performs a GET operation on the base s3 endpoint 
#' and returns a list of all buckets owned by the authenticated 
#' sender of the request.
#' 
#' @details If authentication is successful, this function provides a list of 
#' buckets available to the authenticated user. In this way, it can serve as 
#' a \dQuote{hello world!} function, to confirm that one's authentication 
#' credentials are working correctly.
#' 
#' @param ... additional arguments passed to \code{\link{s3HTTP}}
#'
#' @return a list of buckets.  if passed with default settings \dQuote{parse_response = TRUE}
#' the response will list the name and creationdate of all buckets owned by the request
#' sender.
#' 
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html}{API Documentation}
#' 
#' @keywords service
#' 
#' @export

bucketlist <- function(...) {
    r <- s3HTTP(verb = "GET", ...)
    #errors and unparsed
    if (inherits(r, "aws_error") | inherits(r, "response")) {
        return(r)
    #parsed
    } else {
        structure(r$Buckets, class = "s3_bucketlist")
    }
}

print.s3_bucketlist <- function(object, ...) {
    x <- do.call("rbind.data.frame", object)
    rownames(x) <- 1:nrow(x)
    print(x)
    invisible(object)
}
