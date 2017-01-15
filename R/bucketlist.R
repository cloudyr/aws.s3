#' @title List Buckets
#' @description List buckets as a data frame
#' @template dots
#' @details \code{bucketlist} performs a GET operation on the base s3 endpoint and returns a list of all buckets owned by the authenticated sender of the request. If authentication is successful, this function provides a list of buckets available to the authenticated user. In this way, it can serve as a \dQuote{hello world!} function, to confirm that one's authentication credentials are working correctly.
#' @return A data frame of buckets.
#' 
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html}{API Documentation}
#' 
#' @keywords service
#' @seealso \code{\link{get_bucket}}, \code{\link{get_object}}
#' @export
bucketlist <- function(...) {
    r <- s3HTTP(verb = "GET", ...)
    if (inherits(r, "aws_error") | inherits(r, "response")) {
        return(r)
    } else {
        out <- do.call("rbind.data.frame", r[["Buckets"]])
        out[] <- lapply(out, as.character)
        names(out)[names(out) %in% "Name"] <- "Bucket"
        rownames(out) <- seq_len(nrow(out))
        out
    }
}
