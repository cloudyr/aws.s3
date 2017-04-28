#' @rdname bucketlist
#' @title List Buckets
#' @description List buckets as a data frame
#' @param add_region A logical (by default \code{FALSE}) indicating whether to add \dQuote{Region} column to the output data frame. This simply induces a loop over \code{\link{get_location}} for each bucket.
#' @template dots
#' @details \code{bucketlist} performs a GET operation on the base s3 endpoint and returns a list of all buckets owned by the authenticated sender of the request. If authentication is successful, this function provides a list of buckets available to the authenticated user. In this way, it can serve as a \dQuote{hello world!} function, to confirm that one's authentication credentials are working correctly.
#' 
#' \code{bucket_list_df} and \code{bucketlist} are identical.
#' @return A data frame of buckets.
#' 
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html}{API Documentation}
#' 
#' @keywords service
#' @seealso \code{\link{get_bucket}}, \code{\link{get_object}}
#' @export
bucketlist <- function(add_region = FALSE, ...) {
    r <- s3HTTP(verb = "GET", ...)
    out <- do.call("rbind.data.frame", r[["Buckets"]])
    out[] <- lapply(out, as.character)
    names(out)[names(out) %in% "Name"] <- "Bucket"
    rownames(out) <- seq_len(nrow(out))
    if (isTRUE(add_region)) {
        out[["Region"]] <- sapply(out[["Bucket"]], function(b) {
            r <- try(get_location(b, ...), silent = TRUE)
            if (inherits(r, "try-error")) {
                NA_character_
            } else {
                r
            }
        })
    }
    out
}

#' @rdname bucketlist
#' @export
bucket_list_df <- bucketlist
