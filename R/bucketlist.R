#' @rdname bucketlist
#' @title List Buckets
#' @description List buckets as a data frame
#' @param add_region A logical (by default \code{FALSE}) indicating whether to add \dQuote{Region} column to the output data frame. This simply induces a loop over \code{\link{get_location}} for each bucket.
#' @template dots
#' @details \code{bucketlist} performs a GET operation on the base s3 endpoint and returns a list of all buckets owned by the authenticated sender of the request. If authentication is successful, this function provides a list of buckets available to the authenticated user. In this way, it can serve as a \dQuote{hello world!} function, to confirm that one's authentication credentials are working correctly.
#' 
#' \code{bucket_list_df} and \code{bucketlist} are identical.
#' @return A data frame of buckets. Can be empty (0 rows, 0 columns) if there are no buckets, otherwise contains typically at least columns \code{Bucket} and \code{CreationDate}.
#' 
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html}{API Documentation}
#' 
#' @keywords service
#' @seealso \code{\link{get_bucket}}, \code{\link{get_object}}
#' @export
bucketlist <- function(add_region = FALSE, ...) {
    r <- s3HTTP(verb = "GET", ...)
    bl <- r[["Buckets"]]
    ## no buckets - empty data frame
    if (!length(bl)) return(data.frame())
    ## single bucket - wrap it since we expect rows
    if (!is.list(bl[[1]]))
        bl <- list(Bucket=bl)
    out <- do.call("rbind.data.frame", bl)
    ## remove row names since they will be bogus
    ## (in AWS it's just "Bucket" for all which is not unique and thus
    ##  not a valid row name anyway)
    row.names(out) <- NULL
    out[] <- lapply(out, as.character)
    names(out)[names(out) %in% "Name"] <- "Bucket"
    rownames(out) <- seq_len(nrow(out))
    if (isTRUE(add_region)) {
        out[["Region"]] <- sapply(out[["Bucket"]], function(b)
            tryCatch(get_location(b, ...),
                     error=function(e) NA_character_)
        )
    }
    out
}

#' @rdname bucketlist
#' @export
bucket_list_df <- bucketlist
