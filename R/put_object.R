#' @rdname put_object
#' @title Put object
#' @description Puts an object into an S3 bucket
#' @param file A character string containing the filename (or full path) of the file you want to upload to S3. Alternatively, an raw vector containing the file can be passed directly, in which case \code{object} needs to be specified explicitly.
#' @param object A character string containing the name the object should have in S3 (i.e., its "object key"). If missing, the filename is used.
#' @param folder A character string containing a folder name. (A trailing slash is not required.)
#' @template bucket
#' @param multipart A logical indicating whether to use multipart uploads. See \url{http://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html}. If \code{file} is less than 100 MB, this is ignored.
#' @template acl
#' @param headers List of request headers for the REST call. If \code{multipart = TRUE}, this only applies to the initialization call.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param show_progress A logical indicating whether to show a progress bar for uploads. Default is given by \code{options("verbose")}.
#' @template dots
#' @details This provide a generic interface for sending files (or serialized, in-memory representations thereof) to S3. Some convenience wrappers are provided for common tasks: e.g., \code{\link{s3save}} and \code{\link{s3saveRDS}}.
#' 
#' Note that S3 is a flat file store. So there is no folder hierarchy as in a traditional hard drive. However, S3 allows users to create pseudo-folders by prepending object keys with \code{foldername/}. The \code{put_folder} function is provided as a high-level convenience function for creating folders. This is not actually necessary as objects with slashes in their key will be displayed in the S3 web console as if they were in folders, but it may be useful for creating an empty directory (which is possible in the web console).
#' 
#' @return If successful, \code{TRUE}.
#' @examples
#' \dontrun{
#'   library("datasets")
#'   
#'   # write file to S3
#'   tmp <- tempfile()
#'   on.exit(unlink(tmp))
#'   utils::write.csv(mtcars, file = tmp)
#'   # put object with an upload progress bar
#'   put_object(tmp, object = "mtcars.csv", bucket = "myexamplebucket", show_progress = TRUE)
#' 
#'   # create a "folder" in a bucket
#'   put_folder("example", bucket = "myexamplebucket")
#'   ## write object to the "folder"
#'   put_object(tmp, object = "example/mtcars.csv", bucket = "myexamplebucket")
#' 
#'   # write serialized, in-memory object to S3
#'   x <- rawConnection(raw(0), "w")
#'   utils::write.csv(mtcars, x)
#'   put_object(rawConnectionValue(x), object = "mtcars.csv", bucket = "myexamplebucketname")
#' 
#'   # use `headers` for server-side encryption
#'   ## require appropriate bucket policy
#'   ## encryption can also be set at the bucket-level using \code{\link{put_encryption}}
#'   put_object(file = tmp, object = "mtcars.csv", bucket = "myexamplebucket",
#'              headers = c('x-amz-server-side-encryption' = 'AES256'))
#' 
#'   # alternative "S3 URI" syntax:
#'   put_object(rawConnectionValue(x), object = "s3://myexamplebucketname/mtcars.csv")
#'   close(x)
#' 
#'   # read the object back from S3
#'   read.csv(text = rawToChar(get_object(object = "s3://myexamplebucketname/mtcars.csv")))
#' 
#'   # multi-part uploads for objects over 5MB
#'   \donttest{
#'   x <- rnorm(3e6)
#'   saveRDS(x, tmp)
#'   put_object(tmp, object = "rnorm.rds", bucket = "myexamplebucket",
#'              show_progress = TRUE, multipart = TRUE)
#'   identical(x, s3readRDS("s3://myexamplebucket/rnorm.rds"))
#'   }
#' }
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' @seealso \code{\link{put_bucket}}, \code{\link{get_object}}, \code{\link{delete_object}}, \code{\link{put_encryption}}
#' @importFrom utils head
#' @export
put_object <-
function(
  file,
  object,
  bucket,
  multipart = FALSE,
  acl = NULL,
  headers = list(),
  verbose = getOption("verbose", FALSE),
  show_progress = getOption("verbose", FALSE),
  ...
) {
    if (missing(object) && is.character(file)) {
        object <- basename(file)
    } else {
        if (missing(bucket)) {
            bucket <- get_bucketname(object)
        }
        object <- get_objectkey(object)
    }
    if (!"x-amz-acl" %in% names(headers)) {
        if (!is.null(acl)) {
            acl <- match.arg(acl, c("private", "public-read", "public-read-write", "aws-exec-read", "authenticated-read", "bucket-owner-read", "bucket-owner-full-control"))
            headers <- c(headers, list(`x-amz-acl` = acl))
        } else {
            headers <- c(headers, list(`x-amz-acl` = "private"))
        }
    }
    if (isTRUE(multipart)) {
        size <- calculate_data_size(file)
        partsize <- 1e7 # 10 MB
        nparts <- ceiling(size/partsize)
        
        # if file is small, there is no need for multipart upload
        if (size < partsize) {
            if (isTRUE(verbose)) {
                message("Uploading file as a single part")
            }
            put_object(file = file, object = object, bucket = bucket, multipart = FALSE, headers = headers, show_progress = show_progress, ...)
            return(TRUE)
        }

        if (is.character(file) && file.exists(file)) {
            # connection is file
            file <- file(file, open="rb", raw=TRUE)
            on.exit(close(file))
        } else if (is.character(file)) {
            # connection is character string
            file <- rawConnection(charToRaw(file), "r")
            on.exit(close(file))
        } else if (is.vector(file)) {
            # connection is binary vector
            file <- rawConnection(file, "r")
            on.exit(close(file))
        } else if (!inherits(file, "connection")) {
            # open connection in binary mode
            stop(paste0("Invalid value of the file parameter: ", typeof(file),
            " but file, character string or binary vector expected"))
        }

        # initialize the upload
        if (isTRUE(verbose)) {
            message("Initializing multi-part upload")
        }
        initialize <- post_object(file = raw(0),
                                  object = object,
                                  bucket = bucket,
                                  query = list(uploads = ""),
                                  headers = headers,
                                  ...)
        id <- initialize[["UploadId"]]
        
        # function to call abort if any part fails
        abort <- function(id) delete_object(object = object, bucket = bucket, query = list(uploadId = id), ...)
        on.exit(abort(id))
        
        # loop over parts
        partlist <- list()

        # loop over parts
        for (i in seq_len(nparts)) {
            if (isTRUE(verbose) | isTRUE(show_progress)) {
                message(sprintf("Uploading part %d of %d-part upload", i, nparts))
            }

            data <- readBin(file, raw(), n=partsize)

            r <- s3HTTP(verb = "PUT", 
                        bucket = bucket,
                        path = paste0('/', object),
                        headers = list(`Content-Length` = length(data)),
                        query = list(partNumber = i, uploadId = id),
                        request_body = data,
                        verbose = verbose,
                        show_progress = show_progress,
                        ...)
            if (inherits(r, "try-error")) {
                stop("Multi-part upload failed")
            } else {
                # record upload details
                partlist[[i]] <- list(Part = list(PartNumber = list(i), ETag = list(attributes(r)[["etag"]])))
            }
        }
        
        # complete
        if (isTRUE(verbose) | isTRUE(show_progress)) {
            message("Completing multi-part upload")
        }
        finalize <- complete_parts(object = object, bucket = bucket, id = id, parts = partlist, ...)
        on.exit(NULL, add = FALSE)
        return(TRUE)
    } else {
        if (!"Content-Length" %in% names(headers)) {
            headers <- c(headers, list(
                         `Content-Length` = calculate_data_size(file)
                         ))
        }
        if (headers[["Content-Length"]] > 1e7) {
            message(sprintf("File size is %d. Consider setting 'multipart = TRUE'.", headers[["Content-Length"]]))
        }
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0('/', object),
                    headers = headers, 
                    request_body = file,
                    verbose = verbose,
                    show_progress = show_progress,
                    ...)
        return(TRUE)
    }
}

#' @rdname put_object
#' @export
put_folder <- function(folder, bucket, ...) {
    if (!grepl("/$", folder)) {
        folder <- paste0(folder, "/")
    }
    put_object(raw(0), object = folder, bucket = bucket, ...)
}

post_object <- function(file, object, bucket, headers = list(), ...) {
    if (missing(object) && is.character(file)) {
        object <- basename(file)
    } else {
        if (missing(bucket)) {
            bucket <- get_bucketname(object)
        }
        object <- get_objectkey(object)
    }
    if (!"Content-Length" %in% names(headers)) {
        headers <- c(headers, list(`Content-Length` = calculate_data_size(file)))
    }
    r <- s3HTTP(verb = "POST", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers, 
                request_body = file,
                ...)
    structure(r, class = "s3_object")
}

list_parts <- function(object, bucket, id, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    get_object(object = object, bucket = bucket, query = list(uploadId = id), ...)
}

upload_part <- function(part, object, bucket, number, id, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    query <- list(partNumber = number, uploadId = id)
    put_object(file = part, object = object, bucket = bucket, query = query, multipart = FALSE, ...)
}

complete_parts <- function(object, bucket, id, parts, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    
    tmp <- tempfile()
    xml2::write_xml(xml2::as_xml_document(list(CompleteMultipartUpload = parts)), tmp, options = "no_declaration")
    post_object(file = tmp, object = object, bucket = bucket, query = list(uploadId = id), ...)
}

#' @title Multipart uploads
#' @description Get a list of multipart uploads for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the multipart upload information.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadListMPUpload.html}{API Documentation}
#' @export
get_uploads <- function(bucket, ...){
    r <- s3HTTP(verb = "GET",
                bucket = bucket,
                query = list(uploads = ""),
                ...)
    return(r)
}

calculate_data_size <- function(data) {
    post_size <- 0
    if (is.character(data)) {
        if (file.exists(data)) {
            post_size <- file.size(data)
        } else {
            post_size <- nchar(data)
        }
    } else if (is.null(data)) {
        post_size <- 0
    } else {
        post_size <- length((data))
    }

    return(as.numeric(post_size))
}
