#' @rdname put_object
#' @title Put/store object in S3
#' @description Stores an object into an S3 bucket
#' @param what character vector, raw vector or a connection (see Details section for important change in 0.3.22!)
#' @param object A character string containing the name the object should have in S3 (i.e., its "object key"). If missing, an attempt is made to infer it.
#' @param folder A character string containing a folder name. (A trailing slash is not required.)
#' @template bucket
#' @param multipart A logical indicating whether to use multipart uploads. See \url{http://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html}. If the content is smaller than \code{partsize} it is ignored.
#' @template acl
#' @param file string, path to a file to store. Mutually exclusive with \code{what}.
#' @param headers List of request headers for the REST call. If \code{multipart = TRUE}, this only applies to the initialization call.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param show_progress A logical indicating whether to show a progress bar for uploads. Default is given by \code{options("verbose")}.
#' @param partsize numeric, size of each part when using multipart upload.  AWS imposes a minimum size (currently 5MB) so setting a too low value may fail. Note that it can be set to \code{Inf} in conjunction with \code{multipart=FALSE} to silence the warning suggesting multipart uploads for large content.
#' @template dots
#' @details This provides a generic interface for storing objects to S3. Some convenience wrappers are provided for common tasks: e.g., \code{\link{s3save}} and \code{\link{s3saveRDS}}.
#' 
#' Note that S3 is a flat file store. So there is no folder hierarchy as in a traditional hard drive. However, S3 allows users to create pseudo-folders by prepending object keys with \code{foldername/}. The \code{put_folder} function is provided as a high-level convenience function for creating folders. This is not actually necessary as objects with slashes in their key will be displayed in the S3 web console as if they were in folders, but it may be useful for creating an empty directory (which is possible in the web console).
#'
#' \strong{IMPORTANT}: In aws.s3 versions before 0.3.22 the first positional argument was \code{file} and \code{put_object} changed behavior depending on whether the file could be found or not. This is inherently very dangerous since \code{put_object} would only store the filename in cases there was any problem with the input. Therefore the first argument was changed to \code{what} which is always the content to store and now also supports connection. If not used, \code{file} is still a named argument and can be set instead - it will be always interpreted as a filename, failing with an error if it doesn't exist.
#'
#' When using connections in \code{what} it is preferrable that they are either unopened or open in binary mode. This condition is mandatory for multipart uploads. Text connections are inherently much slower and may not deliver identical results since they mangle line endings. \code{put_object} will automatically open unopened connections and always closes the connection before returning.
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
#'   put_object(file = tmp, object = "mtcars.csv", bucket = "myexamplebucket", show_progress = TRUE)
#' 
#'   # create a "folder" in a bucket (NOT required! Folders are really just 0-length files)
#'   put_folder("example", bucket = "myexamplebucket")
#'   ## write object to the "folder"
#'   put_object(file = tmp, object = "example/mtcars.csv", bucket = "myexamplebucket")
#' 
#'   # write serialized, in-memory object to S3
#'   x <- rawConnection(raw(), "w")
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
#'   put_object(file = tmp, object = "rnorm.rds", bucket = "myexamplebucket",
#'              show_progress = TRUE, multipart = TRUE, partsize=1e6)
#'   identical(x, s3readRDS("s3://myexamplebucket/rnorm.rds"))
#'   }
#' }
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' @seealso \code{\link{put_bucket}}, \code{\link{get_object}}, \code{\link{delete_object}}, \code{\link{put_encryption}}
#' @importFrom utils head
#' @export
put_object <-
function(
  what,
  object,
  bucket,
  multipart = FALSE,
  acl = NULL,
  file,
  headers = list(),
  verbose = getOption("verbose", FALSE),
  show_progress = getOption("verbose", FALSE),
  partsize = 10e7,
  ...
  ) {
    if (missing(what) && missing(file))
        stop("Either `what' or `file' must be specified")
    if (!missing(what) && !missing(file))
        stop("`what' and `file' are mutually exclusive")
    if (!missing(what) && is.character(what) && length(what) == 1L && file.exists(what)) {
        warning("The use of first argument in `put_object()' as filename is was ambiguous and is deprecated, please use file=")
        file <- what
    }
    size <- NA
    if (!missing(file)) {
        if (inherits(file, "connection"))
            what <- file
        else {
            size <- file.size(file)
            what <- file(file, raw=TRUE)
        }
    }
    ## what is always populated, with a connection from "file" if necessary

    ## we cache connection info
    what.info <- if (inherits(what, "connection")) summary(what) else NULL
    
    ## auto-detect file name if object is not provided
    if (missing(object) && inherits(what, "connection") && what.info$class == "file") {
        if (missing(bucket))
            stop("`bucket' must be specified if `object' is missing")
        object <- what.info$description
    } else if (missing(object)) { ## error if not specified otherwise
        stop("input is not a file connection, you must specify `object'")
    } else { ## all other cases ...
        if (missing(bucket)) {
            bucket <- get_bucketname(object)
        }
        object <- get_objectkey(object)
    }

    if (inherits(what, "connection")) {
        if (isOpen(what, "w") && !isOpen(what, "r"))
            stop("Input connection is already open for writing only, cannot use as input.")
        if (isOpen(what,"r") && what.info$text == "text") {
            if (multipart && (is.na(size) || size > partsize))
                stop("Input connection is already open in text mode, multipart uploads are only possible in binary mode.")
            warning("Input connection is already open in text mode, have to fall back to reading lines which is inefficient and result will be platform-dependent.")
            con <- what
            what <- readLines(what)
            close(con)
            if (length(what)) ## make sure the last line is terminated
                what <- c(what, "")
        }
    }

    if (is.character(what)) {
        if (length(what) > 1)
            what <- paste(what, collapse=if (.Platform$OS.type == "unix") "\n" else "\r\n")
        what <- if (length(what)) charToRaw(what) else raw()
    }

    if (!(inherits(what, "connection") || is.raw(what)))
        stop("Invalid payload of `what' - must be a raw vector, character vector or a connection")

    ## detect size
    ## NB: it can still be NA for non-seekable connections
    if (is.na(size)) {
        if (inherits(what, "connection") && summary(what)$class == "file") {
            size <- file.size(summary(what)$description)
        } else if (is.raw(what)) {
            size <- length(what)
        }
    }

    if (multipart && !is.na(size) && size <= partsize) {
        if (verbose)
            message("Content too small, uploading as single part")
        multipart <- FALSE
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
        if (!is.finite(partsize))
            stop("partsize must be finite for multipart uploads")
        if (is.raw(what)) {
            what <- rawConnection(what, "rb")
        } else if (!isOpen(what, "r")) {
            open(what, "rb")
        }
        ## at this point `what' is a connection and will be open
        on.exit(close(what))

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
        
        # function to call abort if any part fails (otherwise the user pays for incomplete payload!)
        abort.upload <- function(id) delete_object(object = object, bucket = bucket, query = list(uploadId = id), ...)

        on.exit(abort.upload(id), TRUE)

        # loop over parts
        partlist <- list()
        i <- 0L
        n <- ceiling(size / partsize) # can be NA

        repeat {
            i <- i + 1L
            if (verbose || show_progress) {
                if (is.na(n))
                    message("Uploading part ", i)
                else
                    message("Uploading part ", i, " of ", n)
            }

            data <- readBin(what, raw(), n=partsize)
            if (length(data) == 0) ## end of payload
                break

            r <- s3HTTP(verb = "PUT", 
                        bucket = bucket,
                        path = paste0('/', object),
                        query = list(partNumber = i, uploadId = id),
                        request_body = data,
                        verbose = verbose,
                        show_progress = show_progress,
                        ...)
            ## FIXME: error handling is a terrible mess
            if (inherits(r, "try-error")) {
                stop("Multi-part upload failed")
            } else {
                # record upload details
                partlist[[i]] <- list(Part = list(PartNumber = list(i), ETag = list(attributes(r)[["etag"]])))
            }
        }

        # complete
        if (verbose || show_progress) {
            message("Completing multi-part upload")
        }

        finalize <- complete_parts(object = object, bucket = bucket, id = id, parts = partlist, ...)
        if (inherits(finalize, "try-error"))
            stop("complete_parts() failed with ", finalize)

        ## remove exit handlers (abort and close)
        on.exit(NULL)
        close(what)
        return(TRUE)
    }

    if (!is.na(size) && size > partsize)
        message("File size is ", size, ", consider setting using multipart=TRUE")

    ## httr doesn't support connections so we have to read it all into memory first
    if (inherits(what, "connection")) {
        con <- what
        if (!isOpen(con, "r"))
            open(con, "rb")
        on.exit(close(con))
        if (!is.na(size)) {
            what <- readBin(con, raw(), size)
            if (length(what) != size)
                stop("Failed to read input, expected ", size, ", got ", length(what))
        } else {
            ## we don't know the size, so read partsize chunks
            what <- raw()
            n <- if (is.finite(partsize)) partsize else 10e6
            while (length(data <- readBin(con, raw(), partsize))) {
                if (length(what) == 0 && length(data) == partsize)
                    message("Input is larger than partsize, consider using multipart=TRUE")
                what <- c(what, data)
            }
        }
    }

    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = paste0('/', object),
                headers = headers, 
                request_body = what,
                verbose = verbose,
                show_progress = show_progress,
                ...)
    if (!length(r)) TRUE else structure(FALSE, response=r)
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
        headers <- c(headers, list(`Content-Length` = formatSize(calculate_data_size(file))))
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

formatSize <- function(size) {
    format(size, scientific = FALSE)
}
