#' @rdname sync
#' @title S3 file sync
#' @description Sync files/directories to/from S3
#' @param path string, path to the directory to synchronize, it will be expanded as needed (NOTE: older versions had a \code{files} argument which expected a full list of files which was ambiguous).
#' @template bucket
#' @param prefix string, if set to non-empty string, leading part of the objects in the bucket much have that prefix, other objects are not considered. In practice, this alows the immitation of sub-directories in the bucket and in that case it is typically required that the training slash is included in the prefix.
#' @param direction A character vector specifying whether to \dQuote{upload} and/or \dQuote{download} files. By default, \code{s3sync} is two-way, uploading any files missing from the bucket and downloading any objects missing from the local directory.
#' @param verbose A logical indicating whether to be verbose (the default is \code{TRUE}).
#' @param create logical, if \code{TRUE} the bucket is created if it doesn't exist, otherwise synchronizing a non-existing bucket is an error.
#' @template dots
#' @details \code{s3sync} synchronizes specified files to an S3 bucket.
#' If the bucket does not exist, it is created (unless \code{create=FALSE}). Similarly, if local directories do not exist (corresponding to leading portions of object keys), they are created, recursively. Object keys are generated based on \code{files} and local files are named (and organized into directories) based on object keys. A slash is interpreted as a directory level.
#' Local objects are copied to S3 and S3 objects are copied locally. This copying is performed conditionally. Objects existing locally but not in S3 are uploaded using \code{\link{put_object}}. Objects existing in S3 but not locally, are saved using \code{\link{save_object}}. If objects exist in both places, the MD5 checksum for each is compared; when identical, no copying is performed. If the checksums differ, local files are replaced with the bucket version if the local file is older and the S3 object is replaced if the local file is newer. If checksums differ but modified times match (which seems unlikely), a warning is issued. Note that multi-part files don't have a full MD5 sum recorded in S3 so they cannot be compared and thus are always assumed to be different.
#' @return A logical.
#' @examples
#' \dontrun{
# '  # create an example bucket
#'   put_bucket("examplebucket")
#'
#'   # sync all files in current directory to bucket (upload-only)
#'   s3sync(bucket = "examplebucket", direction = "upload")
#'
#'   # two-way sync
#'   s3sync(bucket = "examplebucket")
#'
#'   # full sync between a subset of the bucket and a test directory in user's home
#'   # corresponding roughly to:
#'   #   aws s3 sync ~/test s3://examplebucket/test/
#'   #   aws s3 sync s3://examplebucket/test/ ~/test
#'   s3sync("~/test", "examplebucket", prefix="test/", region="us-east-2")
#' }
#' 
#' @references 
#'  \href{http://docs.aws.amazon.com/cli/latest/reference/s3/sync.html}{aws s3 sync command line}
#' @seealso \code{\link{get_bucket}}, \code{\link{put_object}}, , \code{\link{save_object}}
#' @importFrom tools md5sum
#' @export
s3sync <- function(path = ".", bucket, prefix = "", direction = c("upload", "download"), verbose = TRUE, create = FALSE, ...) {
    if (length(prefix) != 1)
        stop("prefix must be a string")

    fi <- file.info(path.expand(path))
    if (nrow(fi) != 1 || !all(fi$isdir))
        stop("s3sync() requires exactly one path to an existing directory")

    if (isTRUE(verbose))
        message("Checking for existence of bucket '", bucket, "'")
    if (!bucket_exists(bucket, ...)) {
        if (isTRUE(create)) {
            message("Creating bucket '", bucket, "'")
            put_bucket(bucket, ...)
        } else
            stop("Bucket '", bucket, "' doesn't exist")
    }

    if (isTRUE(direction == "both")) direction <- c("upload", "download")
    direction <- match.arg(tolower(direction), c("upload", "download"), several.ok = TRUE)

    files <- list.files(path, recursive = TRUE)

    if (isTRUE(verbose)) {
        message(sprintf(ngettext(length(files), 
                                 "%d local file to sync", 
                                 "%d local files to sync"), 
                        length(files)))
    }
    
    # return all bucket objects
    if (isTRUE(verbose))
        message("Listing contents of bucket '", bucket, "'")

    b <- get_bucket(bucket, max = Inf, ...)
    if (isTRUE(verbose)) {
        message(sprintf(ngettext(length(b), 
                                 "%d object found in bucket '%s'",
                                 "%d objects found in bucket '%s'"),
                        length(b), bucket))
    }

    ## list all object keys
    s3.names <- unname(unlist(lapply(b, `[[`, "Key")))

    if (isTrue(verbose)) {
        cat("all keys:\n")
        print(s3.names)
    }

    ## filter only objects matching the prefix
    if (nchar(prefix)) {
        kp <- substr(s3.names, 1, nchar(prefix))
        s3.names <- substr(s3.names[kp == prefix], nchar(prefix) + 1L, 1e4)
        ## also subset b since we use it later
        b <- b[kp == prefix]
        cat("names after prefix filter:\n")
        print(s3.names)
    }

    ## download missing objects that are missing locally
    if ("download" %in% direction) {
        missingfiles <- s3.names[! s3.names %in% files]
        if (isTRUE(verbose)) {
            message(sprintf(ngettext(length(missingfiles), 
                                     "%d bucket object not found in local directory", 
                                     "%d bucket objects not found in local directory"), 
                            length(missingfiles)))
        }

        for (fn in missingfiles) {
            key <- paste0(prefix, fn)
            dst <- file.path(path, fn)
            dst.dir <- dirname(dst)
            ## create missing directory if needed locally
            if (!dir.exists(dst.dir)) {
                if (isTRUE(verbose))
                    message("Creating directory '", dst.dir, "'")
                dir.create(dst.dir, recursive = TRUE)
            }
            ## save object
            if (isTRUE(verbose))
                message("<== Saving object '", key, "' to '", dst, "'")
            save_object(object = key, bucket = bucket, file = dst, ...)
        }
    }

    ## upload files that are missing from bucket
    if ("upload" %in% direction) {
        missingobjects <- files[!files %in% s3.names]
        if (isTRUE(verbose)) {
            message(sprintf(ngettext(length(missingobjects), 
                                     "%d local file not found in bucket '%s'",
                                     "%d local files not found in bucket '%s'"), 
                            length(missingobjects), bucket))
        }
        if (length(missingobjects))
            for (name in missingobjects) {
                put_object(file = file.path(path, name), object = paste0(prefix, name), bucket = bucket, ...)
                if (isTRUE(verbose))
                    message("==> Putting file '",  file.path(path, name), "' to bucket '", bucket, "' as '", paste0(prefix, name), "'")
            }
    }

    ## conditionally modify files and keys for objects/files existing locally and in S3
    whichinboth <- which(s3.names %in% files)
    if (length(whichinboth)) {
        inboth <- s3.names[whichinboth]
        ## get ETags
         md5objects <- gsub("\"", "", unname(unlist(lapply(b[whichinboth], `[[`, "ETag"))))

        print(b[whichinboth])
        
        ## ETags do NOT work for multi-part uploads, because we don't know the chunk size
        multiparts <- grep("-", md5objects)

        ## compute local md5 for non-multiparts
        md5files <- if (length(multiparts)) {
                        warning("One or more objects in the bucket are multi-part uploads without a full checksum, I have to assume they are different.")
                        x <- rep("", length(inboth))
                        ## only bother with checksums for those we *can* compare
                        ## multiparts are likely big, so we don't want to check them when we know we can't use it
                        x[-multiparts] <- tools::md5sum(file.path(path, inboth[-multiparts]))
                        x
                    } else
                        tools::md5sum(file.path(path, inboth))

        md5files[is.na(md5files)] <- "X"
        md5objects[is.na(md5objects)] <- "Y"
        matched <- md5files == md5objects
        print(md5files)
        print(md5objects)
        print(matched)
        ## subset to objects/files with mismatched md5sums
        tosync <- inboth[!matched]
        if (length(tosync)) {
            if (isTRUE(verbose))
                message("Checking md5sum for ", length(tosync), " updated file(s)/object(s)")

            ## check time modified
            modifiedfiles <- file.info(file.path(path, tosync))$mtime
            modifiedobjects <- unname(unlist(lapply(b[whichinboth[!matched]], `[[`, "LastModified")))
            modifiedobjects <- as.POSIXct(strptime(modifiedobjects, format = "%FT%H:%M:%OSZ", tz = "UTC"))

            ## difference in seconds
            timediff <- as.numeric(modifiedfiles) - as.numeric(modifiedobjects)

            ## sync files
            ## upload files newer than object
            if ("upload" %in% direction) {
                filestoupload <- tosync[timediff > 0]
                if (isTRUE(verbose)) {
                    message(sprintf(ngettext(sum(timediff > 0),
                                             "%d updated file to upload to bucket '%s'",
                                             "%d updated files to upload to bucket '%s'"),
                                    sum(timediff > 0), bucket))
                }

                for (name in filestoupload) {
                    if (isTRUE(verbose))
                        message("==> Putting file '",  name, "' to bucket '", bucket, "'")
                    put_object(file = file.path(path, name), object = paste0(prefix, name), bucket = bucket, ...)
                }
            }

            ## download objects newer than file
            if ("download" %in% direction) {
                filestodownload <- tosync[timediff < 0]
                if (isTRUE(verbose)) {
                    message(sprintf(ngettext(sum(timediff < 0),
                                             "%d updated object to save locally from bucket '%s'",
                                             "%d updated objects to save locally from bucket '%s'"),
                                    sum(timediff < 0), bucket))
                }
                for (name in filestodownload) {
                    ## create directory if needed
                    thisdir <- dirname(file.path(path, name))
                    if (!dir.exists(thisdir)) {
                        if (isTRUE(verbose))
                            message("Creating directory '", thisdir, "'")
                        dir.create(thisdir, recursive = TRUE)
                    }
                    if (isTRUE(verbose))
                        message("<== Saving object '", paste0(prefix, name), "' to '", file.path(path, name), "'")
                    save_object(object = paste0(prefix, name), bucket = bucket, file = file.path(path, name), ...)
                }
            }

            ## flag objects with identical last modified times
            if (isTRUE(verbose)) {
                message(sprintf(ngettext(sum(timediff == 0),
                                         "%d object/file pair appear to differ but has the same LastModified Time",
                                         "%d object/file pairs appear to differ but have the same LastModified Time"),
                                sum(timediff == 0)))
            }
        }
    }
    TRUE # this should probably be a directory listing with some note on what action was performed
}
