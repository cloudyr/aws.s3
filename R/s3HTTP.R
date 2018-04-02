#' @title S3 HTTP Requests
#' @description This is the workhorse function for executing API requests for S3.
#' @details This is mostly an internal function for executing API requests. In almost all cases, users do not need to access this directly.
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param bucket A character string with the name of the bucket, or an object of class \dQuote{s3_bucket}. If the latter and a region can be inferred from the bucket object attributes, then that region is used instead of \code{region}.
#' @param path A character string with the name of the object to put in the bucket (sometimes called the object or 'key name' in the AWS documentation.)
#' @param query Any query arguments, passed as a named list of key-value pairs.
#' @param headers A list of request headers for the REST call.   
#' @param request_body A character string containing request body data.
#' @param write_disk If \code{verb = "GET"}, this is, Ootionally, an argument like \code{\link[httr]{write_disk}} to write the result directly to disk.
#' @param accelerate A logical indicating whether to use AWS transfer acceleration, which can produce significant speed improvements for cross-country transfers. Acceleration only works with buckets that do not have dots in bucket name.
#' @param dualstack A logical indicating whether to use \dQuote{dual stack} requests, which can resolve to either IPv4 or IPv6. See \url{http://docs.aws.amazon.com/AmazonS3/latest/dev/dual-stack-endpoints.html}.
#' @param parse_response A logical indicating whether to return the response as is, or parse and return as a list. Default is \code{TRUE}.
#' @param check_region A logical indicating whether to check the value of \code{region} against the apparent bucket region. This is useful for avoiding (often confusing) out-of-region errors. Default is \code{TRUE}.
#' @param url_style A character string specifying either \dQuote{path} (the default), or \dQuote{virtual}-style S3 URLs.
#' @param base_url A character string specifying the base URL for the request. There is no need to set this, as it is provided only to generalize the package to (potentially) support S3-compatible storage on non-AWS servers. The easiest way to use S3-compatible storage is to set the \env{AWS_S3_ENDPOINT} environment variable.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param region A character string containing the AWS region. Ignored if region can be inferred from \code{bucket}. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. If missing, defaults to value stored in environment variable \env{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key. If missing, defaults to value stored in environment variable \env{AWS_SECRET_ACCESS_KEY}.
#' @param session_token Optionally, a character string containing an AWS temporary Session Token. If missing, defaults to value stored in environment variable \env{AWS_SESSION_TOKEN}.
#' @param use_https Optionally, a logical indicating whether to use HTTPS requests. Default is \code{TRUE}.
#' @param ... Additional arguments passed to an HTTP request function. such as \code{\link[httr]{GET}}.
#' @return the S3 response, or the relevant error.
#' @import httr
#' @importFrom xml2 read_xml as_list
#' @importFrom utils URLencode
#' @import aws.signature
#' @export
s3HTTP <- 
function(verb = "GET",
         bucket = "", 
         path = "", 
         query = NULL,
         headers = list(), 
         request_body = "",
         write_disk = NULL,
         accelerate = FALSE,
         dualstack = FALSE,
         parse_response = TRUE, 
         check_region = TRUE,
         url_style = c("path", "virtual"),
         base_url = Sys.getenv("AWS_S3_ENDPOINT", "s3.amazonaws.com"),
         verbose = getOption("verbose", FALSE),
         region = NULL, 
         key = NULL, 
         secret = NULL, 
         session_token = NULL,
         use_https = TRUE,
         ...) {
    
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # validate bucket name and region
    bucketname <- get_bucketname(bucket)
    if (isTRUE(check_region) && (bucketname != "")) {
        if (isTRUE(verbose)) {
            message(sprintf("Checking bucket region using get_location('%s')", bucketname))
        }
        bucketregion <- get_region(x = bucket, key = key, secret = secret, session_token = session_token, ...)
        if (!is.null(bucketregion) && bucketregion != "") {
            region <- bucketregion
        }
        if (isTRUE(verbose)) {
            message(sprintf("Executing request using bucket region %s", region))
        }
    }
    
    # validate arguments and setup request URL
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    
    url_style <- match.arg(url_style)
    url <- setup_s3_url(bucketname, region, path, accelerate, url_style = url_style, base_url = base_url, verbose = verbose, use_https = use_https)
    p <- parse_url(url)
    action <- if (p$path == "") "/" else paste0("/", p$path)
    hostname <- paste(p$hostname, p$port, sep=ifelse(length(p$port), ":", ""))
    canonical_headers <- c(list(host = hostname,
                                `x-amz-date` = d_timestamp), headers)
    if (is.null(query) && !is.null(p$query)) {
        query <- p[["query"]]
    }
    if (all(sapply(query, is.null))) {
        query <- NULL
    }
    
    # assess whether request is authenticated or not
    if (is.null(key) || key == "") {
        if (isTRUE(verbose)) {
            message("Executing request without AWS credentials")
        }
        headers[["x-amz-date"]] <- d_timestamp
        Sig <- list()
        H <- do.call(add_headers, headers)
    } else {
        if (isTRUE(verbose)) {
            message("Executing request with AWS credentials")
        }
        Sig <- aws.signature::signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               verb = verb,
               action = action,
               query_args = query,
               canonical_headers = canonical_headers,
               request_body = request_body,
               key = key, 
               secret = secret, 
               session_token = session_token)
        headers[["x-amz-date"]] <- d_timestamp
        headers[["x-amz-content-sha256"]] <- Sig$BodyHash
        if (!is.null(session_token) && session_token != "") {
            headers[["x-amz-security-token"]] <- session_token
        }
        headers[["Authorization"]] <- Sig[["SignatureHeader"]]
        H <- do.call(add_headers, headers)
    }
    
    # execute request
    if (verb == "GET") {
        if (!is.null(write_disk)) {
            r <- GET(url, H, query = query, write_disk, ...)
        } else {
            r <- GET(url, H, query = query, ...)
        }
    } else if (verb == "HEAD") {
        r <- HEAD(url, H, query = query, ...)
        s <- http_status(r)
        if (tolower(s$category) == "success") {
            out <- TRUE
            attributes(out) <- c(attributes(out), headers(r))
            return(out)
        } else {
            message(s$message)
            out <- FALSE
            attributes(out) <- c(attributes(out), headers(r))
            return(out)
        }
    } else if (verb == "DELETE") {
        r <- DELETE(url, H, query = query, ...)
        s <- http_status(r)
        if (tolower(s$category) == "success") {
            out <- TRUE
            attributes(out) <- c(attributes(out), headers(r))
            return(out)
        } else {
            message(s$message)
            out <- FALSE
            attributes(out) <- c(attributes(out), headers(r))
            return(out)
        }
    } else if (verb == "POST") {
        r <- POST(url, H, query = query, ...)
    } else if (verb == "PUT") {
        if (is.character(request_body) && request_body == "") {
            r <- PUT(url, H, query = query, ...)
        } else if (is.character(request_body) && file.exists(request_body)) {
            r <- PUT(url, H, body = upload_file(request_body), query = query, ...)
        } else {
            r <- PUT(url, H, body = request_body, query = query, ...)
        }
    } else if (verb == "OPTIONS") {
        r <- VERB("OPTIONS", url, H, query = query, ...)
    }
    
    # handle response, failing if HTTP error occurs
    if (isTRUE(parse_response)) {
        out <- parse_aws_s3_response(r, Sig, verbose = verbose)
    } else {
        out <- r
    }
    attributes(out) <- c(attributes(out), headers(r))
    out
}

parse_aws_s3_response <- function(r, Sig, verbose = getOption("verbose")){
    if (isTRUE(verbose)) {
        message("Parsing AWS API response")
    }
    ctype <- headers(r)[["content-type"]]
    if (is.null(ctype) || ctype == "application/xml"){
        content <- content(r, as = "text", encoding = "UTF-8")
        if (content != "") {
            response_contents <- as_list(read_xml(content))
            response <- flatten_list(response_contents)
        } else {
            response <- NULL
        }
    } else {
        response <- r
    }
    if (isTRUE(verbose)) {
        message(http_status(r)[["message"]])
    }
    if (http_error(r) | (http_status(r)[["category"]] == "Redirection")) {
        h <- headers(r)
        out <- structure(response, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
        print(out)
        stop_for_status(r)
    }
    return(response)
}

setup_s3_url <- 
function(bucketname, 
         region, 
         path, 
         accelerate = FALSE, 
         dualstack = FALSE,
         url_style = c("path", "virtual"), 
         base_url = Sys.getenv("AWS_S3_ENDPOINT", "s3.amazonaws.com"),
         verbose = getOption("verbose", FALSE),
         use_https = TRUE) 
{
    url_style <- match.arg(url_style)
    
    # handle S3-compatible storage URLs
    if (base_url != "s3.amazonaws.com") {
        if (isTRUE(verbose) && url_style != "path") {
            message("Non-AWS base URL requested. Switching to path-style URLs.")
        }
        url_style <- "path"
        accelerate <- FALSE
        dualstack <- FALSE
    } else {
        # if accelerate = TRUE, must use virtual paths
        if (isTRUE(accelerate)) {
            if (isTRUE(verbose) && url_style != "virtual") {
                message("Option 'accelerate = TRUE' requested. Switching to virtual-style paths.")
            }
            url_style <- "virtual"
        }
        # handle region
        if (is.null(region) || region %in% c("us-east-1", "")) {
            if (is.null(region) || region == "") {
                if (isTRUE(verbose)) {
                    message("Option 'region' is missing, so 'us-east-1' assumed.")
                }
            }
            # handle dualstack
            if (isTRUE(dualstack)) {
                # handle accelerate
                if (isTRUE(accelerate)) {
                    base_url <- "s3-accelerate.dualstack.amazonaws.com"
                } else {
                    base_url <- "s3.dualstack.amazonaws.com"
                }
            } else {
                # handle accelerate
                if (isTRUE(accelerate)) {
                    base_url <- "s3-accelerate.amazonaws.com"
                } else {
                    base_url <- "s3.amazonaws.com"
                }
            }
        } else {
            # handle dualstack
            if (isTRUE(dualstack)) {
                # handle accelerate
                if (isTRUE(accelerate)) {
                    base_url <- paste0("s3-accelerate.dualstack.", region, ".amazonaws.com")
                } else {
                    base_url <- paste0("s3.dualstack.", region, ".amazonaws.com")
                }
            } else {
                # handle accelerate
                if (isTRUE(accelerate)) {
                    base_url <- paste0("s3-accelerate.amazonaws.com")
                } else {
                    base_url <- paste0("s3-", region, ".amazonaws.com")
                }
            }
        }
    }
    
    # define prefix http:// or https://
    if (isTRUE(use_https)) {
        prefix <- "https://"
    } else {
        prefix <- "http://"
    }
    
    # handle bucket name
    if (bucketname == "") {
        url <- paste0(prefix, base_url)
    } else {
        if (url_style == "virtual") {
            if (isTRUE(accelerate) && grepl("\\.", bucketname)) {
                stop("To use 'accelerate' for bucket name with dots (.), 'url_style' must be 'path'")
            }
            url <- paste0(prefix, bucketname, ".", base_url)
        } else {
            url <- paste0(prefix, base_url, "/", bucketname)
        }
    }
    
    terminal_slash <- grepl("/$", path)
    path <- if (path == "") "/" else {
        paste(sapply(
            strsplit(path, '/')[[1]],
            function(i) URLencode(i, TRUE),
            USE.NAMES = FALSE
        ), collapse = '/')
    }
    url <- if (grepl('^[\\/].*', path)) { 
        paste0(url, path) 
    } else { 
        paste(url, path, sep = "/") 
    }
    if (isTRUE(terminal_slash)) {
        url <- paste0(url, "/")
    }
    if (isTRUE(verbose)) {
        message(sprintf("S3 Request URL: %s", url))
    }
    return(url)
}
