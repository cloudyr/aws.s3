#' @title S3 HTTP Requests
#' @description This is the workhorse function for executing API requests for S3.
#' @details This is mostly an internal function for executing API requests. In almost all cases, users do not need to access this directly.
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param bucket A character string with the name of the bucket, or an object of class \dQuote{s3_bucket}. If the latter and a region can be inferred from the bucket object attributes, then that region is used instead of \code{region}.
#' @param path A character string with the name of the object to put in the bucket (sometimes called the object or 'key name' in the AWS documentation.)
#' @param query Any query arguments, passed as a named list of key-value pairs.
#' @param headers A list of request headers for the REST call.   
#' @param request_body A character string containing request body data.
#' @param write_disk,write_fn Deprecated. argument not supported by `httr2`.
#' Use `verb="connection"` or `file` instead.
#' @param file File path. If specified and `verb="GET"` the response content
#' will be written to this file.
#' @param accelerate A logical indicating whether to use AWS transfer acceleration, which can produce significant speed improvements for cross-country transfers. Acceleration only works with buckets that do not have dots in bucket name.
#' @param dualstack A logical indicating whether to use \dQuote{dual stack} requests, which can resolve to either IPv4 or IPv6. See \url{https://docs.aws.amazon.com/AmazonS3/latest/API/dual-stack-endpoints.html}.
#' @param parse_response A logical indicating whether to return the response as is, or parse and return as a list. Default is \code{TRUE}.
#' @param check_region A logical indicating whether to check the value of \code{region} against the apparent bucket region. This is useful for avoiding (often confusing) out-of-region errors. Default is \code{FALSE}.
#' @param url_style A character string specifying either \dQuote{path} (the default), or \dQuote{virtual}-style S3 URLs.
#' @param base_url A character string specifying the base hostname for the request (it is a misnomer, the actual URL is constructed from this name, region and \code{use_https} flag. There is no need to set this, as it is provided only to generalize the package to (potentially) support S3-compatible storage on non-AWS servers. The easiest way to use S3-compatible storage is to set the \env{AWS_S3_ENDPOINT} environment variable. When using non-AWS servers, you may also want to set \code{region=""}.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param show_progress A logical indicating whether to show a progress bar for downloads and uploads. Default is given by \code{options("verbose")}.
#' @param region A character string containing the AWS region. Ignored if region can be inferred from \code{bucket}. If missing, an attempt is made to locate it from credentials. Defaults to \dQuote{us-east-1} if all else fails. Should be set to \code{""} when using non-AWS endpoints that don't include regions (and \code{base_url} must be set).
#' @param key A character string containing an AWS Access Key ID. If missing, defaults to value stored in environment variable \env{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key. If missing, defaults to value stored in environment variable \env{AWS_SECRET_ACCESS_KEY}.
#' @param session_token Optionally, a character string containing an AWS temporary Session Token. If missing, defaults to value stored in environment variable \env{AWS_SESSION_TOKEN}.
#' @param use_https Optionally, a logical indicating whether to use HTTPS requests. Default is \code{TRUE}.
#' @param ... Additional arguments passed to an HTTP request function. such as \code{\link[httr]{GET}}.
#' @return the S3 response, or the relevant error.
#' @import httr2
#' @importFrom xml2 read_xml as_list
#' @importFrom utils URLencode
#' @importFrom curl handle_setheaders new_handle curl
#' @import aws.signature
#' @export
s3HTTP <- 
function(verb = "GET",
         bucket = "", 
         path = "", 
         query = NULL,
         headers = list(), 
         request_body = "",
         file,
         write_disk,
         write_fn,
         accelerate = FALSE,
         dualstack = FALSE,
         parse_response = TRUE, 
         check_region = FALSE,
         url_style = c("path", "virtual"),
         base_url = Sys.getenv("AWS_S3_ENDPOINT", "s3.amazonaws.com"),
         verbose = getOption("verbose", FALSE),
         show_progress = getOption("verbose", FALSE),
         region = NULL, 
         key = NULL, 
         secret = NULL, 
         session_token = NULL,
         use_https = TRUE,
         ...) {
    #TODO test deprecated args
    # locate and validate credentials
    credentials <- aws.signature::locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    ## allow region="" to override any config - the only way to use 3rd party URLs without region
    region <- if (length(region) && !nzchar(region)) region else credentials[["region"]]
    
    # validate bucket name and region
    bucketname <- get_bucketname(bucket)
    if (isTRUE(check_region) && (bucketname != "")) {
        if (isTRUE(verbose)) {
            message("Checking bucket region using get_location('", bucketname, "')")
        }
        bucketregion <- get_region(x = bucket, key = key, secret = secret, session_token = session_token, ...)
        if (!is.null(bucketregion) && bucketregion != "") {
            region <- bucketregion
        }
        if (isTRUE(verbose)) {
            message("Executing request using bucket region ", region)
        }
    }
    
    # validate arguments and setup request URL
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    
    url_style <- match.arg(url_style)
    url <- setup_s3_url(bucketname, region, path, accelerate, url_style = url_style, base_url = base_url, verbose = verbose, use_https = use_https)
    p <- httr2::url_parse(url)
    request <-
        httr2::request(url) |>
        httr2::req_method(ifelse(verb == "connection", "GET", verb))
    request <-
        do.call(httr2::req_url_query, c(list(.req = request), query))

    if (show_progress) request <- httr2::req_progress(request)
    
    action <- p$path
    hostname <- paste(p$hostname, p$port, sep=ifelse(length(p$port), ":", ""))
    
    # parse headers
    request <- do.call(httr2::req_headers, c(
        list(.req = request,
             host = hostname,
             `x-amz-date` = d_timestamp), headers))
    
    # # parse query arguments TODO this is handled by httr2::request
    # if (is.null(query) && !is.null(p$query)) {
    #     query <- p[["query"]]
    # }
    # if (all(sapply(query, is.null))) {
    #     query <- NULL
    # }
    # assess whether request is authenticated or not
    if (is.null(key) || key == "") {
        if (isTRUE(verbose)) {
            message("Executing request without AWS credentials")
        }
        Sig <- list()
    } else {
        # if authenticated, figure out the request signature
        if (isTRUE(verbose))
            message("Executing request with AWS credentials")

        ## we need to augment canonical headers with
        ## x-amz-content-sha256 since signature_v4_auth() doesn't do it
        ## the following is what signature_v4_auth() does and it's terribly fragile!
        ## We really need to convince them that using conditionals on file presence is really, really bad!
        ## But for compatibility we keep it until fixed...
        body_hash <- tolower(digest::digest(request_body,
                                            file = is.character(request_body) && file.exists(request_body),
                                            algo = "sha256", serialize = FALSE))
        request <- request |>
            httr2::req_headers(`x-amz-content-sha256` = body_hash)

        Sig <- aws.signature::signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               # For s3connection() we hack the 'verb' argument. It's otherwise a GET request.
               verb = if (verb == "connection") "GET" else verb,
               action = action,
               query_args = query,
               canonical_headers = httr2::req_headers(request),
               request_body = request_body,
               key = key,
               secret = secret,
               session_token = session_token,
               verbose = verbose)
        if (!is.null(session_token) && session_token != "") {
            request <- request |>
                httr2::req_headers(`x-amz-security-token` = session_token)
        }
        request <- request |>
            httr2::req_headers(`Authorization` = Sig[["SignatureHeader"]])
    }
    # execute request
    if (verb %in% c("POST", "PUT")) {
        # POST or PUT verb
        if (is.character(request_body) && file.exists(request_body)) {
            request <- request |>
                httr2::req_body_file(request_body)
        } else {
            request <- request |>
                httr2::req_body_raw(request_body)
        }
    }
    
    if (verb == "connection") {
        return(httr2::req_perform_connection(request)$body)
    } else {
        r <- httr2::req_perform(request)
    }

    # handle response, failing if HTTP error occurs
    if (isTRUE(parse_response)) {
        out <- parse_aws_s3_response(r, Sig, verbose = verbose)
    } else {
        out <- r
    }
    attributes(out) <- c(attributes(out), httr2::resp_headers(r))
    out
}

parse_aws_s3_response <- function(r, Sig, verbose = getOption("verbose")){
    if (isTRUE(verbose)) {
        message("Parsing AWS API response")
    }
    ctype <- httr2::resp_header(r, "content-type")
    if (is.null(ctype) || ctype == "application/xml"){
        response <- httr2::resp_body_xml(r) |>
            xml2::as_list() |>
            flatten_list()
    } else {
        response <- r
    }
    if (isTRUE(verbose)) {
        message(sprintf("%s: %s", httr2::resp_status(r), httr2::resp_status_desc(r)))
    }
    if (floor(httr2::resp_status(r)/100) == 3) {
        stop("Unexpected redirection")
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
    # Figure out 'path' or 'virtual' style. Default is 'path'.
    url_style <- match.arg(url_style)
    
    # handle S3-compatible storage URLs
    if (base_url != "s3.amazonaws.com") {
        if (isTRUE(verbose)) {
            message("Non-AWS base URL requested.")
        }
        accelerate <- FALSE
        dualstack <- FALSE
        if (!is.null(region) && region != "") {
            base_url <- paste0(region, ".", base_url)
        }
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
            # virtual-style URL
            if (isTRUE(accelerate) && grepl("\\.", bucketname)) {
                stop("To use 'accelerate' for bucket name with dots (.), 'url_style' must be 'path'")
            }
            url <- paste0(prefix, bucketname, ".", base_url)
        } else {
            # path-style URL (the default)
            url <- paste0(prefix, base_url, "/", bucketname)
        }
    }
    
    # cleanup terminal slashes
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
        message("S3 Request URL: ", url)
    }
    return(url)
}
