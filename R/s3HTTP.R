#' @title S3 HTTP Requests
#' 
#' @description This is the workhorse function for executing API requests for S3.
#'
#' @details This is mostly an internal function for executing API requests. 
#' In almost all cases, users do not need to access this directly.
#' 
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param bucket A character string with the name of the bucket, or an object of class \dQuote{s3_bucket}. If the latter and a region can be inferred from the bucket object attributes, then that region is used instead of \code{region}.
#' @param path A character string with the name of the object to put in the bucket 
#' (sometimes called the object or 'key name' in the AWS documentation.)
#' @param query any queries, passed as a named list 
#' @param headers a list of request headers for the REST call.   
#' @param request_body character string of request body data.
#' @param accelerate A logical indicating whether to use AWS transfer acceleration, which can produce significant speed improvements for cross-country transfers. Acceleration only works with buckets that do not have dots in bucket name.
#' @param region A character string containing the AWS region. Ignored if region can be inferred from \code{bucket}.
#' If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.
#' @param parse_response return the response as is, or parse and return as a list?  
#' default is TRUE.
#' @param ... Additional arguments passed to an HTTP request function.
#' such as \code{\link[httr]{GET}}.
#'
#' @return the S3 response, or the relevant error.
#' 
#' @importFrom httr GET POST PUT HEAD DELETE VERB upload_file parse_url add_headers
#' @importFrom httr http_error http_status warn_for_status content headers
#' @importFrom xml2 read_xml as_list
#' @importFrom utils URLencode
#' @import aws.signature
#' @export
s3HTTP <- function(verb = "GET",
                   bucket = "", 
                   path = "", 
                   query = NULL,
                   headers = list(), 
                   request_body = "",
                   accelerate = FALSE,
                   region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"), 
                   key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                   secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                   parse_response = TRUE, 
                   ...) {
    
    bucketname <- get_bucketname(bucket)
    bucketregion <- get_region(bucket)
    if (!is.null(bucketregion)) {
        region <- bucketregion
    }
    if (region == "") {
        region <- "us-east-1"
    }
    
    encodedPath <- if (path == "") "/" else {
        paste(sapply(
            strsplit(path, '/')[[1]],
            function(i) URLencode(i, TRUE),
            USE.NAMES = FALSE
        ), collapse = '/')
    }

    url <- setup_s3_url(bucketname, region, encodedPath, accelerate)
    p <- parse_url(url)

    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    action <- if (p$path == "") "/" else paste0("/", p$path)
    canonical_headers <- c(list(host = p$hostname,
                                `x-amz-date` = d_timestamp), headers)

    if (is.null(query) && !is.null(p$query)) {
        query <- p$query
    }
    if (all(sapply(query, is.null))) {
        query <- NULL
    }
    if (key == "") {
        headers$`x-amz-date` <- d_timestamp
        Sig <- list()
        H <- do.call(add_headers, headers)
    } else {
        Sig <- aws.signature::signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               verb = verb,
               action = action,
               query_args = query,
               canonical_headers = canonical_headers,
               request_body = request_body,
               key = key, secret = secret)
        headers$`x-amz-date` <- d_timestamp
        headers$`x-amz-content-sha256` <- Sig$BodyHash
        headers$Authorization <- Sig$SignatureHeader
        H <- do.call(add_headers, headers)
    }
    if (verb == "GET") {
      r <- GET(url, H, query = query, ...)
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
    
    if (parse_response) {
      out <- parse_aws_s3_response(r, Sig)
    } else {
      out <- r
    }
    attributes(out) <- c(attributes(out), headers(r))
    out
}

parse_aws_s3_response <- function(r, Sig, verbose = getOption("verbose")){
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
    if (http_error(r)) {
        warn_for_status(r)
        h <- headers(r)
        out <- structure(response, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- response
    }
    
    return(out)
}

setup_s3_url <- function(bucketname, region, path, accelerate) {
    if (bucketname == "") {
        if (region == "us-east-1") {
            url <- paste0("https://s3.amazonaws.com")
        } else {
            url <- paste0("https://s3-", region, ".amazonaws.com")
        }
    } else {
        if (accelerate) {
            if (grepl("\\.", bucketname)) {
                stop("To use accelerate, bucket name must not contain dots (.)")
            }
            url <- paste0("https://", bucketname, ".s3-accelerate.amazonaws.com")
        } else {
            if (region == "us-east-1") {
                url <- paste0("https://", bucketname, ".s3.amazonaws.com")
            } else {
                url <- paste0("https://", bucketname, ".s3-", region, ".amazonaws.com")
            }
        }
    }
    url <- if (grepl('^[\\/].*', path)) { paste0(url, path) } else { paste(url, path, sep = "/") }
    return(url)
}
