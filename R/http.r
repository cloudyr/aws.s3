utils::globalVariables(c("S"))

#' @title S3 HTTP Requests
#' 
#' @description This is the workhorse function for executing API requests for S3.
#'
#' @details This is mostly an internal function for executing API requests. 
#' In almost all cases, users do not need to access this directly.
#' 
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param url A character string containing the URL for the API endpoint.
#' @param bucket A character string containing the name of AWS bucket to use.
#' @param path A character string with the name of the object to put in the bucket 
#' (sometimes called the object or 'key name' in the AWS documentation.)
#' @param headers a list of request headers for the REST call.   
#' @param request_body character string of request body data.
#' @param region A character string containing the AWS region.
#' If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.
#' @param parse_response return the response as is, or parse and return as a list?  
#' default is TRUE.
#' @param ... Additional arguments passed to an HTTP request function, 
#' such as \code{\link[httr]{GET}}.
#'
#' @author Thomas J. Leeper \email{thosjleeper@@gmail.com}
#' 
#' @return the S3 response, or the relevant error.
#' 
#' @export

s3HTTP <- function(verb = "GET",
                   url = "https://s3.amazonaws.com",
                   bucket = "", 
                   path = "", 
                   headers = list(), 
                   request_body = "",
                   region = "us-east-1", 
                   key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                   secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                   parse_response = TRUE, 
                   ...) {
    ## Endpoint must match region (the default s3.amazonaws.com is us-east-1 region only)
    if (region != "us-east-1" && url == "https://s3.amazonaws.com")
      url = paste0("https://s3-", region, ".amazonaws.com")
    if (bucket != "" | path != "")
      url <- paste0(url, "/", bucket, path)
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    p <- httr::parse_url(url)
    action <- if (p$path == "") "/" else paste0("/", p$path)
    
    if (key == "") {
        headers$`x-amz-date` <- d_timestamp
        H <- do.call(httr::add_headers, headers)
    } else {
        Sig <- aws.signature::signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               verb = verb,
               action = action,
               query_args = p$query,
               canonical_headers = list(host = p$hostname,
                                        `x-amz-date` = d_timestamp),
               request_body = request_body,
               key = key, secret = secret)
        headers$`x-amz-date` <- d_timestamp
        headers$`x-amz-content-sha256` <- Sig$BodyHash
        headers$Authorization <- Sig$SignatureHeader
        H <- do.call(httr::add_headers, headers)
    }
    
    if (verb == "GET") {
      r <- httr::GET(url, H, ...)
    } else if (verb == "HEAD") {
      r <- httr::HEAD(url, H, ...)
      s <- httr::http_status(r)
      if (s$category == "success") {
          return(TRUE)
      } else {
          message(s$message)
          return(FALSE)
      }
    } else if (verb == "DELETE") {
      r <- httr::DELETE(url, H, ...)
      s <- httr::http_status(r)
      if (s$category == "success") {
          return(TRUE)
      } else {
          message(s$message)
          return(FALSE)
      }
    } else if (verb == "POST") {
      r <- httr::POST(url, H, ...)
    } else if (verb == "PUT") {
      if(request_body == "") {
        r <- httr::PUT(url, H, ...)
      } else if (file.exists(request_body)) {
        r <- httr::PUT(url, H, body = httr::upload_file(request_body), ...)
      } else {
        r <- httr::PUT(url, H, body = request_body, ...)
      }
    } else if (verb == "OPTIONS") {
      r <- httr::VERB("OPTIONS", url, H, ...)
    }

    #if parse_response, use httr's parsed method to extract as XML, then convert to list
    if (parse_response) {
      response_contents <- try(httr::content(r, "parsed"), silent = TRUE)
      if (!inherits(response_contents, "try-error")) {
        if (!is.null(response_contents)) {
            response <- XML::xmlToList(response_contents)
        } else {
            response <- NULL
        }
      } else {
        response_contents <- httr::content(r, "text")
        if (!is.null(response_contents)) {
            response <- XML::xmlToList(response_contents)
        } else {
            response <- NULL
        }
      }
    #otherwise just return the raw response
    } else if (!parse_response) {
      response <- r
    }
    
    #raise errors if bad values are passed. 
    if (httr::http_status(r)$category == "client error") {
      httr::warn_for_status(r)
      h <- httr::headers(r)
      out <- structure(response, headers = h, class = "aws_error")
    } else {
      out <- response
    }
    
    if (inherits(out, "aws_error")) {
      if (exists("S")) {
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
      }
    }
  out
}
