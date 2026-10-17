max_results <- 5

fx_search <- function(api, q, highlight = FALSE) {
  if (api == cma_label) {
    api_url <- fx_search_cma(q, highlight, verbose = TRUE)
  } else if (api == met_label) {
    api_url <- fx_search_met(q, highlight, verbose = TRUE)
  } else {
    api_url <- "(unknown)"
  }
  return(api_url)
}

fx_search_result <- function(api, artwork, verbose = FALSE) {
  if (api == cma_label) {
    img_url <- artwork$images$web$url
    title <- artwork$title
    artist <- artwork$creators[[1]][["description"]]
    creation_date <- artwork$creation_date
  } else if (api == met_label) {
    img_url <- artwork$primaryImageSmall
    title <- artwork$title
    artist <- artwork$artist
    creation_date <- artwork$objectDate
  } else {
    img_url <- ""
    title <- ""
    artist <- ""
    creation_date <- ""
  }
  search_result <- list(
    img_url = img_url,
    title = title,
    artist = artist,
    creation_date = creation_date
  )
  if (verbose) {
    print(paste("search_result:", search_result))
  }
  return(search_result)
}

fx_image_attachment <- function(image_url) {
  tryCatch(
    {
      if (
        !is.character(image_url) ||
          length(image_url) != 1L ||
          is.na(image_url) ||
          !nzchar(trimws(image_url))
      ) {
        return(NULL)
      }

      image_path <- tempfile()
      on.exit(unlink(image_path), add = TRUE)

      response <- httr::GET(
        image_url,
        httr::timeout(10),
        httr::write_disk(image_path, overwrite = TRUE)
      )
      httr::stop_for_status(response)

      content_type <- httr::headers(response)[["content-type"]]
      if (is.null(content_type)) {
        stop("Image response has no content type.")
      }

      mime <- trimws(strsplit(content_type, ";", fixed = TRUE)[[1]][1])

      shinychat::chat_attachment(
        image_path,
        mime = mime,
        name = "Selected artwork"
      )
    },
    error = function(e) {
      message("Artwork image unavailable: ", conditionMessage(e))
      NULL
    }
  )
}
