max_results <- 5

fx_search <- function(api, q) {
  if (api == cma_label) {
    api_url = fx_search_cma(q, verbose = TRUE)
  } else if (api == met_label) {
    api_url = fx_search_met(q, verbose = TRUE)
  } else {
    api_url = "(unknown)"
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
  if (verbose) message(paste("search_result:", search_result))
  return(search_result)
}
