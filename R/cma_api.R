## The Cleveland Museum of Art Open Access API
## https://www.clevelandart.org/open-access-api
## https://openaccess-api.clevelandart.org/ (documentation)

cma_api <- "https://openaccess-api.clevelandart.org/api/artworks/"

cma_label <- "Cleveland (CMA)"

fx_search_cma <- function(q = "", highlight = FALSE, verbose = FALSE) {
  # The search endpoint returns a dataframe artworks whose metadata contains the
  # query string.
  # Example: https://openaccess-api.clevelandart.org/api/artworks/?q=baseball
  if (q == "*") q <- ""
  if (highlight) { highlight <- "&highlight=1" } else {highlight <- ""}
  search_url <- paste0(
    cma_api, "?",
    "q=", URLencode(q),
    "&has_image=1",
    "&limit=100", # If limit not provided, API return 1000 records
    highlight
  )
  if (verbose) message(paste("search_url:", search_url))
  result_set <- jsonlite::fromJSON(search_url)
  
  # The API returns _all_ IDs. Just keep the ones we plan to work with.
  sampled_set <- dplyr::slice_sample(result_set$data, n = 5)
  
  # Convert the dataframe into a list of artworks to display, where each artwork
  # is a list of attributes.
  artworks_list <- lapply(
    split(sampled_set, seq(nrow(sampled_set))), \(row) as.list(row)
  )

  return(artworks_list)
}
