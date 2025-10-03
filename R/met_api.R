## The Metropolitan Museum of Art Collection API
## https://metmuseum.github.io/

met_api <- "https://collectionapi.metmuseum.org/public/collection/v1/"

met_label <- "Metropolitan (MMA)"

fx_search_met <- function(q, verbose = FALSE) {
  # The search endpoint returns a vector of Object IDs of artworks whose
  # metadata contains the query string.
  # Example: https://collectionapi.metmuseum.org/public/collection/v1/search?q=baseball
  search_url <- paste0(
    met_api, 
    "search?isHighlight=true&isOnView=true&hasImages=true", 
    "&q=", URLencode(q)
  )
  if (verbose) message(paste("search_url:", search_url))
  result_set <- jsonlite::fromJSON(search_url)

  # The API returns _all_ IDs. Just keep the ones we plan to work with.
  sampled_set <- sample(result_set$objectIDs, size = 5)

  # Loop through the IDs to construct a list of artworks to display.
  artworks_list <- list()
  for (id in sampled_set) {
    # The object endpoint returns the metadata associated with the object.
    # Not every ID has a record! Use a try-catch to gracefully ignore 404 errors.
    # Example: https://collectionapi.metmuseum.org/public/collection/v1/objects/437532
    object_url <- paste0(met_api, "objects/", id)
    art = tryCatch({
      jsonlite::fromJSON(object_url)
    }, error = function(e) {
      message(conditionMessage(e))
      NULL
    })
    if (!is.null(art)) {
      artworks_list[[length(artworks_list) + 1]] = art
    }
  }
  return(artworks_list)
}
