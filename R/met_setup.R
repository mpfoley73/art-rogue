met_api <- "https://collectionapi.metmuseum.org/public/collection/v1/"

# Create a named vector of Met departments

dept_url <- paste0(met_api, "departments")

dept_list <- jsonlite::fromJSON(dept_url)

met_departments <- setNames(
  dept_list$departments$departmentId,
  dept_list$departments$displayName
)

