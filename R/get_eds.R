library(httr2)

# area = NULL without price areas, columns = NULL for all
get_eds <- function(dataset, start, end, area = "DK1", columns = NULL) {
  req <- request("https://api.energidataservice.dk/dataset") |>
    req_url_path_append(dataset) |>
    req_url_query(start = start, end = end, limit = 0)                # 0 = all records
  if (!is.null(area))    req <- req_url_query(req, filter = jsonlite::toJSON(list(PriceArea = area)))
  if (!is.null(columns)) req <- req_url_query(req, columns = paste(columns, collapse = ","))
  resp <- req |>
    req_retry(max_tries = 5, backoff = \(i) 30) |>                    # rate limit helper
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  resp$records
}
