
library(httr2)

get_eds <- function(dataset, start, end, area = "DK1") {
  resp <- request("https://api.energidataservice.dk/dataset") |>
    req_url_path_append(dataset) |>
    req_url_query(
      start  = start,
      end    = end,
      filter = jsonlite::toJSON(list(PriceArea = area)),
      limit  = 0                                  # 0 = all records
    ) |>
    req_retry(max_tries = 5, backoff = \(i) 30) |>  # rate limit helper
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  resp$records
}

