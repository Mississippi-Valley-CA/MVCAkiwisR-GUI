#' Get values for time series id or list of timeseries ids.
#'
#' @export
#' @description Returns time series values for given time series id and date range.
#' @author Ryan Whaley, \email{rdgwhaley@@gmail.com}
#' @param ts_id Either: a single time series id or a vector of time series ids.
#'  Time series ids can be found using the `ki_timeseries_list` function.
#' @param start_date A date string formatted "YYYY-MM-DD". Defaults to yesterday.
#' @param end_date A date string formatted "YYYY-MM-DD". Defaults to today.
#' @return A tibble with following columns by default:
#'  Timestamp, Value, Units, station_name, station_no, stationparameter_name, ts_name, ts_id
#' @examples
#' \dontrun{
#' ki_timeseries_values(
#'   ts_id = "1395042",
#'   start_date = "2015-12-01",
#'   end_date = "2018-01-01"
#' )
#' }
#'
ki_timeseries_values <- function(ts_id, start_date, end_date) {
  # Default to past 24 hours
  if (missing(start_date) || missing(end_date)) {
    message("No start or end date provided, trying to return data for past 24 hours")
    start_date <- Sys.Date() - 1
    end_date <- Sys.Date()
  } else {
    check_date(start_date, end_date)
  }

  if (missing(ts_id)) {
    stop("Please enter a valid ts_id.")
  } else {
    # Account for multiple ts_ids
    ts_id_string <- paste(ts_id, collapse = ",")
  }

  api_query <- list(
    format = "json",
    datasource = 0,
    timezone = "EST",
    service = "kisters",
    type = "queryServices",
    request = "getTimeseriesValues",
    ts_id = ts_id_string,
    from = start_date,
    to = end_date,
    metadata = "true",
    returnfields = "Timestamp,Value",
    md_returnfields = "station_name,parametertype_name,station_no,stationparameter_name,ts_name,ts_id,ts_unitsymbol"
  )

  # Send request
  raw <- tryCatch({
    httr::GET(
      url = "https://waterdata.quinteconservation.ca/KiWIS/KiWIS?",
      query = api_query,
      httr::timeout(120)
    )
  }, error = function(e) {
    return(e)
  })

  check_ki_response(raw)

  # Parse response
  raw_content <- httr::content(raw, "text")

  # Parse text
  json_content <- jsonlite::fromJSON(raw_content)

  if (length(names(json_content)) == 3) {
    stop(json_content$message)
  }
  if ("rows" %in% names(json_content)) {
    num_rows <- sum(as.numeric(json_content$rows))
    if (num_rows == 0) {
      stop("No data available for selected ts_id(s).")
    }
  }

  ts_cols <- unlist(strsplit(json_content$columns[[1]], ","))

  content_dat <- purrr::map_df(
    1:length(json_content$data),
    function(ts_chunk) {
      ts_data <- tibble::as_tibble(
        json_content$data[[ts_chunk]],
        .name_repair = "minimal",
      )

      names(ts_data) <- ts_cols

      dplyr::mutate(
        ts_data,
        Timestamp = as.POSIXct(ts_data$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz="EST"),
        Value = as.numeric(ts_data$Value),
        Units = json_content$ts_unitsymbol[[ts_chunk]],
        'Station Name' = json_content$station_name[[ts_chunk]],
        'Station No' = json_content$station_no[[ts_chunk]],
        'Parameter Name' = json_content$stationparameter_name[[ts_chunk]],
        'Parameter Type' = json_content$parametertype_name[[ts_chunk]],
        Timeseries = json_content$ts_name[[ts_chunk]],
        'TS ID' = json_content$ts_id[[ts_chunk]],
      )
    }
  )

  return(content_dat)
}
