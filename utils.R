#' User provided date string checking
#' @noRd
#' @description Checks user provided date strings to ensure they can be cast to yyyy-mm-dd
#' @keywords internal
check_date <- function(start_date, end_date){
  start_status <- tryCatch({
    lubridate::ymd(start_date)
  }, warning = function(w){
    stop("start_date must be in yyyy-mm-dd format", call. = FALSE)
  })

  end_status <- tryCatch({
    lubridate::ymd(end_date)
  }, warning = function(w){
    stop("end_date must be in yyyy-mm-dd format", call. = FALSE)
  })

  if(lubridate::ymd(start_date) > lubridate::ymd(end_date)){
    stop("start_date is greater than end_date")
  }
}

#' Checking if internet connection available
#' @noRd
#' @description Checks if connection to internet can be made. Useful to check before running API-related tests
#' @author Sam Albers
#' @keywords internal
has_internet <- function(){
  z <- try(suppressWarnings(
    readLines('https://www.google.ca', n = 1)
    ), silent = TRUE)
  !inherits(z, "try-error")
}

#' Checking if Quinte is live before running tests
#' @noRd
#' @description Checks if connection to KiWIS Quinte can be made.
#' @keywords internal
exp_live <- function(){
  exp_url <- "https://waterdata.quinteconservation.ca/KiWIS/KiWIS?datasource=0&service=kisters&type=queryServices&request=getstationlist&format=json"

  raw <- tryCatch(
    {
      httr::GET(
        exp_url,
        httr::timeout(15)
      )
    },
    error = function(e) {
      return(e)
    }
  )

  !inherits(raw, "error")
}

#' Verifying HTTP response
#' @noRd
#' @description Checks HTTP response from KiWIS server
#' @keywords internal
check_ki_response <- function(response){
  # Check for query error
  if(inherits(response, "error")){
    stop("Query returned error: ", raw$message)
  }

  # Check for timeout / 404
  if(!inherits(response, "response")){
    stop("Check that Quinte KiWIS is accessible via a web browser.")
  }
}
