#' Compute periods in increment of months
#' 
#' Computes the total number of months between two dates including fractional months
#'
#' @param start_date  Date object representing the starting date
#' @param end_date Date object representing the ending date
#' @return numeric representing the fractional number of months between the two dates provided
#' @author Anand Krishnakumar
#' 
getMonthlyPeriods <- function(start_date, end_date) {
  
  first_month = zoo::as.yearmon(start_date)
  first_month_day = as.numeric(format(start_date, "%d"))
  first_month_day_count = as.numeric(zoo::as.Date(first_month, frac = 1) - start_date) + 1
  first_month_total_day_count = as.numeric(zoo::as.Date(first_month, frac = 1) - 
                                             zoo::as.Date(first_month, frac = 0)) + 1
  
  last_month = zoo::as.yearmon(end_date)
  last_month_day = as.numeric(format(end_date, "%d"))
  last_month_day_count = last_month_day
  last_month_total_day_count = as.numeric(zoo::as.Date(last_month, frac = 1) - 
                                            zoo::as.Date(last_month, frac = 0)) + 1
  
  whole_month_count = length(seq(start_date, end_date, by = "month")) - ifelse(first_month_day > last_month_day, 1, 2)
  
  # Compute the number of periods as a fractional number of months
  num_periods = first_month_day_count/first_month_total_day_count + whole_month_count + 
    last_month_day_count/last_month_total_day_count
  
  return(num_periods)
}
