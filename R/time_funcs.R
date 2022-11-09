#' Age in years
#'
#' Calculates age in years
#' @param from date POSIXlt/string or similar to be converted to POSIXlt.
#' e.g. "YYYY-MM-DD"
#' @param to date POSIXlt/string or similar to be converted to POSIXlt.
#' Defaults to to now()
#'
#' @keywords age
#' @export
#' @examples
#' age("1988-12-19", "2017-11-22")
#' # 28
age <- function(from, to = strptime(lubridate::now(), format = "%Y-%m-%d")) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

#' Judge daily frequency of date/time vars
#'
#' Gives T/F for whether a list of date/times are at a certain frequency per
#' day (or part thereof). Requires lubridate:: package.
#' @param datims vector of POSIXct date/times.
#' @param p_start start of period to be considered, POSIXct date/time.
#' @param p_end end of period to be considered, POSIXct date/time.
#' @param freq frequency required per 24 hour period.
#' @param n_days number of days to be considered, from p_start. Only the
#' earliest of p_end/p_start  + n_days is taken.
#'
#' @keywords hourly daily per
#' @export
#' @examples
#' datims <- c("2015-12-06 08:44:00",
#' "2015-12-06 12:00:00", "2015-12-06 13:00:00", "2015-12-06 16:00:00",
#' "2015-12-06 16:00:00", "2015-12-06 20:20:00", "2015-12-06 23:45:00",
#' "2015-12-07 04:00:00", "2015-12-07 16:45:00", "2015-12-07 19:45:00",
#' "2015-12-07 23:30:00", "2015-12-08 04:00:00", "2015-12-08 07:30:00",
#' "2015-12-08 12:00:00")
#' datims <- as.POSIXct(datims)
#'
#' perday(datims, datims[1], datims[14], freq = 6, n_days = 3)
#' # FALSE
#' perday(datims, datims[1], datims[14], freq = 5, n_days = 3)
#' # TRUE

perday <- function(datims, p_start, p_end, freq,
                   n_days = 3) {
  # check args, catch invalid data
  if(any(missing(datims),
         missing(p_start),
         missing(p_end),
         missing(freq))) {
    stop("must provide args: datims, p_start, p_end, freq")
  }
  if(is.na(p_end)) p_end = p_start + lubridate::hours(n_days * 24)
  if(is.na(p_start)) {
    warning("arg: p_start has value NA")
    return(NA)
  }
  if(p_end < p_start) {
    warning(paste0("p_start =", p_start, "after p_end = ", p_end))
    p_end = p_start + lubridate::hours(n_days * 24)
  }

  p_dur_days <- as.numeric(
    lubridate::as.duration(
      lubridate::interval(p_start, p_end)),
    "days")

  # set intervals: e.g. if px is discharged after 36 hours intervals are:
  # day1: (p_start, p_start + 24) [duration == 1 day]
  # day2: (p_start + 24, p_end)  [duration == 12 hrs]
  # day3+: (p_start + 48, p_start + 48) [duration == 0]
  intervals <- lubridate::interval(p_start + lubridate::hours(24 * (0:(n_days-1))),
                                   p_start + lubridate::minutes(
                                     as.integer(round(24 * 60 *
                                                        pmax(0:(n_days-1),
                                                             pmin(p_dur_days, 1:n_days))))))

  # set required frequency in each interval
  req_freq = floor(as.numeric(lubridate::as.duration(intervals), "days") * freq)

  # test if each datim is in each interval bool_mat has (row, col) = (n_datim, n_days)
  bool_mat = t(mapply(lubridate::`%within%`, datims,  MoreArgs = list(intervals)))

  if(n_days > 1) period_sums = colSums(bool_mat)
  else if(n_days == 1) period_sums = sum(bool_mat)

  return(all(period_sums >= req_freq))
}

