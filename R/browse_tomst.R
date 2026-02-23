#' Quickly probe a TOMST dendrometer CSV for date coverage
#'
#' Reads a TOMST dendrometer file and returns the minimum and maximum calendar
#' dates present. If the file is empty (first cell equals "File is empty"),
#' returns `NA` for both bounds.
#'
#' @param nameFile character(1). Path to the TOMST CSV export (semicolon
#'   separated, decimal comma, no header).
#'
#' @details
#' Timestamps are parsed in UTC. Dates without times are interpreted as 00:00.
#'
#' @return A tibble/data frame with two columns:
#' \itemize{
#'   \item `min_date` (`Date` or `NA`)
#'   \item `max_date` (`Date` or `NA`)
#' }
#'
#' @examples
#' \dontrun{
#' preread_tomst_dendro("data/TMS_xxx.csv")
#' }
#'
#' @export
preread_tomst_dendro <- function(nameFile) { ... }

browse_tomst <- function(nameFile){

  date_format = "%Y.%m.%d %H:%M"
  timezone = "UTC"
  File <- read.csv(nameFile, sep = ";",  header=FALSE, skip=0, dec=",", stringsAsFactors=FALSE)

  if(File$V1[1] == "File is empty")
    out<-tibble(min_date = NA,
                max_date = NA)
  else{
    ts<-File$V2
    ts2 <- gsub("^(\\d{4}\\.\\d{2}\\.\\d{2})$", "\\1 00:00", ts)
    File$ts<-as.POSIXct(ts2, format=date_format, tz=timezone) #raw dendrometer timezone (UTC to be confirmed)
    File$date <- as.Date(File$ts)

    out<-tibble(min_date = min(File$date),
                max_date = max(File$date))
  }
  return(out)

}
