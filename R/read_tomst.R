#' Read and preprocess a TOMST dendrometer CSV export
#'
#' Reads a semicolon-delimited TOMST dendrometer file (no header), parses the
#' timestamp in UTC, filters the time window, computes the diameter variation
#' relative to the first span in the window, and returns a tidy data frame.
#'
#' @param nameFile character(1). Path to the TOMST CSV export. Expected columns:
#'   `V2` (timestamp as "YYYY.MM.DD" or "YYYY.MM.DD HH:MM"), `V4` (temperature),
#'   `V7` (span).
#' @param dendrometer character(1). Identifier for the dendrometer; added as
#'   a factor column `series`.
#' @param ts_start,ts_end character(1) or POSIXct. Start and end of the time
#'   selection. If character, they are parsed in the same timezone as the file
#'   (`UTC` by default).
#'
#' @details
#' The function accepts dates with or without time; dates without time are set
#' to 00:00. Timezone is assumed to be `"UTC"` for both parsing and filtering.
#' Rows with `span == 0` trigger a warning. If no rows are within the selected
#' window, an empty data frame with the expected columns is returned.
#'
#' @return A `data.frame` with columns:
#' \itemize{
#'   \item `ts`   (`POSIXct`): timestamp (UTC)
#'   \item `date` (`Date`): date (UTC)
#'   \item `span` (`numeric`): raw span (`V7`)
#'   \item `value`(`numeric`): span zeroed to first value within window
#'   \item `temp` (`numeric`): temperature (`V4`)
#'   \item `series` (`factor`): factor with level equal to `dendrometer`
#' }
#'
#' @examples
#' \dontrun{
#' df <- read_tomst("data/TMS_xxx.csv", dendrometer = "D1",
#'                         ts_start = "2023-05-01 00:00",
#'                         ts_end   = "2023-05-31 23:59")
#' }
#'
#' @export
read_tomst <- function(nameFile, dendrometer, ts_start, ts_end){

  date_format = "%Y.%m.%d %H:%M"
  timezone = "UTC"
  File <- read.csv(nameFile, sep = ";",  header=FALSE, skip=0, dec=",", stringsAsFactors=FALSE)
  print(dendrometer)
  ts<-File$V2
  ts2 <- gsub("^(\\d{4}\\.\\d{2}\\.\\d{2})$", "\\1 00:00", ts)
  File$ts<-as.POSIXct(ts2, format=date_format, tz=timezone) #raw dendrometer timezone (UTC to be confirmed)
  File$date <- as.Date(File$ts)

  # Keep data of dates we're interested in:
  File<-File[which(File$ts>=as.POSIXct(ts_start, tz = timezone) & File$ts<=as.POSIXct(ts_end, tz = timezone)),] #date selection
  File$span<-as.numeric(File$V7)
  File$value<-File$span-File$span[1] #zeroing variations in diameter
  File$temp<-as.numeric(File$V4)


  if(nrow(File)==0){
    warning(paste("! No valid data within the surveyed period for dendrometer",dendrometer))
    return(File)

  }else{

    if(any(File$span == 0)){
      warning(paste("Span contains zero for dendrometer",dendrometer))
      warning(paste("You should consider removing this reading of ",dendrometer))
    }

    File$series<-as.factor(dendrometer)
    File<-subset(File,select=c(ts,date,span,value,temp,series))
    return(File)
  }
}
