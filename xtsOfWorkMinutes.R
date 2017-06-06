#DESCRIPTION:  R function to calculate minutes of work in a time range.

#method 'xtsOfWorkMinutes' return xts object with dates worked as indexes and minutes worked as values for each index.

#EXPLANATION OF PARAMETERS
#rangeFrom (POSIXlt type) starting day/time of the range for which you want to calculate minutes
#rangeTo (POSIXlt type) ending day/time of the range for which you want to calculate minutes
#dayStart (POSIXlt type) is a start working hour:minutes withing day limit. Default: dayStart=strptime("08:30",format = "%H:%M")
#dayEnd (POSIXlt type) is a ending working hour:minutes withing day limit. Default: dayEnd = strptime("17:30",format = "%H:%M")
#lunchAdjust (negative integer value) is a number of minutes the total day work minutes need to be adjust for lunch break. Default: lunchAdjust = - 60
#weekends (string type vector). Default: weekends = c('Saturday','Sunday')
#holidays (Date type vector).  A vector of dates (format:'2016-07-04') that should be excluded because they are holidays. Example: holidays <- as.Date(c(2016-12-23','2016-12-26','2016-12-30'))
#vacationDays (Date type vector).  A vector of dates (format:'2016-07-04') that should be excluded because they are vacation days. Example: vacationDays <- as.Date(c(2016-12-23','2016-12-26','2016-12-30'))

xtsOfWorkMinutes <- function(rangeFrom,
                             rangeTo,
                             dayStart=strptime("08:30",format = "%H:%M"),
                             dayEnd = strptime("17:30",format = "%H:%M"),
                             lunchAdjust = - 60,
                             weekends = c('Saturday','Sunday'),
                             holidays=NULL,
                             vacationDays=NULL){
  require(xts)
  
  #LOCAL FUNCTIONS TO CALCULATE MINUTES OF THE FIRST AND LAST DATE IN RANGE, AS THESE MAY BE PARTIAL OF THE FULL WORK DAY  
  lastDayMinutes <- function(B=rangeTo,work.start=dayStart,work.end = dayEnd,lunch.minutes = lunchAdjust){
    #end.action before the work start
    if(((B$hour * 60) + B$min) < ((work.start$hour * 60) + work.start$min))
    {
      #remove the B date or 
      m <- 0
    } else if(((B$hour * 60) + B$min) > ((work.end$hour * 60) + work.end$min)){
      #end.action after work end
      m <- (((work.end$hour * 60) + work.end$min) - ((work.start$hour * 60) + work.start$min))
    } else {
      #end.action after work start and before work end
      m <- (((B$hour * 60) + B$min) - ((work.start$hour * 60) + work.start$min))
    }
    #adjust for lunch hour
    if(B$hour >= 13){
      m <- m + lunch.minutes
    }
    return(m)
  }
  firstDayMinutes <- function(A=rangeFrom,work.start=dayStart,work.end = dayEnd,lunch.minutes = lunchAdjust){ 
    #start.action before the work start
    if(((A$hour * 60) + A$min) < ((work.start$hour * 60) + work.start$min))
    {
      #start.action before work start: full day
      m <- (((work.end$hour * 60) + work.end$min) - ((work.start$hour * 60) + work.start$min))
    } else if(((A$hour * 60) + A$min) > ((work.end$hour * 60) + work.end$min)){
      #start.action after work end 
      m <- 0
    } else {
      #start.action after work start and before work end: remainder of the day
      m <- (   ((work.end$hour * 60) + work.end$min) - ((A$hour * 60) + A$min)  )
    }
    #adjust for lunch hour
    if(A$hour <= 12){
      m <- m + lunch.minutes
    }
    return(m)
  }
  
  #EXECUTION  
  range.days <- seq(as.Date(rangeFrom),as.Date(rangeTo), by = 'days')
  
  range.minutes <- rep((((dayEnd$hour * 60) + dayEnd$min) - ((dayStart$hour * 60) + dayStart$min)) + lunchAdjust ,length(range.days))
  
  if(length(range.days)>1)
  {
    range.minutes[1]                      <- firstDayMinutes(rangeFrom)
    range.minutes[length(range.minutes)]  <- lastDayMinutes(rangeTo)
    
  } else  {
    
    adm <- range.minutes[1] #all day minutes
    fdnw <- adm - firstDayMinutes(rangeFrom) #first day not worked
    ldnw <- adm - lastDayMinutes(rangeTo) #last day not worked
    range.minutes[1]  <- adm - (fdnw + ldnw)
  }
  
  L <- unlist(lapply(range.days, function(x) !x %in% holidays & !weekdays(x) %in% weekends & !x %in% vacationDays))
  
  temp <- as.POSIXlt(range.days)
  if(length(range.days)>1){
      temp[1] <- rangeFrom
      temp[length(temp)] <- rangeTo
  } else
  {
    temp[1] <- rangeFrom
  }
 
  working.days <- temp[L]
  
  working.minutes <- range.minutes[L]
  working.minutes <- as.data.frame(working.minutes)
  as.xts(working.minutes,order.by = working.days)
}




