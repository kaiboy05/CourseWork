####################################################
# Programme that process data into visulizable data
#
# Target: GPC, HOG
####################################################

rawdata <- read.csv("data.csv")

getStock <- function(stock){
  #Turn Integers into Date
  dates <- paste(rawdata$Year, rawdata$Month, rawdata$Day, sep="-")
  dates <- as.Date(dates)
  
  #Get the stock price and return
  cprice <- get(stock, rawdata)
  
  lprice <- c(0, cprice)
  lprice <- lprice[-length(lprice)]
  
  return <- cprice - lprice
  return[1] = NA
  
  #Put into data frame
  result <- data.frame(dates, cprice, return)
  colnames(result) <- c("Date", "Closing Price", "Return of the Day")
  
  return(result)
}

Stock <- getStock("GPC")
View(Stock)


