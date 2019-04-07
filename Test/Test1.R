####################################################
# Programme that process data into visulizable data
#
# Target: GPC, HOG
####################################################

pic = TRUE
par(mfrow = c(1, 1))
count = 1

rawdata <- read.csv("data.csv")

if(!pic){sink("output.txt")}

#Function that get the needed stock
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

#Function that get the needed year
FilterStock <- function(stock, year = 2015:2018, month = 0){
  if(all(month < 1) | all(month > 12)){
    return(subset(stock, format(Date, "%Y") %in% year))
  }
  else{
    return(subset(stock,  as.numeric(format(Date, "%Y")) %in% year & 
                          as.numeric(format(Date, "%m")) %in% month))
  }
}

#Function that print summary statistics of years
Stocksummary = function(stock, name){
  for(i in 2015:2018){
    cat("Summary of the return of", name, "in", i, ":\n")
    print(summary(na.omit(FilterStock(stock, i)$`Return of the Day`)))
    cat("\n")
    for(j in 1:12){
      cat("Summary of the return of", name, "in", i, month.name[j], ":\n")
      print(summary(na.omit(FilterStock(stock, i, j)$`Return of the Day`)))
      cat("\n")
    }
  }
}

#Function that print summary statistics of year or month and return the summary table
StocksummarySpecific = function(stock, name, year = 2015:2018, month = 0){
  s <- FilterStock(stock, year, month)
  if(all(month < 1) | all(month > 12)){
    cat("Summary of the return of", name, "in", year, ":\n")
  }
  else{
    cat("Summary of the return of", name, "in", year, month.name[month], ":\n")
  }
  s <- summary(na.omit(s$`Return of the Day`))
  cat("\n")
  return(s)
}

#Function that plot the return of stock of a year or month
StockGraph = function(stock, name, year = 2015:2018, month = 0){
  s <- FilterStock(stock, year, month)
  title = ""
  if(length(month) > 1){
    title = paste("Return of", name, "from", year, month.name[month[1]], 
                                    "to", year, month.name[month[length(month)]])
  }
  else{
    if(month < 1 | month > 12){
      if(length(year) > 1){
        title = paste("Return of", name, "from", year[1], "to", year[length(year)])
      }
      else{
        title = paste("Return of", name, "in", year)
      }
    }
    else{
      title = paste("Return of", name, "in", year, month.name[month])
    }
  }
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  plot(x = s$Date, y = s$`Return of the Day`, type = "l", col = "blue",
        xlab = "Date", ylab = "Return ($)", main = title)
  lines(x = s$Date, y = rep(0, nrow(s)), col = "red")
  if(!pic){
    dev.off()
    count <<- count + 1
  }
}

#Function that compare year by year
StockGraphCompareYear = function(stock, name){
  if(pic){par(mfrow = c(2, 1))}
  for(i in 2015:2018){
    StockGraph(stock, name, i)
  }
}

#Function that compare month by month in whole year
StockGraphCompareAllMonth = function(stock, name, year){
  if(pic){par(mfrow = c(4, 3))}
  for(i in year){
    for(j in 1:12){
      StockGraph(stock, name, i, j)
    }
  }
}

#Function that compare month by month in years
StockGraphCompareMonth = function(stock, name, year, month){
  if(pic){par(mfrow = c(2, 1))}
  for(i in year){
    for(j in month){
      StockGraph(stock, name, i, j)
    }
  }
}

#Function that compare seasons in years
StockGraphCompareSeason = function(stock, name, year, month){
  if(pic){par(mfrow = c(2, 1))}
  for(i in year){
    StockGraph(stock, name, i, month)
  }
}

main = function(){
  #Get the stocks
  S1 = "GPC"
  S2 = "HOG"
  
  Stock1 <- getStock(S1)
  Stock2 <- getStock(S2)
  
  
  #View(Stock1)
  #View(Stock2)
  
  #Get the summary statistics
  cat("Summary of the return of", S1, "from 2015 to 2018:")
  print(summary(na.omit(Stock1$`Return of the Day`)))
  cat("\n")
  cat("Summary of the return of", S2, "from 2015 to 2018:")
  print(summary(na.omit(Stock2$`Return of the Day`)))
  cat("\n\n")
  
  #Get the summary statistics year by year
  Stocksummary(Stock1, S1)
  Stocksummary(Stock2, S2)
}

main()

if(!pic){sink()}