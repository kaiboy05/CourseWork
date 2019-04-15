####################################################
# Programme that process data into visulizable data
#
# Target: GPC, HOG
####################################################

pic = TRUE
reg = TRUE
Isprint = FALSE
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

#Function that generate graph title
genTitleName = function(type, name, year, month, abb){
  if(abb){
    title = name
    if(length(year) > 1){
      title = paste(title, year[1], "-", year[length(year)])
    }
    else{
      title = paste(title, year)
    }
    if(length(month) > 1){
      title = paste(title, month.abb[month[1]], "-", month.abb[month[length(month)]])
    }
    else{
      if(all(month >= 1) & all(month <= 12)){
        title = paste(title, month.abb[month])
      }
    }
  }
  else{
    if(length(month) > 1){
      title = paste(type, "of the return of", name, "from", year, month.name[month[1]], 
                    "to", year, month.name[month[length(month)]])
    }
    else{
      if(all(month < 1) | all(month > 12)){
        if(length(year) > 1){
          title = paste(type, " of the return of", name, "from", year[1], "to", year[length(year)])
        }
        else{
          title = paste(type, " of the return of", name, "in", year)
        }
      }
      else{
        title = paste(type, " of the return of", name, "in", year, month.name[month])
      }
    }
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
  
  nyear = year[1]
  title = ""
  if(length(year) > 1){
    nyear = paste(nyear, "-", year[length(year)])
  }
  
  if(all(month < 1) | all(month > 12)){
    title = paste("Summary of the return of", name, "in", nyear, ":\n")
  }
  else{
    title = paste("Summary of the return of", name, "in", nyear, month.name[month], ":\n")
  }
  cat(title)
  s <- summary(na.omit(s$`Return of the Day`))
  cat("\n")
  return(s)
}

#Function that plot the return of stock of a year or month
StockGraph = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  title = genTitleName("Graph", name, year, month, abb)
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  plot(x = s$Date, y = s$`Return of the Day`, type = "l", col = "blue",
        xlab = "Date", ylab = "Return (£)", main = title)
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
      StockGraph(stock, name, i, j, abb = TRUE)
    }
  }
}

#Function that compare month by month in years
StockGraphCompareMonth = function(stock, name, year, month){
  if(pic){par(mfrow = c(2, 1))}
  for(i in year){
    for(j in month){
      StockGraph(stock, name, i, j, abb = TRUE)
    }
  }
}

#Function that compare seasons in years
StockGraphCompareSeason = function(stock, name, year, month){
  if(pic){par(mfrow = c(2, 1))}
  for(i in year){
    StockGraph(stock, name, i, month, abb = TRUE)
  }
}

#Function that show histogram of the return
StockHist = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  title = genTitleName("Histogram", name, year, month, abb)
  if(all(month == 0)){
    nbreak = 50
  }
  else{
    nbreak = 10 + length(month) * 3
  }
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  h <- hist(s$`Return of the Day`, breaks = nbreak, xlab = "Return (£)", main = title)
  if(!pic){
    dev.off()
    count <<- count + 1
  }
  return(h)
}

#Function that show histogram of the return month by month in whole year
StockHistCompareAllMonth = function(stock, name, year = 2015:2018){
  if(pic){par(mfrow = c(4, 3))}
  for(i in year){
    for(j in 1:12){
      StockHist(stock, name, i, j, TRUE)
    }
  }
}

#Function that compare the stocks seasonly by histogram
StockHistCompareSeason = function(stock, name, year, month){
  if(pic){par(mfrow = c(2, 1))}
  for(i in year){
    StockHist(stock, name, i, month, abb = TRUE)
  }
}

#Function that show boxplot of the return
StockBox = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  title = genTitleName("Boxplot", name, year, month, abb)
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  b <- boxplot(s$`Return of the Day`, ylab = "Return (£)", xlab = name, horizontal = TRUE, 
               names = name, main = title, col = "orange", border = "brown", notch = TRUE)
  if(!pic){
    dev.off()
    count <<- count + 1
  }
  return(b)
}

#Function that show boxplot compare of the return
StockBoxCompare = function(stock1, name1, stock2, name2, year = 2015:2018, month = 0){
  s <- FilterStock(stock1, year, month)
  s1 <- FilterStock(stock2, year, month)
  title = ""
  if(length(month) > 1){
    title = paste("Boxplot of the return of", name1, "and", name2, "from", year, month.name[month[1]], 
                  "to", year, month.name[month[length(month)]])
  }
  else{
    if(month < 1 | month > 12){
      if(length(year) > 1){
        title = paste("Boxplot of the return of", name1, "and", name2, "from", year[1], "to", year[length(year)])
      }
      else{
        title = paste("Boxplot of the return of", name1, "and", name2, "in", year)
      }
    }
    else{
      title = paste("Boxplot of the return of", name1, "and", name2, "in", year, month.name[month])
    }
  }
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  b <- boxplot(s$`Return of the Day`, s1$`Return of the Day`, ylab = "Return (£)", xlab = "Stock",
               names = c(name1, name2), main = title, col = "orange", border = "brown", notch = TRUE)
  if(!pic){
    dev.off()
    count <<- count + 1
  }
  return(b)
}

#Function that plot scatter graph between two stocks
StockScatterComapre = function(stock1, name1, stock2, name2, year = 2015:2018, month = 0, abb = FALSE){
  par(mfrow = c(1, 1))
  s <- FilterStock(stock1, year, month)
  s1 <- FilterStock(stock2, year, month)
  lim = c(min(s$`Return of the Day`, s1$`Return of the Day`, na.rm = TRUE), 
          max(s$`Return of the Day`, s1$`Return of the Day`, na.rm = TRUE))
  title = ""
  if(length(month) > 1){
    title = paste("Scatter Plot comapre from", year, month.name[month[1]], 
                  "to", year, month.name[month[length(month)]])
    
  }
  else{
    if(all(month < 1) | all(month > 12)){
      if(length(year) > 1){
        title = paste("Scatter Plot comapre from", year[1], "to", year[length(year)])
      }
      else{
        title = paste("Scatter Plot comapre in", year)
      }
    }
    else{
      title = paste("Scatter Plot comapre in", year, month.name[month])
    }
  }
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  plot(s$`Return of the Day`, s1$`Return of the Day`, main = title, xlab = name1, ylab = name2,
       pch = 19, xlim = lim, ylim = lim)
  if(reg){
    abline(lm(formula = na.omit(s$`Return of the Day`) ~ na.omit(s1$`Return of the Day`)), col = "blue")
  }
  if(!pic){
    dev.off()
    count <<- count + 1
  }
  return(cor(na.omit(s$`Return of the Day`), na.omit(s1$`Return of the Day`)))
}

#Function that plot histograms with normal comparison
StockHistWithNormal = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  h <- StockHist(stock, name, year, month, abb)
  s <- FilterStock(stock, year, month)
  d <- na.omit(s$`Return of the Day`)
  xfit <- seq(min(d), max(d), length = 100)
  yfit <- dnorm(xfit, mean = mean(d), sd = sd(d))
  print(yfit)
  yfit <- yfit * diff(h$mids[1:2]) * length(d)
  title = genTitleName("Histogram", name, year, month, abb)
  if(!pic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
  plot(h, xlab = "Return (£)", ylab = "Frequency")
  lines(xfit, yfit, col="blue", lwd=2)
  if(!pic){
    dev.off()
    count <<- count + 1
  }
  return(h)
}

#Function that carry out t-test
StockTtest = function(stock, name, year = 2015:2017, tyear = 2018, month = 0, print = Isprint) {
  fstock = FilterStock(stock, year, month)
  tstock = FilterStock(stock, tyear, month)
  
  fmag = na.omit(sapply(fstock$`Return of the Day`, abs))
  tmag = na.omit(sapply(tstock$`Return of the Day`, abs))
  
  title = ""
  nyear = year[1]
  if(length(year) > 1){
    nyear = paste(nyear, "-", year[length(year)])
  }
  if(length(month) > 1){
    title = paste("t-test of", name, nyear, "and", tyear, "during", month.name[month[1]], 
                  "-", month.name[month[length(month)]])
  }
  else{
    if(all(month < 1) | all(month > 12)){
      title = paste("t-test of", name, nyear, "and", tyear)
    }
    else{
      title = paste("t-test of", name, nyear, "and", tyear, "in", month.name[month])
    }
  }
  
  cat(title)
  
  test = t.test(tmag, fmag)
  
  if(print){
    print(test)
  }
  
  return(test)
}

#Main Function
main = function(){
  #Get the stocks
  S1 = "GPC"
  S2 = "HOG"
  
  Stock1 <- getStock(S1)
  Stock2 <- getStock(S2)
  
  StocksummarySpecific(Stock1, S1)
  StocksummarySpecific(Stock2, S2)
  
  StockHistWithNormal(Stock1, S1)
}

#main()

if(!pic){sink()}