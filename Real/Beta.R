####################################################
# Programme that process data into visulizable data
#
# Target: GPC, HOG
####################################################

savePic = FALSE
SinkOut = FALSE
count = 1
mag = 3

backtoDefault = function(){
  par(mfrow = c(1, 1))
}

sinkSet = function(){
  if(SinkOut){
    sink("output.txt")
  }
}

sinkEnd = function(){
  if(SinkOut){
    sink()
  }
}

SavePicSet = function(){
  backtoDefault()
  if(savePic){
    name = paste("fig", count, ".jpg", sep = "")
    jpeg(name)
  }
}

SavePicEnd = function(){
  if(savePic){
    dev.off()
    count <<- count + 1
  }
}

getStock = function(stock){
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

FilterStock <- function(stock, year = 2015:2018, month = 0){
  if(all(month < 1) | all(month > 12)){
    return(subset(stock, format(Date, "%Y") %in% year))
  }
  else{
    return(subset(stock,  as.numeric(format(Date, "%Y")) %in% year & 
                    as.numeric(format(Date, "%m")) %in% month))
  }
}

genYear = function(year, first = FALSE, isAbb = FALSE){
  s = ""
  s = paste(s, year[1])
  if(length(year) == 1){
    return(s)
  }
  if(!first){
    if(isAbb){
      s = paste(s, "-", year[length(year)])
    }
    else{
      s = paste(s, "to", year[length(year)])
    }
  }
  return(s)
}

toMonthName = function(month, isAbb = FALSE){
  if(isAbb){
    return(month.abb[month])
  }
  else{
    return(month.name[month])
  }
}

genMonth = function(month, first = FALSE, isAbb = FALSE){
  s = ""
  if(month[1] == 0){
    return(s)
  }
  s = paste(s, toMonthName(month, isAbb)[1])
  if(length(month) == 1){
    return(s)
  }
  if(!first){
    if(isAbb){
      s = paste(s, "-", toMonthName(month[length(month)], isAbb))
    }
    else{
      s = paste(s, "to", toMonthName(month[length(month)], isAbb))
    }
  }
  return(s)
}

genTitleName = function(type, name, year, month, first = FALSE, isAbb = FALSE){
  if(isAbb){
    s = ""
  }
  else{
    s = paste(type, "of")
  }
  s = paste(s, name, genYear(year, first, isAbb), genMonth(month, first, isAbb))
  return(s)
}

genTitleName2 = function(type, name, name1, year, month, first = FALSE, isAbb = FALSE){
  if(isAbb){
    s = ""
  }
  else{
    s = paste(type, "of")
  }
  s = paste(s, name, "and", name1, genYear(year, first, isAbb), genMonth(month, first, isAbb))
  return(s)
}

Stocksummary = function(stock, name, year = 2015:2018, month = 0){
  s <- FilterStock(stock, year, month)
  
  title = genTitleName("Summary", name, year, month)
  
  s <- summary(na.omit(s$`Return of the Day`))
  
  cat(title)
  cat("\n")
  print(s)
  cat("\n")
}

StockSD = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  
  title = genTitleName("Standard Deviation", name, year, month, FALSE, abb)
  
  sdv = sd(na.omit(s$`Return of the Day`))
  
  cat(title)
  cat("\n")
  cat(sdv)
  cat("\n\n")
}

StockHist = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  title = genTitleName("Histogram", name, year, month, FALSE, abb)
  
  if(all(month == 0)){
    nbreak = 50
  }
  else{
    nbreak = 10 + length(month) * 3
  }
  
  SavePicSet()
  
  h <- hist(s$`Return of the Day`, breaks = nbreak, xlab = "Return (£)", main = title, freq = FALSE)
  
  SavePicEnd()
  
  
  return(h)
}

StockHistWithNormal = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  h <- StockHist(stock, name, year, month, abb)
  
  s <- FilterStock(stock, year, month)
  d <- na.omit(s$`Return of the Day`)
  
  xfit <- seq(min(d), max(d), length = 100)
  yfit <- dnorm(xfit, mean = mean(d), sd = sd(d))
  
  title = genTitleName("Histogram", name, year, month, FALSE, abb)
  
  SavePicSet()
  
  plot(h, freq = FALSE, xlab = "Return (£)", ylab = "Density", main = title)
  lines(xfit, yfit, col="blue", lwd=2)
  
  SavePicEnd()
  
  
  return(h)
}

StockBox = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  
  title = genTitleName("Boxplot", name, year, month, FALSE, abb)
  
  SavePicSet()
  
  b <- boxplot(s$`Return of the Day`, ylab = "Return (£)", xlab = name, horizontal = TRUE, 
               names = name, main = title, col = "orange", border = "brown", notch = TRUE)
  
  SavePicEnd()
  
  
  return(b)
}

StockBoxCompare = function(stock1, name1, stock2, name2, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock1, year, month)
  s1 <- FilterStock(stock2, year, month)
  
  title = genTitleName2("Boxplot", name1, name2, year, month, FALSE, abb)
  
  SavePicSet()
  
  b <- boxplot(s$`Return of the Day`, s1$`Return of the Day`, ylab = "Return (£)", xlab = "Stock",
               names = c(name1, name2), main = title, col = "orange", border = "brown", notch = TRUE)
  
  SavePicEnd()
  
  
  return(b)
}

StockGraph = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  s <- FilterStock(stock, year, month)
  
  title = genTitleName("Graph", name, year, month)
  
  SavePicSet()
  
  plot(x = s$Date, y = s$`Return of the Day`, type = "l", col = "blue",
       xlab = "Date", ylab = "Return (£)", main = title)
  lines(x = s$Date, y = rep(0, nrow(s)), col = "red")
  
  SavePicEnd()
}

StockGraphPredict = function(stock, name, year = 2015:2018, month = 0, abb = FALSE){
  stock1 <- StockPredict(stock)
  
  s <- FilterStock(stock, year, month)
  s1 <- FilterStock(stock1, year, month)
  
  View(s)
  View(s1)
  
  title = genTitleName("Graph", name, year, month)
  
  SavePicSet()
  
  plot(x = s$Date, y = s$`Return of the Day`, type = "l", col = "blue",
       xlab = "Date", ylab = "Return (£)", main = title)
  lines(x = s$Date, y = s1$`Return of the Day`, col = "green")
  lines(x = s$Date, y = rep(0, nrow(s)), col = "red")
  
  SavePicEnd()
}

StockScatterComapre = function(stock1, name1, stock2, name2, year = 2015:2018, month = 0, abb = FALSE, regression = TRUE){
  s <- FilterStock(stock1, year, month)
  s1 <- FilterStock(stock2, year, month)
  lim = c(min(s$`Return of the Day`, s1$`Return of the Day`, na.rm = TRUE), 
          max(s$`Return of the Day`, s1$`Return of the Day`, na.rm = TRUE))
  
  title = genTitleName2("Scatter Plot", name1, name2, year, month, FALSE, abb)
  
  SavePicSet()
  
  plot(s$`Return of the Day`, s1$`Return of the Day`, main = title, xlab = name1, ylab = name2,
       pch = 19, xlim = lim, ylim = lim)
  if(regression){
    abline(lm(formula = na.omit(s$`Return of the Day`) ~ na.omit(s1$`Return of the Day`)), col = "blue")
  }
  
  SavePicEnd()
  
  return(cor(na.omit(s$`Return of the Day`), na.omit(s1$`Return of the Day`)))
}

StockTtest = function(stock, name, year = 2015:2017, tyear = 2018, month = 0) {
  fstock = FilterStock(stock, year, month)
  tstock = FilterStock(stock, tyear, month)
  
  fmag = na.omit(sapply(fstock$`Return of the Day`, abs))
  tmag = na.omit(sapply(tstock$`Return of the Day`, abs))
  
  title = genTitleName("T-test", name, year, month)
  
  test = t.test(tmag, fmag)
  test$data.name = paste(genYear(tyear), "and", genYear(year))
  
  cat(title)
  print(test)
}

StockPredict = function(stock){
  r = stock$`Return of the Day`
  r[1] = 0
  r1 = r[-1]
  r1 = append(r1, 0)
  r2 = r1 - r
  r2 = append(r2, 0, 0)
  r2 = r2[-length(r2)]
  r2 = r2 / mag
  p = r2 + r
  p = append(p, 0, 0)
  p = p[-length(p)]
  
  result <- data.frame(stock$`Date`, p)
  colnames(result) <- c("Date", "Return of the Day")
  
  return(result)
}

StockRealPredictSummary = function(stock, name){
  pstock = StockPredict(stock)
  
  RealPred = data.frame(stock$`Return of the Day`, pstock$`Return of the Day`)
  colnames(RealPred) = c("R", "P")
  RealPos = subset(RealPred, R >= 0)
  RealNeg = subset(RealPred, R < 0)
  PosPos = subset(RealPos, P >= 0)
  PosNeg = subset(RealPos, P < 0)
  NegPos = subset(RealNeg, P >= 0)
  NegNeg = subset(RealNeg, P < 0)
  
  S11 = nrow(PosPos)
  S12 = nrow(PosNeg)
  S21 = nrow(NegPos)
  S22 = nrow(NegNeg)
  
  S13 = S11 + S12
  S23 = S21 + S22
  S31 = S11 + S21
  S32 = S12 + S22
  
  S33 = S31 + S32
  
  S <- matrix(c(S11, S12, S13, S21, S22, S23, S31, S32, S33), ncol = 3, byrow = TRUE)
  
  colnames(S) <- c("Predict Positive", "Predict Negative", "Total")
  rownames(S) <- c("Real Positive", "Real Negative", "Total")
  
  summary <- as.table(S)
  
  title = genTitleName("Predict and Real Summary Table", name, 2015:2018, 0)
  cat(title, "\n")
  print(summary)
}

main = function(){
  S1 = "GPC"
  S2 = "HOG"
  
  Stock1 <- getStock(S1)
  Stock2 <- getStock(S2)
  
  Stocksummary(Stock1, S1)
  StockSD(Stock1, S1)
  StockHistWithNormal(Stock1, S1)
  StockBox(Stock1, S1)
  
  Stocksummary(Stock2, S2)
  StockSD(Stock2, S2)
  StockHistWithNormal(Stock2, S2)
  StockBox(Stock2, S2)
  
  StockBoxCompare(Stock1, S1, Stock2, S2)
  StockScatterComapre(Stock1, S1, Stock2, S2)
  
  StockTtest(Stock1, S1)
  StockTtest(Stock1, S1, 2015)
  StockTtest(Stock1, S1, 2016)
  StockTtest(Stock1, S1, 2017)
  
  StockRealPredictSummary(Stock2, S2)
}

sinkSet()

rawdata <- read.csv("data.csv")
main()

sinkEnd()



