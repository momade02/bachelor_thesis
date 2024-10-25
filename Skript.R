############### Packages ###############

  library(tidyverse)
    #library(readr)
    #library(ggplot2)
    #library(dplyr)
  library(data.table)
  library(broom)
  library(mosaic)
    #library(ggformula)
  library(gridExtra)
  library(ggfortify)
  library(ggcorrplot)
  library(qqplotr)
  library(reshape2)
  library(plotly)
  library(corrplot)
  library(viridisLite)
  library(reshape2)
  library(tseries)
  library(forecast)
  library(zoo)
  library(moments)
  library(PerformanceAnalytics)  
  library(TTR)
  library(matrixTests)
  library(lmtest)
  library(car)
  library(quantmod)



############### Loading Data ###############

# All monthly data -> 01.01.2010 - 01.09.2023 
  
  DataAll <- read_csv2("Data.csv", show_col_types = FALSE)
  view(DataAll)


############### Hedge Fund Index Autocorrelation ###############
  
# Chi-squared distribution  
  
  qchisq(0.95, df = 1)
  qchisq(0.95, df = 5)
  
# Checking for Autocorrelation
  
  BoxLjungTest1 <- Box.test(DataAll$EHIMainReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIMainReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIMainReturnACF <- acf(DataAll$EHIMainReturn, plot = FALSE)
  EHIMainReturnACF$acf[2]
  #acf(DataAll$EHIMainReturn, main="Autocorrelation EHIMain")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIArbitrageReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIArbitrageReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIArbitrageReturnACF <- acf(DataAll$EHIArbitrageReturn, plot = FALSE)
  EHIArbitrageReturnACF$acf[2]
  #acf(DataAll$EHIArbitrageReturn, main="Autocorrelation EHIArbitrage")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIManagedFuturesReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIManagedFuturesReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIManagedFuturesReturnACF <- acf(DataAll$EHIManagedFuturesReturn, plot = FALSE)
  EHIManagedFuturesReturnACF$acf[2]
  #acf(DataAllEHIManagedFuturesReturn, main="Autocorrelation EHIManagedFutures")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIDistressedDebtReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIDistressedDebtReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIDistressedDebtReturnACF <- acf(DataAll$EHIDistressedDebtReturn, plot = FALSE)
  EHIDistressedDebtReturnACF$acf[2]
  acf(DataAll$EHIDistressedDebtReturn, main="Autocorrelation EHIDistressedDebt")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIEventDrivenReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIEventDrivenReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIEventDrivenReturnACF <- acf(DataAll$EHIEventDrivenReturn, plot = FALSE)
  EHIEventDrivenReturnACF$acf[2]
  #acf(DataAll$EHIEventDrivenReturn, main="Autocorrelation EHIEventDriven")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIFixedIncomeReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIFixedIncomeReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIFixedIncomeReturnACF <- acf(DataAll$EHIFixedIncomeReturn, plot = FALSE)
  EHIFixedIncomeReturnACF$acf[2]
  #acf(DataAll$EHIFixedIncomeReturn, main="Autocorrelation EHIFixedIncome")
  
  BoxLjungTest1 <- Box.test(DataAll$EHILSEquityReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHILSEquityReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHILSEquityReturnACF <- acf(DataAll$EHILSEquityReturn, plot = FALSE)
  EHILSEquityReturnACF$acf[2]
  #acf(DataAll$EHILSEquityReturn, main="Autocorrelation EHILSEquity")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIMacroReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIMacroReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIMacroReturnACF <- acf(DataAll$EHIMacroReturn, plot = FALSE)
  EHIMacroReturnACF$acf[2]
  #acf(DataAll$EHIMacroReturn, main="Autocorrelation EHIMacro")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIMultiStrategyReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIMultiStrategyReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIMultiStrategyReturnACF <- acf(DataAll$EHIMultiStrategyReturn, plot = FALSE)
  EHIMultiStrategyReturnACF$acf[2]
  #acf(DataAll$EHIMultiStrategyReturn, main="Autocorrelation EHIMultiStrategy")
  
  BoxLjungTest1 <- Box.test(DataAll$EHIRelativeValueReturn, lag = 1, type = "Ljung-Box")
  BoxLjungTest5 <- Box.test(DataAll$EHIRelativeValueReturn, lag = 5, type = "Ljung-Box")
  print(BoxLjungTest1)
  print(BoxLjungTest5)
  EHIRelativeValueReturnACF <- acf(DataAll$EHIRelativeValueReturn, plot = FALSE)
  EHIRelativeValueReturnACF$acf[2]
  #acf(DataAll$EHIRelativeValueReturn, main="Autocorrelation EHIRelativeValue")

  
############### Hedge Fund Index Returns Inspection ###############  

# Inspecting the downloaded hedge fund returns
    
  inspect(DataAll[c("EHIMainReturn", "EHIArbitrageReturn", "EHIManagedFuturesReturn", 
                    "EHIDistressedDebtReturn", "EHIEventDrivenReturn", "EHIFixedIncomeReturn", 
                    "EHILSEquityReturn", "EHIMacroReturn", "EHIMultiStrategyReturn", "EHIRelativeValueReturn")])
  
  
############### Calculating Risk Factor Returns ###############
  
# Hedge Fund returns are already loaded from eurekahedge.com
  # Explanation: https://www.eurekahedge.com/Indices/hedge-fund-index-methodology
  
# Risk factor returns need to be calculated   
  
# Absolute risk factor returns (yields)
  
  DataAll$GB1MReturn <- rep(nrow(DataAll))
  DataAll$GB1MReturn[1] <- 0
  for (i in 2:nrow(DataAll)) {DataAll$GB1MReturn[i] <- (DataAll$GB1MValue[i]) - DataAll$GB1MValue[i - 1]}
  
  DataAll$BAA10YReturn <- rep(nrow(DataAll))
  DataAll$BAA10YReturn[1] <- 0
  for (i in 2:nrow(DataAll)) {DataAll$BAA10YReturn[i] <- (DataAll$BAA10YValue[i]) - DataAll$BAA10YValue[i - 1]}
  
# Continuous risk factor returns (everything else)
  
  SPXReturn <- diff(log(DataAll$SPXValue))
  SPXReturn <- c(0, SPXReturn)
  DataAll$SPXReturn <- SPXReturn
  
  IEFReturn <- diff(log(DataAll$IEFValue)) #ETF 
  IEFReturn <- c(0, IEFReturn)
  DataAll$IEFReturn <- IEFReturn
  
  DXYReturn <- diff(log(DataAll$DXYValue))
  DXYReturn <- c(0, DXYReturn)
  DataAll$DXYReturn <- DXYReturn
  
  VIXYReturn <- diff(log(DataAll$VIXYValue))
  VIXYReturn <- c(0, VIXYReturn)
  DataAll$VIXYReturn <- VIXYReturn
  DataAll$VIXYReturn[1:14] <- 0
  
  GSGReturn <- diff(log(DataAll$GSGValue))
  GSGReturn <- c(0, GSGReturn)
  DataAll$GSGReturn <- GSGReturn
  
  RSSCReturn <- diff(log(DataAll$RSSCValue))
  RSSCReturn <- c(0, RSSCReturn)
  DataAll$RSSCReturn <- RSSCReturn  
  
  RSLCReturn <- diff(log(DataAll$DXYValue))
  RSLCReturn <- c(0, RSLCReturn)
  DataAll$RSLCReturn <- RSLCReturn
  DataAll$RSLCReturn[1:16] <- 0
  
  RSVReturn <- diff(log(DataAll$RSVValue))
  RSVReturn <- c(0, RSVReturn)
  DataAll$RSVReturn <- RSVReturn  
  
  RSGReturn <- diff(log(DataAll$RSGValue))
  RSGReturn <- c(0, RSGReturn)
  DataAll$RSGReturn <- RSGReturn
  
  MTUMReturn <- diff(log(DataAll$MTUMValue))
  MTUMReturn <- c(0, MTUMReturn)
  DataAll$MTUMReturn <- MTUMReturn
  DataAll$MTUMReturn[1:41] <- 0
  
  PHDGReturn <- diff(log(DataAll$PHDGValue))
  PHDGReturn <- c(0, PHDGReturn)
  DataAll$PHDGReturn <- PHDGReturn
  DataAll$PHDGReturn[1:37] <- 0

# View updated data frame
  
  view(DataAll)
  

############### Calculating Fama-French Factors SIZE & VALUE ###############  
  
# Calculating the Fama French Factors SIZE & VALUE
    
  DataAll$SIZEReturn <- DataAll$RSSCReturn - DataAll$RSLCReturn
  DataAll$SIZEReturn[1:16] <- 0 #RSLC has no data before 01.04.2011
  DataAll$VALUEReturn <- DataAll$RSVReturn - DataAll$RSGReturn
  
# View updated Data frames
  
  view(DataAll)
  
    
############### Factor Returns Inspection ###############
 
# Inspecting the calculated factor returns
  
  inspect(DataAll[c("GB1MReturn", "BAA10YReturn", "SPXReturn", "IEFReturn", 
                    "DXYReturn", "VIXYReturn", "GSGReturn", "SIZEReturn", 
                    "VALUEReturn", "MTUMReturn", "PHDGReturn")])
  
  
############### Calculating Hedge Fund & Risk Factor Indices/Benchmarks ###############
  
# All performance graphs start at 100 to make the visual comparison of the performance possible 
  
# Hedge fund index performance
  
  DataAll$EHIMainIndex <- rep(nrow(DataAll))
  DataAll$EHIMainIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIMainIndex[i] <- DataAll$EHIMainIndex[i - 1] * (1 + DataAll$EHIMainReturn[i])}
  
  DataAll$EHIArbitrageIndex <- rep(nrow(DataAll))
  DataAll$EHIArbitrageIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIArbitrageIndex[i] <- DataAll$EHIArbitrageIndex[i - 1] * (1 + DataAll$EHIArbitrageReturn[i])}
  
  DataAll$EHIManagedFuturesIndex <- rep(nrow(DataAll))
  DataAll$EHIManagedFuturesIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIManagedFuturesIndex[i] <- DataAll$EHIManagedFuturesIndex[i - 1] * (1 + DataAll$EHIManagedFuturesReturn[i])}
  
  DataAll$EHIDistressedDebtIndex <- rep(nrow(DataAll))
  DataAll$EHIDistressedDebtIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIDistressedDebtIndex[i] <- DataAll$EHIDistressedDebtIndex[i - 1] * (1 + DataAll$EHIDistressedDebtReturn[i])}
  
  DataAll$EHIEventDrivenIndex <- rep(nrow(DataAll))
  DataAll$EHIEventDrivenIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIEventDrivenIndex[i] <- DataAll$EHIEventDrivenIndex[i - 1] * (1 + DataAll$EHIEventDrivenReturn[i])}
  
  DataAll$EHIFixedIncomeIndex <- rep(nrow(DataAll))
  DataAll$EHIFixedIncomeIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIFixedIncomeIndex[i] <- DataAll$EHIFixedIncomeIndex[i - 1] * (1 + DataAll$EHIFixedIncomeReturn[i])}
  
  DataAll$EHILSEquityIndex <- rep(nrow(DataAll))
  DataAll$EHILSEquityIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHILSEquityIndex[i] <- DataAll$EHILSEquityIndex[i - 1] * (1 + DataAll$EHILSEquityReturn[i])}
  
  DataAll$EHIMacroIndex <- rep(nrow(DataAll))
  DataAll$EHIMacroIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIMacroIndex[i] <- DataAll$EHIMacroIndex[i - 1] * (1 + DataAll$EHIMacroReturn[i])}
  
  DataAll$EHIMultiStrategyIndex <- rep(nrow(DataAll))
  DataAll$EHIMultiStrategyIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIMultiStrategyIndex[i] <- DataAll$EHIMultiStrategyIndex[i - 1] * (1 + DataAll$EHIMultiStrategyReturn[i])}
  
  DataAll$EHIRelativeValueIndex <- rep(nrow(DataAll))
  DataAll$EHIRelativeValueIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$EHIRelativeValueIndex[i] <- DataAll$EHIRelativeValueIndex[i - 1] * (1 + DataAll$EHIRelativeValueReturn[i])}

# Risk factor performance
  
  DataAll$GB1MIndex <- rep(0, nrow(DataAll))
  DataAll$GB1MIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$GB1MIndex[i] <- DataAll$GB1MIndex[i - 1] * (1 + DataAll$GB1MReturn[i])}
  
  DataAll$BAA10YIndex <- rep(0, nrow(DataAll))
  DataAll$BAA10YIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$BAA10YIndex[i] <- DataAll$BAA10YIndex[i - 1] * (1 + DataAll$BAA10YReturn[i])}
  
  DataAll$SPXIndex <- rep(0, nrow(DataAll))
  DataAll$SPXIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$SPXIndex[i] <- DataAll$SPXIndex[i - 1] * exp(DataAll$SPXReturn[i])}
  
  DataAll$IEFIndex <- rep(0, nrow(DataAll))
  DataAll$IEFIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$IEFIndex[i] <- DataAll$IEFIndex[i - 1] * exp(DataAll$IEFReturn[i])}
  
  DataAll$DXYIndex <- rep(0, nrow(DataAll))
  DataAll$DXYIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$DXYIndex[i] <- DataAll$DXYIndex[i - 1] * exp(DataAll$DXYReturn[i])}
  
  DataAll$VIXYIndex <- rep(0, nrow(DataAll))
  DataAll$VIXYIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$VIXYIndex[i] <- DataAll$VIXYIndex[i - 1] * exp(DataAll$VIXYReturn[i])}
  
  DataAll$GSGIndex <- rep(0, nrow(DataAll))
  DataAll$GSGIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$GSGIndex[i] <- DataAll$GSGIndex[i - 1] * exp(DataAll$GSGReturn[i])}
  
  DataAll$SIZEIndex <- rep(0, nrow(DataAll))
  DataAll$SIZEIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$SIZEIndex[i] <- DataAll$SIZEIndex[i - 1] * exp(DataAll$SIZEReturn[i])}
  
  DataAll$VALUEIndex <- rep(0, nrow(DataAll))
  DataAll$VALUEIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$VALUEIndex[i] <- DataAll$VALUEIndex[i - 1] * exp(DataAll$VALUEReturn[i])}
  
  DataAll$MTUMIndex <- rep(0, nrow(DataAll))
  DataAll$MTUMIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$MTUMIndex[i] <- DataAll$MTUMIndex[i - 1] * exp(DataAll$MTUMReturn[i])}
  
  DataAll$PHDGIndex <- rep(0, nrow(DataAll))
  DataAll$PHDGIndex[1] <- 100
  for (i in 2:nrow(DataAll)) {DataAll$PHDGIndex[i] <- DataAll$PHDGIndex[i - 1] * exp(DataAll$PHDGReturn[i])}
  
# Viewing the new data sets
  
  view(DataAll)

  
############### Performance Visualization ###############

# Hedge Fund Indices + S&P500 + Risk-Free Rate
  
  ggplot(DataAll, aes(x = Date)) + 
   
    geom_line(aes(y = EHIMainIndex, color = "EHIMainIndex")) +
    geom_line(aes(y = EHIArbitrageIndex, color = "EHIArbitrageIndex")) +
    geom_line(aes(y = EHIManagedFuturesIndex, color = "EHIManagedFuturesIndex")) +
    geom_line(aes(y = EHIDistressedDebtIndex, color = "EHIDistressedDebtIndex")) +
    geom_line(aes(y = EHIEventDrivenIndex, color = "EHIEventDrivenIndex")) +
    geom_line(aes(y = EHIFixedIncomeIndex, color = "EHIFixedIncomeIndex")) +
    geom_line(aes(y = EHILSEquityIndex, color = "EHILSEquityIndex")) +
    geom_line(aes(y = EHIMacroIndex, color = "EHIMacroIndex")) +
    geom_line(aes(y = EHIMultiStrategyIndex, color = "EHIMultiStrategyIndex")) +
    geom_line(aes(y = EHIRelativeValueIndex, color = "EHIRelativeValueIndex")) +
    geom_line(aes(y = SPXIndex, color = "SPXIndex")) + #S&P500 for comparison
    geom_line(aes(y = GB1MIndex, color = "GB1MIndex")) + #Risk-free rate for comparison
    
    labs(x = "Time", y = "Return", color = "Hedge Funds") +
    
    scale_color_manual(values = c(EHIMainIndex = "red", EHIArbitrageIndex = "olivedrab", EHIManagedFuturesIndex = "blueviolet", EHIDistressedDebtIndex = "green1", EHIEventDrivenIndex = "green3", EHIFixedIncomeIndex = "turquoise", 
                                  EHILSEquityIndex = "darkorange1", EHIMacroIndex = "brown", EHIMultiStrategyIndex = "hotpink", EHIRelativeValueIndex = "blue", SPXIndex = "black", GB1MIndex = "darkgrey")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 25)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    ggtitle("Hedge Fund Returns")  
  
# Risk Factors
  
  ggplot(DataAll, aes(x = Date)) +
    
    geom_line(aes(y = BAA10YIndex, color = "BAA10YIndex")) +
    geom_line(aes(y = SPXIndex, color = "SPXIndex")) +
    geom_line(aes(y = IEFIndex, color = "IEFIndex")) +
    geom_line(aes(y = DXYIndex, color = "DXYIndex")) +
    geom_line(aes(y = VIXYIndex, color = "VIXYIndex")) +
    geom_line(aes(y = GSGIndex, color = "GSGIndex")) +
    geom_line(aes(y = SIZEIndex, color = "SIZEIndex")) +
    geom_line(aes(y = VALUEIndex, color = "VALUEIndex")) +
    geom_line(aes(y = MTUMIndex, color = "MTUMIndex")) +
    geom_line(aes(y = PHDGIndex, color = "PHDGIndex")) +
    geom_line(aes(y = GB1MIndex, color = "GB1MIndex")) + #Risk-Free Rate for comparison
    
    labs(x = "Time", y = "Return", color = "Factors") +
    
    scale_color_manual(values = c(BAA10YIndex = "olivedrab", SPXIndex = "green1", 
                                  IEFIndex = "darkorange1", DXYIndex = "hotpink", VIXYIndex = "blue3", 
                                  GSGIndex = "deepskyblue2", SIZEIndex = "turquoise2", VALUEIndex = "yellow", 
                                  MTUMIndex = "red", PHDGIndex = "darkgrey", GB1MIndex = "black")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 25)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    ggtitle("Factor Returns") 


############### Isolation of the Risk-Free Asset ###############
  
# Isolation of the risk-free asset "GB1MReturn" from the factors
# Note: Overall impact on the returns is rather low
  
  DataAll$SPXReturn <- DataAll$SPXReturn - DataAll$GB1MReturn
  DataAll$IEFReturn <- DataAll$IEFReturn - DataAll$GB1MReturn
  
# View Changes 
  
  view(DataAll)
  
############### Factor Correlation ###############
  
  # Correlation of the factors in the console with numbers 
  cor(DataAll[c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "SIZEReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")])
  
  # Correlation of the factors visualized in a plot with colors 
  corr.DataAll <- cor(DataAll[c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "SIZEReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")])
  corrplot(corr.DataAll)
  
  

############### Analysis of the Regression ###############
  
# Regressions for all hedge fund indices
  
  regression_EHIMainReturn <- lm(EHIMainReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIMainReturn)
  
  regression_EHIArbitrageReturn <- lm(EHIArbitrageReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIArbitrageReturn)
  
  regression_EHIManagedFuturesReturn <- lm(EHIManagedFuturesReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIManagedFuturesReturn)
  
  regression_EHIDistressedDebtReturn <- lm(EHIDistressedDebtReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIDistressedDebtReturn)
  
  regression_EHIEventDrivenReturn <- lm(EHIEventDrivenReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIEventDrivenReturn)
  
  regression_EHIFixedIncomeReturn <- lm(EHIFixedIncomeReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIFixedIncomeReturn)
  
  regression_EHILSEquityReturn <- lm(EHILSEquityReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHILSEquityReturn)
  
  regression_EHIMacroReturn <- lm(EHIMacroReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIMacroReturn)
  
  regression_EHIMultiStrategyReturn <- lm(EHIMultiStrategyReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIMultiStrategyReturn)
  
  regression_EHIRelativeValueReturn <- lm(EHIRelativeValueReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = DataAll)
  summary(regression_EHIRelativeValueReturn)
  
# Data frame with the results of all regressions

  regression_models <- list(
    EHIMain = regression_EHIMainReturn,
    EHIArbitrage = regression_EHIArbitrageReturn,
    EHIManagedFutures = regression_EHIManagedFuturesReturn,
    EHIDistressedDebt = regression_EHIDistressedDebtReturn,
    EHIEventDriven = regression_EHIEventDrivenReturn,
    EHIFixedIncome = regression_EHIFixedIncomeReturn,
    EHILSEquity = regression_EHILSEquityReturn,
    EHIMacro = regression_EHIMacroReturn,
    EHIMultiStrategy = regression_EHIMultiStrategyReturn,
    EHIRelativeValue = regression_EHIRelativeValueReturn
  )
  
  coefficients_df <- data.frame()
  for(strategy in names(regression_models)) {
    coef_vector <- coef(summary(regression_models[[strategy]]))
    strategy_coef_df <- data.frame(t(coef_vector[,"Estimate"]), check.names = FALSE)
    strategy_coef_df$Strategy <- strategy
    coefficients_df <- rbind(coefficients_df, strategy_coef_df)
  }
  row.names(coefficients_df) <- coefficients_df$Strategy
  
  view(coefficients_df)
  
# 2D heatmap of all betas in all funds

  #coefficients_df$BAA10YReturn <- NULL #for deleting the BAA10YReturn column
  
  coefficients_long <- melt(coefficients_df, id.vars = 'Strategy')
  coefficients_long
  

  heatmap <- ggplot(coefficients_long, aes(x = variable, y = Strategy, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("blue", "cyan", "green", "yellow", "red")) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Heatmap Betas",
         x = "Intercept + Risk Factors",
         y = "Hedge Fund Strategies")
  
  heatmap  
  
# Alphas for every hedge fund strategy (column diagram)
  
  ggplot(data = coefficients_df, aes(x = Strategy, y = `(Intercept)`)) +
    geom_bar(stat = "identity", fill = "grey1") +
    labs(title = "Intercept (Alpha) of Hedge Fund Strategies",
         x = "Hedge Fund Strategies", y = "Value") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# All Alpha Developments 
  
  alphas_df <- data.frame(matrix(nrow = 106, ncol = 10))
  alphas_df$Date <- DataAll$Date[(60):nrow(DataAll)]   
  colnames(alphas_df)[1:10] <- c("EHIMain", "EHIArbitrage", "EHIManagedFutures", "EHIDistressedDebt", 
                                 "EHIEventDriven", "EHIFixedIncome", "EHILSEquity", 
                                 "EHIMacro", "EHIMultiStrategy", "EHIRelativeValue")
  
  # Continue at the end of the following section
  
# Rolling windows of the Intercept (Alpha) for all months (60-month window)  
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  # EHIMain
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMainReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMainReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIMainBetas <- as.data.frame(betas)
 
    #view(EHIMainBetas)
  
  intercepts <- EHIMainBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIMain <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIMain Intercept (Alpha) Development",
         x = "Time",
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIArbitrage
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIArbitrageReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIArbitrageReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIArbitrageBetas <- as.data.frame(betas)
  
    #view(EHIArbitrageBetas)
  
  intercepts <- EHIArbitrageBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIArbitrage <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIArbitrage Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIManagedFutures
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIManagedFuturesReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIManagedFuturesReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIManagedFuturesBetas <- as.data.frame(betas)
  
    #view(EHIManagedFuturesBetas)
  
  intercepts <- EHIManagedFuturesBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIManagedFutures <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIManagedFutures Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIDistressedDebt
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIDistressedDebtReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIDistressedDebtReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIDistressedDebtBetas <- as.data.frame(betas)
  
    #view(EHIDistressedDebtBetas)
  
  intercepts <- EHIDistressedDebtBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIDistressedDebt <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIDistressedDebt Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIEventDriven
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIEventDrivenReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIEventDrivenReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIEventDrivenBetas <- as.data.frame(betas)
  
    #view(EHIEventDrivenBetas)
  
  intercepts <- EHIEventDrivenBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIEventDriven <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIEventDriven Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIFixedIncome
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIFixedIncomeReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIFixedIncomeReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIFixedIncomeBetas <- as.data.frame(betas)
  
    #view(EHIFixedIncomeBetas)
  
  intercepts <- EHIFixedIncomeBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIFixedIncome <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIFixedIncome Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHILSEquity
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHILSEquityReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHILSEquityReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHILSEquityBetas <- as.data.frame(betas)
  
    #view(EHILSEquityBetas)
  
  intercepts <- EHILSEquityBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHILSEquity <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHILSEquity Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIMacro
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMacroReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMacroReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIMacroBetas <- as.data.frame(betas)
  
    #view(EHIMacroBetas)
  
  intercepts <- EHIMacroBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIMacro <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIMacro Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIMultiStrategy
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMultiStrategyReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMultiStrategyReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIMultiStrategyBetas <- as.data.frame(betas)
  
    #view(EHIMultiStrategyBetas)
  
  intercepts <- EHIMultiStrategyBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIMultiStrategy <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIMultiStrategy Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # EHIRelativeValue
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIRelativeValueReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIRelativeValueReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIRelativeValueBetas <- as.data.frame(betas)
  
    #view(EHIRelativeValueBetas)
  
  intercepts <- EHIRelativeValueBetas[, 1]
  intercept_df <- data.frame(Index = 1:length(intercepts), Intercept = intercepts)
  alphas_df$EHIRelativeValue <- intercept_df$Intercept
  intercept_df$Date <- DataAll$Date[(60):nrow(DataAll)] 
  
  ggplot(intercept_df, aes(x = Date, y = Intercept)) +
    geom_line() + 
    theme_bw() +
    labs(title = "EHIRelativeValue Intercept (Alpha) Development", 
         x = "Time", 
         y = "Value") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# All Alpha Developments 
  
  ggplot(alphas_df, aes(x = Date)) + 
    
    geom_line(aes(y = EHIMain, color = "EHIMain")) +
    geom_line(aes(y = EHIArbitrage, color = "EHIArbitrage")) +
    geom_line(aes(y = EHIManagedFutures, color = "EHIManagedFutures")) +
    geom_line(aes(y = EHIDistressedDebt, color = "EHIDistressedDebt")) +
    geom_line(aes(y = EHIEventDriven, color = "EHIEventDriven")) +
    geom_line(aes(y = EHIFixedIncome, color = "EHIFixedIncome")) +
    geom_line(aes(y = EHILSEquity, color = "EHILSEquity")) +
    geom_line(aes(y = EHIMacro, color = "EHIMacro")) +
    geom_line(aes(y = EHIMultiStrategy, color = "EHIMultiStrategy")) +
    geom_line(aes(y = EHIRelativeValue, color = "EHIRelativeValue")) +
    
    labs(x = "Time", y = "Value", color = "Hedge Funds") +
    
    scale_color_manual(values = c(EHIMain = "red", EHIArbitrage= "olivedrab", EHIManagedFutures = "blueviolet", EHIDistressedDebt = "green1", EHIEventDriven = "green3", EHIFixedIncome = "turquoise", 
                                  EHILSEquity = "darkorange1", EHIMacro = "brown", EHIMultiStrategy = "hotpink", EHIRelativeValue = "blue")) +
    
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-0.01), max(0.01), by = 0.0005)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    ggtitle("Hedge Fund Alphas")  
  

############### EHIMain Replication ###############

# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMainReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMainReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIMainBetas <- as.data.frame(betas)
  
    #EHIMainBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIMainBetas <- EHIMainBetas[-nrow(EHIMainBetas), ]
  
  view(EHIMainBetas)
  
# Creating the replication data 

  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIMainBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIMainBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIMainBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIMainReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIMainReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIMainBetas, Factors)
  }
  
  OutSampleData$EHIMainReplicationReturn <- EHIMainReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
   
  OutSampleData$EHIMainIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMainIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMainIndex[i] <- OutSampleData$EHIMainIndex[i - 1] * (1 + OutSampleData$EHIMainReturn[i])}
  
  OutSampleData$EHIMainCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMainCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMainCloneIndex[i] <- OutSampleData$EHIMainCloneIndex[i - 1] * (1 + OutSampleData$EHIMainReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
   
   labs(x = "Date", y = "Return", color = "Type") +
   
   geom_line(aes(y = EHIMainIndex, color = "EHIMain")) +
   geom_line(aes(y = EHIMainCloneIndex, color = "EHIMainReplication")) +
   
   scale_color_manual(values = c("EHIMain" = "blue", "EHIMainReplication" = "red")) +
   
   geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
   
   scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
   
   scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
   
   theme_bw() +
   
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   
   labs(title = "EHIMain vs. EHIMainReplication", x = "Time", y = "Return")

# Correlation Test
  
  cor(OutSampleData[c("EHIMainReturn", "EHIMainReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIMainReturn, OutSampleData$EHIMainReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   

  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIMainReturn", "EHIMainReplicationReturn")])
  
  
############### EHIArbitrage Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn","VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIArbitrageReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))
    } 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIArbitrageReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
    
  EHIArbitrageBetas <- as.data.frame(betas)
  
    #EHIArbitrageBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIArbitrageBetas <- EHIArbitrageBetas[-nrow(EHIArbitrageBetas), ]
  
  view(EHIArbitrageBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIArbitrageBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIArbitrageBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIArbitrageBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIArbitrageReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIArbitrageReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIArbitrageBetas, Factors)
  }
  
  OutSampleData$EHIArbitrageReplicationReturn <- EHIArbitrageReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIArbitrageIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIArbitrageIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIArbitrageIndex[i] <- OutSampleData$EHIArbitrageIndex[i - 1] * (1 + OutSampleData$EHIArbitrageReturn[i])}
  
  OutSampleData$EHIArbitrageCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIArbitrageCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIArbitrageCloneIndex[i] <- OutSampleData$EHIArbitrageCloneIndex[i - 1] * (1 + OutSampleData$EHIArbitrageReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIArbitrageIndex, color = "EHIArbitrage")) +
    geom_line(aes(y = EHIArbitrageCloneIndex, color = "EHIArbitrageReplication")) +
    
    scale_color_manual(values = c("EHIArbitrage" = "blue", "EHIArbitrageReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIArbitrage vs. EHIArbitrageReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIArbitrageReturn", "EHIArbitrageReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIArbitrageReturn, OutSampleData$EHIArbitrageReplicationReturn, paired = TRUE)
  t_test_result

# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIArbitrageReturn", "EHIArbitrageReplicationReturn")])

  
############### EHIManagedFutures Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIManagedFuturesReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIManagedFuturesReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIManagedFuturesBetas <- as.data.frame(betas)
  
    #EHIManagedFuturesBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIManagedFuturesBetas <- EHIManagedFuturesBetas[-nrow(EHIManagedFuturesBetas), ]
  
  view(EHIManagedFuturesBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIManagedFuturesBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIManagedFuturesBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIManagedFuturesBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIManagedFuturesReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIManagedFuturesReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIManagedFuturesBetas, Factors)
  }
  
  OutSampleData$EHIManagedFuturesReplicationReturn <- EHIManagedFuturesReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIManagedFuturesIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIManagedFuturesIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIManagedFuturesIndex[i] <- OutSampleData$EHIManagedFuturesIndex[i - 1] * (1 + OutSampleData$EHIManagedFuturesReturn[i])}
  
  OutSampleData$EHIManagedFuturesCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIManagedFuturesCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIManagedFuturesCloneIndex[i] <- OutSampleData$EHIManagedFuturesCloneIndex[i - 1] * (1 + OutSampleData$EHIManagedFuturesReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIManagedFuturesIndex, color = "EHIManagedFutures")) +
    geom_line(aes(y = EHIManagedFuturesCloneIndex, color = "EHIManagedFuturesReplication")) +
    
    scale_color_manual(values = c("EHIManagedFutures" = "blue", "EHIManagedFuturesReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIManagedFutures vs. EHIManagedFuturesReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIManagedFuturesReturn", "EHIManagedFuturesReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIManagedFuturesReturn, OutSampleData$EHIManagedFuturesReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIManagedFuturesReturn", "EHIManagedFuturesReplicationReturn")])
  
  
############### EHIDistressedDebt Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIDistressedDebtReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIDistressedDebtReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIDistressedDebtBetas <- as.data.frame(betas)
  
    #EHIDistressedDebtBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIDistressedDebtBetas <- EHIDistressedDebtBetas[-nrow(EHIDistressedDebtBetas), ]
  
  view(EHIDistressedDebtBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIDistressedDebtBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIDistressedDebtBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIDistressedDebtBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIDistressedDebtReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIDistressedDebtReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIDistressedDebtBetas, Factors)
  }
  
  OutSampleData$EHIDistressedDebtReplicationReturn <- EHIDistressedDebtReplicationReturn
  
  view(OutSampleData)
 
# Visualizing both Indexes
  
  OutSampleData$EHIDistressedDebtIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIDistressedDebtIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIDistressedDebtIndex[i] <- OutSampleData$EHIDistressedDebtIndex[i - 1] * (1 + OutSampleData$EHIDistressedDebtReturn[i])}
  
  OutSampleData$EHIDistressedDebtCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIDistressedDebtCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIDistressedDebtCloneIndex[i] <- OutSampleData$EHIDistressedDebtCloneIndex[i - 1] * (1 + OutSampleData$EHIDistressedDebtReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIDistressedDebtIndex, color = "EHIDistressedDebt")) +
    geom_line(aes(y = EHIDistressedDebtCloneIndex, color = "EHIDistressedDebtReplication")) +
    
    scale_color_manual(values = c("EHIDistressedDebt" = "blue", "EHIDistressedDebtReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIDistressedDebt vs. EHIDistressedDebtReplication", x = "Time", y = "Return")
  
# Correlation Tests
  
  cor(OutSampleData[c("EHIDistressedDebtReturn", "EHIDistressedDebtReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIDistressedDebtReturn, OutSampleData$EHIDistressedDebtReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIDistressedDebtReturn", "EHIDistressedDebtReplicationReturn")])
 
    
############### EHIEventDriven Replication ###############  
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIEventDrivenReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIEventDrivenReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
    
  EHIEventDrivenBetas <- as.data.frame(betas)
  
    #EHIEventDrivenBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIEventDrivenBetas <- EHIEventDrivenBetas[-nrow(EHIEventDrivenBetas), ]
  
  view(EHIEventDrivenBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIEventDrivenBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIEventDrivenBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIEventDrivenBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIEventDrivenReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIEventDrivenReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIEventDrivenBetas, Factors)
  }
  
  OutSampleData$EHIEventDrivenReplicationReturn <- EHIEventDrivenReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIEventDrivenIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIEventDrivenIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIEventDrivenIndex[i] <- OutSampleData$EHIEventDrivenIndex[i - 1] * (1 + OutSampleData$EHIEventDrivenReturn[i])}
  
  OutSampleData$EHIEventDrivenCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIEventDrivenCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIEventDrivenCloneIndex[i] <- OutSampleData$EHIEventDrivenCloneIndex[i - 1] * (1 + OutSampleData$EHIEventDrivenReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIEventDrivenIndex, color = "EHIEventDriven")) +
    geom_line(aes(y = EHIEventDrivenCloneIndex, color = "EHIEventDrivenReplication")) +
    
    scale_color_manual(values = c("EHIEventDriven" = "blue", "EHIEventDrivenReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIEventDriven vs. EHIEventDrivenReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIEventDrivenReturn", "EHIEventDrivenReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIEventDrivenReturn, OutSampleData$EHIEventDrivenReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIEventDrivenReturn", "EHIEventDrivenReplicationReturn")])
  
  
############### EHIFixedIncome Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIFixedIncomeReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIFixedIncomeReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIFixedIncomeBetas <- as.data.frame(betas)

    #EHIFixedIncomeBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIFixedIncomeBetas <- EHIFixedIncomeBetas[-nrow(EHIFixedIncomeBetas), ]
  
  view(EHIFixedIncomeBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIFixedIncomeBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIFixedIncomeBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIFixedIncomeBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIFixedIncomeReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIFixedIncomeReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIFixedIncomeBetas, Factors)
  }
  
  OutSampleData$EHIFixedIncomeReplicationReturn <- EHIFixedIncomeReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIFixedIncomeIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIFixedIncomeIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIFixedIncomeIndex[i] <- OutSampleData$EHIFixedIncomeIndex[i - 1] * (1 + OutSampleData$EHIFixedIncomeReturn[i])}
  
  OutSampleData$EHIFixedIncomeCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIFixedIncomeCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIFixedIncomeCloneIndex[i] <- OutSampleData$EHIFixedIncomeCloneIndex[i - 1] * (1 + OutSampleData$EHIFixedIncomeReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIFixedIncomeIndex, color = "EHIFixedIncome")) +
    geom_line(aes(y = EHIFixedIncomeCloneIndex, color = "EHIFixedIncomeReplication")) +
    
    scale_color_manual(values = c("EHIFixedIncome" = "blue", "EHIFixedIncomeReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIFixedIncome vs. EHIFixedIncomeReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIFixedIncomeReturn", "EHIFixedIncomeReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIFixedIncomeReturn, OutSampleData$EHIFixedIncomeReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIFixedIncomeReturn", "EHIFixedIncomeReplicationReturn")])

  
############### EHILSEquity Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHILSEquityReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHILSEquityReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
   
  EHILSEquityBetas <- as.data.frame(betas)
  
    #EHILSEquityBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHILSEquityBetas <- EHILSEquityBetas[-nrow(EHILSEquityBetas), ]
  
  view(EHILSEquityBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHILSEquityBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHILSEquityBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHILSEquityBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHILSEquityReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHILSEquityReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHILSEquityBetas, Factors)
  }
  
  OutSampleData$EHILSEquityReplicationReturn <- EHILSEquityReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHILSEquityIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHILSEquityIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHILSEquityIndex[i] <- OutSampleData$EHILSEquityIndex[i - 1] * (1 + OutSampleData$EHILSEquityReturn[i])}
  
  OutSampleData$EHILSEquityCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHILSEquityCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHILSEquityCloneIndex[i] <- OutSampleData$EHILSEquityCloneIndex[i - 1] * (1 + OutSampleData$EHILSEquityReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHILSEquityIndex, color = "EHILSEquity")) +
    geom_line(aes(y = EHILSEquityCloneIndex, color = "EHILSEquityReplication")) +
    
    scale_color_manual(values = c("EHILSEquity" = "blue", "EHILSEquityReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHILSEquity vs. EHILSEquityReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHILSEquityReturn", "EHILSEquityReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHILSEquityReturn, OutSampleData$EHILSEquityReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHILSEquityReturn", "EHILSEquityReplicationReturn")])
 
  
############### EHIMacro Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMacroReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMacroReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIMacroBetas <- as.data.frame(betas)
  
    #EHIMacroBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIMacroBetas <- EHIMacroBetas[-nrow(EHIMacroBetas), ]
  
  view(EHIMacroBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIMacroBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIMacroBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIMacroBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIMacroReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIMacroReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIMacroBetas, Factors)
  }
  
  OutSampleData$EHIMacroReplicationReturn <- EHIMacroReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIMacroIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMacroIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMacroIndex[i] <- OutSampleData$EHIMacroIndex[i - 1] * (1 + OutSampleData$EHIMacroReturn[i])}
  
  OutSampleData$EHIMacroCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMacroCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMacroCloneIndex[i] <- OutSampleData$EHIMacroCloneIndex[i - 1] * (1 + OutSampleData$EHIMacroReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIMacroIndex, color = "EHIMacro")) +
    geom_line(aes(y = EHIMacroCloneIndex, color = "EHIMacroReplication")) +
    
    scale_color_manual(values = c("EHIMacro" = "blue", "EHIMacroReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIMacro vs. EHIMacroReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIMacroReturn", "EHIMacroReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIMacroReturn, OutSampleData$EHIMacroReplicationReturn, paired = TRUE)
  t_test_result
 
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIMacroReturn", "EHIMacroReplicationReturn")])
  
  
############### EHIMultiStrategy Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMultiStrategyReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMultiStrategyReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIMultiStrategyBetas <- as.data.frame(betas)
  
    #EHIMultiStrategyBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIMultiStrategyBetas <- EHIMultiStrategyBetas[-nrow(EHIMultiStrategyBetas), ]
  
  view(EHIMultiStrategyBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIMultiStrategyBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIMultiStrategyBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIMultiStrategyBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIMultiStrategyReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIMultiStrategyReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIMultiStrategyBetas, Factors)
  }
  
  OutSampleData$EHIMultiStrategyReplicationReturn <- EHIMultiStrategyReplicationReturn
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIMultiStrategyIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMultiStrategyIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMultiStrategyIndex[i] <- OutSampleData$EHIMultiStrategyIndex[i - 1] * (1 + OutSampleData$EHIMultiStrategyReturn[i])}
  
  OutSampleData$EHIMultiStrategyCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMultiStrategyCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMultiStrategyCloneIndex[i] <- OutSampleData$EHIMultiStrategyCloneIndex[i - 1] * (1 + OutSampleData$EHIMultiStrategyReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIMultiStrategyIndex, color = "EHIMultiStrategy")) +
    geom_line(aes(y = EHIMultiStrategyCloneIndex, color = "EHIMultiStrategyReplication")) +
    
    scale_color_manual(values = c("EHIMultiStrategy" = "blue", "EHIMultiStrategyReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIMultiStrategy vs. EHIMultiStrategyReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIMultiStrategyReturn", "EHIMultiStrategyReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIMultiStrategyReturn, OutSampleData$EHIMultiStrategyReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIMultiStrategyReturn", "EHIMultiStrategyReplicationReturn")])
  
  
############### EHIRelativeValue Replication ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIRelativeValueReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIRelativeValueReturn", "GB1MReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
  EHIRelativeValueBetas <- as.data.frame(betas)
  
    #EHIRelativeValueBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIRelativeValueBetas <- EHIRelativeValueBetas[-nrow(EHIRelativeValueBetas), ]
  
  view(EHIRelativeValueBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIRelativeValueBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIRelativeValueBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas) + EHIRelativeValueBetas[row, "(Intercept)"]
    return(predicted_value)
  }
  
  EHIRelativeValueReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIRelativeValueReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIRelativeValueBetas, Factors)
  }
  
  OutSampleData$EHIRelativeValueReplicationReturn <- EHIRelativeValueReplicationReturn
  
  view(OutSampleData)

# Visualizing both Returns
  
  OutSampleData$EHIRelativeValueIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIRelativeValueIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIRelativeValueIndex[i] <- OutSampleData$EHIRelativeValueIndex[i - 1] * (1 + OutSampleData$EHIRelativeValueReturn[i])}
  
  OutSampleData$EHIRelativeValueCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIRelativeValueCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIRelativeValueCloneIndex[i] <- OutSampleData$EHIRelativeValueCloneIndex[i - 1] * (1 + OutSampleData$EHIRelativeValueReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIRelativeValueIndex, color = "EHIRelativeValue")) +
    geom_line(aes(y = EHIRelativeValueCloneIndex, color = "EHIRelativeValueReplication")) +
    
    scale_color_manual(values = c("EHIRelativeValue" = "blue", "EHIRelativeValueReplication" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIRelativeValue vs. EHIRelativeValueReplication", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIRelativeValueReturn", "EHIRelativeValueReplicationReturn")])
  
  t_test_result <- t.test(OutSampleData$EHIRelativeValueReturn, OutSampleData$EHIRelativeValueReplicationReturn, paired = TRUE)
  t_test_result
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIRelativeValueReturn", "EHIRelativeValueReplicationReturn")])

  



############### EHIMain Clone ###############
    
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMainReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMainReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIMainBetas <- as.data.frame(betas)
  
    #EHIMainBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIMainBetas <- EHIMainBetas[-nrow(EHIMainBetas), ]
  
  view(EHIMainBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIMainBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIMainBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIMainReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIMainReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIMainBetas, Factors)
  }
  
  OutSampleData$EHIMainReplicationReturn <- EHIMainReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIMainReturn <- OutSampleData$EHIMainReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIMainReplicationReturn <- OutSampleData$EHIMainReplicationReturn - OutSampleData$GB1MReturn 
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIMainReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIMainReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIMainReplicationReturn <- OutSampleData$EHIMainReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIMainIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMainIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMainIndex[i] <- OutSampleData$EHIMainIndex[i - 1] * (1 + OutSampleData$EHIMainReturn[i])}
  
  OutSampleData$EHIMainCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMainCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMainCloneIndex[i] <- OutSampleData$EHIMainCloneIndex[i - 1] * (1 + OutSampleData$EHIMainReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIMainIndex, color = "EHIMain")) +
    geom_line(aes(y = EHIMainCloneIndex, color = "EHIMainClone")) +
    
    scale_color_manual(values = c("EHIMain" = "blue", "EHIMainClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIMain vs. EHIMainClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIMainReturn", "EHIMainReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIMainReturn", "EHIMainReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIMain <- mean(OutSampleData$EHIMainReturn, na.rm = TRUE) / sd(OutSampleData$EHIMainReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIMain)
  sharpe_ratio_EHIMainReplication <- mean(OutSampleData$EHIMainReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIMainReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIMainReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIMainReplicationReturn - OutSampleData$EHIMainReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIMainReplicationReturn - OutSampleData$EHIMainReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIMainReplicationReturn - OutSampleData$EHIMainReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIMainReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIMainReturn^2)))
  theils_inequ_coef
  
  
############### EHIArbitrage Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIArbitrageReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data) 
    return(coef(fit))
  } 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIArbitrageReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
    
  EHIArbitrageBetas <- as.data.frame(betas)
  
    #EHIArbitrageBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIArbitrageBetas <- EHIArbitrageBetas[-nrow(EHIArbitrageBetas), ]
  
  view(EHIArbitrageBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIArbitrageBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIArbitrageBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIArbitrageReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIArbitrageReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIArbitrageBetas, Factors)
  }
  
  OutSampleData$EHIArbitrageReplicationReturn <- EHIArbitrageReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIArbitrageReturn <- OutSampleData$EHIArbitrageReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIArbitrageReplicationReturn <- OutSampleData$EHIArbitrageReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIArbitrageReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIArbitrageReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIArbitrageReplicationReturn <- OutSampleData$EHIArbitrageReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIArbitrageIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIArbitrageIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIArbitrageIndex[i] <- OutSampleData$EHIArbitrageIndex[i - 1] * (1 + OutSampleData$EHIArbitrageReturn[i])}
  
  OutSampleData$EHIArbitrageCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIArbitrageCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIArbitrageCloneIndex[i] <- OutSampleData$EHIArbitrageCloneIndex[i - 1] * (1 + OutSampleData$EHIArbitrageReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIArbitrageIndex, color = "EHIArbitrage")) +
    geom_line(aes(y = EHIArbitrageCloneIndex, color = "EHIArbitrageClone")) +
    
    scale_color_manual(values = c("EHIArbitrage" = "blue", "EHIArbitrageClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIArbitrage vs. EHIArbitrageClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIArbitrageReturn", "EHIArbitrageReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIArbitrageReturn", "EHIArbitrageReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIArbitrage <- mean(OutSampleData$EHIArbitrageReturn, na.rm = TRUE) / sd(OutSampleData$EHIArbitrageReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIArbitrage)
  sharpe_ratio_EHIArbitrageReplication <- mean(OutSampleData$EHIArbitrageReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIArbitrageReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIArbitrageReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIArbitrageReplicationReturn - OutSampleData$EHIArbitrageReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIArbitrageReplicationReturn - OutSampleData$EHIArbitrageReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIArbitrageReplicationReturn - OutSampleData$EHIArbitrageReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIArbitrageReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIArbitrageReturn^2)))
  theils_inequ_coef
  
  
############### EHIManagedFutures Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIManagedFuturesReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIManagedFuturesReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
    
  EHIManagedFuturesBetas <- as.data.frame(betas)
  
    #EHIManagedFuturesBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIManagedFuturesBetas <- EHIManagedFuturesBetas[-nrow(EHIManagedFuturesBetas), ]
  
  view(EHIManagedFuturesBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIManagedFuturesBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIManagedFuturesBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIManagedFuturesReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIManagedFuturesReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIManagedFuturesBetas, Factors)
  }
  
  OutSampleData$EHIManagedFuturesReplicationReturn <- EHIManagedFuturesReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIManagedFuturesReturn <- OutSampleData$EHIManagedFuturesReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIManagedFuturesReplicationReturn <- OutSampleData$EHIManagedFuturesReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIManagedFuturesReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIManagedFuturesReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIManagedFuturesReplicationReturn <- OutSampleData$EHIManagedFuturesReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIManagedFuturesIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIManagedFuturesIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIManagedFuturesIndex[i] <- OutSampleData$EHIManagedFuturesIndex[i - 1] * (1 + OutSampleData$EHIManagedFuturesReturn[i])}
  
  OutSampleData$EHIManagedFuturesCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIManagedFuturesCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIManagedFuturesCloneIndex[i] <- OutSampleData$EHIManagedFuturesCloneIndex[i - 1] * (1 + OutSampleData$EHIManagedFuturesReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIManagedFuturesIndex, color = "EHIManagedFutures")) +
    geom_line(aes(y = EHIManagedFuturesCloneIndex, color = "EHIManagedFuturesClone")) +
    
    scale_color_manual(values = c("EHIManagedFutures" = "blue", "EHIManagedFuturesClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIManagedFutures vs. EHIManagedFuturesClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIManagedFuturesReturn", "EHIManagedFuturesReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIManagedFuturesReturn", "EHIManagedFuturesReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIManagedFutures <- mean(OutSampleData$EHIManagedFuturesReturn, na.rm = TRUE) / sd(OutSampleData$EHIManagedFuturesReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIManagedFutures)
  sharpe_ratio_EHIManagedFuturesReplication <- mean(OutSampleData$EHIManagedFuturesReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIManagedFuturesReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIManagedFuturesReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIManagedFuturesReplicationReturn - OutSampleData$EHIManagedFuturesReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIManagedFuturesReplicationReturn - OutSampleData$EHIManagedFuturesReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIManagedFuturesReplicationReturn - OutSampleData$EHIManagedFuturesReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIManagedFuturesReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIManagedFuturesReturn^2)))
  theils_inequ_coef
  
  
############### EHIDistressedDebt Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIDistressedDebtReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIDistressedDebtReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIDistressedDebtBetas <- as.data.frame(betas)
  
    #EHIDistressedDebtBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIDistressedDebtBetas <- EHIDistressedDebtBetas[-nrow(EHIDistressedDebtBetas), ]
  
  view(EHIDistressedDebtBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIDistressedDebtBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIDistressedDebtBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIDistressedDebtReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIDistressedDebtReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIDistressedDebtBetas, Factors)
  }
  
  OutSampleData$EHIDistressedDebtReplicationReturn <- EHIDistressedDebtReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIDistressedDebtReturn <- OutSampleData$EHIDistressedDebtReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIDistressedDebtReplicationReturn <- OutSampleData$EHIDistressedDebtReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIDistressedDebtReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIDistressedDebtReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIDistressedDebtReplicationReturn <- OutSampleData$EHIDistressedDebtReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIDistressedDebtIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIDistressedDebtIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIDistressedDebtIndex[i] <- OutSampleData$EHIDistressedDebtIndex[i - 1] * (1 + OutSampleData$EHIDistressedDebtReturn[i])}
  
  OutSampleData$EHIDistressedDebtCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIDistressedDebtCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIDistressedDebtCloneIndex[i] <- OutSampleData$EHIDistressedDebtCloneIndex[i - 1] * (1 + OutSampleData$EHIDistressedDebtReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIDistressedDebtIndex, color = "EHIDistressedDebt")) +
    geom_line(aes(y = EHIDistressedDebtCloneIndex, color = "EHIMainClone")) +
    
    scale_color_manual(values = c("EHIDistressedDebt" = "blue", "EHIMainClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIDistressedDebt vs. EHIDistressedDebtClone", x = "Time", y = "Return")
  
  
  view(OutSampleData)  
  
  
# Correlation Tests
  
  cor(OutSampleData[c("EHIDistressedDebtReturn", "EHIDistressedDebtReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIDistressedDebtReturn", "EHIDistressedDebtReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIDistressedDebt <- mean(OutSampleData$EHIDistressedDebtReturn, na.rm = TRUE) / sd(OutSampleData$EHIDistressedDebtReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIDistressedDebt)
  sharpe_ratio_EHIDistressedDebtReplication <- mean(OutSampleData$EHIDistressedDebtReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIDistressedDebtReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIDistressedDebtReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIDistressedDebtReplicationReturn - OutSampleData$EHIDistressedDebtReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIDistressedDebtReplicationReturn - OutSampleData$EHIDistressedDebtReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIDistressedDebtReplicationReturn - OutSampleData$EHIDistressedDebtReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIDistressedDebtReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIDistressedDebtReturn^2)))
  theils_inequ_coef
  
  
############### EHIEventDriven Clone ###############  
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIEventDrivenReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIEventDrivenReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIEventDrivenBetas <- as.data.frame(betas)
  
    #EHIEventDrivenBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIEventDrivenBetas <- EHIEventDrivenBetas[-nrow(EHIEventDrivenBetas), ]
  
  view(EHIEventDrivenBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIEventDrivenBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIEventDrivenBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIEventDrivenReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIEventDrivenReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIEventDrivenBetas, Factors)
  }
  
  OutSampleData$EHIEventDrivenReplicationReturn <- EHIEventDrivenReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIEventDrivenReturn <- OutSampleData$EHIEventDrivenReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIEventDrivenReplicationReturn <- OutSampleData$EHIEventDrivenReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIEventDrivenReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIEventDrivenReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIEventDrivenReplicationReturn <- OutSampleData$EHIEventDrivenReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIEventDrivenIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIEventDrivenIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIEventDrivenIndex[i] <- OutSampleData$EHIEventDrivenIndex[i - 1] * (1 + OutSampleData$EHIEventDrivenReturn[i])}
  
  OutSampleData$EHIEventDrivenCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIEventDrivenCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIEventDrivenCloneIndex[i] <- OutSampleData$EHIEventDrivenCloneIndex[i - 1] * (1 + OutSampleData$EHIEventDrivenReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIEventDrivenIndex, color = "EHIEventDriven")) +
    geom_line(aes(y = EHIEventDrivenCloneIndex, color = "EHIEventDrivenClone")) +
    
    scale_color_manual(values = c("EHIEventDriven" = "blue", "EHIEventDrivenClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIEventDriven vs. EHIEventDrivenClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIEventDrivenReturn", "EHIEventDrivenReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIEventDrivenReturn", "EHIEventDrivenReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIEventDriven <- mean(OutSampleData$EHIEventDrivenReturn, na.rm = TRUE) / sd(OutSampleData$EHIEventDrivenReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIEventDriven)
  sharpe_ratio_EHIEventDrivenReplication <- mean(OutSampleData$EHIEventDrivenReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIEventDrivenReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIEventDrivenReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIEventDrivenReplicationReturn - OutSampleData$EHIEventDrivenReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIEventDrivenReplicationReturn - OutSampleData$EHIEventDrivenReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIEventDrivenReplicationReturn - OutSampleData$EHIEventDrivenReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIEventDrivenReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIEventDrivenReturn^2)))
  theils_inequ_coef
  
  
############### EHIFixedIncome Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIFixedIncomeReturn ~ BAA10YReturn + SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIFixedIncomeReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIFixedIncomeBetas <- as.data.frame(betas)
  
    #EHIFixedIncomeBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIFixedIncomeBetas <- EHIFixedIncomeBetas[-nrow(EHIFixedIncomeBetas), ]
  
  view(EHIFixedIncomeBetas)
  
# Creating the replication data 
  
  Factors <- c("BAA10YReturn", "SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIFixedIncomeBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIFixedIncomeBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIFixedIncomeReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIFixedIncomeReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIFixedIncomeBetas, Factors)
  }
  
  OutSampleData$EHIFixedIncomeReplicationReturn <- EHIFixedIncomeReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIFixedIncomeReturn <- OutSampleData$EHIFixedIncomeReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIFixedIncomeReplicationReturn <- OutSampleData$EHIFixedIncomeReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIFixedIncomeReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIFixedIncomeReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIFixedIncomeReplicationReturn <- OutSampleData$EHIFixedIncomeReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIFixedIncomeIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIFixedIncomeIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIFixedIncomeIndex[i] <- OutSampleData$EHIFixedIncomeIndex[i - 1] * (1 + OutSampleData$EHIFixedIncomeReturn[i])}
  
  OutSampleData$EHIFixedIncomeCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIFixedIncomeCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIFixedIncomeCloneIndex[i] <- OutSampleData$EHIFixedIncomeCloneIndex[i - 1] * (1 + OutSampleData$EHIFixedIncomeReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIFixedIncomeIndex, color = "EHIFixedIncome")) +
    geom_line(aes(y = EHIFixedIncomeCloneIndex, color = "EHIFixedIncomeClone")) +
    
    scale_color_manual(values = c("EHIFixedIncome" = "blue", "EHIFixedIncomeClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIFixedIncome vs. EHIFixedIncomeClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIFixedIncomeReturn", "EHIFixedIncomeReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIFixedIncomeReturn", "EHIFixedIncomeReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIFixedIncome <- mean(OutSampleData$EHIFixedIncomeReturn, na.rm = TRUE) / sd(OutSampleData$EHIFixedIncomeReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIFixedIncome)
  sharpe_ratio_EHIFixedIncomeReplication <- mean(OutSampleData$EHIFixedIncomeReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIFixedIncomeReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIFixedIncomeReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIFixedIncomeReplicationReturn - OutSampleData$EHIFixedIncomeReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIFixedIncomeReplicationReturn - OutSampleData$EHIFixedIncomeReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIFixedIncomeReplicationReturn - OutSampleData$EHIFixedIncomeReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIFixedIncomeReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIFixedIncomeReturn^2)))
  theils_inequ_coef
  
  
############### EHILSEquity Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHILSEquityReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHILSEquityReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
    
  EHILSEquityBetas <- as.data.frame(betas)
  
    #EHILSEquityBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHILSEquityBetas <- EHILSEquityBetas[-nrow(EHILSEquityBetas), ]
  
  view(EHILSEquityBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHILSEquityBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHILSEquityBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHILSEquityReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHILSEquityReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHILSEquityBetas, Factors)
  }
  
  OutSampleData$EHILSEquityReplicationReturn <- EHILSEquityReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHILSEquityReturn <- OutSampleData$EHILSEquityReturn - OutSampleData$GB1MReturn
  OutSampleData$EHILSEquityReplicationReturn <- OutSampleData$EHILSEquityReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHILSEquityReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHILSEquityReplicationReturn, na.rm = TRUE))
  OutSampleData$EHILSEquityReplicationReturn <- OutSampleData$EHILSEquityReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHILSEquityIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHILSEquityIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHILSEquityIndex[i] <- OutSampleData$EHILSEquityIndex[i - 1] * (1 + OutSampleData$EHILSEquityReturn[i])}
  
  OutSampleData$EHILSEquityCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHILSEquityCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHILSEquityCloneIndex[i] <- OutSampleData$EHILSEquityCloneIndex[i - 1] * (1 + OutSampleData$EHILSEquityReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHILSEquityIndex, color = "EHILSEquity")) +
    geom_line(aes(y = EHILSEquityCloneIndex, color = "EHILSEquityClone")) +
    
    scale_color_manual(values = c("EHILSEquity" = "blue", "EHILSEquityClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHILSEquity vs. EHILSEquityClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHILSEquityReturn", "EHILSEquityReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHILSEquityReturn", "EHILSEquityReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHILSEquity <- mean(OutSampleData$EHILSEquityReturn, na.rm = TRUE) / sd(OutSampleData$EHILSEquityReturn, na.rm = TRUE)
  print(sharpe_ratio_EHILSEquity)
  sharpe_ratio_EHILSEquityReplication <- mean(OutSampleData$EHILSEquityReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHILSEquityReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHILSEquityReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHILSEquityReplicationReturn - OutSampleData$EHILSEquityReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHILSEquityReplicationReturn - OutSampleData$EHILSEquityReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHILSEquityReplicationReturn - OutSampleData$EHILSEquityReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHILSEquityReplicationReturn^2)) + sqrt(mean(OutSampleData$EHILSEquityReturn^2)))
  theils_inequ_coef
  
  
############### EHIMacro Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMacroReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data) 
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMacroReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIMacroBetas <- as.data.frame(betas)
  
    #EHIMacroBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIMacroBetas <- EHIMacroBetas[-nrow(EHIMacroBetas), ]
  
  view(EHIMacroBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIMacroBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIMacroBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIMacroReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIMacroReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIMacroBetas, Factors)
  }
  
  OutSampleData$EHIMacroReplicationReturn <- EHIMacroReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIMacroReturn <- OutSampleData$EHIMacroReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIMacroReplicationReturn <- OutSampleData$EHIMacroReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIMacroReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIMacroReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIMacroReplicationReturn <- OutSampleData$EHIMacroReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIMacroIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMacroIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMacroIndex[i] <- OutSampleData$EHIMacroIndex[i - 1] * (1 + OutSampleData$EHIMacroReturn[i])}
  
  OutSampleData$EHIMacroCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMacroCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMacroCloneIndex[i] <- OutSampleData$EHIMacroCloneIndex[i - 1] * (1 + OutSampleData$EHIMacroReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIMacroIndex, color = "EHIMacro")) +
    geom_line(aes(y = EHIMacroCloneIndex, color = "EHIMacroClone")) +
    
    scale_color_manual(values = c("EHIMacro" = "blue", "EHIMacroClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIMacro vs. EHIMacroClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIMacroReturn", "EHIMacroReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIMacroReturn", "EHIMacroReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIMacro <- mean(OutSampleData$EHIMacroReturn, na.rm = TRUE) / sd(OutSampleData$EHIMacroReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIMacro)
  sharpe_ratio_EHIMacroReplication <- mean(OutSampleData$EHIMacroReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIMacroReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIMacroReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIMacroReplicationReturn - OutSampleData$EHIMacroReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIMacroReplicationReturn - OutSampleData$EHIMacroReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIMacroReplicationReturn - OutSampleData$EHIMacroReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIMacroReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIMacroReturn^2)))
  theils_inequ_coef
  
  
############### EHIMultiStrategy Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIMultiStrategyReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIMultiStrategyReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIMultiStrategyBetas <- as.data.frame(betas)
  
    #EHIMultiStrategyBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIMultiStrategyBetas <- EHIMultiStrategyBetas[-nrow(EHIMultiStrategyBetas), ]
  
  view(EHIMultiStrategyBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIMultiStrategyBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIMultiStrategyBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIMultiStrategyReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIMultiStrategyReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIMultiStrategyBetas, Factors)
  }
  
  OutSampleData$EHIMultiStrategyReplicationReturn <- EHIMultiStrategyReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIMultiStrategyReturn <- OutSampleData$EHIMultiStrategyReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIMultiStrategyReplicationReturn <- OutSampleData$EHIMultiStrategyReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIMultiStrategyReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIMultiStrategyReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIMultiStrategyReplicationReturn <- OutSampleData$EHIMultiStrategyReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Indexes
  
  OutSampleData$EHIMultiStrategyIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMultiStrategyIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMultiStrategyIndex[i] <- OutSampleData$EHIMultiStrategyIndex[i - 1] * (1 + OutSampleData$EHIMultiStrategyReturn[i])}
  
  OutSampleData$EHIMultiStrategyCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIMultiStrategyCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIMultiStrategyCloneIndex[i] <- OutSampleData$EHIMultiStrategyCloneIndex[i - 1] * (1 + OutSampleData$EHIMultiStrategyReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIMultiStrategyIndex, color = "EHIMultiStrategy")) +
    geom_line(aes(y = EHIMultiStrategyCloneIndex, color = "EHIMultiStrategyClone")) +
    
    scale_color_manual(values = c("EHIMultiStrategy" = "blue", "EHIMultiStrategyClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIMultiStrategy vs. EHIMultiStrategyClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIMultiStrategyReturn", "EHIMultiStrategyReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIMultiStrategyReturn", "EHIMultiStrategyReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIMultiStrategy <- mean(OutSampleData$EHIMultiStrategyReturn, na.rm = TRUE) / sd(OutSampleData$EHIMultiStrategyReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIMultiStrategy)
  sharpe_ratio_EHIMultiStrategyReplication <- mean(OutSampleData$EHIMultiStrategyReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIMultiStrategyReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIMultiStrategyReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIMultiStrategyReplicationReturn - OutSampleData$EHIMultiStrategyReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIMultiStrategyReplicationReturn - OutSampleData$EHIMultiStrategyReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIMultiStrategyReplicationReturn - OutSampleData$EHIMultiStrategyReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIMultiStrategyReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIMultiStrategyReturn^2)))
  theils_inequ_coef
  
  
############### EHIRelativeValue Clone ###############
  
# Rolling 60-month windows 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn")
  
  functionLR <- function(data) {
    data <- as.data.frame(data)
    fit <- lm(EHIRelativeValueReturn ~ SPXReturn + IEFReturn + DXYReturn + VIXYReturn + GSGReturn + VALUEReturn + MTUMReturn + PHDGReturn - 1, data = data)
    return(coef(fit))} 
  
  adjustedData <- DataAll[1:nrow(DataAll), c("EHIRelativeValueReturn", Factors)]
  
  betas <- rollapply(data = adjustedData, 
                     width = 60, 
                     FUN = functionLR, 
                     align = "right",
                     by.column = FALSE)
  
    betas <- as.data.frame(betas)
    
    risk_free_adjustment <- 1 - rowSums(betas)
    
    betas$GB1MReturn <- risk_free_adjustment
  
  EHIRelativeValueBetas <- as.data.frame(betas)
  
    #EHIRelativeValueBetas$Date <- DataAll$Date[(60):nrow(DataAll)] #-> Optional column with dates 
  
  EHIRelativeValueBetas <- EHIRelativeValueBetas[-nrow(EHIRelativeValueBetas), ]
  
  view(EHIRelativeValueBetas)
  
# Creating the replication data 
  
  Factors <- c("SPXReturn", "IEFReturn", "DXYReturn", "VIXYReturn", "GSGReturn", "VALUEReturn", "MTUMReturn", "PHDGReturn", "GB1MReturn")
  
  OutSampleData <- DataAll[61:165, ]
  view(OutSampleData) 
  
  assignmentBetas <- function(row, data, EHIRelativeValueBetas, Factors) {
    factor_values <- data[row, Factors, drop = FALSE]
    betas <- EHIRelativeValueBetas[row, Factors, drop = FALSE]
    predicted_value <- sum(factor_values * betas)
    return(predicted_value)
  }
  
  EHIRelativeValueReplicationReturn <- numeric(nrow(OutSampleData))
  
  for (i in 1:nrow(OutSampleData)) {
    EHIRelativeValueReplicationReturn[i] <- assignmentBetas(i, OutSampleData, EHIRelativeValueBetas, Factors)
  }
  
  OutSampleData$EHIRelativeValueReplicationReturn <- EHIRelativeValueReplicationReturn
  
  view(OutSampleData)
  
# Subtracting the risk-free rate
  
  OutSampleData$EHIRelativeValueReturn <- OutSampleData$EHIRelativeValueReturn - OutSampleData$GB1MReturn
  OutSampleData$EHIRelativeValueReplicationReturn <- OutSampleData$EHIRelativeValueReplicationReturn - OutSampleData$GB1MReturn
  
# Renormalizing the returns of the Fund and Clone
  
  normalization <- (sd(OutSampleData$EHIRelativeValueReturn, na.rm = TRUE)) / 
    (sd(OutSampleData$EHIRelativeValueReplicationReturn, na.rm = TRUE))
  OutSampleData$EHIRelativeValueReplicationReturn <- OutSampleData$EHIRelativeValueReplicationReturn * normalization
  
  view(OutSampleData)
  
# Visualizing both Returns
  
  OutSampleData$EHIRelativeValueIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIRelativeValueIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIRelativeValueIndex[i] <- OutSampleData$EHIRelativeValueIndex[i - 1] * (1 + OutSampleData$EHIRelativeValueReturn[i])}
  
  OutSampleData$EHIRelativeValueCloneIndex <- rep(nrow(OutSampleData))
  OutSampleData$EHIRelativeValueCloneIndex[1] <- 100
  for (i in 2:nrow(OutSampleData)) {OutSampleData$EHIRelativeValueCloneIndex[i] <- OutSampleData$EHIRelativeValueCloneIndex[i - 1] * (1 + OutSampleData$EHIRelativeValueReplicationReturn[i])}
  
  ggplot(data = OutSampleData, aes(x = Date)) +
    
    labs(x = "Date", y = "Return", color = "Type") +
    
    geom_line(aes(y = EHIRelativeValueIndex, color = "EHIRelativeValue")) +
    geom_line(aes(y = EHIRelativeValueCloneIndex, color = "EHIRelativeValueClone")) +
    
    scale_color_manual(values = c("EHIRelativeValue" = "blue", "EHIRelativeValueClone" = "red")) +
    
    geom_hline(yintercept = 100, color = "black", linetype = "dashed", linewidth = 0.5) +
    
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    
    scale_y_continuous(breaks = seq(min(-100), max(700), by = 5)) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    labs(title = "EHIRelativeValue vs. EHIRelativeValueClone", x = "Time", y = "Return")
  
# Correlation Test
  
  cor(OutSampleData[c("EHIRelativeValueReturn", "EHIRelativeValueReplicationReturn")])
  
# Performance Tests   
  
  # Mean return (mean) & standard deviation (sd)
  inspect(OutSampleData[c("EHIRelativeValueReturn", "EHIRelativeValueReplicationReturn")])
  
  # Sharpe ratio
  sharpe_ratio_EHIRelativeValue <- mean(OutSampleData$EHIRelativeValueReturn, na.rm = TRUE) / sd(OutSampleData$EHIRelativeValueReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIRelativeValue)
  sharpe_ratio_EHIRelativeValueReplication <- mean(OutSampleData$EHIRelativeValueReplicationReturn, na.rm = TRUE) / sd(OutSampleData$EHIRelativeValueReplicationReturn, na.rm = TRUE)
  print(sharpe_ratio_EHIRelativeValueReplication)
  
  # RMSE -> Root Mean Square Error 
  rmse <- sqrt(mean((OutSampleData$EHIRelativeValueReplicationReturn - OutSampleData$EHIRelativeValueReturn)^2))
  rmse
  
  # Tracking Error
  tracking_error <- sd(OutSampleData$EHIRelativeValueReplicationReturn - OutSampleData$EHIRelativeValueReturn)
  tracking_error
  
  # Theil’s Inequality Coefficients
  rmse <- sqrt(mean((OutSampleData$EHIRelativeValueReplicationReturn - OutSampleData$EHIRelativeValueReturn)^2))
  theils_inequ_coef <- rmse / (sqrt(mean(OutSampleData$EHIRelativeValueReplicationReturn^2)) + sqrt(mean(OutSampleData$EHIRelativeValueReturn^2)))
  theils_inequ_coef
  
  