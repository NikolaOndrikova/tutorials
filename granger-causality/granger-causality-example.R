### Hackernoon - Granger Causality

# install.packages(c("stats",
#                     "forecast",  
#                     "vars", 
#                     "aod",
#                     "aTSA"))
library(stats)
library(forecast)
library(vars)
library(aod)
library(aTSA)


### import data
climate_df <- read.csv("./google-trends-granger-causality.csv")

### function to prepare our time series (you can add differencing if needed)
stacionarise <- function(vec){
  set.seed(12345)
  ts_data = ts(vec, start=c(2016, 50),frequency=52)
  
  # clean the times series
  ts.stl <- stl(ts_data, s.window = "periodic", robust = TRUE)  # decompose the TS
  ts.sa <- seasadj(ts.stl) # de-seasonalize
  stl_remain = ts.stl$time.series[,3]
  #diff_data = diff(stl_remain,1)
  
  
  # testing stationarity 
  cat("-----------------------------------------------------", "\n")
  cat("-----------------------------------------------------", "\n")
  cat("p-value < 0.05 indicates the TS is stationary", "\n")
  #print(adf.test(diff_data))
  print(adf.test(stl_remain))
  
  #return(diff_data)
  return(stl_remain)
}

### clean time series
stationary_data = apply(climate_df[,2:3], MARGIN = 2, FUN = stacionarise)

plot(stationary_data[,1], type="l")
lines(stationary_data[,2])

### estimation of a VAR by utilising OLS per equation, p=6 is number of lags, but you can experiment with different values
set.seed(12345)
V.6 <-VAR(stationary_data, p=6 ,type="both")

### stability analysis 
cat(1/roots(V.6)[[1]], " *Shoud be > 1, if stable.", "\n")

print(summary(V.6))

### answering the main question - what is causing what?
climateChange_heatWave <- wald.test(b=coef(V.6$varresult[[1]]), Sigma=vcov(V.6$varresult[[1]]), 
                                    Terms=c(2,4,6,8,10))

heatWave_climateChange <- wald.test(b=coef(V.6$varresult[[2]]), Sigma=vcov(V.6$varresult[[2]]), 
                                    Terms= c(1,3,5,7,9))


cat("Does interest in climate change comes before interest in heat wave?","\n")
print(climateChange_heatWave$result$chi2)
cat("Does interest in heat wave comes before interest in climate change?", "\n")
print(heatWave_climateChange$result$chi2)

