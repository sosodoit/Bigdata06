#******************************************************************************#
#                                 필수 load
#******************************************************************************#
source('./AIR/packages_need.R', encoding='utf-8')    # 필수 패키지 load
source('./AIR/sgg_separate.R', encoding='utf-8')     # week data

#******************************************************************************#
#                         구별로 대기오염물질 o3 분석
#                          --> 39(측정소)*o3
#******************************************************************************#
freq <- 365.25/7

f.o3 <- function(tr, te){
  
  train.o3 <<- tr[,c(2,4)]
  test.o3 <<- te[,c(2,4)]
  
  train.o3.ts <<- ts(train.o3, start=decimal_date(as.Date("2010-01-01")), frequency= freq)
  test.o3.ts <<- ts(test.o3, start=decimal_date(as.Date("2020-01-01")), frequency= freq)
  
  o3.tr <<- train.o3.ts[,2]
  o3.te <<- test.o3.ts[,2]
  
  mod_lst <<- list (
    
    mod_exponential = ets(o3.tr, ic='aicc', restrict=FALSE),
    mod_sts = StructTS(o3.tr),
    mod_neural = nnetar(o3.tr),
    mod_stl = stlm(o3.tr, ic='aicc', robust=TRUE, method='ets'),
    mod_tbats = tbats(o3.tr, ic='aicc', seasonal.periods=c(7,365.25)),
    mod_bats = bats(o3.tr, ic='aicc', seasonal.periods=c(7,365.25)),
    mod_arima = auto.arima(o3.tr, ic='aicc', stepwise=FALSE)
    
  )
  
  forecasts <<- lapply(mod_lst, forecast, 330)
  forecasts$naive <<- naive(train.o3.ts, 330)
  forecasts$snaive <<- snaive(train.o3.ts, 330)

  acc <<- lapply(forecasts, function(f){
    accuracy(f, o3.te)[2,,drop=FALSE]
  })
  
  acc <<- do.call(rbind, acc)
  row.names(acc) <<- names(forecasts)
  acc <<- acc[order(acc[,'MASE']),] %>% round(2)
  
}

f.o3(sgg10, sgg10.te) # 구로구
save.image(file = "./analysis_O3_sgg10.RData")

