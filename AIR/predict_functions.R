source('./AIR/packages_need.R', encoding='utf-8')
source('./AIR/sgg_separate.R', encoding='utf-8')
##################################################################################
# 예측값 월별 평균!!!! (5: tbats, 4: stl 3 : neural)
##################################################################################
mon.avg.tbats <- function() {
  
  day_vec <- c(0,31,60,91,121,152,182,213,244,274,305,335,366)
  a <- data.frame(predict(mod_lst[[5]]))
  d <- data.frame(cbind(rownames(a), a$Point.Forecast))[1:51,] #우선 2020년만 예측하려고 나머지 제외함!
  colnames(d) <- c("day", "co")
  d$day <- 365.25 * as.numeric(substr(d$day,5,8))
  
  month.average <- NULL
  for (i in 1:(length(day_vec)-1)) {
    month.average <- c(month.average, mean(as.numeric(d[(d$day >= day_vec[i] & d$day<= day_vec[i+1]),]$co)))
  }
  #print(month.average)
  return(month.average)
}

##################################################################################
# 예측값 월별 평균!!!! (arima)
##################################################################################
mon.avg.arima <- function(k) {
  f.pred <- forecast(k, 52) # 두번째 인자는 몇개의 주를 예측할 것이냐.
  
  day_vec <- c(0,31,60,91,121,152,182,213,244,274,305,335,366)
  a <- data.frame(f.pred)
  d <- data.frame(cbind(rownames(a), a$Point.Forecast))[1:51,] #우선 2020년만 예측하려고 나머지 제외함!
  colnames(d) <- c("day", "co")
  d$day <- 365.25 * as.numeric(substr(d$day,5,8))
  
  month.average <- NULL
  for (i in 1:(length(day_vec)-1)) {
    month.average <- c(month.average, mean(as.numeric(d[(d$day >= day_vec[i] & d$day<= day_vec[i+1]),]$co)))
  }
  #print(month.average)
  return(month.average)
}

##################################################################################
# AIC 추출 함수
##################################################################################
lst.aic <- data.frame(matrix(ncol = 1, nrow = 6))
getAIC <- function(lst) {
  a <- AIC(lst[[1]])
  a <- c(a, ((-2) * lst[[2]]$loglik + 2 * 1))
  #a <- c(a, lst[[3]]$loglik + 2 * 1)#
  a <- c(a, lst[[4]]$model$aic)
  a <- c(a, lst[[5]]$AIC)
  a <- c(a, lst[[6]]$AIC)
  a <- c(a, AIC(lst[[7]]))
  
  return (data.frame(a))
}
models <- c("exponential", "sts", "stl", "tbats", "bats", "arima")


##################################################################################
# 데이터 저장
##################################################################################
# 이거 rmd에 있던 구마다의 월별평균 가져와쑴!
library(dplyr)
library(tidyverse)

getAIRS <- function(x,var) {
  kind_ssg <<- as.character(unique(AIR$SGG))
  sgg <- AIR[AIR$SGG==kind_ssg[x],]
  result <- sgg[,c(var)]
  return(data.frame(result))
}

week <- unique(AIR$week)
down_airdata <- function(air_metric="NO2",var) {
  
  result <- data.frame()
  result <- rbind(getAIRS(1,var),result)
  for( i in 2:length(kind_ssg)){
    result <- cbind(result,getAIRS(i,var))
  }
  
  result <- cbind(week,result)
  names(result) <- c("week","강남구","강남대로","강동구","강변북로","강북구","강서구","공항대로"
                     ,"관악구","광진구","구로구","금천구","노원구","도봉구","도산대로"
                     ,"동대문구", "동작구","동작대로","마포구","서대문구","서초구","성동구"
                     ,"성북구","송파구","신촌로","양천구","영등포구","영등포로","용산구"
                     ,"은평구","정릉로","종로","종로구", "중구","중랑구","천호대로"
                     , "청계천로","한강대로","홍릉로","화랑로")
  
  result %>% write_rds(paste0("./data/air_", air_metric, "_df.rds"))
}

# 
# down_airdata("NO2",3)
# down_airdata("O3",4)
# down_airdata("CO",5)
# down_airdata("SO2",6)
# down_airdata("PM10",7)
##################################################################################
# 데이터 불러오기
##################################################################################

getData <- function(){
  
  no2 <- o3 <- co <- so2 <- pm10 <- data.frame()
  
  no2 <<- readRDS(file = "./data/air_NO2_df.rds")
  o3 <<- readRDS(file = "./data/air_O3_df.rds")
  co <<- readRDS(file = "./data/air_CO_df.rds")
  so2 <<- readRDS(file = "./data/air_SO2_df.rds")
  pm10 <<- readRDS(file = "./data/air_PM10_df.rds")
}

##################################################################################
# 월별 구 평균 : monthly.lst에 각각의 물질에 대한 월별 평균이 들어간다.
##################################################################################
 # reshape2 package 필요
monthly.average <- function(yy) {
  yy$week <- substr(yy$week,1,7)
  
  m.result <- data.frame(matrix(ncol = 1, nrow = 132))
  for (i in 1:length(kind_ssg)) {
    k <- dcast(yy, week~., value.var = c(kind_ssg[i]), fun.aggregate = mean)
    m.result <- cbind(m.result, k[,2])
  }
  m.result[,1] <- k[,1]
  names(m.result) <- names(no2)
  names(m.result)[1] <- "month"
  return (m.result)
}
getData()
monthly.lst <- list()
monthly.lst[[1]] <- monthly.average(no2)
monthly.lst[[2]] <- monthly.average(o3)
monthly.lst[[3]] <- monthly.average(co)
monthly.lst[[4]] <- monthly.average(so2)
monthly.lst[[5]] <- monthly.average(pm10)


