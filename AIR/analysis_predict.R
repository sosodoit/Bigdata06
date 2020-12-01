#source('./AIR/packages_need.R', encoding='utf-8')
source('./AIR/sgg_separate.R', encoding='utf-8') 

#in lag.default (y, -lag) : "K" is not an integer (ets warning 오류로 보임.)

#############################################
# comments (from cow)
#############################################
# 우선 rdata를 불러서 모형선정 및 예측그림을 파악함.
# 코드가 다 같아서, 틀리면 지우기 귀찮으니 맨 위가 CO인데 그것만 확인하면 됨!!!
# 그림도 정말 내가 확인하기 위한 그림이고 ggplot을 해볼라했는데 검색하다가 시간만 가서 때려침
# 난 그냥 plot도 이쁘다 생각해 :-) ㅋㅋㅋㅋ
# 그리고 이 comment바로 밑에 있는 건 예측값 월별 평균 내는 코드얌(tbats/stl기준)
# (시간 꽤 걸린 거 같은데 지금보니 10즐도 안되는 거 실화인가ㅏㅏㅏㅏㅏ )
# 써놨다시피 2023년?까지도 예측되어있는데, 우선 2020년에 맞춰놨옹
# 나중에 미래 이렇게 될 것이다 - 하려면 [1:51, ] 이거만 52~로 맞춰주면 됨!!
#############################################



#############################################
# 예측값 월별 평균!!!! (5: tbats, 4: stl 3 : neural)
#############################################
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
#############################################
#arima (위는 그림그리는 방법 / 아래는 월별 평균!)
#mod_lst[[7]]$arma # arima는 8주밖에 예측을 안하네..
#plot(predict(mod_lst[[7]]$model))

# arima는 predict가 아니라 forecast쓰는 것 같음!
library(forecast)
f.pred <- forecast(mod_lst[[7]], 52) 
plot(f.pred, xlim = c(2010, 2021), ylim = c(0, 0.01))
par (new = TRUE)
plot(test.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0,0.01), col = "red")

# $fitted : train 적합값
# $mean : 예측값으로 보임..
mon.avg.arima <- function() {
  
  f.pred <- forecast(mod_lst[[7]], 52) # 두번째 인자는 몇개의 주를 예측할 것이냐.
  
  day_vec <- c(0,31,60,91,121,152,182,213,244,274,305,335,366)
  a <- data.frame(f.pred)
  d <- data.frame(cbind(rownames(a), a$Point.Forecast))[1:51,] #우선 2020년만 예측하려고 나머지 제외함!
  colnames(d) <- c("day", "co")
  d$day <- 365.25 * as.numeric(substr(d$day,5,8))
  
  month.average <- NULL
  for (i in 1:(length(day_vec)-1)) {
    month.average <- c(month.average, mean(as.numeric(d[(d$day >= day_vec[i] & d$day<= day_vec[i+1]),]$co)))
  }
  print(month.average)
  
}
#############################################

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

down_airdata("NO2",3)
down_airdata("O3",4)
down_airdata("CO",5)
down_airdata("SO2",6)
down_airdata("PM10",7)

no2 <- readRDS(file = "./data/air_NO2_df.rds")
o3 <- readRDS(file = "./data/air_O3_df.rds")
co <- readRDS(file = "./data/air_CO_df.rds")
so2 <- readRDS(file = "./data/air_SO2_df.rds")
pm10 <- readRDS(file = "./data/air_PM10_df.rds")

# 월별 구 평균 : m.result
library(reshape2) # reshape2 package 필요
yy <- no2
yy$week <- substr(yy$week,1,7)

m.result <- data.frame(matrix(ncol = 1, nrow = 132))
for (i in 1:length(kind_ssg)) {
  k <- dcast(yy, week~., value.var = c(kind_ssg[i]), fun.aggregate = mean)
  m.result <- cbind(m.result, k[,2])
}
m.result[,1] <- k[,1]
names(m.result) <- names(no2)
names(m.result)[1] <- "month"

#m.result에 구마다의 실제값이 들어있음. 
#ms.a가 예측값. 


ms.a <- data.frame(matrix(NA, ncol = 1, nrow = 11))
m.b <- data.frame(m.a[-12])
for (i in 1:39) {
  ms.a <- cbind(ms.a, m.b)
}
ms.a <- ms.a[,-1]
m.res <- (m.result[m.result$month>='2020-01',-1])-ms.a 


res.vec <- NULL
for (i in 1:39) {
  res.vec <- c(res.vec, m.res[,i])
}
plot(density(res.vec)) #분포를 그려봄 (구 상관없이 그려본것.)
#(구마다의 차이에 의미를 두고 그려야 하나??)

# 여기에 드디어 co가 있소이다아ㅏ
########################################################################################################
#CO
co.plot <- function(k) { #그림
  plot(predict(k), xlim = c(2010, 2021), ylim = c(0,1.1))
  par(new = TRUE)
  plot(test.co.ts[,2], xlim = c(2010,2021), ylim = c(0,1.1), col = "red")
  par(new = FALSE)
}

par(mfrow = c(2,2)) # 그림 한 번에 슉 보도록!

load(file="./data/analysis_CO_sgg10.RData") #구로구
acc.co.sgg10 <- acc
co.plot(mod_lst[[5]])
m.a <- mon.avg.tbats()

load(file="./data/analysis_CO_sgg14.RData") #도산대로
acc.co.sgg14 <- acc
co.plot(mod_lst[[5]])

load(file="./data/analysis_CO_sgg20.RData") #서초구
acc.co.sgg20 <- acc
co.plot(mod_lst[[4]])

load(file="./data/analysis_CO_sgg39.RData") #화랑로
acc.co.sgg39 <- acc
co.plot(mod_lst[[4]])

# mod_tbats와 mod_stl이 거의 비등하게 좋은 성능을 가지는 것으로 보임.
# mod_tbats
# mod_stl

# 대로와 구를 볼 때, 대로 CO가 높은 면을 보였었음.
# 구와 대로 모두 감소하는 추세가 있는 것으로 보임
# 이거 유의한 차이인지를 어떻게 확인하지? 
# 최적의 시계열 모형을 각각 적어주어야 하는 것??
########################################################################################################
#SO2
so2.plot <- function(k) {
plot(predict(mod_lst[[k]]), xlim = c(2010, 2021), ylim = c(0, 0.012))
par(new = TRUE)
plot(test.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red")
}
so2.plot2 <- function(k) {
  plot(predict(forecast(mod_lst[[k]])), xlim = c(2010, 2021), ylim = c(0, 0.012))
  par(new = TRUE)
  plot(test.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red")
}

par(mfrow = c(2,2))

load(file="./data/analysis_SO2_sgg2.RData") #강남대로
acc.so2.sgg2 <- acc
#sts(2)
so2.plot2(2)

load(file="./data/analysis_SO2_sgg5.RData") #강북구
acc.so2.sgg5 <- acc
#tbats(5)
so2.plot(5)

load(file="./data/analysis_SO2_sgg16.RData") #동작구
acc.so2.sgg16 <- acc
#neural(3)
so2.plot(3)

load(file="./data/analysis_SO2_sgg36.RData") #청계천로
acc.so2.sgg36 <- acc
#neural
so2.plot(3)

# 제대로 예측하는 것 같지 않음.. 대체로 일직선으로 예측하는 경향
# 예측한 것 보다 실제가 더 낮음..
# 강북구는 2016년부터 감소가 진행되었던 상황인데, 더욱 줄어든 것으로 보임

# 아 혹시, 2016년에 SO2 관련한 정책이 있나?? 모든 구에서 한번에 떨어지는 것으로 보임. 확인v
########################################################################################################
#NO2
no2.plot <- function(k) {
  plot(predict(mod_lst[[k]]), xlim = c(2010, 2021), ylim = c(0, 0.08))
  par(new = TRUE)
  plot(test.no2.ts[,2], xlim = c(2010, 2021),  ylim = c(0, 0.08), col = "red")
}
no2.plot2 <- function(k) {
  plot(forecast(mod_lst[[k]]), xlim = c(2010, 2021),  ylim = c(0, 0.08))
  par(new = TRUE)
  plot(test.no2.ts[,2], xlim = c(2010, 2021),  ylim = c(0, 0.08), col = "red")
}
no2.plot3 <- function(k) { #### arima는 분리해서 못그리나...
  plot(mod_lst[[k]]$fitted, xlim = c(2010, 2021),  ylim = c(0, 0.08))
  par(new = TRUE)
  plot(forecast(mod_lst[[k]]), xlim = c(2010, 2021),  ylim = c(0, 0.08))
  plot(test.no2.ts[,2], xlim = c(2010, 2021),  ylim = c(0, 0.08), col = "red")
}

par(mfrow = c(2,2))

load(file="./data/analysis_NO2_sgg2.RData") #강남대로
acc.no2.sgg2 <- acc
# sts
no2.plot2(2)

load(file="./data/analysis_NO2_sgg17.RData") #동작대로
acc.no2.sgg17 <- acc
# tbats
no2.plot(5)
m.a <- mon.avg.tbats()

load(file="./data/analysis_NO2_sgg24.RData") #신촌로
acc.no2.sgg24 <- acc
# neural
no2.plot(3)

load(file="./data/analysis_NO2_sgg38.RData") #홍릉로
acc.no2.sgg38 <- acc
# arima
no2.plot3(7)

# 강남대로는 이상치가 많은 데이터였던 것으로 보임.
# 홍릉로는 원래 변동폭이 차이가 없는 데이터
# 예상보다 더 감소한 것으로 보임.
########################################################################################################
#PM10
load(file="./data/analysis_PM10.RData") # 먼지모름
acc.pm10.sgg <- acc

pm10.plot <- function(k) { #그림
  plot(predict(mod_lst[[k]]), xlim = c(2009, 2021), ylim = c(0, 90))
  par(new = TRUE)
  plot(test.pm10.ts[,2], xlim = c(2009, 2021), col = "red", ylim = c(0, 90))
}



load(file="./data/analysis_PM10_sgg10.RData") #구로구
acc.pm10.sgg10 <- acc
# stl
pm10.plot(4)

load(file="./data/analysis_PM10_sgg14.RData") #도산대로
acc.pm10.sgg14 <- acc
# stl
pm10.plot(4)

load(file="./data/analysis_PM10_sgg39.RData") #화랑로
acc.pm10.sgg39 <- acc
# stl
pm10.plot(4)

# 미세먼지 > 도로 구에 상관없이 비슷한 양상, 추세
# 화랑로 미세먼지 이상치 무슨일..

########################################################################################################
#O3
o3.plot <- function(k) { #그림
  plot(predict(mod_lst[[k]]), xlim = c(2010, 2021), ylim = c(0, 0.05))
  par(new = TRUE)
  plot(test.o3.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.05), col = "red")
}

load(file="./data/analysis_O3_sgg10.RData") #구로구
acc.o3.sgg10 <- acc
# stl
o3.plot(4)

load(file="./data/analysis_O3_sgg16.RData") #동작구
acc.o3.sgg16 <- acc
# stl
o3.plot(4)

load(file="./data/analysis_O3_sgg28.RData") #용산구
acc.o3.sgg28 <- acc
# stl
o3.plot(4)

load(file="./data/analysis_O3_sgg30.RData") #정릉로
acc.o3.sgg30 <- acc
# stl
o3.plot(4)

load(file="./data/analysis_O3_sgg36.RData") #청계천로
acc.o3.sgg36 <- acc
# arima

# 거의 제대로된 예측.
# 오존은 코로나와 상관없이 비슷한 추세를 보임.
# 대로와 구 모두 비슷해보임. 


########################################################################################################
# 하나의 거대 모형을 만들까..
