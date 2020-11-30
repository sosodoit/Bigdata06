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
a <- data.frame(predict(mod_lst[[2]]))
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
mon.avg.tbats()

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
########################################################################################################
#SO2
load(file="./data/analysis_SO2_sgg2.RData") #강남대로
acc.so2.sgg2 <- acc
load(file="./data/analysis_SO2_sgg5.RData") #강북구
acc.so2.sgg5 <- acc
load(file="./data/analysis_SO2_sgg16.RData") #동작구
acc.so2.sgg16 <- acc
load(file="./data/analysis_SO2_sgg36.RData") #청계천로
acc.so2.sgg36 <- acc
plot(predict(mod_lst[[3]]), xlim = c(2010, 2021), ylim = c(0, 0.012))
par(new = TRUE)
plot(test.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red")

########################################################################################################
#NO2
load(file="./data/analysis_NO2_sgg2.RData") #강남대로
acc.no2.sgg2 <- acc
load(file="./data/analysis_NO2_sgg17.RData") #동작대로
acc.no2.sgg17 <- acc
load(file="./data/analysis_NO2_sgg24.RData") #신촌로
acc.no2.sgg24 <- acc
load(file="./data/analysis_NO2_sgg38.RData") #홍릉로
acc.no2.sgg38 <- acc
########################################################################################################
#PM10
load(file="./data/analysis_PM10.RData") # 먼지모름
acc.pm10.sgg <- acc


load(file="./data/analysis_PM10_sgg10.RData") #구로구
acc.pm10.sgg10 <- acc
load(file="./data/analysis_PM10_sgg14.RData") #도산대로
acc.pm10.sgg14 <- acc
load(file="./data/analysis_PM10_sgg39.RData") #화랑로
acc.pm10.sgg39 <- acc
# stl 모형이 좋은 것으로 보임.
plot(predict(mod_lst[[4]]), xlim = c(2009, 2021), ylim = c(0, 90))
par(new = TRUE)
plot(test.pm10.ts[,2], xlim = c(2009, 2021), col = "red", ylim = c(0, 90))


########################################################################################################
#O3
load(file="./data/analysis_O3_sgg10.RData") #구로구
acc.o3.sgg10 <- acc
load(file="./data/analysis_O3_sgg16.RData") #동작구
acc.o3.sgg16 <- acc
load(file="./data/analysis_O3_sgg28.RData") #용산구
acc.o3.sgg28 <- acc
load(file="./data/analysis_O3_sgg30.RData") #정릉로
acc.o3.sgg30 <- acc
load(file="./data/analysis_O3_sgg36.RData") #청계천로
acc.o3.sgg36 <- acc

# stl 모형이 좋은 것으로 보임.
plot(predict(mod_lst[[4]]), xlim = c(2010, 2021), ylim = c(0, 0.05))
par(new = TRUE)
plot(test.o3.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.05), col = "red")



########################################################################################################
# 하나의 거대 모형을 만들까..
