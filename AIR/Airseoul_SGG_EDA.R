# 연구목적
# 
# **미국 각 지역의 코로나19 사망률과 대기오염과의 상관관계를 들여다 본 하버드대 
# 보건대학원 연구진들은 장기간에 걸쳐 대기 오염이 심했던 지역일수록 코로나19 사망률이 
# 훨씬 높다는 분석 결과를 발표했습니다. 
# 사망의 주원인인 기저질환 대부분이 나쁜 공기와 밀접한 연관이 있다는 것입니다.**

# 데이터 분석
library(xts)
library(forecast)
library(tseries)

# 데이터 조작
library(dplyr)
library(ggplot2)

########### 전처리 완료 데이터 로드
airseoul <- read.csv("./data/airseoul.csv", stringsAsFactors = TRUE)[,-1]
airseoul$week <- as.Date(as.character(airseoul$week), format="%Y-%m-%d")

########### 측정소별 분석 데이터 준비
sgg <- unique(airseoul$SGG)
main = c("강남구", "강남대로", "강동구",  "강변북로", "강북구",  "강서구", "공항대로", "관악구", "광진구", "구로구", "금천구", "노원구", "도봉구", "도산대로", "동대문구", "동작구", "동작대로", "마포구", "서대문구", "서초구", "성동구", "성북구", "송파구",  "신촌로",  "양천구", "영등포구", "영등포로", "용산구", "은평구", "정릉로", "종로",  "종로구",  "중구", "중랑구", "천호대로", "청계천로", "한강대로", "홍릉로",  "화랑로")

sggi <- list()
for(i in 1:length(sgg)){
  
  sggi[[i]] <- airseoul %>% filter(SGG==sgg[i])
}

df <- x <- list()
for(i in 1:length(sgg)){
  
  # 측정소별 물질5개 들어감
  x[[i]] <- sggi[[i]][3:7]; dt <- sggi[[1]]$week
  
  for(j in 1:5){
    
    df[[i]] <- xts(x[[i]], dt) # x = {no2, o3, co, so2, pm10}
  }
}

########### 정상성 여부 확인 no ====> 변환, 차분
########### using 시계열 그림 + SACF 
library(xts)
xts.plot <- function(i,j) {
  if(j==1) plot.xts(df[[i]][,j], ylim=c(0,0.08), main=main[i], major.ticks='months', minor.ticks=FALSE, col=i)
  else if(j==2) plot.xts(df[[i]][,j], ylim=c(0,0.08), main=main[i], major.ticks='months', minor.ticks=FALSE, col=i)
  else if(j==3) plot.xts(df[[i]][,j], ylim=c(0,1.2), main=main[i], major.ticks='months', minor.ticks=FALSE, col=i)
  else if(j==4) plot.xts(df[[i]][,j], ylim=c(0.0002,0.01), main=main[i], major.ticks='months', minor.ticks=FALSE, col=i)
  else if(j==5) plot.xts(df[[i]][,j], ylim=c(0,90), main=main[i], major.ticks='months', minor.ticks=FALSE, col=i)
}

############################################
########## i=1~39 측정소선택!! #############
############################################
########## 강남구 5개 대기 오염 물질
par(mfrow=c(5,5))
i=39
xts.plot(i,1)
xts.plot(i,2)
xts.plot(i,3) 
xts.plot(i,4)
xts.plot(i,5)





########### 정상성 여부 확인 no ====> 변환, 차분
########### using 시계열 그림 + SACF 
library(xts)
xts.plot <- function(i,j) {
  plot.xts(df[[i]][,j], main=main[i], major.ticks='months', minor.ticks=FALSE, col=i)
}

############################################
########## i=1~39 측정소선택!! #############
############################################
i=2
########### 강남구 5개 대기 오염 물질
par(mfrow=c(2,3))

xts.plot(i,1) # SACF X
xts.plot(i,2) # SACF X
xts.plot(i,3) # SACF X
xts.plot(i,4) # SACF X 
xts.plot(i,5) # SACF X

acf(df[[i]], type="correlation", plot=TRUE)

###########
########### NO2
library(astsa) # acf2 & sarima
acf2(df[[i]][,1], main="NO2") # max.lag=24 차 까지
dif_1 = diff(df[[i]][,1], lag=1)
dif_12 = diff(df[[i]][,1], lag=12)
dif_112 = diff(dif_1, lag=12)

########### 한 번에
plot.xts(cbind(df[[i]][,1], dif_1, dif_12, dif_112), main=main[i])
plot.ts(cbind(ts(df[[i]][,1]), ts(dif_1), ts(dif_12), ts(dif_112)), main=main[i])
########### 나눠서
par(mfrow=c(2,2))
plot.xts(df[[i]][,1], main=main[i]) # ,type='o'
plot.xts(dif_1)  #
plot.xts(dif_12)
plot.xts(dif_112)

########### 차수 결정
library(forecast)
ndiffs(df[[i]][,1]) #몇 번 차분(difference)해야 stationary하게 되는지 검사
acf2(dif_1, main="NO2")

fitted <- auto.arima(dif_1); fitted
forecasted <- forecast(fitted,21) # 21개의 데이터를 예측하여 forecasted를 생성합니다.
plot(forecasted)

#write.csv(forecasted,file="forecasted.csv",row.names=FALSE)

###########
########### O3
acf2(df[[i]][,2], main="O3")

dif_1 = diff(df[[i]][,2], lag=1)
dif_12 = diff(df[[i]][,2], lag=12)
dif_112 = diff(dif_1, lag=12)

par(mfrow=c(2,2))
plot.xts(df[[i]][,2], main=main[i])
plot.xts(dif_1) #
plot.xts(dif_12)
plot.xts(dif_112)

########### 차수 결정
ndiffs(df[[i]][,2]) #몇 번 차분(difference)해야 stationary하게 되는지 검사
acf2(dif_1, main="강남대로 O3")

fitted <- auto.arima(dif_1); fitted
forecasted <- forecast(fitted,21) # 21개의 데이터를 예측하여 forecasted를 생성합니다.
plot(forecasted)

#write.csv(forecasted,file="forecasted.csv",row.names=FALSE)

###########
########### CO
acf2(df[[i]][,3], main="CO")

dif_1 = diff(df[[i]][,3], lag=1)
dif_12 = diff(df[[i]][,3], lag=12)
dif_112 = diff(dif_1, lag=12)

par(mfrow=c(2,2))
plot.xts(df[[i]][,3], main=main[i])
plot.xts(dif_1) #
plot.xts(dif_12)
plot.xts(dif_112)

########### 차수 결정
ndiffs(df[[i]][,3]) #몇 번 차분(difference)해야 stationary하게 되는지 검사
acf2(dif_1, main="CO")

fitted <- auto.arima(dif_1); fitted
forecasted <- forecast(fitted,21) # 21개의 데이터를 예측하여 forecasted를 생성합니다.
plot(forecasted)

#write.csv(forecasted,file="forecasted.csv",row.names=FALSE)

###########
########### SO2
acf2(df[[i]][,4], main="SO2")

dif_1 = diff(df[[i]][,4], lag=1)
dif_12 = diff(df[[i]][,4], lag=12)
dif_112 = diff(dif_1, lag=12)

par(mfrow=c(2,2))
plot.xts(df[[i]][,4], main=main[i])
plot.xts(dif_1) #
plot.xts(dif_12)
plot.xts(dif_112)

########### 차수 결정
ndiffs(df[[i]][,4]) #몇 번 차분(difference)해야 stationary하게 되는지 검사
acf2(dif_1, main="SO2")

fitted <- auto.arima(dif_1); fitted
forecasted <- forecast(fitted,21) # 21개의 데이터를 예측하여 forecasted를 생성합니다.
plot(forecasted)

#write.csv(forecasted,file="forecasted.csv",row.names=FALSE)

###########
########### PM10
acf2(df[[i]][,5], main="PM10")

dif_1 = diff(df[[i]][,5], lag=1)
dif_12 = diff(df[[i]][,5], lag=12)
dif_112 = diff(dif_1, lag=12)

par(mfrow=c(2,2))
plot.xts(df[[i]][,5], main=main[i])
plot.xts(dif_1) #
plot.xts(dif_12)
plot.xts(dif_112)

########### 차수 결정
ndiffs(df[[i]][,5]) #몇 번 차분(difference)해야 stationary하게 되는지 검사
acf2(dif_1, main="PM10")

fitted <- auto.arima(dif_1); fitted
forecasted <- forecast(fitted,21) # 21개의 데이터를 예측하여 forecasted를 생성합니다.
plot(forecasted)

#write.csv(forecasted,file="forecasted.csv",row.names=FALSE)

###########
