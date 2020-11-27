#***************************************# 
# 주제 : 
# 목차 1) 데이터 셋 만들기 
#      2) EDA
#      3) 시계열분석
#***************************************# 
#             필요패키지                # 
#***************************************# 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forecast)
# library(MASS)
# library(glmnet)
# library(randomForest)
# library(gbm)
# library(rpart)
# library(boot)
# library(data.table)
# library(ROCR)
# library(gridExtra)
#***************************************# 
#             데이터로드                #
#***************************************# 
df <- read.csv(file = './data/airseoul.csv', header = T)[,-1]
df$SGG <- factor(df$SGG) #factor화
df$week <- as.Date(df$week) #Date 타입으로 변환
#***************************************# 
#         구별 데이터 분리              #      
#***************************************# 
sgg1 = df[df$SGG==kind_ssg[1],]
sgg2 = df[df$SGG==kind_ssg[2],]
sgg3 = df[df$SGG==kind_ssg[3],]
sgg4 = df[df$SGG==kind_ssg[4],]
sgg5 = df[df$SGG==kind_ssg[5],]
sgg6 = df[df$SGG==kind_ssg[6],]
sgg7 = df[df$SGG==kind_ssg[7],]
sgg8 = df[df$SGG==kind_ssg[8],]
sgg9 = df[df$SGG==kind_ssg[9],]
sgg10 = df[df$SGG==kind_ssg[10],]

sgg11 = df[df$SGG==kind_ssg[11],]
sgg12 = df[df$SGG==kind_ssg[12],]
sgg13 = df[df$SGG==kind_ssg[13],]
sgg14 = df[df$SGG==kind_ssg[14],]
sgg15 = df[df$SGG==kind_ssg[15],]
sgg16 = df[df$SGG==kind_ssg[16],]
sgg17 = df[df$SGG==kind_ssg[17],]
sgg18 = df[df$SGG==kind_ssg[18],]
sgg19 = df[df$SGG==kind_ssg[19],]
sgg20 = df[df$SGG==kind_ssg[20],]

sgg21 = df[df$SGG==kind_ssg[21],]
sgg22 = df[df$SGG==kind_ssg[22],]
sgg23 = df[df$SGG==kind_ssg[23],]
sgg24 = df[df$SGG==kind_ssg[24],]
sgg25 = df[df$SGG==kind_ssg[25],]
sgg26 = df[df$SGG==kind_ssg[26],]
sgg27 = df[df$SGG==kind_ssg[27],]
sgg28 = df[df$SGG==kind_ssg[28],]
sgg29 = df[df$SGG==kind_ssg[29],]
sgg30 = df[df$SGG==kind_ssg[30],]

sgg30 = df[df$SGG==kind_ssg[30],]
sgg31 = df[df$SGG==kind_ssg[31],]
sgg32 = df[df$SGG==kind_ssg[32],]
sgg33 = df[df$SGG==kind_ssg[33],]
sgg34 = df[df$SGG==kind_ssg[34],]
sgg35 = df[df$SGG==kind_ssg[35],]
sgg36 = df[df$SGG==kind_ssg[36],]
sgg37 = df[df$SGG==kind_ssg[37],]
sgg38 = df[df$SGG==kind_ssg[38],]
sgg39 = df[df$SGG==kind_ssg[39],]
#***************************************# 
#           기본 시계열 분해            #
#***************************************# 
#***************************************# 
#     forecast:: 계절변동 시각화        #
#***************************************# 
# x1 <- df %>% group_by(SGG) %>% select(NO2)
# #x1 <- sgg1 %>% select(NO2) 
# df_ts <- ts(x1, start = 2009, end = 2019, freq=52)
# df_ts_test <- ts(x1, start=2020, end = c(2020, 11), freq=52)
# seasonplot(df_ts, xlab="", main="",
#            year.labels=TRUE, year.labels.left=TRUE, col=1:12, pch=20)
#***************************************# 
#           전체 시계열 그림            #
#   : 분석 전, 구마다 시계열 그림       #
#***************************************# 
par(mfrow = c(4,2))
kind_ssg <-unique(df$SGG)
for (i in kind_ssg) {
  plot(df[df$SGG == i,]$NO2, type = "l")
}
#***************************************# 
p=ggplot(df, aes(x=week,y=NO2))+
  geom_line(mapping=aes(x=week,y=NO2,color=NO2))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::NO2 현황")+
   ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(df, aes(x=week,y=O3))+
  geom_line(mapping=aes(x=week,y=O3,color=O3))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::O3 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(df, aes(x=week,y=CO))+
  geom_line(mapping=aes(x=week,y=CO,color=CO))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::CO 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(df, aes(x=week,y=SO2))+
  geom_line(mapping=aes(x=week,y=SO2,color=SO2))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::SO2 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(df, aes(x=week,y=PM10))+
  geom_line(mapping=aes(x=week,y=PM10,color=PM10))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::PM10 현황")+
  ggthemes::theme_hc()

#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")
#***************************************#
#         원시계열 탐색 끝              #
#***************************************#
sgg1 == 강남구
#write.table(gangnam, file = "gangnam.txt", row.names = F, quote = F) #sas에서 쓸 txt파일 생성
k <- ts(sgg1$NO2, start = c(2009,12), end = c(2020, 11), freq = 52)
diff.k <- diff(k, lag = 12) #차분

par(mfrow = c(1,2))
plot(sgg1$NO2, type="l", main="sgg1::NO2 2010년 ~ 2020년", axes=F)
axis(2,las=2)
plot(diff.k, main="sgg1::NO2 차분", axes=F)
axis(2,las=2); axis(1,las=1)

#시계열 분해
stl.run <- stl(k, s.window = "periodic")
plot(stl.run)

############# 이거 전처리 할 때 써놓은 코드인데, 없는 날짜 볼 때 유용했던 거라.. 남겨둠!
# https://lightblog.tistory.com/47 #숫자를 날자로 바꾸기
# original <- seq(as.Date("2009-12-28"), as.Date("2020-11-02"), by = "7 days")
# as.Date(setdiff(original, unique(df$week)), origin = "1970-01-01") #없는 날짜.
# as.Date(setdiff(original, unique(df[df$SGG=="강남구",]$week)), origin = "1970-01-01")
#***************************************# 
#                 분석                  #
#           강남구 - NO2                #
#***************************************# 
sgg1.tr = sgg1 %>% filter(week < '2020-01-06')
sgg1.te = sgg1 %>% filter(week >= '2020-01-06')
ts.train <- ts(sgg1.tr, start = c(2009,12,28), frequency = 52)
ts.test <- ts(sgg1.tr, start = c(2020,1,6), frequency = 52)
plot(ts.train[,-c(1,2)])
#***************************************#
f = function(sgg){
  ts1 <- ts(sgg, start = c(2009,12,28), end = c(2020,1,6), freq=52)
  ts1_test <- ts(sgg, start=c(2020,1,6),end = c(2020,11,25), freq=52)
  ts1 <- ts1[,-c(1,2)]; ts1_test <- ts1_test[,-c(1,2)]
  plot(ts1, type="l", main="2010년 ~ 2019년")
  plot(ts1_test, type="o", main="2020년")
}
f(sgg1); #...# ; f(sgg39)
#***************************************#
#NO2 - 동서남북 그려서 얼마나 다른지 알아봄
#***************************************#
f = function(sgg, y){
  ts1 <- ts(sgg, start = c(2009,12,28), end = c(2020,1,6), freq=52)
  ts1_test <- ts(sgg, start=c(2020,1,6),end = c(2020,11,25), freq=52)
  plot(ts1, type="l", main="2010년 ~ 2019년",ylim=y)
  plot(ts1_test, type="o", main="2020년",ylim=y)
}
f(sgg1$NO2,c(0,0.08)); #...# ; f(sgg39)
#***************************************#
f = function(sgg, y, i){
  ts1 <- ts(sgg, start = c(2009,12,28), freq=52)
  plot(ts1,ylim=y, col=i,lwd=1.5)
  par(new = TRUE)
}
f(sgg3$NO2,c(0,0.08),3); #동
f(sgg6$NO2,c(0,0.08),6); #서
f(sgg1$NO2,c(0,0.08),1); #남
f(sgg5$NO2,c(0,0.08),5); #북
# legend
# 강북이 대기 오염 농도가 대체적으로 낮은 걸 볼 수 있음. (추워서 바람부나?)
#***************************************#
library(ggpmisc)
f = function(sgg){
  ts1 <- ts(sgg, start = c(2009,12,28), freq=52)
  ggplot(ts1, as.numeric = FALSE) + 
    geom_line() + 
    stat_peaks(colour = "red") + #극대점 빨간점으로 표시
    stat_peaks(geom = "text", colour = "red", #극대점 날짜("%Y-%m") 표시 
               vjust = -0.5, x.label.fmt = "%Y-%m") +
    stat_valleys(colour = "blue") +  #극소점 파란점으로 표시
    stat_valleys(geom = "text", colour = "blue", angle = 45, 
                 vjust = 1.5, hjust = 1,  x.label.fmt = "%Y-%m") #극소점 날짜("%Y-%m") 표시 
}
f(sgg3$NO2); #동
f(sgg6$NO2); #서
f(sgg1$NO2); #남
f(sgg5$NO2); #북
# legend
#***************************************#
# 대로
f = function(sgg, y, i){
  ts1 <- ts(sgg, start = c(2009,12,28), freq=52)
  plot(ts1,ylim=y, col=i,lwd=1.5)
  par(new = TRUE)
}
f(sgg2$NO2,c(0,0.08),3); 
f(sgg4$NO2,c(0,0.08),6); 
f(sgg7$NO2,c(0,0.08),1); 
f(sgg14$NO2,c(0,0.08),14); 
f(sgg17$NO2,c(0,0.08),17); 
f(sgg27$NO2,c(0,0.08),27); 
f(sgg30$NO2,c(0,0.08),30); 
f(sgg35$NO2,c(0,0.08),35); 
f(sgg36$NO2,c(0,0.08),36); 
f(sgg37$NO2,c(0,0.08),37); 
f(sgg38$NO2,c(0,0.08),38); 
f(sgg39$NO2,c(0,0.08),39); 

# 3개를 보면, 한강대로는 대기오염 농도가 강한 것을 볼 수 있음
# 청릉로는 대기오염 농도가 다른 곳에 비해 약함.
# 대로마다 다른 것을 볼 수 있음.
# legend
#***************************************#ㅋㅋ
tr = 10:19; te = 19:20
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,25),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(2.5,0.5,22,0.5,0.05)
points(tr, train*0+0.5, pch=19, col="blue")
points(te,  test*0+0.5,  pch=19, col="red")
text(24,0.5,"Year")
text(10,0.6,"Train data",col="blue")
text(20,0.6,"Test data",col="red")
#***************************************#
# 평균적인 시계열(train dataset)
#***************************************#
train = df[df$week < '2020-01-06',]
test = df[df$week >= '2020-01-06',]
#***************************************#
c <- train %>% group_by(week) %>% summarise(mean(NO2))
tts <- ts(c$`mean(NO2)`, start = c(2009,12,28), frequency = 52)
plot(tts, ylim = c(0,0.07))
#***************************************#
f = function(sgg){
  ts1 <- ts(sgg, start = c(2009,12,28), freq=52)
  ggplot(ts1, as.numeric = FALSE) + 
    geom_line() + 
    stat_peaks(colour = "red") + #극대점 빨간점으로 표시
    stat_valleys(colour = "blue")+#극소점 파란점으로 표시
    ggthemes::theme_hc()
}

f(c$`mean(NO2)`)
#***************************************#
#***************************************# <== 여기까지;ㅁ;
#***************************************#
# 시계열 분해와 변동 요인 제거 + 그래프
m <- decompose(ts.train.no2)
attributes(m)
plot(m)
plot(ts.train.no2 - m$seasonal) # 계절성 제거된 그래프
par (new = TRUE)
plot(ts.train.no2 - m$trend, col = "blue") # 추세요인 제거 그래프
plot(ts.train.no2 - m$seasonal - m$trend) # 불규칙 요인만 출력

#***************************************#
# 자기상관함수 시각화
acf(ts.train.no2, main = "자기상관함수", col = "red")
pacf(ts.train.no2, main = "부분자기상관함수", col = "blue")
# 파란색 선 안에 들어오면 자기상관성이 없음. 
# 시계열 데이터 시간의 의존성 여부가 무작위성을 띄느냐를 판단 가능.

#***************************************#
# 차분 시각화 
plot(diff(ts.train.no2, differences = 1)) #영어 해석을 잘 모르겠는데 1차 차분같..

# 이동 평균은 시계열 자료를 대상으로 일정한 기간의 자료를 평균으로
# 계산하고, 이동시킨 추세를 파악하여 추세를 예측하는 분석 기법
# 지수평활법은 가중치!


diff = diff(ts.train.no2)
library(forecast)
auto.arima(ts.train.no2)
model <- arima(tsdata, order = c(0,1,1), seasonal = list(order = c(0,0,2)))
model

#***************************************#
# 모형 진단(모형 타당성 검정)
# -1) 자기상관함수에 의한 모형 진단
tsdiag(model)
# -2) Box-Ljung에 의한 잔차형 모형 진단
Box.test(model$residuals, lag = 1, type = "Ljung")
# 모형의 잔차를 이용하여 카이제곱검정 방법으로 시계열 모형이 통계적으로 적절한지 검정
# p-value가 0.05이상이면 모형이 통계적으로 적절

#***************************************#
# 미래 예측
forecast(model) #향후 2년 예측

par(mfrow = c(1,2))
plot(forecast(model),ylim = c(0,0.08))

pre <- forecast(model, h = 12)
plot(pre, ylim = c(0,0.08), xlim = c(2009,2022))

par(new = TRUE)
plot(ts.test.no2, xlim = c(2010,2022),ylim = c(0,0.08), col = "red")

#***************************************#
# 정상성 시계열의 계절형
ts_feature <- stl(tsdata, s.window = "periodic")
plot(ts_feature)



##############################################
##############################################
# 여기서부터 심각
##############################################
##############################################

# 예측정확도
#https://otexts.com/fppkr/accuracy.html

library(ggplot2)
df.gangnam = train[train$SGG == '강남구',][,3]
df.gangnam = ts(df.gangnam)
fit <- window(df.gangnam, deltat = 7, extend = TRUE)
fit1 <- meanf(fit)
fit2 <- rwf(fit)
fit3 <- snaive(fit) ################# 왜 오류가 나는지 몰겠음..
autoplot(window(df.gangnam)) +
  autolayer(fit1, series = "평균", PI = FALSE) +
  autolayer(fit2, series = "단순", PI = FALSE) +
  guides(colour = guide_legend(title = "예측")) +
  xlab("연도") + ylab("NO2")
#autolayer(fit3, series = "계절성 단순", PI = FALSE) +

fit11 <- window(df.gangnam)
accuracy(fit1, fit11)

#ts(rnorm(52), start = c(2014+9/365.25), frequency=365.25/7)

##############################################
df <- read.csv(file = 'C:\\Users\\hanso\\Desktop\\빅통분\\airseoul.csv', header = T)[,-1]
df$SGG <- factor(df$SGG)
df$week <- as.Date(df$week)

train = df[df$week < '2020-01-06',]
test = df[df$week >= '2020-01-06',]


train <- window(train[,3], start=2009, end = c(2019,12))
test.no2 <- ts(test[,3],frequency = 52, start = c(2020,1,2))
test <- window(test.no2, start = 2020, deltat = 7)

par(mfrow = c(2,1))
par(mar = c(2,3,0,2), xaxs = 'i', yaxs = 'i')

plot(train, type="c", pch =20, xaxt='n', xlab="")
text(train, col=1:12, labels=1:12, cex=.7)

plot(train, type = "o", pch = 20, xlab = "")

# 기본 시계열 분해 -> 오류
d.train <- decompose(train, type = "multiplicative") #왜..오류..
d.train <- decompose(train)
plot()

# forecast package 계절변동 시각화
seasonplot(ts.train.no2,
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(ts.train.no2)
axis(1, at=1:12, labels=month.abb, cex=0.8)

# 모형선정
models <- list (
  mod_arima = auto.arima(train, ic='aicc', stepwise=FALSE),
  mod_exponential = ets(train, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(train, p=12, size=25),
  mod_tbats = tbats(train, ic='aicc', seasonal.periods=12),
  mod_bats = bats(train, ic='aicc', seasonal.periods=12),
  #mod_stl = stlm(train, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(train)
  #gem추가예정
  #stl은 왜 오류나는지 모르겠음.
)

forecasts <- lapply(models, forecast, 12)
forecasts$naive <- naive(train, 12)

par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n", ylim = c(0,0.15))
  lines(test.no2, col='red')
}


##########################################################
# https://kkokkilkon.tistory.com/49
# 구 마다 5개씩 오류들을 그려보고 싶었음..
library(reshape2) 
train.gangnam <- train[train$SGG=="강남구",]
melt_data <- melt(train.gangnam, id.vars = c("NO2", "O3","CO","SO2","PM10"))

g <- ggplot(melt_data) + geom_line(aes(x = seq, y = value, colour = variable), cex = 0.8, show.legend = T)
g


##########################################################
##########################################################
##########################################################
# 앞으로의 예정
# 195개에 대해서 각자의 베스트 모형 선택
# (근데 하나만 정해서 같은 거 넣는 건 아닌가..?)
# (원래 내 생각은 NO2에서 가장 잘 맞는 모형을 하나만 고르는 거엿는데..)
# 베스트 모형에 대해서 예측. 2020년의 3월 실제와 예측의 차이를 구해서, 분포 그려보기
# 우선..
