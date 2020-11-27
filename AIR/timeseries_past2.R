#***************************************# 
#             필요패키지                # 
#***************************************# 
library(tidyverse)
library(lubridate)
library(xts)
library(forecast)

library(dplyr)
library(ggplot2) #정적시계열
library(dygraphs) #동적시계열 시각화할때 주로쓴데

#install.packages("DT")
#library(DT)
#***************************************# 
#             데이터로드                #
#***************************************# 
# AIRSEOUL <== 대기분석 이거 쓸
AIR <- read.csv('./data/airseoul.csv', header = T)[,-1]
AIR$SGG <- factor(AIR$SGG)
AIR$week <- as.Date(AIR$week)

# COVID19
# C19 = read.csv("./data/C19/C19seoul.csv")[,-1]

# MERGE <== c19+대기20년도 분석 이걸 쓸
# C19AIR20_day = read.csv("./data/C19/C19AIR20_day.csv")
# C19AIR20_week = read.csv("./data/C19/C19AIR20_week.csv")
#***************************************# 
#         구별 데이터 분리              #      
#***************************************# 
kind_ssg <-as.character(unique(AIR$SGG)); 
df <- AIR
tr = df[df$week < '2020-01-06',]
te = df[df$week >= '2020-01-06',]
#***************************************#
sgg1 = tr[tr$SGG==kind_ssg[1],]
sgg2 = tr[tr$SGG==kind_ssg[2],]
sgg3 = tr[tr$SGG==kind_ssg[3],]
sgg4 = tr[tr$SGG==kind_ssg[4],]
sgg5 = tr[tr$SGG==kind_ssg[5],]
sgg6 = tr[tr$SGG==kind_ssg[6],]
sgg7 = tr[tr$SGG==kind_ssg[7],]
sgg8 = tr[tr$SGG==kind_ssg[8],]
sgg9 = tr[tr$SGG==kind_ssg[9],]
sgg10 = tr[tr$SGG==kind_ssg[10],]

sgg11 = tr[tr$SGG==kind_ssg[11],]
sgg12 = tr[tr$SGG==kind_ssg[12],]
sgg13 = tr[tr$SGG==kind_ssg[13],]
sgg14 = tr[tr$SGG==kind_ssg[14],]
sgg15 = tr[tr$SGG==kind_ssg[15],]
sgg16 = tr[tr$SGG==kind_ssg[16],]
sgg17 = tr[tr$SGG==kind_ssg[17],]
sgg18 = tr[tr$SGG==kind_ssg[18],]
sgg19 = tr[tr$SGG==kind_ssg[19],]
sgg20 = tr[tr$SGG==kind_ssg[20],]

sgg21 = tr[tr$SGG==kind_ssg[21],]
sgg22 = tr[tr$SGG==kind_ssg[22],]
sgg23 = tr[tr$SGG==kind_ssg[23],]
sgg24 = tr[tr$SGG==kind_ssg[24],]
sgg25 = tr[tr$SGG==kind_ssg[25],]
sgg26 = tr[tr$SGG==kind_ssg[26],]
sgg27 = tr[tr$SGG==kind_ssg[27],]
sgg28 = tr[tr$SGG==kind_ssg[28],]
sgg29 = tr[tr$SGG==kind_ssg[29],]
sgg30 = tr[tr$SGG==kind_ssg[30],]

sgg30 = tr[tr$SGG==kind_ssg[30],]
sgg31 = tr[tr$SGG==kind_ssg[31],]
sgg32 = tr[tr$SGG==kind_ssg[32],]
sgg33 = tr[tr$SGG==kind_ssg[33],]
sgg34 = tr[tr$SGG==kind_ssg[34],]
sgg35 = tr[tr$SGG==kind_ssg[35],]
sgg36 = tr[tr$SGG==kind_ssg[36],]
sgg37 = tr[tr$SGG==kind_ssg[37],]
sgg38 = tr[tr$SGG==kind_ssg[38],]
sgg39 = tr[tr$SGG==kind_ssg[39],]
#***************************************#
sgg1.te = te[te$SGG==kind_ssg[1],]
sgg2.te = te[te$SGG==kind_ssg[2],]
sgg3.te = te[te$SGG==kind_ssg[3],]
sgg4.te = te[te$SGG==kind_ssg[4],]
sgg5.te = te[te$SGG==kind_ssg[5],]
sgg6.te = te[te$SGG==kind_ssg[6],]
sgg7.te = te[te$SGG==kind_ssg[7],]
sgg8.te = te[te$SGG==kind_ssg[8],]
sgg9.te = te[te$SGG==kind_ssg[9],]
sgg10.te = te[te$SGG==kind_ssg[10],]

sgg11 = te[te$SGG==kind_ssg[11],]
sgg12 = te[te$SGG==kind_ssg[12],]
sgg13 = te[te$SGG==kind_ssg[13],]
sgg14 = te[te$SGG==kind_ssg[14],]
sgg15 = te[te$SGG==kind_ssg[15],]
sgg16 = te[te$SGG==kind_ssg[16],]
sgg17 = te[te$SGG==kind_ssg[17],]
sgg18 = te[te$SGG==kind_ssg[18],]
sgg19 = te[te$SGG==kind_ssg[19],]
sgg20 = te[te$SGG==kind_ssg[20],]

sgg21 = te[te$SGG==kind_ssg[21],]
sgg22 = te[te$SGG==kind_ssg[22],]
sgg23 = te[te$SGG==kind_ssg[23],]
sgg24 = te[te$SGG==kind_ssg[24],]
sgg25 = te[te$SGG==kind_ssg[25],]
sgg26 = te[te$SGG==kind_ssg[26],]
sgg27 = te[te$SGG==kind_ssg[27],]
sgg28 = te[te$SGG==kind_ssg[28],]
sgg29 = te[te$SGG==kind_ssg[29],]
sgg30 = te[te$SGG==kind_ssg[30],]

sgg30 = te[te$SGG==kind_ssg[30],]
sgg31 = te[te$SGG==kind_ssg[31],]
sgg32 = te[te$SGG==kind_ssg[32],]
sgg33 = te[te$SGG==kind_ssg[33],]
sgg34 = te[te$SGG==kind_ssg[34],]
sgg35 = te[te$SGG==kind_ssg[35],]
sgg36 = te[te$SGG==kind_ssg[36],]
sgg37 = te[te$SGG==kind_ssg[37],]
sgg38 = te[te$SGG==kind_ssg[38],]
sgg39 = te[te$SGG==kind_ssg[39],]
#***************************************# 
#     구별 대기물질별 데이터 분할       #      
#***************************************# 
s1.pm10 = sgg1[,c(2,7)]
s1.te.pm10 = sgg1.te[,c(2,7)]
s2.pm10 = sgg1[,c(2,7)]
s39.pm10 = sgg1[,c(2,7)]
#***************************************# 
#               pm10 분석               #      
#***************************************#
#           시계열 데이터 변환          #
#***************************************#
s1.pm10.xts <- xts(s1.pm10$PM10, order.by=s1.pm10$week, frequency=52)
s1.te.pm10.xts <- xts(s1.te.pm10$PM10, order.by=s1.te.pm10$week, frequency=52)
s2.pm10.xts <- xts(s2.pm10$PM10, order.by=s2.pm10$week, frequency=52)
s39.pm10.xts <- xts(s39.pm10$PM10,  order.by=s39.pm10$week, frequency=52)
#***************************************#
### pm10 예측 
s1.pm10_ts <- ts(s1.pm10.xts, start = c(2009,12,28), end = c(2020,1,6), frequency=52)
s1.te.pm10_ts <- ts(s1.te.pm10.xts, start=c(2020,1,6),end = c(2020,11,25), frequency=52)
# 시계열 데이터 시각화 및 시계열 예측 적합성 
autoplot(s1.pm10_ts)
# autoplot() 시각화를 통해 추세(Trend), 계절성(Seasonality), 순환(Cyclic)이 존재하는지 확인한다.
# ggAcf() 함수를 통해 자기상관이 존재하는지 시각적으로 확인하고, 
# Ljung-Box 검정을 통해 자기상관이 존재하는지 통계량으로 검정한다.
ggAcf(s1.pm10_ts) +
  ggtitle("s1.pm10 자기상관함수(ACF)")
Box.test(s1.pm10_ts, lag = 24, fitdf = 0, type = "Lj")
# 시계열 모형 적합
# pm10 시계열을 잘 설명하는 시계열 모형을 만들고 이를 바탕으로 예측모형을 생성한다. 벤치마크 모형으로 
# 가장 최근 시계열 관측점을 다음 시점 예측값으로 사용하는 naive 모형부터 ETS, ARIMA, TBATS 모형까지 다양한다.

# 벤치마크 시계열 예측모형
s1.pm10_ts %>% naive() %>% checkresiduals()
s1.pm10_ts %>% naive() %>% forecast() %>% autoplot()

### 1. ETS 모형
s1.pm10_ts %>% ets(lambda=BoxCox.lambda(s1.pm10_ts)) %>% checkresiduals()
s1.pm10_ts %>% ets(lambda=BoxCox.lambda(s1.pm10_ts)) %>% forecast() %>% autoplot()

### 2. ARIMA 모형
s1.pm10_ts %>% auto.arima() %>% checkresiduals()
s1.pm10_ts %>% auto.arima() %>% forecast() %>% autoplot()

### 3. TBATS 모형
s1.pm10_ts %>% tbats() %>% checkresiduals()
s1.pm10_ts %>% tbats() %>% forecast() %>% autoplot()

# pm10 예측모형 선정과 예측
tr = s1.pm10_ts
te = s1.te.pm10_ts

mod_lst <- list (
  mod_arima  = auto.arima(tr, ic='aicc', stepwise=FALSE),
  mod_ets    = ets(tr, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(tr, p=12, size=25),
  mod_tbats  = tbats(tr, ic='aicc'),
  mod_bats   = bats(tr, ic='aicc'),
  # mod_stl    = stlm(tr, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts    = StructTS(tr)
)

forecasts <- lapply(mod_lst, forecast, 3)
forecasts$naive <- naive(tr, 3)

acc <- lapply(forecasts, function(f){
  accuracy(f, te)[2,,drop=FALSE]
})

acc <- do.call(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),] %>% round(2)

## 모형적합 및 예측
s1.pm10_ts %>% bats() %>% checkresiduals()
s1.pm10_ts %>% bats() %>% forecast(h=5) %>% autoplot() +
  labs(x="", y="pm10", title="sgg1 pm10 향후 5년 예측")

s1.pm10_ts %>% bats() %>% forecast(h=5) %>% 
  as_tibble() %>% 
  DT::datatable() %>% 
  DT::formatRound(c(1:5), digits=1)
#***************************************#
#***************************************#
#***************************************#
# 동적 시각화 차이 심한 곳 비교할 때?
sgg1.pm10 = train %>% filter(SGG==kind_ssg[1]) %>% select(week,PM10)
sgg39.pm10 = train %>% filter(SGG==kind_ssg[39]) %>% select(week,PM10)

s1.pm10.xts <- xts(sgg1.pm10$PM10, order.by=sgg1.pm10$week, frequency=52)
s39.pm10.xts <- xts(sgg39.pm10$PM10,  order.by=sgg39.pm10$week, frequency=52)

airpoll_stocks <- cbind(s1.pm10.xts, s39.pm10.xts)
names(airpoll_stocks) <- c("s1.pm10", "s39.pm10")

dygraph(airpoll_stocks, ylab="PM10", main="sgg별 PM10") %>%
  dySeries("s1.pm10", label="s1:강남구") %>%
  dySeries("s39.pm10", label="s39:화랑로") %>%
  dyOptions(colors = c("red","green")) %>%
  dyRangeSelector()

