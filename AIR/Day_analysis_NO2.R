#                               * Comment here *
#이거 전처리 할 때 써놓은 코드인데, 없는 날짜 볼 때 유용했던 거라.. 남겨둠!
# https://lightblog.tistory.com/47 #숫자를 날짜로 바꾸기
# original <- seq(as.Date("2009-12-28"), as.Date("2020-11-02"), by = "7 days")
# as.Date(setdiff(original, unique(df$DATE)), origin = "1970-01-01") #없는 날짜.
# as.Date(setdiff(original, unique(df[df$SGG=="강남구",]$DATE)), origin = "1970-01-01")


#******************************************************************************#
#                                 필수 load
#******************************************************************************#
source('./AIR/packages_need.R', encoding='utf-8')    # 필수 패키지 load
source('./AIR/Day_sgg_separate.R', encoding='utf-8') # daily data

#******************************************************************************#
#                         구별로 대기오염물질 no2 분석
#                          --> 39(측정소)*no2
#******************************************************************************#
freq <- 7

f.no2 <- function(sgg){
  train.no2 <- sgg[,c(2,3)]
  test.no2 <- sgg.te[,c(2,3)]
}

f.no2(sgg1)
#******************************************************************************#
#                            1. 시계열 데이터 변환                                
#******************************************************************************#
# train
train.no2.xts <- xts(train.no2$NO2, order.by=train.no2$DATE, frequency = freq)

# test
test.no2.xts <- xts(test.no2$NO2, order.by=test.no2$DATE, frequency=freq)

# ts <- xts
train.no2.ts <- ts(train.no2.xts,start=c(2010,1), frequency= 365)
test.no2.ts <- ts(test.no2.xts, start=c(2020,1), frequency= 365)
#******************************************************************************#
# 체크용
# require(graphics)
# print(ts(test.no2.xts, frequency=freq), calendar = TRUE)
#******************************************************************************#
#                          2. 모형 전 시계열 데이터 진단
#                           --> 통계적으로 적절하지 않다고 나옴
#******************************************************************************#
autoplot(train.no2.ts)
# autoplot() 시각화를 통해 추세(Trend), 계절성(Seasonality), 순환(Cyclic)이 존재하는지 확인한다.
# ggAcf() 함수를 통해 자기상관이 존재하는지 시각적으로 확인하고, 
# Ljung-Box 검정을 통해 자기상관이 존재하는지 통계량으로 검정한다.
ggAcf(train.no2.ts) + ggtitle("s1.NO2 자기상관함수(ACF)")
Box.test(train.no2.ts, lag = 24, fitdf = 0, type = "Lj")
#******************************************************************************#
#                           2.1 자기상관함수 시각화
#******************************************************************************#
acf(train.no2.ts, main = "자기상관함수", col = "red")
pacf(train.no2.ts, main = "부분자기상관함수", col = "blue")
# 파란색 선 안에 들어오면 자기상관성이 없음. 
# 시계열 데이터 시간의 의존성 여부가 무작위성을 띄느냐를 판단 가능.
#******************************************************************************#
#                   2.2 시계열 분해와 변동 요인 제거 + 그래프                     
#                               기본 시계열 분해                               #
#******************************************************************************#
m <- decompose(train.no2.ts)
attributes(m)
plot(m)
plot(train.no2.ts - m$seasonal) # 계절성 제거된 그래프
par (new = TRUE)
plot(train.no2.ts - m$trend, col = "blue") # 추세요인 제거 그래프
plot(train.no2.ts - m$seasonal - m$trend) # 불규칙 요인만 출력
#******************************************************************************#
#                                 차분 시각화 
#******************************************************************************#
plot(diff(train.no2.ts, differences = 1), main="차분")
par (new = TRUE)
plot(diff(train.no2.ts, differences = 12), col=2)
legend("bottomright", c("1차 차분","12차 차분"), lty=c(1,1) ,col=c(1,2))
# 이동 평균은 시계열 자료를 대상으로 일정한 기간의 자료를 평균으로
# 계산하고, 이동시킨 추세를 파악하여 추세를 예측하는 분석 기법
# 지수평활법은 가중치!
#******************************************************************************#
#                               시계열 모형 적합
# pm10 시계열을 잘 설명하는 시계열 모형을 만들고 이를 바탕으로 예측모형을 생성한다. 
# 벤치마크 모형으로 가장 최근 시계열 관측점을 다음 시점 예측값으로 사용하는 naive 모형부터 ETS, ARIMA, TBATS 모형까지 다양한다.
#******************************************************************************#
# 벤치마크 시계열 예측모형
train.no2.ts %>% naive() %>% checkresiduals()
train.no2.ts %>% naive() %>% forecast() %>% autoplot()

### 1. ETS 모형
train.no2.ts %>% ets(lambda=BoxCox.lambda(train.no2.ts)) %>% checkresiduals()
train.no2.ts %>% ets(lambda=BoxCox.lambda(train.no2.ts)) %>% forecast() %>% autoplot()

### 2. ARIMA 모형
train.no2.ts %>% auto.arima() %>% checkresiduals()
train.no2.ts %>% auto.arima() %>% forecast() %>% autoplot()

### 3. TBATS 모형
train.no2.ts %>% tbats() %>% checkresiduals()
train.no2.ts %>% tbats() %>% forecast() %>% autoplot()
#******************************************************************************#
#                           no2 예측모형 선정과 예측
#******************************************************************************#
# 구별 for문 

mod_lst <- list (
  mod_arima = auto.arima(tr, ic='aicc', stepwise=FALSE),
  mod_exponential = ets(train.no2.ts, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(train.no2.ts),
  mod_tbats = tbats(train.no2.ts, ic='aicc', seasonal.periods=c(24,24*7)),
  mod_bats = bats(train.no2.ts, ic='aicc', seasonal.periods=c(24,24*7)),
  mod_sts = StructTS(train.no2.ts),
  mod_stl = stlm(train.no2.ts, ic='aicc', robust=TRUE, method='ets')
)


# stlm은 비계절성의 시계열 데이터에 대해서 ETS(A,N,N) 모형을 추정한다.
# R에서 forecast 패키지의 nnetar
# 일반적인 예측 변수를 가진 신경망을 적합시키는 방법
# 예측변수에 포함되는 시차변수를 생성시킬 떄 결측치가 있는 행은 꼭 삭제해야 함.
# nnetar 함수가 이러한 작업 수행
# 신경망은 예측변수와 출력변수가 모두 [0,1], [-1,1] 사이의 값을 갖는 척도일 떄 가장 좋은 성과.
# 신경망 예측시에는 모든 입력변수 뿐만 아니라 시계열 데이터 자체를 신경망에 적용하기 전에 척도를 바꿔야.
# TBATS : 박스콕스 변환, ARMA 오차, 추세 및 계절성 요소를 가지는 지수평활 상태공간 모형
# ets : R에서 단순지수평활법을 이용한 예측
# 학습용 기간의 우도로 불리는 값을 최대화시킴으로서 최적의 알파값과 초기 수준 값선택. 
# naive 함수 : 단순 예측치를 계산하는 함수
# snaive 함수 : 계절성이 있는 단순 예측치를 계산하는 함수

forecasts <- lapply(mod_lst, forecast, 330) #2)데일리데이터니까 330일
forecasts$naive <- naive(train.no2.ts,330)
forecasts$snaive = snaive(train.no2.ts,330)

train.no2$date <- as.integer(train.no2$DATE)
mod_gam <- gam(NO2 ~ s(date), data = train.no2, method = "REML")
summary(mod_gam)
par(mfrow=c(2,2))
gam.check(mod_gam)
par(mfrow=c(1,1))
ggGam(mod_gam)
#a=predict(mod_gam, newdata=test.no2$NO2)

acc <- lapply(forecasts, function(f){
  accuracy(f, test.no2.ts)[2,,drop=FALSE]
})

acc <- do.call(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),] %>% round(2)
# 실제값(test.no2.ts)과 예측값(forecasts) 저장 => 구별 물질1개 => 분포보기

#******************************************************************************#
##                              모형적합 및 예측
#******************************************************************************#
train.no2.ts %>% StructTS() %>% checkresiduals()
train.no2.ts %>% StructTS() %>% forecast(h=5) %>% autoplot() +
  labs(x="", y="pm10", title="sgg1 no2 향후 5년 예측")

train.no2.ts %>% StructTS() %>% forecast(h=5) %>% 
  as_tibble() %>% 
  DT::datatable() %>% 
  DT::formatRound(c(1:5), digits=1)

#******************************************************************************#
#                           arima 모형 넣고 잔차를 적합
#                           --> 통계적으로 유의. 적절한 모형이라고 나옴.
#******************************************************************************#
#                           모형 진단(모형 타당성 검정)
#******************************************************************************#
diff = diff(train.no2.ts)
auto.arima(train.no2.ts)
model <- arima(train.no2.ts, order = c(0,1,1), seasonal = list(order = c(0,0,2))) #####확인
model
#******************************************************************************#
# -1) 자기상관함수에 의한 모형 진단
tsdiag(model)
# -2) Box-Ljung에 의한 잔차형 모형 진단
Box.test(model$residuals, lag = 1, type = "Ljung")
# 모형의 잔차를 이용하여 카이제곱검정 방법으로 시계열 모형이 통계적으로 적절한지 검정
# p-value가 0.05이상이면 모형이 통계적으로 적절

#******************************************************************************#
#******************************************************************************#
#                                 Question
#                                 미래 예측
#******************************************************************************#
# forecast(model) # 디폴트 : 향후 2개 예측
# h = ifelse(frequency(object) > 1, 2 * frequency(object), 10)
forecast(model,11) 
# 우리는 11개월치만 예상해야되는데, 왜..2*frequency라 써있으면서 11개가 추정되는지 몰겠음.4*11
par(mfrow = c(1,2))
pre <- forecast(model, h = 44) 
plot(pre, ylim = c(0,0.08), xlim = c(2009,2022))
plot(forecast(model),ylim = c(0,0.08), xlim = c(2009,2022))
par(new = TRUE)
plot(test.no2.ts, xlim = c(2009,2022),ylim = c(0,0.08), col = "red") ####################왜지...
#******************************************************************************#
# 정상성 시계열의 계절형 : 분해하는건 똑같아보이는데.. 그래프가 좀 다르게나옴
ts_feature <- stl(train.no2.ts, s.window = "periodic")
plot(ts_feature)
#******************************************************************************#
#                                 예측정확도
#                   https://otexts.com/fppkr/accuracy.html
#******************************************************************************#
# train.no2.ts <- ts(train.no2.xts, start = c(2009,12,28), frequency=freq)
# test.no2.ts <- ts(test.no2.xts, start = c(2020,1,6), end=c(2020,11,25), frequency=freq)

fit <- window(train.no2.ts, deltat = 7, extend = TRUE)
fit1 <- meanf(fit)
fit2 <- rwf(fit)
fit3 <- snaive(fit, h = nrow(te)) ################# 왜 오류가 나는지 몰겠음..

autoplot(window(train.no2.ts)) +
  autolayer(fit1, series = "평균", PI = FALSE) +
  autolayer(fit2, series = "단순", PI = FALSE) +
  guides(colour = guide_legend(title = "예측")) +
  xlab("연도") + ylab("NO2")
#autolayer(fit3, series = "계절성 단순", PI = FALSE) +

fit11 <- window(train.no2.ts)
accuracy(fit1, test.no2.ts)
accuracy(fit1, test.no2.ts)
#ts(rnorm(52), start = c(2014+9/365.25), frequency=365.25/7)

#******************************************************************************#
par(mfrow = c(2,1))
par(mar = c(2,3,0,2), xaxs = 'i', yaxs = 'i')

#그냥 그림인데 이뻐서..
plot(train.no2.ts, type="c", pch =20, xaxt='n', xlab="")
text(train.no2.ts, col=1:12, labels=1:12, cex=.7) #일단위할떄 필요

# 대체 뭔데 이건 그림이 다르지?????
# train <- window(tr[,3], start=2009, end = c(2019,12))
# plot(train, type="c", pch =20, xaxt='n', xlab="")
# text(train, col=1:12, labels=1:12, cex=.7)
# plot(train, type = "o", pch = 20, xlab = "")
#******************************************************************************#
#                       forecast package 계절변동 시각화
#******************************************************************************#
seasonplot(train.no2.ts,
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(train.no2.ts) #에 그림이 바뀜..
axis(1, at=1:12, labels=month.abb, cex=0.8)




par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n", ylim = c(0,0.15))
  lines(test.no2, col='red')
}

#******************************************************************************#



#******************************************************************************#
# https://kkokkilkon.tistory.com/49
# 구 마다 5개씩 측정값들을 그려보고 싶었음.. --> 지울까아..
library(reshape2) 
train.gangnam <- tr[tr$SGG=="강남구",]
melt_data <- melt(train.gangnam, id.vars = c("NO2", "O3","CO","SO2","PM10"))

g <- ggplot(melt_data) + geom_line(aes(x = seq, y = value, colour = variable), cex = 0.8, show.legend = T)
g


#******************************************************************************#
# 동적 시각화 차이 심한 곳 비교할 때?
sgg1.pm10 = tr %>% filter(SGG==kind_ssg[1]) %>% select(DATE,PM10)
sgg39.pm10 = tr %>% filter(SGG==kind_ssg[39]) %>% select(DATE,PM10)

s1.pm10.xts <- xts(sgg1.pm10$PM10, order.by=sgg1.pm10$DATE, frequency=52)
s39.pm10.xts <- xts(sgg39.pm10$PM10,  order.by=sgg39.pm10$DATE, frequency=52)

airpoll_stocks <- cbind(s1.pm10.xts, s39.pm10.xts)
names(airpoll_stocks) <- c("s1.pm10", "s39.pm10")

dygraph(airpoll_stocks, ylab="PM10", main="sgg별 PM10") %>%
  dySeries("s1.pm10", label="s1:강남구") %>%
  dySeries("s39.pm10", label="s39:화랑로") %>%
  dyOptions(colors = c("red","green")) %>%
  dyRangeSelector()

#save.image(file = "./timeseries_pm10.RData")

