source('sgg_separate.R', encoding='utf-8') #구 분할 / 데이터로드 여기 포함 (안돌면 경로 바꾸세요!)
source('packages_need.R', encoding='utf-8') #필요 패키지 존재

############# 이거 전처리 할 때 써놓은 코드인데, 없는 날짜 볼 때 유용했던 거라.. 남겨둠!
# https://lightblog.tistory.com/47 #숫자를 날자로 바꾸기
# original <- seq(as.Date("2009-12-28"), as.Date("2020-11-02"), by = "7 days")
# as.Date(setdiff(original, unique(df$week)), origin = "1970-01-01") #없는 날짜.
# as.Date(setdiff(original, unique(df[df$SGG=="강남구",]$week)), origin = "1970-01-01")

#***************************************#
#***************************************#
#***************************************#

df <- read.csv(file = 'C:\\Users\\hanso\\Desktop\\빅통분\\airseoul.csv', header = T)[,-1]
df$SGG <- factor(df$SGG)

tr <- df[df$week < '2020-01-06',]
te <- df[df$week >= '2020-01-06',]
freq <- 365.25/7


# train <- window(train[,3], start=2009, end = c(2019,12))
# test.no2 <- ts(test[,3],frequency = 52, start = c(2020,1,2))
# test <- window(test.no2, start = 2020, deltat = 7)


# 시계열 분해와 변동 요인 제거 + 그래프
gangnam_NO2 <- df[df$SGG == "강남구",]$NO2
ts.train.no2 <- ts(gangnam_NO2, start = c(2010,1,1), frequency = freq)
ts.test.no2 <- ts(te$NO2, start = c(2020,1,6), frequency = freq)

# 기본 시계열 분해
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


#***************************************#
# 언니는 그냥 바로 시계열 데이터 진단 --> 통계적으로 적절하지 않다고 나옴
# 나는 arima 모형 넣고 잔차를 적합. --> 통계쩍으로 유의. 적절한 모형이라고 나옴.
# 두 개 차이 보이면 될듯?

######
#소희#
######
autoplot(ts.train.no2)
# autoplot() 시각화를 통해 추세(Trend), 계절성(Seasonality), 순환(Cyclic)이 존재하는지 확인한다.
# ggAcf() 함수를 통해 자기상관이 존재하는지 시각적으로 확인하고, 
# Ljung-Box 검정을 통해 자기상관이 존재하는지 통계량으로 검정한다.
ggAcf(ts.train.no2) + ggtitle("s1.NO2 자기상관함수(ACF)")
Box.test(ts.train.no2, lag = 24, fitdf = 0, type = "Lj")

######
#소현#
######
# 모형 진단(모형 타당성 검정)
diff = diff(ts.train.no2)
auto.arima(ts.train.no2)
model <- arima(ts.train.no2, order = c(0,1,1), seasonal = list(order = c(0,0,2)))
model

# -1) 자기상관함수에 의한 모형 진단
tsdiag(model)
# -2) Box-Ljung에 의한 잔차형 모형 진단
Box.test(model$residuals, lag = 1, type = "Ljung")
# 모형의 잔차를 이용하여 카이제곱검정 방법으로 시계열 모형이 통계적으로 적절한지 검정
# p-value가 0.05이상이면 모형이 통계적으로 적절


#***************************************#
####################################################################################Question
# 미래 예측
#forecast(model) #향후 2년 예측

forecast(model,11) 
#h = ifelse(frequency(object) > 1, 2 * frequency(object), 10)
#우리는 11개월치만 예상해야되는데, 왜..2*frequency라 써있으면서 11개가 추정되는지 몰겠음.4*11

par(mfrow = c(1,2))
pre <- forecast(model, h = 44) 
plot(pre, ylim = c(0,0.08), xlim = c(2009,2022))


plot(forecast(model),ylim = c(0,0.08), xlim = c(2009,2022))

par(new = TRUE)
plot(ts.test.no2, xlim = c(2009,2022),ylim = c(0,0.08), col = "red") ####################왜지...

#***************************************#
# 정상성 시계열의 계절형 : 분해하는건 똑같아보이는데.. 그래프가 좀 다르게나옴
ts_feature <- stl(ts.train.no2, s.window = "periodic")
plot(ts_feature)


##############################################
##############################################
##############################################
##############################################

# 예측정확도
#https://otexts.com/fppkr/accuracy.html

df.gangnam = tr[tr$SGG == '강남구',][,3]
df.gangnam = ts(df.gangnam)
fit <- window(df.gangnam, deltat = 7, extend = TRUE)
fit1 <- meanf(fit)
fit2 <- rwf(fit)
fit3 <- snaive(fit, h = nrow(te)) ################# 왜 오류가 나는지 몰겠음..
autoplot(window(df.gangnam)) +
  autolayer(fit1, series = "평균", PI = FALSE) +
  autolayer(fit2, series = "단순", PI = FALSE) +
  guides(colour = guide_legend(title = "예측")) +
  xlab("연도") + ylab("NO2")
#autolayer(fit3, series = "계절성 단순", PI = FALSE) +

fit11 <- window(df.gangnam)
accuracy(fit1, ts.test.no2)
accuracy(fit1, te[,3])
#ts(rnorm(52), start = c(2014+9/365.25), frequency=365.25/7)

##############################################

par(mfrow = c(2,1))
par(mar = c(2,3,0,2), xaxs = 'i', yaxs = 'i')

#그냥 그림인데 이뻐서..
plot(ts.train.no2, type="c", pch =20, xaxt='n', xlab="")
text(ts.train.no2, col=1:12, labels=1:12, cex=.7)

# 대체 뭔데 이건 그림이 다르지?????
# train <- window(tr[,3], start=2009, end = c(2019,12))
# plot(train, type="c", pch =20, xaxt='n', xlab="")
# text(train, col=1:12, labels=1:12, cex=.7)
# plot(train, type = "o", pch = 20, xlab = "")





# forecast package 계절변동 시각화
seasonplot(ts.train.no2,
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(ts.train.no2) #에 그림이 바뀜..
axis(1, at=1:12, labels=month.abb, cex=0.8)

# 모형선정
mod_lst <- list (
  #mod_arima = auto.arima(tr, ic='aicc', stepwise=FALSE),
  mod_exponential = ets(ts.train.no2, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(ts.train.no2, p=12, size=25),
  mod_tbats = tbats(ts.train.no2, ic='aicc', seasonal.periods=12),
  mod_bats = bats(ts.train.no2, ic='aicc', seasonal.periods=12),
  #mod_stl = stlm(train, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(ts.train.no2)
  
  #gem추가예정
  #stl은 왜 오류나는지 모르겠음.
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
)

# 1년당 365.25일과 1주당 7일을 갖는다고 할 떄 1년은 52.17857주 
# -> dshw사용불가 (계절성이 있는 기간이 중첩되어 있을 때 사용)

# naive 함수 : 단순 예측치를 계산하는 함수
# snaive 함수 : 계절성이 있는 단순 예측치를 계산하는 함수

forecasts <- lapply(mod_lst, forecast, 3) #12 - 달의 평균으로 . 10개씩 나옴.
forecasts$naive <- naive(tr, 3)

forecasts[[1]]


#########################################################
######################## 비상
#########################################################

acc <- lapply(forecasts, function(f){
  accuracy(f, ts.test.no2)[2,,drop=FALSE]
})
acc <- do.call(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),] %>% round(2)



par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n", ylim = c(0,0.15))
  lines(test.no2, col='red')
}
#########################################################
#########################################################
#########################################################


######
#소희#
######

# 시계열 모형 적합
# pm10 시계열을 잘 설명하는 시계열 모형을 만들고 이를 바탕으로 예측모형을 생성한다. 벤치마크 모형으로 
# 가장 최근 시계열 관측점을 다음 시점 예측값으로 사용하는 naive 모형부터 ETS, ARIMA, TBATS 모형까지 다양한다.
# 밑에 부분에서 갑자기 오류가 나면 dev.off() 입력.


# 예측모형 그리기
# 벤치마크 시계열 예측모형
ts.train.no2 %>% naive() %>% checkresiduals()
ts.train.no2 %>% naive() %>% forecast() %>% autoplot()

### 1. ETS 모형
ts.train.no2 %>% ets(lambda=BoxCox.lambda(ts.train.no2)) %>% checkresiduals()
ts.train.no2 %>% ets(lambda=BoxCox.lambda(ts.train.no2)) %>% forecast() %>% autoplot()

### 2. ARIMA 모형
ts.train.no2 %>% auto.arima() %>% checkresiduals()
ts.train.no2 %>% auto.arima() %>% forecast() %>% autoplot()

### 3. TBATS 모형
ts.train.no2 %>% tbats() %>% checkresiduals()
ts.train.no2 %>% tbats() %>% forecast() %>% autoplot()

## 모형적합 및 예측
ts.train.no2 %>% bats() %>% checkresiduals()
ts.train.no2 %>% bats() %>% forecast(h=5) %>% autoplot() +
  labs(x="", y="pm10", title="sgg1 pm10 향후 5년 예측")

ts.train.no2 %>% bats() %>% forecast(h=5) %>% 
  as_tibble() %>% 
  DT::datatable() %>% 
  DT::formatRound(c(1:5), digits=1)



##########################################################
# https://kkokkilkon.tistory.com/49
# 구 마다 5개씩 측정값들을 그려보고 싶었음.. --> 지울까아..
library(reshape2) 
train.gangnam <- tr[tr$SGG=="강남구",]
melt_data <- melt(train.gangnam, id.vars = c("NO2", "O3","CO","SO2","PM10"))

g <- ggplot(melt_data) + geom_line(aes(x = seq, y = value, colour = variable), cex = 0.8, show.legend = T)
g


##########################################################
##########################################################
##########################################################
#***************************************#
#***************************************#
#***************************************#
# 동적 시각화 차이 심한 곳 비교할 때?
sgg1.pm10 = tr %>% filter(SGG==kind_ssg[1]) %>% select(week,PM10)
sgg39.pm10 = tr %>% filter(SGG==kind_ssg[39]) %>% select(week,PM10)

s1.pm10.xts <- xts(sgg1.pm10$PM10, order.by=sgg1.pm10$week, frequency=52)
s39.pm10.xts <- xts(sgg39.pm10$PM10,  order.by=sgg39.pm10$week, frequency=52)

airpoll_stocks <- cbind(s1.pm10.xts, s39.pm10.xts)
names(airpoll_stocks) <- c("s1.pm10", "s39.pm10")

dygraph(airpoll_stocks, ylab="PM10", main="sgg별 PM10") %>%
  dySeries("s1.pm10", label="s1:강남구") %>%
  dySeries("s39.pm10", label="s39:화랑로") %>%
  dyOptions(colors = c("red","green")) %>%
  dyRangeSelector()
