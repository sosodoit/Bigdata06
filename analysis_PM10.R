source('sgg_separate.R', encoding='utf-8') #구 분할 / 데이터로드 여기 포함 (안돌면 경로 바꾸세요!)
source('packages_need.R', encoding='utf-8') #필요 패키지 존재

#source('myFunction.R', encoding='utf-8')
# 사용자 정의 함수 저장: 많은 함수를 작성한 경우 
# 이 함수들을 하나의 R 파일로 저장한 후에 필요할때 마다  
# source() 함수를 이용해서 불러오는 것이 좋다. 
#***************************************# 
#     구별 대기물질별 데이터 분할       #      
#***************************************# 
#               pm10 분석               #    
#***************************************# 
# train
s1.pm10 = sgg1[,c(2,7)]
s2.pm10 = sgg2[,c(2,7)]
s3.pm10 = sgg3[,c(2,7)]
s4.pm10 = sgg4[,c(2,7)]
s5.pm10 = sgg5[,c(2,7)]
s6.pm10 = sgg6[,c(2,7)]
s7.pm10 = sgg7[,c(2,7)]
s8.pm10 = sgg8[,c(2,7)]
s9.pm10 = sgg9[,c(2,7)]
s10.pm10 = sgg10[,c(2,7)]

s11.pm10 = sgg11[,c(2,7)]
s12.pm10 = sgg12[,c(2,7)]
s13.pm10 = sgg13[,c(2,7)]
s14.pm10 = sgg14[,c(2,7)]
s15.pm10 = sgg15[,c(2,7)]
s16.pm10 = sgg16[,c(2,7)]
s17.pm10 = sgg17[,c(2,7)]
s18.pm10 = sgg18[,c(2,7)]
s19.pm10 = sgg19[,c(2,7)]
s20.pm10 = sgg20[,c(2,7)]

s21.pm10 = sgg21[,c(2,7)]
s22.pm10 = sgg22[,c(2,7)]
s23.pm10 = sgg23[,c(2,7)]
s24.pm10 = sgg24[,c(2,7)]
s25.pm10 = sgg25[,c(2,7)]
s26.pm10 = sgg26[,c(2,7)]
s27.pm10 = sgg27[,c(2,7)]
s28.pm10 = sgg28[,c(2,7)]
s29.pm10 = sgg29[,c(2,7)]
s30.pm10 = sgg30[,c(2,7)]

s31.pm10 = sgg31[,c(2,7)]
s32.pm10 = sgg32[,c(2,7)]
s33.pm10 = sgg33[,c(2,7)]
s34.pm10 = sgg34[,c(2,7)]
s35.pm10 = sgg35[,c(2,7)]
s36.pm10 = sgg36[,c(2,7)]
s37.pm10 = sgg37[,c(2,7)]
s38.pm10 = sgg38[,c(2,7)]
s39.pm10 = sgg39[,c(2,7)]

# test
s1.te.pm10 = sgg1.te[,c(2,7)]
s2.te.pm10 = sgg2.te[,c(2,7)]
s3.te.pm10 = sgg3.te[,c(2,7)]
s4.te.pm10 = sgg4.te[,c(2,7)]
s5.te.pm10 = sgg5.te[,c(2,7)]
s6.te.pm10 = sgg6.te[,c(2,7)]
s7.te.pm10 = sgg7.te[,c(2,7)]
s8.te.pm10 = sgg8.te[,c(2,7)]
s9.te.pm10 = sgg9.te[,c(2,7)]
s10.te.pm10 = sgg10.te[,c(2,7)]

s11.te.pm10 = sgg11.te[,c(2,7)]
s12.te.pm10 = sgg12.te[,c(2,7)]
s13.te.pm10 = sgg13.te[,c(2,7)]
s14.te.pm10 = sgg14.te[,c(2,7)]
s15.te.pm10 = sgg15.te[,c(2,7)]
s16.te.pm10 = sgg16.te[,c(2,7)]
s17.te.pm10 = sgg17.te[,c(2,7)]
s18.te.pm10 = sgg18.te[,c(2,7)]
s19.te.pm10 = sgg19.te[,c(2,7)]
s20.te.pm10 = sgg20.te[,c(2,7)]

s21.te.pm10 = sgg21.te[,c(2,7)]
s22.te.pm10 = sgg22.te[,c(2,7)]
s23.te.pm10 = sgg23.te[,c(2,7)]
s24.te.pm10 = sgg24.te[,c(2,7)]
s25.te.pm10 = sgg25.te[,c(2,7)]
s26.te.pm10 = sgg26.te[,c(2,7)]
s27.te.pm10 = sgg27.te[,c(2,7)]
s28.te.pm10 = sgg28.te[,c(2,7)]
s29.te.pm10 = sgg29.te[,c(2,7)]
s30.te.pm10 = sgg30.te[,c(2,7)]

s31.te.pm10 = sgg31.te[,c(2,7)]
s32.te.pm10 = sgg32.te[,c(2,7)]
s33.te.pm10 = sgg33.te[,c(2,7)]
s34.te.pm10 = sgg34.te[,c(2,7)]
s35.te.pm10 = sgg35.te[,c(2,7)]
s36.te.pm10 = sgg36.te[,c(2,7)]
s37.te.pm10 = sgg37.te[,c(2,7)]
s38.te.pm10 = sgg38.te[,c(2,7)]
s39.te.pm10 = sgg39.te[,c(2,7)]

#***************************************#
#           시계열 데이터 변환          #
#***************************************#
# train
s1.pm10.xts <- xts(s1.pm10$PM10, order.by=s1.pm10$week, frequency=52)
s2.pm10.xts <- xts(s2.pm10$PM10, order.by=s2.pm10$week, frequency=52)

s39.pm10.xts <- xts(s39.pm10$PM10,  order.by=s39.pm10$week, frequency=52)

# test
s1.te.pm10.xts <- xts(s1.te.pm10$PM10, order.by=s1.te.pm10$week, frequency=52)
s2.te.pm10.xts <- xts(s2.te.pm10$PM10, order.by=s2.te.pm10$week, frequency=52)

s39.te.pm10.xts <- xts(s39.te.pm10$PM10,  order.by=s39.te.pm10$week, frequency=52)
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
# 작업공간의 모든 객체 저장 및 읽기 
# 하나의 데이터 뿐만 아니라, 모든 작업 내역을 그대로 저장할 수 있다. 
# save() 대신 save.image()를 사용하면 된다.
save.image(file = "./timeseries_pm10.RData")
#***************************************#
#*
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

