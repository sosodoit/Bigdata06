#******************************************************************************#
#                                 필수 load
#******************************************************************************#
#source('./AIR/packages_need.R', encoding='utf-8')    # 필수 패키지 load
#source('./AIR/sgg_separate.R', encoding='utf-8')     # week data
load( "./analysis_PM10.RData" )
#******************************************************************************#
#                         구별로 대기오염물질 pm10 분석
#                          --> 39(측정소)*pm10
#******************************************************************************#
freq <- 365.25/7

f.pm10 <- function(tr, te){
  
  train.pm10 <<- tr[,c(2,7)]
  test.pm10 <<- te[,c(2,7)]

  train.pm10.ts <<- ts(train.pm10, start=decimal_date(as.Date("2010-01-01")), frequency= freq)
  test.pm10.ts <<- ts(test.pm10, start=decimal_date(as.Date("2020-01-01")), frequency= freq)

  pm10.tr <<- train.pm10.ts[,2]
  pm10.te <<- test.pm10.ts[,2]
  
  mod_lst <<- list (
    
    mod_exponential = ets(pm10.tr, ic='aicc', restrict=FALSE),
    mod_sts = StructTS(pm10.tr),
    mod_neural = nnetar(pm10.tr),
    mod_stl = stlm(pm10.tr, ic='aicc', robust=TRUE, method='ets'),
    mod_tbats = tbats(pm10.tr, ic='aicc', seasonal.periods=c(7,365.25)),
    mod_bats = bats(pm10.tr, ic='aicc', seasonal.periods=c(7,365.25)),
    mod_arima = auto.arima(pm10.tr, ic='aicc', stepwise=FALSE)
    
  )
  
  forecasts <<- lapply(mod_lst, forecast, 330)
  forecasts$naive <<- naive(train.pm10.ts, 330)
  forecasts$snaive <<- snaive(train.pm10.ts, 330)
  
  acc <<- lapply(forecasts, function(f){
    accuracy(f, pm10.te)[2,,drop=FALSE]
  })
  
  acc <<- do.call(rbind, acc)
  row.names(acc) <<- names(forecasts)
  acc <<- acc[order(acc[,'MASE']),] %>% round(2)
}

#save.image(file = "./analysis_PM10.RData")

f.pm10(sgg1, sgg1.te)
save.image(file = "./data/analysis_PM10.RData")
f.pm10(sgg39, sgg39.te)
save.image(file = "./data/analysis_PM10_sgg39.RData")

f.pm10(sgg10, sgg10.te)
save.image(file = "./data/analysis_PM10_sgg10.RData")
f.pm10(sgg14, sgg14.te)
save.image(file = "./data/analysis_PM10_sgg14.RData")



#******************************************************************************#
# 하나씩 로드 후 분석 및 예측
load( "./data/analysis_PM10.RData" )
load( "./data/analysis_PM10_sgg39.RData" )

#                          2. 모형 전 시계열 데이터 진단
#******************************************************************************#
autoplot(train.pm10.ts[,2])
ggAcf(train.pm10.ts[,2]) + ggtitle("PM10 자기상관함수(ACF)")
Box.test(train.pm10.ts[,2], lag = 12, fitdf = 0, type = "Lj")
  
#                           2.1 자기상관함수 시각화
#******************************************************************************#
acf(train.pm10.ts[,2], main = "자기상관함수", col = "red")
pacf(train.pm10.ts[,2], main = "부분자기상관함수", col = "blue")
  
#                   2.2 시계열 분해와 변동 요인 제거 + 그래프                     
#                               기본 시계열 분해                               #
#******************************************************************************#
m <<- decompose(train.pm10.ts[,2])
attributes(m)
plot(m)

plot(train.pm10.ts[,2] - m$seasonal, ylim = c(-30,90),  ylab="", main="PM10") # 계절성 제거된 그래프
lines(train.pm10.ts[,2] - m$trend, col = "blue")                 # 추세요인 제거 그래프
lines(train.pm10.ts[,2] - m$seasonal - m$trend, col = "red") # 불규칙 요인만 출력
  
#                              2.3 차분 시각화 
#******************************************************************************#
par(mfrow=c(2,1))
plot(diff(train.pm10.ts[,2], differences = 1),  ylab="", main="1차 차분")
plot(diff(train.pm10.ts[,2], differences = 12), ylab="", main="12차 차분")
par(mfrow=c(1,1))
  
# dif1 <<- diff(train.pm10.ts[,2], differences = 1)
# m2 <<- decompose(dif1)
# plot(m2)

#                              최종모형적합 및 예측
#******************************************************************************#
# pm10.tr %>% StructTS() %>% checkresiduals()
# pm10.tr %>% StructTS() %>% forecast(h=5) %>% autoplot() +
#   labs(x="", y="PM10", title="PM10 향후 5년 예측")
# 
# pm10.tr %>% StructTS() %>% forecast(h=5) %>% 
#   as_tibble() %>% 
#   DT::datatable() %>% 
#   DT::formatRound(c(1:5), digits=1)
# 
#                           모형 진단(모형 타당성 검정)
#******************************************************************************#
# diff = diff(pm10.tr)
# auto.arima(pm10.tr)
# model <- arima(pm10.tr, order = c(0,1,1), seasonal = list(order = c(0,0,2))) 
# model
# 
# # -1) 자기상관함수에 의한 모형 진단
# tsdiag(model)
# # -2) Box-Ljung에 의한 잔차형 모형 진단
# Box.test(model$residuals, lag = 1, type = "Ljung")


#                                 미래 예측
#******************************************************************************#

# forecast(model,330) 
# par(mfrow = c(1,2))
# pre <- forecast(model, h = 330) 
# plot(pre, ylim = c(0,0.08), xlim = c(2009,2022))
# plot(forecast(model),ylim = c(0,0.08), xlim = c(2009,2022))
# par(new = TRUE)
# plot(test.pm10.ts, xlim = c(2009,2022),ylim = c(0,0.08), col = "red")


#                                 예측정확도
#                   https://otexts.com/fppkr/accuracy.html
#******************************************************************************#

# fit <- window(train.pm10.ts, deltat = 7, extend = TRUE)
# fit1 <- meanf(fit)
# fit2 <- rwf(fit)
# fit3 <- snaive(fit, h = nrow(te))
# 
# autoplot(window(train.pm10.ts)) +
#   autolayer(fit1, series = "평균", PI = FALSE) +
#   autolayer(fit2, series = "단순", PI = FALSE) +
#   guides(colour = guide_legend(title = "예측")) +
#   xlab("연도") + ylab("pm10")
# #autolayer(fit3, series = "계절성 단순", PI = FALSE) +
# 
# fit11 <- window(train.pm10.ts)
# accuracy(fit1, test.pm10.ts)
# accuracy(fit1, test.pm10.ts)
# #ts(rnorm(52), start = c(2014+9/365.25), frequency=365.25/7)
# 

#******************************************************************************#
# par(mfrow = c(2,1))
# par(mar = c(2,3,0,2), xaxs = 'i', yaxs = 'i')
# 
# #그냥 그림인데 이뻐서..
# plot(train.pm10.ts, type="c", pch =20, xaxt='n', xlab="")
# text(train.pm10.ts, col=1:12, labels=1:12, cex=.7) #일단위할떄 필요

#******************************************************************************#
#                       forecast package 계절변동 시각화
#******************************************************************************#

# seasonplot(pm10.tr,
#            year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)
# 
# monthplot(pm10.tr) 
# axis(1, at=1:12, labels=month.abb, cex=0.8)
# 
# par(mfrow=c(4, 2))
# par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')
# 
# for(f in forecasts){
#   plot(f, main="", xaxt="n", ylim = c(0,0.15))
#   lines(pm10.te, col='red')
# }



#******************************************************************************#
# https://kkokkilkon.tistory.com/49
# 구 마다 5개씩 측정값들을 그려보고 싶었음.
# library(reshape2) 
# train.gangnam <- tr[tr$SGG=="강남구",]
# melt_data <- melt(train.gangnam, id.vars = c("pm10", "O3","CO","SO2","PM10"))
# 
# g <- ggplot(melt_data) + geom_line(aes(x = seq, y = value, colour = variable), cex = 0.8, show.legend = T)
# g

#******************************************************************************#
# 동적 시각화 차이 심한 곳 비교할 때?
# sgg1.pm10 = tr %>% filter(SGG==kind_ssg[1]) %>% select(DATE,PM10)
# sgg39.pm10 = tr %>% filter(SGG==kind_ssg[39]) %>% select(DATE,PM10)
# 
# s1.pm10.xts <- xts(sgg1.pm10$PM10, order.by=sgg1.pm10$DATE, frequency=52)
# s39.pm10.xts <- xts(sgg39.pm10$PM10,  order.by=sgg39.pm10$DATE, frequency=52)
# 
# airpoll_stocks <- cbind(s1.pm10.xts, s39.pm10.xts)
# names(airpoll_stocks) <- c("s1.pm10", "s39.pm10")
# 
# dygraph(airpoll_stocks, ylab="PM10", main="sgg별 PM10") %>%
#   dySeries("s1.pm10", label="s1:강남구") %>%
#   dySeries("s39.pm10", label="s39:화랑로") %>%
#   dyOptions(colors = c("red","green")) %>%
#   dyRangeSelector()


