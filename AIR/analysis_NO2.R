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
source('./AIR/sgg_separate.R', encoding='utf-8')     # week data

#******************************************************************************#
#                         구별로 대기오염물질 no2 분석
#                          --> 39(측정소)*no2
#******************************************************************************#
freq <- 365.25/7
mod_lst <- list()
f.no2 <- function(tr, te){
  
  train.no2 <<- tr[,c(2,3)]
  test.no2 <<- te[,c(2,3)]
  
  train.no2.ts <<- ts(train.no2, start=decimal_date(as.Date("2010-01-01")), frequency= freq)
  test.no2.ts <<- ts(test.no2, start=decimal_date(as.Date("2020-01-01")), frequency= freq)
  
  no2.tr <<- train.no2.ts[,2]
  no2.te <<- test.no2.ts[,2]
  
  mod_lst <<- list (
    
    #mod_exponential = ets(no2.tr, ic='aicc', restrict=FALSE),
    #mod_sts = StructTS(no2.tr),
    #mod_neural = nnetar(no2.tr),
    #mod_stl = stlm(no2.tr, ic='aicc', robust=TRUE, method='ets'),
    #mod_tbats = tbats(no2.tr, ic='aicc', seasonal.periods=c(7,365.25)),
    #mod_bats = bats(no2.tr, ic='aicc', seasonal.periods=c(7,365.25)),
    mod_arima = auto.arima(no2.tr, ic='aicc', stepwise=FALSE)
    
  )
  
  forecasts <<- lapply(mod_lst, forecast, 330)
  forecasts$naive <<- naive(train.no2.ts, 330)
  forecasts$snaive <<- snaive(train.no2.ts, 330)
  
  acc <<- lapply(forecasts, function(f){
    accuracy(f, no2.te)[2,,drop=FALSE]
  })
  
  acc <<- do.call(rbind, acc)
  row.names(acc) <<- names(forecasts)
  acc <<- acc[order(acc[,'MASE']),] %>% round(2)
  
}

# f.no2(sgg10, sgg10.te) # 
# save.image(file = "./data/analysis_NO2_sgg10.RData")
# f.no2(sgg20, sgg20.te) # 
# save.image(file = "./data/analysis_NO2_sgg20.RData")
# f.no2(sgg14, sgg14.te) # 
# save.image(file = "./data/analysis_NO2_sgg14.RData")
# f.no2(sgg39, sgg39.te) # 
# save.image(file = "./data/analysis_NO2_sgg39.RData")

# 구를 한번에 도는 코드 추가
sgg_lst <- list(sgg1, sgg2, sgg3, sgg4, sgg5, sgg6, sgg7, sgg8, sgg9, sgg10,
                sgg11, sgg12, sgg13, sgg14, sgg15, sgg16, sgg17, sgg18, sgg19, sgg20,
                sgg21, sgg22, sgg23, sgg24, sgg25, sgg26, sgg27, sgg28, sgg29, sgg30,
                sgg31, sgg32, sgg33, sgg34, sgg35, sgg36, sgg37, sgg38, sgg39)

sgg_lst.te <- list(sgg1.te, sgg2.te, sgg3.te, sgg4.te, sgg5.te, sgg6.te, sgg7.te, sgg8.te, sgg9.te, sgg10.te,
                sgg11.te, sgg12.te, sgg13.te, sgg14.te, sgg15.te, sgg16.te, sgg17.te, sgg18.te, sgg19.te, sgg20.te,
                sgg21.te, sgg22.te, sgg23.te, sgg24.te, sgg25.te, sgg26.te, sgg27.te, sgg28.te, sgg29.te, sgg30.te,
                sgg31.te, sgg32.te, sgg33.te, sgg34.te, sgg35.te, sgg36.te, sgg37.te, sgg38.te, sgg39.te)

for ( i in 1:length(kind_ssg)) {
  area <- paste0("sgg", i) # 이걸로 호출하는 방법을 생각해보겠음.. 잘안되네용..
  
  f.no2(sgg_lst[[i]], sgg_lst.te[[i]])
  
  #save.image(file = paste0("./data/analysis_NO2_",area,".RData"))
  mod_lst %>% write_rds("./data/analysis_NO2_",area,".rds")
}

#******************************************************************************#
# 하나씩 로드 후 분석 및 예측
load( "./data/analysis_NO2_sgg10.RData" )
load( "./data/analysis_NO2_sgg20.RData" )
load( "./data/analysis_NO2_sgg14.RData" )
load( "./data/analysis_NO2_sgg39.RData" )

#                          2. 모형 전 시계열 데이터 진단
#******************************************************************************#
autoplot(train.no2.ts[,2])
ggAcf(train.no2.ts[,2]) + ggtitle("no2 자기상관함수(ACF)")
Box.test(train.no2.ts[,2], lag = 12, fitdf = 0, type = "Lj")

#                           2.1 자기상관함수 시각화
#******************************************************************************#
acf(train.no2.ts[,2], main = "자기상관함수", col = "red")
pacf(train.no2.ts[,2], main = "부분자기상관함수", col = "blue")

#                   2.2 시계열 분해와 변동 요인 제거 + 그래프                     
#                               기본 시계열 분해                               #
#******************************************************************************#
m <<- decompose(train.no2.ts[,2])
attributes(m)
plot(m)

plot(train.no2.ts[,2] - m$seasonal, ylim = c(-30,90),  ylab="", main="no2") # 계절성 제거된 그래프
lines(train.no2.ts[,2] - m$trend, col = "blue")                 # 추세요인 제거 그래프
lines(train.no2.ts[,2] - m$seasonal - m$trend, col = "red") # 불규칙 요인만 출력

#                              2.3 차분 시각화 
#******************************************************************************#
par(mfrow=c(2,1))
plot(diff(train.no2.ts[,2], differences = 1),  ylab="", main="1차 차분")
plot(diff(train.no2.ts[,2], differences = 12), ylab="", main="12차 차분")
par(mfrow=c(1,1))

# dif1 <<- diff(train.no2.ts[,2], differences = 1)
# m2 <<- decompose(dif1)
# plot(m2)

#                              최종모형적합 및 예측
#******************************************************************************#
# no2.tr %>% StructTS() %>% checkresiduals()
# no2.tr %>% StructTS() %>% forecast(h=5) %>% autoplot() +
#   labs(x="", y="no2", title="no2 향후 5년 예측")
# 
# no2.tr %>% StructTS() %>% forecast(h=5) %>% 
#   as_tibble() %>% 
#   DT::datatable() %>% 
#   DT::formatRound(c(1:5), digits=1)
# 
#                           모형 진단(모형 타당성 검정)
#******************************************************************************#
# diff = diff(no2.tr)
# auto.arima(no2.tr)
# model <- arima(no2.tr, order = c(0,1,1), seasonal = list(order = c(0,0,2))) 
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
# plot(test.no2.ts, xlim = c(2009,2022),ylim = c(0,0.08), col = "red")


#                                 예측정확도
#                   https://otexts.com/fppkr/accuracy.html
#******************************************************************************#

# fit <- window(train.no2.ts, deltat = 7, extend = TRUE)
# fit1 <- meanf(fit)
# fit2 <- rwf(fit)
# fit3 <- snaive(fit, h = nrow(te))
# 
# autoplot(window(train.no2.ts)) +
#   autolayer(fit1, series = "평균", PI = FALSE) +
#   autolayer(fit2, series = "단순", PI = FALSE) +
#   guides(colour = guide_legend(title = "예측")) +
#   xlab("연도") + ylab("no2")
# #autolayer(fit3, series = "계절성 단순", PI = FALSE) +
# 
# fit11 <- window(train.no2.ts)
# accuracy(fit1, test.no2.ts)
# accuracy(fit1, test.no2.ts)
# #ts(rnorm(52), start = c(2014+9/365.25), frequency=365.25/7)
# 

#******************************************************************************#
# par(mfrow = c(2,1))
# par(mar = c(2,3,0,2), xaxs = 'i', yaxs = 'i')
# 
# #그냥 그림인데 이뻐서..
# plot(train.no2.ts, type="c", pch =20, xaxt='n', xlab="")
# text(train.no2.ts, col=1:12, labels=1:12, cex=.7) #일단위할떄 필요

#******************************************************************************#
#                       forecast package 계절변동 시각화
#******************************************************************************#

# seasonplot(no2.tr,
#            year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)
# 
# monthplot(no2.tr) 
# axis(1, at=1:12, labels=month.abb, cex=0.8)
# 
# par(mfrow=c(4, 2))
# par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')
# 
# for(f in forecasts){
#   plot(f, main="", xaxt="n", ylim = c(0,0.15))
#   lines(no2.te, col='red')
# }



#******************************************************************************#
# https://kkokkilkon.tistory.com/49
# 구 마다 5개씩 측정값들을 그려보고 싶었음.
# library(reshape2) 
# train.gangnam <- tr[tr$SGG=="강남구",]
# melt_data <- melt(train.gangnam, id.vars = c("no2", "no2","no2","SO2","no2"))
# 
# g <- ggplot(melt_data) + geom_line(aes(x = seq, y = value, colour = variable), cex = 0.8, show.legend = T)
# g

#******************************************************************************#
# 동적 시각화 차이 심한 곳 비교할 때?
# sgg1.no2 = tr %>% filter(SGG==kind_ssg[1]) %>% select(DATE,no2)
# sgg39.no2 = tr %>% filter(SGG==kind_ssg[39]) %>% select(DATE,no2)
# 
# s1.no2.xts <- xts(sgg1.no2$no2, order.by=sgg1.no2$DATE, frequency=52)
# s39.no2.xts <- xts(sgg39.no2$no2,  order.by=sgg39.no2$DATE, frequency=52)
# 
# airpoll_stocks <- cbind(s1.no2.xts, s39.no2.xts)
# names(airpoll_stocks) <- c("s1.no2", "s39.no2")
# 
# dygraph(airpoll_stocks, ylab="no2", main="sgg별 no2") %>%
#   dySeries("s1.no2", label="s1:강남구") %>%
#   dySeries("s39.no2", label="s39:화랑로") %>%
#   dyOptions(colors = c("red","green")) %>%
#   dyRangeSelector()

