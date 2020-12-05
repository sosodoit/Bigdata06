source('./AIR/packages_need.R', encoding='utf-8')
source('./AIR/sgg_separate.R', encoding='utf-8')
source('./arima/together_arima.R', encoding='utf-8') # arima list 가져오기
source('./AIR/predict_functions.R', encoding='utf-8')

#############################################
#############################################
#m.result에 구마다의 실제값이 들어있음. 
#ms.a가 예측값. 

# ms.a <- data.frame(matrix(NA, ncol = 1, nrow = 11))
# m.b <- data.frame(m.a[-12])
# for (i in 1:39) {
#   ms.a <- cbind(ms.a, m.b)
# }
# m.result <- monthly.lst[[3]]
# ms.a <- ms.a[,-1]
# m.res <- (m.result[m.result$month>='2020-01',-1])-ms.a 

#######################################################################################################
#######################################################################################################
m.a <- data.frame(matrix(ncol = 40, nrow = 11)) # 달마다의 예측값 평균
names(m.a) <- name_ssg

##### 여기 이름만 수정해서 돌려주세용!!!!
arima.mat<-arima.pm10
pred.arima <- data.frame(matrix(ncol = 39, nrow = 11))
for ( i in 1:39 ){
  pred.arima[,i] <- data.frame(matrix(mon.avg.arima(arima.mat[[i]])))[-12,]
}
colnames(pred.arima) <- name_ssg[-1]
rownames(pred.arima) <- c("1월", "2월", "3월", "4월", "5월", "6월",
                         "7월", "8월", "9월", "10월", "11월")

# 월 평균 : 예측 - 실제
observe <- monthly.lst[[5]] ##### 여기 숫자 바꿔야 해요....
#1:no2 / 2:o3 / 3:co / 4:so2 / 5:pm10
observe.2020 <- observe[observe$month >= '2020-01',]
observe.2020 <- observe.2020[,-1]
rownames(observe.2020) <- c("1월", "2월", "3월", "4월", "5월", "6월",
                          "7월", "8월", "9월", "10월", "11월")

m.res <- pred.arima - observe.2020
saveRDS(m.res, file="./data/monthly_residuals_pm10.rds")
#######################################################################################################
#######################################################################################################
#o3만 - sts

m.a <- data.frame(matrix(ncol = 40, nrow = 11)) # 달마다의 예측값 평균
names(m.a) <- name_ssg

##### 여기 이름만 수정해서 돌려주세용!!!!
arima.mat<-arima.O3
pred.arima <- data.frame(matrix(ncol = 39, nrow = 11))
for ( i in 1:39 ){
  pred.arima[,i] <- data.frame(matrix(mon.avg.sts(arima.mat[[i]])))[-12,]
}
colnames(pred.arima) <- name_ssg[-1]
rownames(pred.arima) <- c("1월", "2월", "3월", "4월", "5월", "6월",
                          "7월", "8월", "9월", "10월", "11월")

# 월 평균 : 예측 - 실제
observe <- monthly.lst[[2]] ##### 여기 숫자 바꿔야 해요....
#1:no2 / 2:o3 / 3:co / 4:so2 / 5:pm10
observe.2020 <- observe[observe$month >= '2020-01',]
observe.2020 <- observe.2020[,-1]
rownames(observe.2020) <- c("1월", "2월", "3월", "4월", "5월", "6월",
                            "7월", "8월", "9월", "10월", "11월")

m.res <- pred.arima - observe.2020
saveRDS(m.res, file="./data/monthly_residuals_o3.rds")

#######################################################################################################
#######################################################################################################






# 구 한번에 보는 코드
for (i in 2:40) {
  k <- (i- 1)
  load(file=paste0("./data/analysis_CO_sgg", k, ".RData"))
  #acc.co.sgg10 <- acc
  co.plot.arima()
  m.a[,i] <- data.frame(mon.avg.arima()[-12])
  names(m.a) <- name_ssg
}
m.result <- monthly.lst[[3]]
m.result - m.a




#######################################################################################################
#######################################################################################################
# 그림 그리는 코드
#######################################################################################################
#######################################################################################################
#CO
co.plot <- function(k) { #그림 1/3/4/5/6
  plot(predict(k), xlim = c(2010, 2021), ylim = c(0,1.1))
  par(new = TRUE)
  plot(test.co.ts[,2], xlim = c(2010,2021), ylim = c(0,1.1), col = "red")
  par(new = FALSE)
}

co.plot.arima <- function() { # 그림 7
  f.pred <- forecast(mod_lst[[7]], 52) 
  plot(f.pred$fitted,xlim = c(2010, 2021), ylim = c(0.2, 1.1))
  par (new = TRUE)
  plot(f.pred$mean,xlim = c(2010, 2021), ylim = c(0.2, 1.1), col = "blue")
  par (new = TRUE)
  plot(test.co.ts[,2], xlim = c(2010, 2021), ylim = c(0.2, 1.1), col = "red")
  #par (new = TRUE)
  #plot(train.co.ts[,2], xlim = c(2010, 2021), ylim = c(0.2, 1.1), col = "red") #2010-2019 실제값
}
co.plot.sts <- function() { # 그림 2
  f.pred <- forecast(mod_lst[[2]], 52) 
  plot(f.pred,xlim = c(2010, 2021), ylim = c(0.2, 1.1))
  par (new = TRUE)
  plot(test.co.ts[,2], xlim = c(2010, 2021), ylim = c(0.2, 1.1), col = "red")
}

par(mfrow = c(2,2)) # 그림 한 번에 슉 보도록!
tbl.co <- data.frame(matrix(ncol = 4, nrow = 6))

load(file="./data/analysis_CO_sgg10.RData") #구로구
acc.co.sgg10 <- acc
tbl.co[,1] <- getAIC(mod_lst)
models[which.min(tbl.co[,1])]
co.plot.arima()
m.a$구로구 <- data.frame("구로구" = mon.avg.arima()[-12])
#arima

load(file="./data/analysis_CO_sgg14.RData") #도산대로
acc.co.sgg14 <- acc
tbl.co[,2] <- getAIC(mod_lst)
models[which.min(tbl.co[,2])]
co.plot.arima()
#arima

load(file="./data/analysis_CO_sgg20.RData") #서초구
acc.co.sgg20 <- acc
tbl.co[,3] <- getAIC(mod_lst)
models[which.min(tbl.co[,3])]
co.plot.arima()
#arima

load(file="./data/analysis_CO_sgg39.RData") #화랑로
acc.co.sgg39 <- acc
tbl4 <- getAIC(mod_lst)
tbl.co[,4] <- getAIC(mod_lst)
models[which.min(tbl.co[,4])]
co.plot.sts()
#sts


# 대로와 구를 볼 때, 대로 CO가 높은 면을 보였었음.
# 구와 대로 모두 감소하는 추세가 있는 것으로 보임
# 이거 유의한 차이인지를 어떻게 확인하지? 
# 최적의 시계열 모형을 각각 적어주어야 하는 것??
########################################################################################################
#SO2
so2.plot <- function(k) { #그림 1/3/4/5/6
  plot(predict(k), xlim = c(2010, 2021), ylim = c(0, 0.012))
  par(new = TRUE)
  plot(test.so2.ts[,2], xlim = c(2010,2021), ylim = c(0, 0.012), col = "red")
  par(new = FALSE)
}
so2.plot.arima <- function() { # 그림 7
  f.pred <- forecast(mod_lst[[7]], 52) 
  plot(f.pred$fitted,xlim = c(2010, 2021), ylim = c(0, 0.012))
  par (new = TRUE)
  plot(f.pred$mean,xlim = c(2010, 2021), ylim = c(0, 0.012), col = "blue")
  par (new = TRUE)
  plot(test.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red")
  #par (new = TRUE)
  #plot(train.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red") #2010-2019 실제값
}
so2.plot.sts <- function() { # 그림 2
  f.pred <- forecast(mod_lst[[2]], 52) 
  plot(f.pred,xlim = c(2010, 2021), ylim = c(0, 0.012))
  par (new = TRUE)
  plot(test.so2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red")
}


par(mfrow = c(2,2))
tbl.so2 <- data.frame(matrix(ncol = 4, nrow = 6))

load(file="./data/analysis_SO2_sgg2.RData") #강남대로
acc.so2.sgg2 <- acc
tbl.so2[,1] <- getAIC(mod_lst)
models[which.min(tbl.so2[,1])]
#sts(2)
so2.plot.sts()

load(file="./data/analysis_SO2_sgg5.RData") #강북구
acc.so2.sgg5 <- acc
tbl.so2[,2] <- getAIC(mod_lst)
models[which.min(tbl.so2[,2])]
#arima
so2.plot.arima()

load(file="./data/analysis_SO2_sgg16.RData") #동작구
acc.so2.sgg16 <- acc
tbl.so2[,3] <- getAIC(mod_lst)
models[which.min(tbl.so2[,3])]
#arima
so2.plot.arima()

load(file="./data/analysis_SO2_sgg36.RData") #청계천로
acc.so2.sgg36 <- acc
tbl.so2[,4] <- getAIC(mod_lst)
models[which.min(tbl.so2[,4])]
#arima
so2.plot.arima()

# 제대로 예측하는 것 같지 않음.. 대체로 일직선으로 예측하는 경향
# 예측한 것 보다 실제가 더 낮음..
# 강북구는 2016년부터 감소가 진행되었던 상황인데, 더욱 줄어든 것으로 보임

# 아 혹시, 2016년에 SO2 관련한 정책이 있나?? 모든 구에서 한번에 떨어지는 것으로 보임. 확인v
########################################################################################################
#NO2
no2.plot <- function(k) { #그림 1/3/4/5/6
  plot(predict(k), xlim = c(2010, 2021), ylim = c(0, 0.08))
  par(new = TRUE)
  plot(test.no2.ts[,2], xlim = c(2010,2021), ylim = c(0, 0.012), col = "red")
  par(new = FALSE)
}
no2.plot.arima <- function() { # 그림 7
  f.pred <- forecast(mod_lst[[7]], 52) 
  plot(f.pred$fitted,xlim = c(2010, 2021), ylim = c(0, 0.08))
  par (new = TRUE)
  plot(f.pred$mean,xlim = c(2010, 2021), ylim = c(0, 0.08), col = "blue")
  par (new = TRUE)
  plot(test.no2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.08), col = "red")
  #par (new = TRUE)
  #plot(train.no2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red") #2010-2019 실제값
}
no2.plot.sts <- function() { # 그림 2
  f.pred <- forecast(mod_lst[[2]], 52) 
  plot(f.pred,xlim = c(2010, 2021), ylim = c(0, 0.08))
  par (new = TRUE)
  plot(test.no2.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.08), col = "red")
}

par(mfrow = c(2,2))
tbl.no2 <- data.frame(matrix(ncol = 4, nrow = 6))

load(file="./data/analysis_NO2_sgg2.RData") #강남대로
acc.no2.sgg2 <- acc
tbl.no2[,1] <- getAIC(mod_lst)
models[which.min(tbl.no2[,1])]
# arima
no2.plot.arima()

load(file="./data/analysis_NO2_sgg17.RData") #동작대로
acc.no2.sgg17 <- acc
tbl.no2[,2] <- getAIC(mod_lst)
models[which.min(tbl.no2[,2])]
# arima
no2.plot.arima()

m.a <- mon.avg.tbats()

load(file="./data/analysis_NO2_sgg24.RData") #신촌로
acc.no2.sgg24 <- acc
tbl.no2[,3] <- getAIC(mod_lst)
models[which.min(tbl.no2[,3])]
# arima
no2.plot.arima()


load(file="./data/analysis_NO2_sgg38.RData") #홍릉로
acc.no2.sgg38 <- acc
tbl.no2[,4] <- getAIC(mod_lst)
models[which.min(tbl.no2[,4])]
# arima
no2.plot.arima()


# 강남대로는 이상치가 많은 데이터였던 것으로 보임.
# 홍릉로는 원래 변동폭이 차이가 없는 데이터
# 예상보다 더 감소한 것으로 보임.
# 홍릉로는 다른 예측과 다른 양상을 보인다. VVVVVVVVVVVVVVVVVVVVVVVVVVVV
########################################################################################################
#PM10
pm10.plot <- function(k) { #그림 1/3/4/5/6
  plot(predict(k), xlim = c(2010, 2021), ylim = c(0, 90))
  par(new = TRUE)
  plot(test.pm10.ts[,2], xlim = c(2010,2021), ylim = c(0, 0.012), col = "red")
  par(new = FALSE)
}
pm10.plot.arima <- function() { # 그림 7
  f.pred <- forecast(mod_lst[[7]], 52) 
  plot(f.pred$fitted,xlim = c(2010, 2021), ylim = c(0, 90))
  par (new = TRUE)
  plot(f.pred$mean,xlim = c(2010, 2021), ylim = c(0, 90), col = "blue")
  par (new = TRUE)
  plot(test.pm10.ts[,2], xlim = c(2010, 2021), ylim = c(0, 90), col = "red")
  #par (new = TRUE)
  #plot(train.pm10.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red") #2010-2019 실제값
}
pm10.plot.sts <- function() { # 그림 2
  f.pred <- forecast(mod_lst[[2]], 52) 
  plot(f.pred,xlim = c(2010, 2021), ylim = c(0, 0.08))
  par (new = TRUE)
  plot(test.pm10.ts[,2], xlim = c(2010, 2021), ylim = c(0, 90), col = "red")
}

par(mfrow = c(2,2))
tbl.pm10 <- data.frame(matrix(ncol = 4, nrow = 6))

load(file="./data/analysis_PM10_sgg10.RData") #구로구
acc.pm10.sgg10 <- acc
tbl.pm10[,1] <- getAIC(mod_lst)
models[which.min(tbl.pm10[,1])]
#arima
pm10.plot.arima()

load(file="./data/analysis_PM10_sgg14.RData") #도산대로
acc.pm10.sgg14 <- acc
tbl.pm10[,2] <- getAIC(mod_lst)
models[which.min(tbl.pm10[,2])]
#arima
pm10.plot.arima()

load(file="./data/analysis_PM10_sgg39.RData") #화랑로
acc.pm10.sgg39 <- acc
tbl.pm10[,3] <- getAIC(mod_lst)
models[which.min(tbl.pm10[,3])]
#arima
pm10.plot.arima()

# 미세먼지 > 도로 구에 상관없이 비슷한 양상, 추세
# 화랑로 미세먼지 이상치 무슨일..

########################################################################################################
#O3
o3.plot <- function(k) { #그림 1/3/4/5/6
  plot(predict(k), xlim = c(2010, 2021), ylim = c(0, 0.05))
  par(new = TRUE)
  plot(test.o3.ts[,2], xlim = c(2010,2021), ylim = c(0, 0.05), col = "red")
  par(new = FALSE)
}
o3.plot.arima <- function() { # 그림 7
  f.pred <- forecast(mod_lst[[7]], 52) 
  plot(f.pred$fitted,xlim = c(2010, 2021), ylim = c(0, 0.05))
  par (new = TRUE)
  plot(f.pred$mean,xlim = c(2010, 2021), ylim = c(0, 0.05), col = "blue")
  par (new = TRUE)
  plot(test.o3.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.05), col = "red")
  #par (new = TRUE)
  #plot(train.o3.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.012), col = "red") #2010-2019 실제값
}
o3.plot.sts <- function() { # 그림 2
  f.pred <- forecast(mod_lst[[2]]) 
  plot(f.pred,xlim = c(2010, 2021), ylim = c(0, 0.05))
  par (new = TRUE)
  plot(test.o3.ts[,2], xlim = c(2010, 2021), ylim = c(0, 0.05), col = "red")
}

par(mfrow = c(2,2))
tbl.o3 <- data.frame(matrix(ncol = 5, nrow = 6))

load(file="./data/analysis_O3_sgg10.RData") #구로구
acc.o3.sgg10 <- acc
tbl.o3[,1] <- getAIC(mod_lst)
models[which.min(tbl.o3[,1])]
# sts
o3.plot.sts()

load(file="./data/analysis_O3_sgg16.RData") #동작구
acc.o3.sgg16 <- acc
tbl.o3[,2] <- getAIC(mod_lst)
models[which.min(tbl.o3[,2])]
# sts
o3.plot.sts()

load(file="./data/analysis_O3_sgg28.RData") #용산구
acc.o3.sgg28 <- acc
tbl.o3[,3] <- getAIC(mod_lst)
models[which.min(tbl.o3[,3])]
# sts
o3.plot.sts()

load(file="./data/analysis_O3_sgg30.RData") #정릉로
acc.o3.sgg30 <- acc
tbl.o3[,4] <- getAIC(mod_lst)
models[which.min(tbl.o3[,4])]
# sts
o3.plot.sts()

load(file="./data/analysis_O3_sgg36.RData") #청계천로
acc.o3.sgg36 <- acc
tbl.o3[,5] <- getAIC(mod_lst)
models[which.min(tbl.o3[,5])]
# sts
o3.plot.sts()

# 오존은 코로나와 상관없이 비슷한 추세를 보임.
# 대로와 구 모두 비슷해보임. 