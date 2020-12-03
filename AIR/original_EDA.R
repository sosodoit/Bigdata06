source('./AIR/sgg_separate.R', encoding='utf-8')
source('./AIR/packages_need.R', encoding='utf-8')

#***************************************# 
#             데이터로드                #
#***************************************# 
# AIRSEOUL <== 대기분석 이거 쓸
setwd("C:\\Users\\hanso\\Desktop\\빅통분\\gitgit")

AIR <- read.csv('./data/airseoul.csv', header = T)[,-1]
AIR$SGG <- factor(AIR$SGG)
AIR$week <- as.Date(AIR$week)


#***************************************# 
#           기본 시계열 분해            #
#***************************************# 
#***************************************# 
#     forecast:: 계절변동 시각화        #
#***************************************# 
# x1 <- AIR %>% group_by(SGG) %>% select(NO2)
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
kind_ssg <-unique(AIR$SGG)
for (i in kind_ssg) {
  plot(AIR[AIR$SGG == i,]$NO2, type = "l")
}
#***************************************# 
p=ggplot(AIR, aes(x=week,y=NO2))+
  geom_line(mapping=aes(x=week,y=NO2,color=NO2))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::NO2 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(AIR, aes(x=week,y=O3))+
  geom_line(mapping=aes(x=week,y=O3,color=O3))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::O3 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(AIR, aes(x=week,y=CO))+
  geom_line(mapping=aes(x=week,y=CO,color=CO))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::CO 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(AIR, aes(x=week,y=SO2))+
  geom_line(mapping=aes(x=week,y=SO2,color=SO2))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::SO2 현황")+
  ggthemes::theme_hc()
#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")

p=ggplot(AIR, aes(x=week,y=PM10))+
  geom_line(mapping=aes(x=week,y=PM10,color=PM10))+
  facet_wrap(~SGG)+
  labs(title="구별 대기질::PM10 현황")+
  ggthemes::theme_hc()

#추세선 그리기
p + stat_smooth(color = "yellow", method = "loess")


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
train = AIR[AIR$week < '2020-01-06',]
test = AIR[AIR$week >= '2020-01-06',]
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
#sgg1 == 강남구
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
