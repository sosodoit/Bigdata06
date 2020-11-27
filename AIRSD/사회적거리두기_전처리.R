source('./AIR/packages_need.R', encoding='utf-8') #필요 패키지 존재
source('./AIR/Day_sgg_separate.R', encoding='utf-8') # 일일 데이터

#***************************************# 
#       사회적거리두기 변수 추가        #
#***************************************# 
# https://terms.naver.com/entry.nhn?docId=5928099&cid=43667&categoryId=43667
#***************************************# 
# 2020년 3월 22일부터 4월 19일까지 강력한 사회적 거리두기를 시행
# 4월 20일부터 5월 5일까지는 다소 완화한 형태의 사회적 거리두기를 시행
# 이후 코로나19 확산세가 주춤하면서 5월 6일부터는 '생활 속 거리두기' 체계로 전환해 시행
# 6월 28일에는 각종 거리두기의 명칭을 '사회적 거리두기'로 통일
# 정부는 수도권에 대한 강화된 사회적 거리두기 2단계 조치를 8월 19일 0시부터 시행
#  8월 23일 0시부터는 2단계 조치를 전국으로 확대
# 이후 정부는 당초 8월 30일 종료 예정이었던 수도권 사회적 거리두기 2단계 조치를 2.5단계로 강화
# 9월 6일 24시까지 1주간 연장
# 이를 다시 9월 13일까지 1주간 연장
# 9월 14일부터 27일까지 2주간은 거리두기 단계를 하향해 2단계를 시행
#  9월 28일부터 10월 11일까지는 추석 특별방역기간으로 정하고 2단계의 핵심 조처들을 시행
# 정부는 10월 12일부터 전국의 사회적 거리두기 단계를 12일부터 1단계로 조정
#***************************************# 
# 정부, 종교·실내체육·유흥시설 운영중단 강력권고(2020. 3. 22~4. 19)
# 완화된 거리두기, 5월 5일까지 연장
# 생활 속 거리두기, 5월 6일부터 시행
# 서울·경기·인천지역 사회적 거리두기 조치, 2단계 격상(8.16) 및 강화된 조치 시행(8.19)
# 2단계 조치 전국으로 확대(8.23.~9.20.)
# 수도권 사회적 거리두기 2.5단계 연장(9.4.~9.13.)
# 수도권 거리두기, 2.5→ 2단계로 하향 조정(9.14.∼9.27.)
# 전국 사회적 거리두기 1단계로 조정(10. 12~)
# 수도권 지역 사회적 거리두기 1단계 → 1.5단계로 격상(11월 19일부터 시행, 인천은 23일부터)
# 수도권 거리두기 11월 24일 0시부터 2단계로 격상, 호남권은 1.5단계
#***************************************# 


# 1 / 1.5 / 2 / 2.5 / 3
# 이대로 넣는데, 2020년도가 
# 사회적 거리두기를 생각하지 않았던 때는 0으로 넣고,


# 3.22 - 4.19 : 강력한 사회적 거리 (1.5) 지금과는 기준이 달라서 ,, 이름만 강력했음.ㅋㅋ
# 4.20 - 5.05 : 완화된 형태의 사회적 거리 (1)
# 5.06 - 8.15 : 생활 속 거리두기 체계 (1)
# 8.16 - 8.29 : 2
# 8.30 - 9.13 : 2.5
# 9.14 - 9.27 : 2
# 9.28 - 10.11 : 2
# 10.12 - 11.18 : 1
# 11.19 - 11.23 : 1.5
# 11.24 - 11.25 : 2


df2 <- df %>% filter(DATE>="2020-01-01") %>% select(SGG,DATE,NO2,O3,CO,SO2,PM10)
sd <- numeric(nrow(df2))
for (i in 1:nrow(df2)) {
  if (df2$DATE[i] < '2020-03-22') { sd[i] = 0 }
  else if (df2$DATE[i] < '2020-04-20') { sd[i] = 1.5 }
  else if (df2$DATE[i] < '2020-05-06') { sd[i] = 1 }
  else if (df2$DATE[i] < '2020-08-16') { sd[i] = 1 }
  else if (df2$DATE[i] < '2020-08-30') { sd[i] = 2 }
  else if (df2$DATE[i] < '2020-09-14') { sd[i] = 2.5 }
  else if (df2$DATE[i] < '2020-09-28') { sd[i] = 2 }
  else if (df2$DATE[i] < '2020-10-12') { sd[i] = 2 }
  else if (df2$DATE[i] < '2020-11-19') { sd[i] = 1.5 }
  else if (df2$DATE[i] < '2020-11-24') { sd[i] = 2 }
  else {sd[i] = 2}
}
df2 <- cbind(df2, sd)
df2$sd <- factor(df2$sd)

# 사회적 거리두기에 따른 대기 오염의 변화
# 범주형 --> 연속형 측정
# 회귀 가변수 처리

lm.fit <- lm(sd ~ NO2 + O3 + CO + SO2 + PM10, data = df2) # 대기오염을 토대로 한, 사회적 거리두기 예측
glm.fit <- glm(sd ~NO2 + O3 + CO + SO2 + PM10, data = df2)

lm.fit.no2 <- lm(NO2 ~ sd + O3 + CO + SO2 + PM10, data = df2) # 질문 
summary(lm.fit.no2)

ts.sgg1 <- ts(sgg1.te, start = c(2020,1), end = c(2020,330), frequency = 365.25)
plot(window(ts.sgg1, start = c(2020,1), end = c(2020,82)))


window(ts.sgg1, start = c(2020,01,01), end = c(2020,3,22))
plot(window(ts.sgg1, start = c(2020,1,1), end = c(2020,3,22)))

abline()

# 진짜 요소들이 sd랑 연관성이 있는지 알고자 함.






#######################################################

##bagging
library(adabag)

air.bagging <- bagging(sd ~ NO2 + O3 + CO + SO2 + PM10, data=df2, mfinal=20) #mfinal 반복수(모형갯수), 디폴트=100
air.bagging$importance #중요 비율을 보여줌 
plot(air.bagging$trees[[10]]) #trees : 배깅기법으로 만든 의사결정나무 / 10번째 tree 값
text(air.bagging$trees[[10]])

pred <- predict(air.bagging,newdata=iris) #bagging으로 예측
table(pred$class,iris[,5])


##부스팅
boo.adabag <- boosting(sd ~ NO2 + O3 + CO + SO2 + PM10, data=df2, mfinal=20)
boo.adabag$importance
plot(boo.adabag$tree[[8]])
text(boo.adabag$tree[[8]])


pred <- predict(boo.adabag,newdata=iris)
tb <- table(pred$class, iris[,5])
tb



#오분류율
1-(sum(diag(tb))/sum(tb))

##nnet함수로 분석

install.packages('ada')

library(ada)

iris <- iris[iris$Species!="setosa",]

n <-dim(iris)[1]

trind <-sample(1:100, floor(.6*100), FALSE)

teind <- setdiff(1:100, trind) #set difference 차집합

iris[,5]<-as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1])

gdis <- ada(Species~., data=iris[trind,],iter=20,nu=1,type='discrete') #iter 학습횟수 nu부스팅을 위한 축소모수

gdis <- addtest(gdis,iris[teind,-5],iris[teind,5])

gdis

plot(gdis,TRUE,TRUE)

pairs(gdis,iris[trind,-5],maxvar=4)





##랜덤포레스트
library(randomForest)
t_index=sample(1:nrow(),size=nrow(stagec)*0.7)

train=stagec[t_index,]

test=stagec[-t_index,]

rf <- randomForest(ploidy~.,data=train,ntree=100,proximity=TRUE)

table(predict(rf),train$ploidy)

print(rf)

plot(rf)

importance(rf)

varImpPlot(rf)



rf.pred <- predict(rf,newdata=test)

table(rf.pred,test$ploidy)



plot(margin(rf)) #마진이랑 ROC랑 비슷

library(e1071)

library(Epi)

ROC(test=rf.pred,stat=test$ploidy,plot="ROC",AUC=T)

install.packages('party')

library(party)



#랜덤포레스트는 party 패키지의 cforest()로도 이용할 수 있음.

set.seed(1234) #set.seed 랜덤 고정

cf <- cforest(ploidy~.,data=train)

cf.pred <- predict(cf,newdata=test, OOB=T, type='response')




