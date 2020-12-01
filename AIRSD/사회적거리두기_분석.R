load( "./data/사회적거리두기_전처리.RData" )

#*********************************미완*****************************************#
# 참고 https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# 고려1 y를 순서가 있는 순서형 척도로 보고 순서형 (다항) 로지스틱 회귀분석
# 고려2 y를 순서가 없는 명목형 척도로 보고 명목형 (다항) 로지스틱 분석
# 고려3 y를 1,2,3,4,5로 놓고 다중회귀분석
# 고려4 각 물질만을 독립변수로하여 ANOVA
# 
# 해당 데이터에 대해 위 분석 방법들이 가정에 맞는지 확인. 

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

head(AIRSD)

## 고려1
table(AIRSD$SD) # 종속변수(순서형) 빈도 확인
summary(AIRSD[,3:7]) #연속형 변수 분포 확인

# no2에 따른 SD 분포 확인
ggplot(AIRSD, aes(x = SD , y = NO2)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(~ SGG, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

model1 <- polr(SD ~ NO2 + O3 + CO + SO2 + PM10, data = AIRSD, Hess=T)
# Hess=TRUE는 optimization을 할 때 사용하는 Hessian matrix
summary(model1)
exp(coef(model1)) # odds ratio

step(model1, scale = 0,
     direction = c("both", "backward", "forward"),
     trace = 1, keep = NULL, steps = 1000, k = 2)


## 변수의 유의성 판단
# coefficient가 0라는 귀무가설 하에서 coef ~ N(0, sd^2)라는걸 이용하여 z-test
# z <- summary(model1)$coefficients/summary(model1)$standard.errors
# z
# 2-tailed z test
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p

model2 <- glm(SD ~ NO2 + O3 + CO + SO2 + PM10 + SGG, data = AIRSD, family = "binomial")
summary(model2)

model3 <- glm(SD ~ NO2 + O3 + CO + SO2 + PM10, data = AIRSD, family = "binomial")
summary(model3)

confint(model3)
confint.default(model3)
exp(coef(model3))

install.packages("aod")
library(aod)
#[b:회귀계수, Sigma:error term의 공분산행렬, Terms:rank변수가 있는 열]
wald.test(b = coef(model3), Sigma = vcov(model3), Terms = 4:6)

#************************XXXXXXXXXXXXXXXXXXXXXXXXX**************************#
#다른 변수들을 고정하고 사거가 변할 때 예측값의 변화

model4 <- glm(SD ~ NO2, data = AIRSD, family = "binomial")
summary(model4)
no2.mean = AIRSD %>% group_by(DATE) %>% summarise(mean(NO2))
sd = unique(AIRSD$SD)
newdata1 <- with(AIRSD, data.frame(NO2 = no2.mean, rank = sd))
#newdata1$prob <- predict(model4, newdata = newdata1, type = "response") # probability 점추정값

#******************************************************************************#
# 사회적 거리두기에 따른 대기 오염의 변화
# 범주형 --> 연속형 측정
# 회귀 가변수 처리

#오류
lm.fit <- lm(sd ~ NO2 + O3 + CO + SO2 + PM10, data = AIRSD) # 대기오염을 토대로 한, 사회적 거리두기 예측
summary(lm.fit)
glm.fit <- glm(sd ~NO2 + O3 + CO + SO2 + PM10, data = AIRSD)
summary(glm.fit)

lm.fit.no2 <- lm(NO2 ~ sd + O3 + CO + SO2 + PM10, data = AIRSD) # 질문 
summary(lm.fit.no2)
#사회적 거리두기가 NO2를 예측하는데에 있어 유의해보임.

ts.sgg1 <- ts(sgg1.te, start = c(2020,1), end = c(2020,330), frequency = 365.25)
plot(window(ts.sgg1, start = c(2020,1), end = c(2020,82)))


#오류
window(ts.sgg1, start = c(2020,01,01), end = c(2020,3,22))
plot(window(ts.sgg1, start = c(2020,1,1), end = c(2020,3,22)))

abline()

# 진짜 요소들이 sd랑 연관성이 있는지 알고자 함.

#******************************************************************************#

##bagging
library(adabag)

air.bagging <- bagging(sd ~ NO2 + O3 + CO + SO2 + PM10, data=AIRSD, mfinal=20) #mfinal 반복수(모형갯수), 디폴트=100
air.bagging$importance #중요 비율을 보여줌 
plot(air.bagging$trees[[10]]) #trees : 배깅기법으로 만든 의사결정나무 / 10번째 tree 값
text(air.bagging$trees[[10]])

pred <- predict(air.bagging,newdata=iris) #bagging으로 예측
table(pred$class,iris[,5])


##부스팅
boo.adabag <- boosting(sd ~ NO2 + O3 + CO + SO2 + PM10, data=AIRSD, mfinal=20)
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


