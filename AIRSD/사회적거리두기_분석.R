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


## 고려1
table(AIRSD$SD) # 종속변수(순서형) 빈도 확인
summary(AIRSD[,3:7]) #연속형 변수 분포 확인

# no2에 따른 SD 분포 확인
ggplot(AIRSD, aes(x = SD , y = NO2)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(~ SGG, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



## 변수의 유의성 판단
# coefficient가 0라는 귀무가설 하에서 coef ~ N(0, sd^2)라는걸 이용하여 z-test
# z <- summary(model1)$coefficients/summary(model1)$standard.errors
# z
# 2-tailed z test
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p

head(AIRSD)






