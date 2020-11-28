#******************************************************************************#
#                                 필수 load
#******************************************************************************#
source('packages_need.R', encoding='utf-8')

#******************************************************************************#
#                 전처리를 위한 "크롤링 dataload" 및 "type setting"
#******************************************************************************#
#2010년 ~ 2020년 총 10년간 일별평균대기오염 데이터
#2010년 ~ 2018년
airdata0 = data.frame()
for(i in 2010:2018) {
  
  path = paste0("./data/일별평균대기오염도_", i, ".xlsx")
  data = read_excel(path)
  airdata0 = rbind(airdata0, data)
  
}
names(airdata0) <- c("DATE", "SGG", "NO2", "O3", "CO", "SO2","PM10","PM25")

#2019년
data1 <- read.csv(file="./data/일별평균대기오염도_2019.csv", encoding = "utf-8")
names(data1) <- c("DATE", "SGG", "NO2", "O3", "CO", "SO2","PM10","PM25")

#2020년 11월25일
data2 <- read.csv(file="./data/일별평균대기오염도_2020.csv", encoding = "UTF-8")[,-1]
names(data2) <- c("DATE", "SGG", "NO2", "O3", "CO", "SO2","PM10","PM25")

#2010년~2020년 totaldata
airdata0 = rbind(airdata0, data1, data2)

write.csv(airdata0, "./data/totaldata.csv")
#******************************************************************************#
#                             결측치 유무 확인
#                       1) 유무 확인 - 기본, naniar 활용
#                       2) 시각화 - naniar, VIM 활용
#                       3) 결측치 대체 - dplyr 활용
#******************************************************************************#
# totaldata 전체 결측치
gg_miss_var(airdata0)
table(is.na(airdata0)) # 53282 결측

# 변수별 결측치
x <- airdata0
for(i in 1:length(x)){
  na = sum(is.na(x[,i]))
  cat("\n", i, "번째 변수의","결측치 합 =", na)
}

# 8번째 변수 PM25의 결측치 합 = 46287, 과거에는 초미세먼지 측정이 없어서 많은 결측치가 있음을 확인하였다. 우리 연구는 과거데이터가 필요하고 PM10이 있으므로 연구 목적에는 변함이 없다고 판단하여 PM25 변수는 제거하기로 하였다.
x <- x[,-8]; cat("\n")

# 전체
gg_miss_var(x, show_pct = T) # 누락비율표시
table(is.na(x)) # 6995, PM25결측치가 매우 많았음을 알 수 있다.
# 측정소별
gg_miss_var(x, facet = SGG) + labs(y = "Look at all the missing ones")

# 각 행별로 NA가 포함된 행인지 아닌지를 반환
# NA가 있을 경우 FALSE, is.na()와는 반대
# NA가 있는 행의 개수 확인
# sum(!complete.cases(x)) # 3189

# 각 행 혹은 변수별로 NA값이 얼마나 있는지 UpSetR로 패턴 탐색
gg_miss_upset(x)

# NO2, O3, SO2, CO, PM10 고려하는 모든 피처에 결측값이 있다.
# 센서데이터는 민감한 데이터로 대기오염 측정 센서의 오작동으로 결측치가 종종 발생된 것으로 생각된다. 이에 대하여 전후 5일 간(5주 간)의 평균 값으로 결측치를 대체하였다.
# 5개 피처 모두 결측값을 가지는 경우가 696행이 있다.

# x <- airdata0
# 일별단위에서 주단위로 변경 후 결측치 대체

# x$NO2 %>% mutate(NO2 = ifelse(is.na(NO2), mean(NO2, na.rm = TRUE), NO2))
# NO2 변수의 값이 na일 경우 평균으로, 아닐 경우 그대로 유지한다는 코드(사용x)
#******************************************************************************#
#                               결측치 처리
#                 SGG 통일 시킨 후에 일일 혹은 주단위 변경
#******************************************************************************#
# 일별데이터 처리시
# x$DATE <- as.Date(as.character(x$DATE), format="%Y%m%d")
# df10 <- x %>% filter(DATE < "2011-01-03")
# df11 <- x %>% filter(DATE >= "2011-01-03" & DATE < "2012-01-02")
# df12 <- x %>% filter(DATE >= "2011-01-03" & DATE < "2013-01-07")
# df13 <- x %>% filter(DATE >= "2013-01-07" & DATE < "2014-01-06")
# df14 <- x %>% filter(DATE >= "2014-01-06" & DATE < "2015-01-05")
# df15 <- x %>% filter(DATE >= "2015-01-05" & DATE < "2016-01-04")
# df16 <- x %>% filter(DATE >= "2016-01-04" & DATE < "2017-01-02")
# df17 <- x %>% filter(DATE >= "2017-01-02" & DATE < "2018-01-01")
# df18 <- x %>% filter(DATE >= "2018-01-01" & DATE < "2019-01-07")
# df19 <- x %>% filter(DATE >= "2019-01-07" & DATE < "2020-01-06")
# df20 <- x %>% filter(DATE >= "2020-01-06" & DATE <= "2020-11-25")
#******************************************************************************#
#주단위데이터 처리시
x$week <- cut(x$DATE, breaks="week")
x$week <- as.Date(as.character(x$week), format="%Y-%m-%d")
df10 <- x %>% filter(week < "2011-01-03")
df11 <- x %>% filter(week >= "2011-01-03" & week < "2012-01-02")
df12 <- x %>% filter(week >= "2011-01-03" & week < "2013-01-07")
df13 <- x %>% filter(week >= "2013-01-07" & week < "2014-01-06")
df14 <- x %>% filter(week >= "2014-01-06" & week < "2015-01-05")
df15 <- x %>% filter(week >= "2015-01-05" & week < "2016-01-04")
df16 <- x %>% filter(week >= "2016-01-04" & week < "2017-01-02")
df17 <- x %>% filter(week >= "2017-01-02" & week < "2018-01-01")
df18 <- x %>% filter(week >= "2018-01-01" & week < "2019-01-07")
df19 <- x %>% filter(week >= "2019-01-07" & week < "2020-01-06")
df20 <- x %>% filter(week >= "2020-01-06" & week <= "2020-11-25")

# unique(df10$SGG) # 40
# unique(df11$SGG) # 40
# unique(df12$SGG) # 40
# unique(df13$SGG) # 40
# unique(df14$SGG) # 40
# unique(df15$SGG) # 40
# unique(df16$SGG) # 39 홍지문 없어짐
# unique(df17$SGG) # 39 홍지문 없어짐
# unique(df18$SGG) # 46 new : 관악산 궁동 남산 북한산 세곡 행주 시흥대로
# unique(df19$SGG) # 50 new : 마포아트센터 서울숲 올림픽공원 자연사박물관
# unique(df20$SGG) # 50
# which(unique(df19$SGG) == unique(df20$SGG))
#******************************************************************************#
# 공통으로 존재하는 측정소만 가져옴
sgg <- as.vector(unique(df16$SGG))

x$SGG <- as.character(x$SGG)
x2 <- x[x$SGG %in% sgg, ]
x2$SGG <- as.factor(x2$SGG)
#******************************************************************************#
#주단위데이터 처리시
x2.week <- x2 %>%
  group_by(SGG, week) %>%
  summarise(NO2=mean(NO2, na.rm=T),
            O3=mean(O3, na.rm=T),
            CO=mean(CO, na.rm=T),
            SO2=mean(SO2, na.rm=T),
            PM10=mean(PM10, na.rm=T), .groups = 'drop')
#******************************************************************************#
# 없는 날짜 채우기
# 주단위데이터 처리시
df <- x2.week
#******************************************************************************#
# 일별데이터 처리시
df <- x2
#******************************************************************************#
original <- seq(as.Date("2010-01-01"), as.Date("2020-11-25"), by = "days")
for (i in 1:length(sgg)) {
  
  missing = as.Date(setdiff(original, unique(df[df$SGG==sgg[i],]$DATE)), origin = "1970-01-01")
  d = data.frame(cbind(as.Date(missing, origin='1970-1-1'),as.character(sgg[i]), NA, NA, NA, NA, NA))
  names(d) <- c("DATE", "SGG", "NO2", "O3", "CO", "SO2", "PM10")
  d[,1] <- as.character(as.Date(as.integer(d[,1]), origin = '1970-1-1'))
  #d[,2] <- as.Date(d[,2])
  df = rbind(df, d)
}
#******************************************************************************#
# 정렬 후 값 채우기
df <- data.frame(df[order(df$DATE, df$SGG),])
df[,3:7] <- as.numeric(unlist(df[,3:7]))
#******************************************************************************#
#결측치 평균 대체
x2.week <- df
df2 <- df
num <- vector()
for(i in 3:7){
  indi <- which(is.na(df2[,i]))
  for (j in indi){
    se <- (j-5):(j+5)
    
    for(k in 1:length(se)) {
      num[k] <- df2[se[k],i]
    }
    df2[j,i] <- mean(unlist(num), na.rm=T)
    #print(x2.week[j,i])
  }
}
sum(is.na(df2))
#******************************************************************************#
#이상치 대체
dig <- diagnose_numeric(df2)
min <- max <- numeric()
for(i in 1:nrow(dig)){
  min[i] <- dig[i,3] - IQR(df2[[i+2]])*1.5
  max[i] <- dig[i,6] + IQR(df2[[i+2]])*1.5
}

st1 <- st2 <- numeric()
for(i in 3:7){
  for(j in 1:nrow(df2)) {
    
    if(df2[j,i] < min[[i-2]]) {
      df2[j,i] = min[[i-2]]; st1<-c(st1,j)}
    else if(df2[j,i] > max[[i-2]]) {
      df2[j,i] = max[[i-2]]; st2<-c(st2,j)}
    
  }
}
#******************************************************************************#
# write.csv(df2, "./data/airseoul_day.csv") # daily
write.csv(df2, "./data/airseoul.csv") # week
