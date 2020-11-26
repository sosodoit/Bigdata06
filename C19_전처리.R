# 시군구를 서울홈페지서 대강 넣어볼까;;
C19 <- read.csv(file = './data/C19/C19data.csv')[,-c(1,5,6,11,13,15)]
glimpse(C19)
#std_day 기준일시
#local_occ_cnt 지역발생수
#def_cnt 확진자수
#deathcnt 사망자수
#isol_ing_cnt 격리중 환자수
#isol_clear_cnt 격리해제수
#inc_dec 전일대비 증감수
#qur_rate 10만명당 발생률
#gubun 시도명

df = C19 %>% filter(gubun=="서울")
df = df[,-3]
df$DATE <- sub("00시", "",df$stdDay)
df$DATE <- as.Date(df$stdDay, format="%Y년%m월%d일")
df = df[,-8]
C19 <- df[,c(8,6,2,1,5,4,3,7)]
C19 <- C19 %>% arrange(DATE)

write.csv(C19, "./data/C19/C19seoul.csv")

###################################################################
###update : 2020년 코로나 데이터 병합##############################
x <- read.csv("./data/totaldata.csv")[,-c(1,9)]
x$DATE <- as.Date(as.character(x$DATE), format="%Y%m%d")

C19 <- read.csv(file = './data/C19/C19seoul.csv')[,-1]
C19$DATE <- as.Date(C19$DATE)

mergedf <- data.frame(x) %>% filter(DATE >= "2020-01-01")
mergedf <- merge(x=mergedf, y=C19,by="DATE")

#write.csv(mergedf, "./data/C19/C19AIR20_day.csv")

mergedf$week <- cut(mergedf$DATE, breaks="week")
mergedf$week <- as.Date(as.character(mergedf$week), format="%Y-%m-%d")

# 공통으로 존재하는 측정소만 가져옴
sgg <- as.vector(unique(df16$SGG))

mergedf$SGG <- as.character(mergedf$SGG)
mergedf2 <- mergedf[mergedf$SGG %in% sgg, ]
mergedf$SGG <- as.factor(mergedf2$SGG)

# 주단위
mergedf2.week <- mergedf2 %>% 
  group_by(SGG, week) %>% 
  summarise(NO2=mean(NO2, na.rm=T),
            O3=mean(O3, na.rm=T),
            CO=mean(CO, na.rm=T),
            SO2=mean(SO2, na.rm=T),
            PM10=mean(PM10, na.rm=T), 
            localOccCnt=mean(localOccCnt, na.rm=T),
            defCnt=mean(defCnt, na.rm=T),
            deathCnt=mean(deathCnt, na.rm=T),
            isolIngCnt=mean(isolIngCnt, na.rm=T),
            isolClearCnt=mean(isolClearCnt, na.rm=T),
            incDec=mean(incDec, na.rm=T),
            qurRate=mean(qurRate, na.rm=T),.groups = 'drop')

#write.csv(mergedf2.week, "./data/C19/C19AIR20_week.csv")
###################################################################
###################################################################

