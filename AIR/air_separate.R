#******************************************************************************#
#                                 data load
#******************************************************************************#
AIR <- read.csv('./data/airseoul.csv', header = T)[,-1]
AIR$SGG <- factor(AIR$SGG)
AIR$week <- as.Date(AIR$week)
#******************************************************************************#
#                         대기오염물질별 데이터 분리
#******************************************************************************#
library(dplyr)
library(tidyverse)

getAIRS <- function(x,var) {
  kind_ssg <<- as.character(unique(AIR$SGG))
  sgg <- AIR[AIR$SGG==kind_ssg[x],]
  result <- sgg[,c(var)]
  return(data.frame(result))
} 

week <- unique(AIR$week)
down_airdata <- function(air_metric="NO2",var) {
  
  result <- data.frame()
  result <- rbind(getAIRS(1,var),result)
  for( i in 2:length(kind_ssg)){
  result <- cbind(result,getAIRS(i,var))
  }
  
  result <- cbind(week,result)
  names(result) <- c("week","강남구","강남대로","강동구","강변북로","강북구","강서구","공항대로"
                     ,"관악구","광진구","구로구","금천구","노원구","도봉구","도산대로"
                     ,"동대문구", "동작구","동작대로","마포구","서대문구","서초구","성동구"
                     ,"성북구","송파구","신촌로","양천구","영등포구","영등포로","용산구"
                     ,"은평구","정릉로","종로","종로구", "중구","중랑구","천호대로"
                     , "청계천로","한강대로","홍릉로","화랑로")
  
  result %>% write_rds(paste0("./data/air_", air_metric, "_df.rds"))
}

down_airdata("NO2",3)
down_airdata("O3",4)
down_airdata("CO",5)
down_airdata("SO2",6)
down_airdata("PM10",7)

# 데이터 불러오기
# no2 <- readRDS(file = "./data/air_NO2_df.rds")
# o3 <- readRDS(file = "./data/air_O3_df.rds")
# co <- readRDS(file = "./data/air_CO_df.rds")
# so2 <- readRDS(file = "./data/air_SO2_df.rds")
# pm10 <- readRDS(file = "./data/air_PM10_df.rds")
