#******************************************************************************#
#                                 필요한 패키지
#******************************************************************************#
# 데이터 로드
library(readxl)

# 데이터 EDA
library(dplyr)
library(naniar)
library(VIM)
library(dlookr) # 이상치
library(reshape2)

# 주로 분석용
library(tidyverse)
library(lubridate)
library(xts)
library(forecast)
library(mgcv)
library(forecast)
#library(devtools)
#devtools::install_github("cardiomoon/ggGam")
#library(ggGam)

# 주로 시각화용
library(ggplot2)  #정적 시계열 시각화시
library(dygraphs) #동적 시계열 시각화시
library(ggpmisc)
#library(DT)
#******************************************************************************#
