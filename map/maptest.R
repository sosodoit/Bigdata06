# https://kuduz.tistory.com/1196
library("dplyr")
library("ggplot2")
library('leaflet')

source('./AIR/predict_functions.R', encoding='utf-8')
getData()
df <- read.csv('./data/airseoul_day.csv', header = T)[,-1]
df$SGG <- factor(df$SGG)
df$DATE <- as.Date(df$DATE)

station = data.frame(
  SGG = c("강남구","강남대로","강동구","강변북로","강북구","강서구","공항대로"
          ,"관악구","광진구","구로구","금천구","노원구","도봉구","도산대로"
          ,"동대문구", "동작구","동작대로","마포구"),
  
  lat = c(37.51771227466781, 37.48205688050086, 37.545137798835405, 37.64809045641726,37.5389553, 37.5445777,37.5742336,
          37.4873547, 37.5471803, 37.4966295, 37.4523531, 37.6574151,37.6541361,37.5143944,
          37.5684189, 37.4809275,37.4809262,37.5555904),
  
  long = c(127.0479158179489, 127.03578681301144, 127.13688455730143, 127.0394524, 127.01192351237547, 126.8332219, 126.8143684,
           126.9249133, 127.0903042, 126.887383, 126.9060964,127.0656876,127.0268011,127.015736,
           127.0324106, 126.9694092, 126.9540883,126.9033715)
)

df2 <- merge(df,station,by="SGG")
df2 <- df2[order(df2$DATE),]
df2 <- df2[,c(1,8,9,3,4,5,6,7)]
  
pal1 <- colorFactor("viridis", df2$SGG) # 측정소마다 다른 색
pal2 <- colorNumeric("viridis", df2$NO2) # 숫자크기마다 다른 색

leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles()

leaflet(df2) %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addProviderTiles('CartoDB.Positron') %>% #CartoDB.PositronNoLabels, Stamen.Toner
  addCircles(lng=~long, lat=~lat, color=~pal1(SGG))

leaflet(df2) %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addProviderTiles('CartoDB.Positron') %>% #CartoDB.PositronNoLabels, Stamen.Toner
  addMarkers(lng=~long, lat=~lat, label=~SGG) # AD



#--------------------------------------------------#
#--------------------------------------------------#
# saveRDS(df2, file="./data/station.rds")
# rm(list=ls())
#--------------------------------------------------#
#--------------------------------------------------#
#--------------------------------------------------#
#--------------------------------------------------#
library("dplyr")
library("ggplot2")
install.packages("map")
library("maps")

# https://m.blog.naver.com/PostView.nhn?blogId=goldenezkang&logNo=220061647790&proxyReferer=https:%2F%2Fwww.google.com%2F

kr_map <- map_data("world", region = "South Korea")
ggplot(kr_map, aes(x=long, y=lat, group=group, fill=region)) + geom_polygon(colour="black") + scale_fill_brewer(palette="Set2")

data(world.cities)
kr.pop <- world.cities[world.cities$country.etc %in% "Korea South",]
kr.pop <- kr.pop[order(kr.pop$pop),]
kr.pop <- tail(kr.pop,10)
kr.pop <- kr.pop[-10,]

ggplot() + 
  geom_polygon(data=kr_map, aes(x=long, y=lat, group=group, fill=region),colour="black") + 
  scale_fill_brewer(palette="Set5")+ geom_point(data=kr.pop, aes(x=long, y=lat, size = pop), shape = 16, color = "green", alpha = 0.4)

#--------------------------------------------------#
# https://statkclee.github.io/spatial/geo-info-choropleth.html

library("dplyr")

library("ggplot2")
library("ggmap")
library("sp")
library("maptools")

library(tidyverse)
install.packages("sf")
library(sf)

korea_map_shp <- sf::st_read("./map/CTPRVN.shp")

## 인코딩 변경
korea_map_shp$CTP_KOR_NM <- iconv(korea_map_shp$CTP_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)

## 좌표계 변경: 웹 메르카도(web mercator)
korea_map_shp <- sf::st_transform(korea_map_shp, "+proj=longlat +datum=WGS84")

## 시각화 
korea_map_shp %>% 
  select(CTP_KOR_NM) %>% 
  plot()

summary(korea_map_shp)
#--------------------------------------------------#
#--------------------------------------------------#
#korea_map = fortify(korea_map_shp)

ggplot(data = korea_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "#FFFFFF", colour = "#000000")

ggplot(data = korea_map, aes(x = long, y = lat, group = group, colour = id)) + 
  geom_polygon(fill = "#FFFFFF")

#--------------------------------------------------#
korea_map_shp@data
slotNames(korea_map_shp)
data_slot = korea_map_shp@data
korea_map = fortify(korea_map_shp)

ggplot(data = korea_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "#FFFFFF", colour = "#000000")

#--------------------------------------------------#
install.packages("rgdal")
library(rgdal)
P <- read.csv("./map/sample.csv", header = TRUE) #시각화할 데이터셋
map <- spTransform(korea_map_shp, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
#slotNames(map)
new_map <- fortify(map)
View(new_map)
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
P_merge <- merge(seoul_map, P, by='id')

ggplot() + 
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')


plot <- ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = A))
plot + scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") 
+ theme_bw() + labs(title = "서울시 A 분포") 
+ theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# https://mrkevinna.github.io/R-%EC%A7%80%EB%8F%84-%EC%8B%9C%EA%B0%81%ED%99%94-2/
# https://statkclee.github.io/spatial/geo-info-choropleth.html



# station = data.frame(
#   SGG = c("강남구","강남대로","강동구","강변북로","강북구","강서구","공항대로"
#           ,"관악구","광진구","구로구","금천구","노원구","도봉구","도산대로"
#           ,"동대문구", "동작구","동작대로","마포구","서대문구","서초구","성동구"
#           ,"성북구","송파구","신촌로","양천구","영등포구","영등포로","용산구"
#           ,"은평구","정릉로","종로","종로구", "중구","중랑구","천호대로"
#           , "청계천로","한강대로","홍릉로","화랑로"),
#   
#   lat = c(37.51771227466781, 37.48205688050086, 37.545137798835405, 37.64809045641726,37.5389553, 37.5445777,37.5742336,
#           37.4873547, 37.5471803, 37.4966295, 37.4523531, 37.6574151,37.6541361,37.5143944,
#           37.5684189, 37.4809275,37.4809262,37.5555904,),
#   
#   long = c(127.0479158179489, 127.03578681301144, 127.13688455730143, 127.0394524, 127.01192351237547, 126.8332219, 126.8143684,
#            126.9249133, 127.0903042, 126.887383, 126.9060964,127.0656876,127.0268011,127.015736,
#            127.0324106, 126.9694092, 126.9540883,126.9033715,)
# )