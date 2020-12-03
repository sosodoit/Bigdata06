#--------------------------------------------------#
#--------------------------------------------------#
#--------------------------------------------------#
# 이거
# https://kuduz.tistory.com/1196
library("dplyr")
library("ggplot2")

library('leaflet')
leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles()

#P <- read.csv("./map/sample.csv", header = TRUE) #시각화할 데이터셋에 위도경도!!!!

#leaflet(P) %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(lng=~long, lat=~lat, color='#006633')
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