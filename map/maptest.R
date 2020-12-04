# https://kuduz.tistory.com/1196
#https://cfss.uchicago.edu/notes/leaflet/
#https://rstudio.github.io/leaflet/map_widget.html
#https://bookdown.org/robinlovelace/geocompr/spatial-class.html#raster-data

library("dplyr")
library("ggplot2")
library('leaflet')

source('./AIR/predict_functions.R', encoding='utf-8')
getData()

so2 <- readRDS('./data/monthly_residuals_SO2.rds')

df <- matrix(ncol=11,nrow=39)
for (i in 1:39) {
  df[i,] <- so2[,i]*100000
}

df <- as.data.frame(df)
colnames(df) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov")
df <- cbind(SGG=name_ssg[-1],df)

station = data.frame(
  SGG = c("강남구","강남대로","강동구","강변북로","강북구","강서구","공항대로"
          ,"관악구","광진구","구로구","금천구","노원구","도봉구","도산대로"
          ,"동대문구", "동작구","동작대로","마포구","서대문구","서초구","성동구"
          ,"성북구","송파구","신촌로","양천구","영등포구","영등포로","용산구"
          ,"은평구","정릉로","종로","종로구", "중구","중랑구","천호대로"
          , "청계천로","한강대로","홍릉로","화랑로"),
  
  lat = c(37.51771227466781, 37.48205688050086, 37.545137798835405, 37.64809045641726,37.5389553, 37.5445777,37.5742336,
          37.4873547, 37.5471803, 37.4966295, 37.4523531, 37.6574151,37.6541361,37.5143944,
          37.5684189, 37.4809275,37.4809262,37.5555904,37.5937254,37.504543,37.5420391,
          37.6067725, 37.5026836,37.55501, 37.5252425,37.526344, 37.5263436, 37.540033,
          37.6104898,37.6035773,37.5710071,37.5720164,37.5642189,37.584868,37.5345222,
          37.5451722,37.5451777,37.5804857,37.6178126),
  
  long = c(127.0479158179489, 127.03578681301144, 127.13688455730143, 127.0394524, 127.01192351237547, 126.8332219, 126.8143684,
           126.9249133, 127.0903042, 126.887383, 126.9060964,127.0656876,127.0268011,127.015736,
           127.0324106, 126.9694092, 126.9540883,126.9033715,126.9475606,126.9923083,127.0474968,
           127.0251684, 127.090327, 126.9341113, 126.8539847, 126.8940673, 126.8875012, 127.0026613,
           126.9314111,127.023819,126.9936949,127.0028188,126.9728733,127.0917968,127.1372589,
           126.9370778,126.9633425,127.0422425,127.0728885)
)


df2 <- merge(station,df,by="SGG")
df2$SGG <- factor(df2$SGG, levels = name_ssg[-1])
library(ggthemes)

devtools::install_github("gadenbuie/ggpomological")

bins <- seq(-200,550,by=50)
#pal <- colorBin("orange.pal", domain = df2$Sep, bins = bins)
pal1 <- colorNumeric(c("red", "blue"), bins)
# pal2 <- colorNumeric("Set1", df2$Jan) # 숫자크기마다 다른 색

# leaflet(df2) %>%
#   setView(lng=126.9784, lat=37.566, zoom=11) %>%
#   addProviderTiles('Stamen.Toner') %>%  
#   addCircles(lng=~long, lat=~lat, color=~pal(Sep), radius=2000)

df2 %>% mutate(popup = str_c("<strong>", SGG, "</strong>",
                             "<br/>",
                             "9월평균SO2: ", round(Sep,2)) %>% map(htmltools::HTML)) %>% 
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>% 
  addProviderTiles('Stamen.Toner') %>%
  addCircleMarkers(label = ~popup, radius =35, color=~pal1(Sep),
                   lng = ~long, lat = ~lat, weight = 0)


# leaflet(df2) %>%
#   setView(lng=126.9784, lat=37.566, zoom=11) %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addMarkers(lng=~long, lat=~lat, label=~SGG)

previewColors(colorNumeric("Blues", domain = NULL), sort(rexp(16)))
previewColors(colorFactor("RdYlBu", domain = NULL), LETTERS[1:5])

