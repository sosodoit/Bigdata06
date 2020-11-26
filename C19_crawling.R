#install.packages("XML")

library(XML)
urlService = "http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson"

pageNo = "100"
numOfRows = "1000"
startCreateDt = "20200101"

endCreateDt = format(Sys.Date(), "%Y%m%d") #오늘날짜가져오기
API_KEY = "LGnNfWT%2BpLQAaqJnhANrMqIfqDXcdmO2mqxP014xRda1djIWbbwtfIkYYnM1aa9ddBSvGTBUgoEbWA5eAssjrw%3D%3D"

url <- paste0(urlService,
              paste0("?ServiceKey=",API_KEY),
              paste0("&pageNo=",pageNo),
              paste0("&numOfRows=",numOfRows),
              paste0( "&startCreateDt=",startCreateDt),
              paste0("&endCreateDt=",endCreateDt ))

xmlDocument <- xmlTreeParse(url, useInternalNodes=TRUE, encoding="UTF-8")

rootNode <- xmlRoot(xmlDocument)
numofRows <- as.numeric(xpathSApply(rootNode, "//numOfRows", xmlValue))
totalCount <- as.numeric(xpathSApply(rootNode, "//totalCount", xmlValue))
loopCount <- round(totalCount / numofRows , 0)

if(loopCount*numofRows < totalCount) {
  loopCount <- loopCount + 1
}


totalData <- data.frame()
for(i in 1:loopCount) {
  url <- paste0(urlService,
          paste0("?ServiceKey=",API_KEY),
          paste0("&pageNo=",i),
          paste0("&numOfRows=",numOfRows),
          paste0( "&startCreateDt=",startCreateDt),
          paste0("&endCreateDt=",endCreateDt ))

doc <- xmlTreeParse(url, useInternalNodes=TRUE, encoding="UTF-8")
rootNode <- xmlRoot(doc)
xmlData <- xmlToDataFrame(nodes = getNodeSet(rootNode,'//item'))

totalData <- rbind(totalData, xmlData)
}

View(totalData)
write.csv(totalData, "./data/C19/C19data.csv", row.names=FALSE)
#saveRDS(totalData, "./data/C19/C19data.rds", refhook = NULL)
