#海象數值模式預報資料-鄉鎮預報之波流預報資料
library(jsonlite)

# 讀取網路檔案
urlMB = "https://opendata.cwb.gov.tw/fileapi/v1/opendataapi/M-B0077-001?Authorization=CWB-00565823-C77E-42C5-AE08-D0C1DD38B2C7&downloadType=WEB&format=JSON"
dataMB = fromJSON(txt=urlMB)


names(dataMB$cwbdata$resources$resource)

##欄位名
colNames = dataMB$cwbdata$resources$resource$metadata$weatherElements$weatherElement$tagName
##地區名
locationNameMB=dataMB$cwbdata$resources$resource$data$marinePointForecasts$marinePointForecast$locationName
table(locationNameMB)
  
  
typeof(locationNameMB)

#====================================================
#資料框
MB = dataMB$cwbdata$resources$resource$data$marinePointForecasts$marinePointForecast
str(MB)   #資料結構
names(MB) #欄位名



#以下項目改成數值資料
#MB$dataTime = as.numeric(MB$dataTime)
MB$waterLevel = as.numeric(MB$waterLevel)
MB$salinity = as.numeric(MB$salinity)
MB$currentSpeedU = as.numeric(MB$currentSpeedU)
MB$currentSpeedV = as.numeric(MB$currentSpeedV)
MB$currentSpeed = as.numeric(MB$currentSpeed)
MB$currentDirection = as.numeric(MB$currentDirection)
MB$significantWaveHeight = as.numeric(MB$significantWaveHeight)
MB$wavePeriod = as.numeric(MB$wavePeriod)
MB$waveLength = as.numeric(MB$waveLength)
MB$waveDirection = as.numeric(MB$waveDirection)
MB$windSpeedU = as.numeric(MB$windSpeedU)
MB$windSpeedV = as.numeric(MB$windSpeedV)
str(MB) 

#年,月,日,時間(幾點)
MB$MByear = as.numeric(substr(MB$dataTime,1,4))
MB$MBmonth = as.numeric(substr(MB$dataTime,6,7))
MB$MBday = as.numeric(substr(MB$dataTime,9,10))
MB$MBhour = as.numeric(substr(MB$dataTime,12,13))

MB$date = as.character(substr(MB$dataTime,1,10))

names(MB)
str(MB)



#=========================================

#目前時間(幾點)
Sys.time()
nowhour = format(Sys.time(),"%H")


filter(MB, (locationName =="宜蘭縣頭城鎮")&(MBday=="31"))

names(MB)
#當天資料(換地區)

Sys.Date()
var1 = "宜蘭縣頭城鎮"

today = filter(MB, (date==Sys.Date())&(locationName==var1))%>%
  select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)

tomorrow = filter(MB, (date==Sys.Date()+1)&(locationName=="宜蘭縣頭城鎮"))%>%
  select(date, MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)

tomorrow2 = filter(MB, (date==Sys.Date()+2)&(locationName=="宜蘭縣頭城鎮"))%>%
  select(date, MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)


DT::datatable(tomorrow2, rownames =FALSE,
              options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
              colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
              caption ='123')


#當天同小時資料
bb = filter(MB, (date==Sys.Date())&(locationName=="宜蘭縣頭城鎮")&(MBhour==format(Sys.time(),"%H")))%>%
  select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)



