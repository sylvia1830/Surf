
#CB0049001  波浪統計-臺灣海域各地浪高週期波向觀測月統計

library(jsonlite)
library(htmltools)
library(plyr)
library(dplyr)
library(plotly)
library(kableExtra)



# 讀取網路檔案====
setwd("~/05170595_app/surf")  ##要先設工作目錄
#url = "https://opendata.cwb.gov.tw/fileapi/v1/opendataapi/C-B0049-001?Authorization=CWB-00565823-C77E-42C5-AE08-D0C1DD38B2C7&downloadType=WEB&format=JSON"
#data = fromJSON(txt=url)
data = fromJSON(txt="C-B0049-001.json")

LocationName = data$Cwbopendata$dataset$location$LocationName

obsrtime = data$Cwbopendata$dataset$location$Time[[1]]$obsrtime

elementName = data$Cwbopendata$dataset$location$Time[[1]]$weatherElement$elementName[[1]]
elementName = c(elementName[c(1,2)], "浪分類", elementName[seq(3,11)])

#測試用====
data$Cwbopendata$dataset$location$Time[[1]]$weatherElement$elementValue

names(data$Cwbopendata$dataset$location$Time)

data$Cwbopendata$dataset$location$Time[[1]]$weatherElement$elementValue[[1]][,1]

#空資料框========================================================
qq = data.frame()
#迴圈 資料elementValue
for(i in seq(1, length(LocationName))){
  #i=1
  obsrtime = data$Cwbopendata$dataset$location$Time[[i]]$obsrtime
  for (j in seq(1, length(obsrtime))){
    #j=1
    
    temp = data$Cwbopendata$dataset$location$Time[[i]]$weatherElement$elementValue[[j]][,1]
    
    print(paste(i,j,sep="/"))
    print(temp)
    temp = c(LocationName[i], obsrtime[j], temp)
    
    qq = rbind(qq, temp, stringsAsFactors=F)
    colnames(qq)<-c("觀測站","時間",elementName)
  }
}
#看資料結構
str(qq)
#以下項目改成數值資料
qq$觀測次數 = as.numeric(qq$觀測次數)
qq$最大示性波高波高 = as.numeric(qq$最大示性波高波高)
qq$最大示性波高尖峰週期 = as.numeric(qq$最大示性波高尖峰週期)
qq$平均示性波高 = as.numeric(qq$平均示性波高)
qq$平均週期 = as.numeric(qq$平均週期)
qq$`示性波高分佈(小於0.6)` = as.numeric(qq$`示性波高分佈(小於0.6)`)
qq$`示性波高分佈 0.6~1.5(小浪)` = as.numeric(qq$`示性波高分佈 0.6~1.5(小浪)`)
qq$`示性波高分佈 1.5~2.5(中浪)` = as.numeric(qq$`示性波高分佈 1.5~2.5(中浪)`)
qq$`示性波高分佈 大於2.5(大浪)` = as.numeric(qq$`示性波高分佈 大於2.5(大浪)`)
str(qq)

#年,月, 最大波高日,時
qq$year = as.numeric(substr(qq$時間,1,4))
qq$month = as.numeric(substr(qq$時間,6,7))
qq$maxday = as.numeric(substr(qq$最大示性波高發生時間,1,2))
qq$maxtime = as.numeric(substr(qq$最大示性波高發生時間,4,5))
qq$place = as.character(substr(qq$觀測站,5,5))

View(qq)

#存成RDS檔
saveRDS(qq,"CB0049001.rds")

#======================================
#各浮標資料
cc=filter(qq, (觀測站=="基隆市 彭佳嶼資料浮標")&(month!=0)&(平均示性波高!=0))


#ANOVA變異數分析==================================================================================
  #宜蘭 花蓮 台東資料
YHT = filter(qq, 觀測站 %in% c("宜蘭縣 龜山島浮標","花蓮縣 花蓮浮標","臺東縣 成功浮球")
             &(month!=0)&(平均示性波高!=0)&(year>2015))
 
#變異數分析
aov.YHT = aov(YHT$平均示性波高~YHT$觀測站)
summary(aov.YHT)

AA=TukeyHSD(aov.YHT)
plot(TukeyHSD((aov.YHT), conf.level = 0.95), cex.axis=0.5 ,srt=45)

#plot(YHT$平均示性波高~YHT$觀測站)


#6個浮標站變異數分析===================================
ALL = filter(qq, 觀測站 %in% c("新北市 龍洞浮標","花蓮縣 花蓮浮標",
                            "宜蘭縣 龜山島浮標","新竹市 新竹浮標",
                            "臺東縣 成功浮球","新北市 富貴角資料浮標")
             &(month!=0)&(平均示性波高!=0)&(year>2015))

aov.ALL = aov(ALL$平均示性波高~ALL$place)

summary(aov.ALL)

TukeyHSD(aov.ALL)

#plot(TukeyHSD((aov.ALL), conf.level = 0.95), cex.axis=0.5)
plot(TukeyHSD((aov.ALL), conf.level = 0.95), las = 1)

  


#6個浮標站相關係數分析=======================

library(ggplot2)
library(GGally)

qq = readRDS("./CB0049001.rds")

#新北市 龍洞浮標AA====
AA=filter(qq, (觀測站=="新北市 龍洞浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
  select(平均示性波高, 平均週期)

#相關係數分析
cor.test(AA$平均示性波高, AA$平均週期)

#繪製資料分佈矩陣圖
ggpairs(AA)


#花蓮縣 花蓮浮標BB====
BB=filter(qq, (觀測站=="花蓮縣 花蓮浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
  select(平均示性波高, 平均週期)

cor.test(BB$平均示性波高, BB$平均週期)
ggpairs(BB)

#宜蘭縣 龜山島浮標CC====
CC=filter(qq, (觀測站=="宜蘭縣 龜山島浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
  select(平均示性波高, 平均週期)

cor.test(CC$平均示性波高, CC$平均週期)
ggpairs(CC)
#新竹市 新竹浮標DD====
DD=filter(qq, (觀測站=="新竹市 新竹浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
  select(平均示性波高, 平均週期)

cor.test(DD$平均示性波高, DD$平均週期)
ggpairs(DD)
#臺東縣 成功浮球EE====
EE=filter(qq, (觀測站=="臺東縣 成功浮球")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
  select(平均示性波高, 平均週期)

cor.test(EE$平均示性波高, EE$平均週期)
ggpairs(EE)
#新北市 富貴角資料浮標FF====
FF=filter(qq, (觀測站=="新北市 富貴角資料浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
  select(平均示性波高, 平均週期)

cor.test(FF$平均示性波高, FF$平均週期)
ggpairs(FF)










#分群========

qq.mxs<-as.matrix(qq)

d<-dist(qq.mxs) #預設為euclidean
head(d)

d<-dist(qq.mxs, method="manhattan") #計算manhattan距離
head(d)

par(mar=rep(2,4),mfrow=c(1,1))
hc<-hclust(dist(qq.mxs)) #可用method參數設定聚合方法，預設為complete
plot(hc)

###

# 將特徵標準化以利分群
qq.x <- apply(qq[, -5], 2, function(x){(x-mean(x))/sd(x)})
library(factoextra)
library(cluster)
# k-means
km.qq <- kmeans(qq, centers = 3)
# 分群視覺化
fviz_cluster(km.qq, data = qq, geom = "point",
             stand = T, ellipse.type = "norm")

