library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(htmltools)
library(leaflet)
library(plyr)
library(dplyr)
library(plotly)
library(GGally)
df <- read.table("q1.txt", header=T, sep=",")
df2 <- read.table("q2.txt", header=T, sep=",")

#海象數值模式預報資料-鄉鎮預報之波流預報資料MB0077001====

urlMB = "https://opendata.cwb.gov.tw/fileapi/v1/opendataapi/M-B0077-001?Authorization=CWB-00565823-C77E-42C5-AE08-D0C1DD38B2C7&downloadType=WEB&format=JSON"
dataMB = fromJSON(txt=urlMB)


names(dataMB$cwbdata$resources$resource)

##欄位名
colNames = dataMB$cwbdata$resources$resource$metadata$weatherElements$weatherElement$tagName
##地區名
locationNameMB=dataMB$cwbdata$resources$resource$data$marinePointForecasts$marinePointForecast$locationName
table(locationNameMB)


typeof(locationNameMB)


###資料框
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
#波浪統計-臺灣海域各地浪高週期波向觀測月統計CB0049001====
qq = readRDS("./CB0049001.rds")


#==================================================================================
ui <- {dashboardPage(
  skin = "blue",
  dashboardHeader(title="GO SURF"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("GO SURF首頁",  tabName="mainPage", icon=icon("home")),
      menuItem("臺灣浪點地圖", tabName = "surf_map", icon = icon("map-marked")),
      menuItem("浪況預報", startExpanded = TRUE, icon = icon("water"),
               menuSubItem("北部", tabName = "north", icon = icon("umbrella-beach")),
               menuSubItem("東北部(宜蘭)", tabName = "northeast", icon = icon("umbrella-beach")),
               menuSubItem("東部", tabName = "east", icon = icon("umbrella-beach")),
               menuSubItem("南部", tabName = "south", icon = icon("umbrella-beach")),
               menuSubItem("西部", tabName = "west", icon = icon("umbrella-beach"))),
      menuItem("臺灣沿海浪況統計", tabName = "stat", icon = icon("chart-bar")),
      menuItem("相關係數分析", tabName = "coc", icon = icon("chart-bar")),
      menuItem("變異數分析", tabName = "anova", icon = icon("chart-bar"))
      
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName="mainPage", tags$h1("GO SURF衝浪預報", style="color:#007799;"),tags$br(),
              align="center",tags$hr(style="border-color: #007799;"),tags$br(),tags$br(),tags$br(),
              tags$h2("衝浪資料的視覺化分析",style="color:#009FCC"),tags$br(),tags$br(),tags$br(),
              tags$hr(style="border-color: #007799;"),
              tags$h3("指導教授：陳明輝", style="color:#007799"),
              tags$h3("學生：周奕君", style="color:#007799")
      ),
      
      tabItem(tabName="surf_map", tags$h1("臺灣浪點地圖",align="center",style="color:#007799;"),
              leafletOutput(outputId="mainmap",width = "100%", height =450 )
      ),
      
      tabItem(tabName="stat", tags$h1("臺灣沿海浪況統計",align="center",style="color:#008888;"),
              tags$hr(style="border-color: #007799;"),
              column(5,
              selectInput(inputId="select_buoy", label="臺灣沿海浮標",
                          choices=c("新北市 龍洞浮標",
                                    "花蓮縣 花蓮浮標",
                                    "宜蘭縣 龜山島浮標",
                                    "新竹市 新竹浮標",
                                    "臺東縣 成功浮球",
                                    "新北市 富貴角資料浮標",
                                    "基隆市 彭佳嶼資料浮標",
                                    "臺中市 臺中資料浮標"),
                          selected="8", multiple=FALSE)),
              tabsetPanel(
                tabPanel("浪高",plotlyOutput(outputId="wavehigh")),
                tabPanel("浪週期",plotlyOutput(outputId="period")))
              
      ),
      tabItem(tabName="anova", tags$h1("不同地區浪高差異",align="center",style="color:#008888;"),
              tags$hr(style="border-color: #007799;"),
              fluidRow("由下圖可知只有「宜蘭縣 龜山島浮標」-「臺東縣 成功浮球」以及「宜蘭縣 龜山島浮標」-「新北市 富貴角資料浮標」浪高有明顯差異，其餘都無明顯差異，下圖為各浮標站之95%信賴區間圖。",
                       style="background-color:	#CCEEFF	;"),
              
              fluidRow(plotOutput(outputId="ano"))
              

      ),
      
      tabItem(tabName="coc", tags$h1("浪高與浪週期關聯性",align="center",style="color:#008888;"),
              tags$hr(style="border-color: #007799;"),
              
              tabsetPanel(
                tabPanel("龍洞浮標",
                         column(8,plotOutput(outputId="AA",width = "100%", height =400)),
                         column(4,tags$h4("相關係數r = 0.7437029"),tags$br(),
                                tags$h4("統計檢定量t = 7.4627"),tags$br(),
                                tags$h4("p-value = 2.11e-09 < α= 0.05") ,tags$br(),
                                tags$h4("表示兩者有顯著相關"))
                         ),
                tabPanel("花蓮浮標",
                         column(8,plotOutput(outputId="BB",width = "100%", height =400)),
                         column(4,tags$h4("相關係數r = 0.704407"),tags$br(),
                                tags$h4("統計檢定量t = 6.7308"),tags$br(),
                                tags$h4("p-value = 2.311e-08 < α= 0.05") ,tags$br(),
                                tags$h4("表示兩者有顯著相關"))),
                tabPanel("龜山島浮標",
                         column(8,plotOutput(outputId="CC",width = "100%", height =400)),
                         column(4,tags$h4("相關係數r = 0.8730003"),tags$br(),
                                tags$h4("統計檢定量t = 12.007"),tags$br(),
                                tags$h4("p-value = 1.24e-15 < α= 0.05") ,tags$br(),
                                tags$h4("表示兩者有顯著相關"))),
                tabPanel("新竹浮標",
                         column(8,plotOutput(outputId="DD",width = "100%", height =400)),
                         column(4,tags$h4("相關係數r = 0.8887193"),tags$br(),
                                tags$h4("統計檢定量t = 13.004"),tags$br(),
                                tags$h4("p-value = 2.2e-16 < α= 0.05") ,tags$br(),
                                tags$h4("表示兩者有顯著相關"))),
                tabPanel("成功浮標",
                         column(8,plotOutput(outputId="EE",width = "100%", height =400)),
                         column(4,tags$h4("相關係數r = 0.3477314"),tags$br(),
                                tags$h4("統計檢定量t = 2.256"),tags$br(),
                                tags$h4("p-value = 0.03007 < α= 0.05") ,tags$br(),
                                tags$h4("表示兩者有顯著相關"))),
                tabPanel("富貴角浮標",
                         column(8,plotOutput(outputId="FF",width = "100%", height =400)),
                         column(4,tags$h4("相關係數r = 0.9492251"),tags$br(),
                                tags$h4("統計檢定量t = 19.786"),tags$br(),
                                tags$h4("p-value = 2.2e-16 < α= 0.05") ,tags$br(),
                                tags$h4("表示兩者有顯著相關")))
                )
              
              
      ),

      
      
      
      #浪點標題====
      tabItem(tabName="north",
              fluidRow(
                column(4, selectInput(inputId="selectI1", label="北部浪點",
                                     choices=c("沙崙",
                                               "白沙灣",
                                               "餐廳",
                                               "中角",
                                               "翡翠灣",
                                               "萬里",
                                               "福隆"),
                                     selected="7", multiple=FALSE) ),
                column(width = 6, offset = 1,
                       leafletOutput(outputId="Nmap",width = "100%", height =150 ))
              ),
              fluidRow(
                       tabsetPanel(
                tabPanel(Sys.Date(), dataTableOutput(outputId="ntt")),
                tabPanel(Sys.Date()+1, dataTableOutput(outputId="nt1")),
                tabPanel(Sys.Date()+2, dataTableOutput(outputId="nt2")))
              )
              ),
      tabItem(tabName="northeast",
              fluidRow(
                column(4, selectInput(inputId="selectI2", label="東北部浪點",
                          choices=c("大溪蜜月灣",
                                    "梗訪",
                                    "雙獅",
                                    "外澳",
                                    "烏石港(北堤)",
                                    "烏石港(南堤)",
                                    "臭水",
                                    "竹安",
                                    "清水",
                                    "無尾港"),
                          selected="10", multiple=FALSE) ),
                column(width = 6, offset = 1,
                       leafletOutput(outputId="NEmap",width = "100%", height =150 ))
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel(Sys.Date(), dataTableOutput(outputId="tt")),
                  tabPanel(Sys.Date()+1, dataTableOutput(outputId="t1")),
                  tabPanel(Sys.Date()+2, dataTableOutput(outputId="t2")))
              )
              ),
      tabItem(tabName="east",
              fluidRow(
                column(4, selectInput(inputId="selectI3", label="東部浪點",
                          choices=c("環保公園",
                                    "花蓮北濱",
                                    "雙橋",
                                    "鹽寮漁港",
                                    "磯崎",
                                    "八仙洞",
                                    "宜灣",
                                    "成功",
                                    "都歷",
                                    "東河",
                                    "金樽",
                                    "小漁港",
                                    "興昌",
                                    "都蘭",
                                    "台東"),
                          selected="14", multiple=FALSE) ),
              column(width = 6, offset = 1,
                     leafletOutput(outputId="Emap",width = "100%", height =150 ))
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel(Sys.Date(), dataTableOutput(outputId="ett")),
                  tabPanel(Sys.Date()+1, dataTableOutput(outputId="et1")),
                  tabPanel(Sys.Date()+2, dataTableOutput(outputId="et2")))
              )
              ),
      tabItem(tabName="south",
              fluidRow(
                column(4, selectInput(inputId="selectI4", label="南部浪點",
                          choices=c("九棚",
                                    "佳樂水",
                                    "南灣",
                                    "射寮"),
                          selected="4", multiple=FALSE) ),
                column(width = 6, offset = 1,
                       leafletOutput(outputId="Smap",width = "100%", height =150 ))
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel(Sys.Date(), dataTableOutput(outputId="stt")),
                  tabPanel(Sys.Date()+1, dataTableOutput(outputId="st1")),
                  tabPanel(Sys.Date()+2, dataTableOutput(outputId="st2")))
              )
              ),
      tabItem(tabName="west",
              fluidRow(
                column(4, selectInput(inputId="selectI5", label="西部浪點",
                          choices=c("旗津",
                                    "漁光島",
                                    "四草橋",
                                    "松柏港",
                                    "外埔漁港",
                                    "竹南"),
                          selected="7", multiple=FALSE) ),
                column(width = 6, offset = 1,
                       leafletOutput(outputId="Wmap",width = "100%", height =150 ))
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel(Sys.Date(), dataTableOutput(outputId="wtt")),
                  tabPanel(Sys.Date()+1, dataTableOutput(outputId="wt1")),
                  tabPanel(Sys.Date()+2, dataTableOutput(outputId="wt2")))
              )
              )
    )
  )
)
}
#=======================================================================================
server <-{ function(input, output, session) {
  
  #北部資料====
  output$ntt<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI1]
    today = filter(MB, (date==Sys.Date())&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(today, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$nt1<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI1]
    tomorrow = filter(MB, (date==Sys.Date()+1)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$nt2<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI1]
    tomorrow = filter(MB, (date==Sys.Date()+2)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  #東北部資料====
  output$tt<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI2]
    today = filter(MB, (date==Sys.Date())&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(today, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$t1<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI2]
    tomorrow = filter(MB, (date==Sys.Date()+1)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$t2<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI2]
    tomorrow2 = filter(MB, (date==Sys.Date()+2)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow2, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  #東部資料====
  output$ett<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI3]
    today = filter(MB, (date==Sys.Date())&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(today, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$et1<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI3]
    tomorrow = filter(MB, (date==Sys.Date()+1)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$et2<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI3]
    tomorrow = filter(MB, (date==Sys.Date()+2)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })

  #南部資料====
  output$stt<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI4]
    today = filter(MB, (date==Sys.Date())&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(today, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$st1<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI4]
    tomorrow = filter(MB, (date==Sys.Date()+1)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$st2<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI4]
    tomorrow = filter(MB, (date==Sys.Date()+2)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  #西部資料====
  output$wtt<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI5]
    today = filter(MB, (date==Sys.Date())&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(today, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$wt1<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI5]
    tomorrow = filter(MB, (date==Sys.Date()+1)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  output$wt2<-renderDataTable({
    areaStr = df$area[df$Name==input$selectI5]
    tomorrow = filter(MB, (date==Sys.Date()+2)&(locationName==areaStr))%>%
      select(MBhour, significantWaveHeight, waveDirection, wavePeriod, currentDirection)
    
    DT::datatable(tomorrow, rownames =FALSE,
                  options = list(dom = 'fpti',columnDefs = list ( list(className = 'dt-right', targets = 2)), pageLength = 6, lengthMenu = c(6, 12, 18, 24)),
                  colnames = c('時間', '浪高(m)','浪向', '浪週期', '海流流向-去向'),
                  caption =areaStr)
  })
  
  
  #全地圖--------------------------------------------------------------------------------
  output$mainmap<-renderLeaflet({
    #粉紅浪點圖示
    WaveIcon <- makeIcon(
      iconUrl = "https://image.flaticon.com/icons/svg/2995/2995734.svg",
      iconWidth = 30, iconHeight = 95,
      iconAnchorX = 13, iconAnchorY = 50)
    map<-leaflet(df) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name), icon = WaveIcon)
    map
  })
  
  #北地圖
  output$Nmap<-renderLeaflet({
    df1 = filter(df, region==1)
    
    WaveIcon <- makeIcon(
      iconUrl = "https://image.flaticon.com/icons/svg/2995/2995734.svg",
      iconWidth = 30, iconHeight = 95,
      iconAnchorX = 13, iconAnchorY = 50)
    map<-leaflet(df1) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name), icon = WaveIcon)
    map
  }) # Nmap
  
  #東北地圖
  output$NEmap<-renderLeaflet({
    df2 = filter(df, region==2)
    
    WaveIcon <- makeIcon(
      iconUrl = "https://image.flaticon.com/icons/svg/2995/2995734.svg",
      iconWidth = 30, iconHeight = 95,
      iconAnchorX = 13, iconAnchorY = 50)
    map<-leaflet(df2) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name), icon = WaveIcon)
    map
  }) # NEmap

  #東地圖
  output$Emap<-renderLeaflet({
    df3 = filter(df, region==3)
    
    WaveIcon <- makeIcon(
      iconUrl = "https://image.flaticon.com/icons/svg/2995/2995734.svg",
      iconWidth = 30, iconHeight = 95,
      iconAnchorX = 13, iconAnchorY = 50)
    map<-leaflet(df3) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name), icon = WaveIcon)
    map
  }) # Emap
  
  #南地圖
  output$Smap<-renderLeaflet({
    df4 = filter(df, region==4)
    
    WaveIcon <- makeIcon(
      iconUrl = "https://image.flaticon.com/icons/svg/2995/2995734.svg",
      iconWidth = 30, iconHeight = 95,
      iconAnchorX = 13, iconAnchorY = 50)
    map<-leaflet(df4) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name), icon = WaveIcon)
    map
  }) # Smap
  
  #西地圖
  output$Wmap<-renderLeaflet({
    df5 = filter(df, region==5)
    
    WaveIcon <- makeIcon(
      iconUrl = "https://image.flaticon.com/icons/svg/2995/2995734.svg",
      iconWidth = 30, iconHeight = 95,
      iconAnchorX = 13, iconAnchorY = 50)
    map<-leaflet(df5) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name), icon = WaveIcon)
    map
  }) # Wmap
  
  #浪高統計圖====
  output$wavehigh<-renderPlotly({
    zz = df2$觀測時間[df2$浮標站==input$select_buoy]
    
    filter(qq, (觀測站==input$select_buoy)&(month!=0)&(平均示性波高!=0)) %>%
      plot_ly(y=~平均示性波高, x=~month, type="box") %>%
      layout(
        title = zz,
        xaxis = list(
          dtick = 1, 
          tick0 = 1, 
          tickmode = "linear"),
        yaxis = list(
          dtick = 0.5, 
          tick0 = 0, 
          tickmode = "linear")
      )
  })
  
  #浪週期統計圖====
  output$period<-renderPlotly({
    zz = df2$觀測時間[df2$浮標站==input$select_buoy]
    filter(qq, (觀測站==input$select_buoy)&(month!=0)&(平均週期!=0)) %>%
      plot_ly(y=~平均週期, x=~month, type="box") %>%
      layout(title = zz,
        xaxis = list(
          dtick = 1, 
          tick0 = 1, 
          tickmode = "linear"),
        yaxis = list(
          dtick = 0.5, 
          tick0 = 0, 
          tickmode = "linear")
      )
  })
  
  #anova====
  output$ano<-renderPlot({
    ALL = filter(qq, 觀測站 %in% c("新北市 龍洞浮標","花蓮縣 花蓮浮標",
                                "宜蘭縣 龜山島浮標","新竹市 新竹浮標",
                                "臺東縣 成功浮球","新北市 富貴角資料浮標")
                 &(month!=0)&(平均示性波高!=0)&(year>2015))
    
    aov.ALL = aov(ALL$平均示性波高~ALL$place)
    
    summary(aov.ALL)
    
    TukeyHSD(aov.ALL)
    
    plot(TukeyHSD((aov.ALL), conf.level = 0.95), las = 1)
    
    
  })
  
  #相關係數====
  output$AA<-renderPlot({
    AA=filter(qq, (觀測站=="新北市 龍洞浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
      select(平均示性波高, 平均週期)
    #相關係數分析
    cor.test(AA$平均示性波高, AA$平均週期)
    #繪製資料分佈矩陣圖
    ggpairs(AA)
  })
  
  output$BB<-renderPlot({
    BB=filter(qq, (觀測站=="花蓮縣 花蓮浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
      select(平均示性波高, 平均週期)
    
    cor.test(BB$平均示性波高, BB$平均週期)
    ggpairs(BB)
  })
  
  output$CC<-renderPlot({
    CC=filter(qq, (觀測站=="宜蘭縣 龜山島浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
      select(平均示性波高, 平均週期)
    
    cor.test(CC$平均示性波高, CC$平均週期)
    ggpairs(CC)
  })
  
  output$DD<-renderPlot({
    DD=filter(qq, (觀測站=="新竹市 新竹浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
      select(平均示性波高, 平均週期)
    
    cor.test(DD$平均示性波高, DD$平均週期)
    ggpairs(DD)
  })
  
  output$EE<-renderPlot({
    EE=filter(qq, (觀測站=="臺東縣 成功浮球")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
      select(平均示性波高, 平均週期)
    
    cor.test(EE$平均示性波高, EE$平均週期)
    ggpairs(EE)
  })
  
  output$FF<-renderPlot({
    FF=filter(qq, (觀測站=="新北市 富貴角資料浮標")&(month!=0)&(平均示性波高!=0)&(平均週期!=0)&(year>2015))%>%
      select(平均示性波高, 平均週期)
    
    cor.test(FF$平均示性波高, FF$平均週期)
    ggpairs(FF)
  })
  
  }
}
#
shinyApp(ui, server)
