library(shiny)
library(leaflet)
library(RPostgreSQL)
library(sf)
library(shinyWidgets)
library(data.table)
library(colourvalues)
library(Rcpp)
library(shinyjs)
source("HexSource.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "jujL6cB3Wm9y", host = "138.197.168.220", 
                 port = 5432, dbname = "cciss")

cw <- fread("./inputs/BigGrid_Crosswalk.csv")
newIDs <- unique(cw$Index)
transCol <- data.table(ID = newIDs,Col = "#FFFFFFFF")
cols <- fread("./inputs/WNA_v12_HexCols.csv")
dists <- dbGetQuery(con,"select distinct dist_code from dist_codes")[,1]
load("./inputs/Dist_MapBoundaries.Rdata")
gcms <- c("CESM1-CAM5","CanESM2","HadGEM2-ES","MIROC5","GISS-E2R","CCSM4","CSIRO-Mk3-6-0","MPI-ESM-LR")
feas <- fread("./inputs/Feasibility_v11_21.csv")
feas <- feas[!is.na(Feasible),.(BGC,SS_NoSpace,Spp,Feasible)]
feas <- feas[Spp != "X",]
allSpp <- sort(unique(feas$Spp))

cppFunction("
NumericVector NewSuitNoCurr(NumericMatrix x, NumericVector vals){
  int n = x.nrow();
  NumericVector res(n);
  for(int i = 0; i < n; i++){
    res[i] = vals[0]*x(i,0)+vals[1]*x(i,1)+vals[2]*x(i,2)+vals[3]*x(i,3);
  }
  return(res);
}")

calcFeas <- function(spp,eda){
    SSPreds <- fread("./inputs/BUL_SSPreds.csv")
    suit <- unique(feas[Spp == spp,])
    SSPreds <- SSPreds[Eda == eda,]
    SSPred <- SSPreds[,.(SiteNo,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
    
    setkey(SSPred,SS.pred)
    setkey(suit,SS_NoSpace)
    suitMerge <- suit[SSPred]
    setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
    suitMerge <- suitMerge[!is.na(Spp),]
    suitVotes <- data.table::dcast(suitMerge, SiteNo + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                   value.var = "SSprob", fun.aggregate = sum)
    suitVotes[,VoteSum := `1`+`2`+`3`+`4`]
    suitVotes[,X := 1 - VoteSum]
    suitVotes[,VoteSum := NULL]
    suitVotes[,X := X + `4`]
    suitVotes[,`:=`(`4` = NULL)]
    setkey(suitVotes, SS_NoSpace, Spp)
    setkey(suit, SS_NoSpace, Spp)
    suitVotes[suit, Curr := i.Feasible]
    suitVotes[is.na(Curr), Curr := 5]
    suitVotes[,NewSuit := NewSuitNoCurr(as.matrix(.SD),c(1,2,3,5)), .SDcols = c("1","2","3","X")]
    suitVotes <- suitVotes[,.(SiteNo,FuturePeriod,NewSuit)]
    return(suitVotes)
}

origDat <- calcFeas("Sx","C4")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Single Map",
             fluidRow(
               column(2,
                      selectInput("Dist","Choose District",choices = dists,multiple = F,selected = "BUL"),
                      radioButtons("Type","Choose Map Type",choices = c("BGC","Feasibility"),selected = "Feasibility")
                      ),
               column(6,
                      leafletjs_hex,
                      leafletOutput("map1",height = "80vh")
                      ),
               column(4,
                      h3("Feasibility Options"),
                      selectInput("sppPick","Select Tree Species",choices = allSpp,selected = "Sx"),
                      selectInput("edaPick","Select Site Position",choices = c("C4","B2","D6"),selected = "C4"),
                      radioButtons("feasPeriod","Select Period",choices = c('2025','2055','2085'), inline = T),
                      hr(),
                      h3("BGC Options"),
                      selectInput("col1_gcm","Select GCM",choices = gcms),
                      radioButtons("col1_scn","Select Scenario", choices = c("rcp45","rcp85")),
                      selectInput("col1_per","Select time period", choices = c("2025","2055","2085"),selected = "2025")
                      )
             )
      )
  
  )

)

server <- function(input, output, session) {
    
    globalServer <- reactiveValues()
    globalFeas <- reactiveValues()
    
    observeEvent(input$Dist,{
      globalServer$Server <- paste0("http://178.128.227.4/data/",input$Dist,"/{z}/{x}/{y}.pbf")
      globalServer$Layer <- input$Dist
    })
    
    observeEvent({c(input$sppPick,input$edaPick)},{
      globalFeas$data <- calcFeas(input$sppPick,input$edaPick)
    },priority = 100)
    
    observeEvent(input$feasPeriod,{
      dat <- globalFeas$data 
      dat <- dat[FuturePeriod == input$feasPeriod,]
      globalFeas$data <- dat
    },priority = 80)
  
    getDistDat1 <- reactive({
        dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from future_sf where dist_code = '",input$Dist,
                                     "' and scenario = '",input$col1_scn,"' and futureperiod = '",
                                     input$col1_per,"' and gcm = '",input$col1_gcm,"'"))
        dat <- as.data.table(dat)
        dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
        dat2 <- dat[,.N,by = .(NewID,bgc_pred)][order(-N), .SD[1], by = NewID]
        dat2[,N := NULL]     
        dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
        dat2
    })

    output$map1 <- renderLeaflet({
        temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
        leaflet() %>%
            fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4]) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron") %>%
            addHexMap() %>%
            invokeMethod(data = "xxx", method = "addHexTiles1", globalServer$Server,globalServer$Layer)
    })
    
    observeEvent({c(input$col1_scn,input$col1_per,
                    input$col1_gcm,input$sppPick,
                    input$edaPick,input$feasPeriod,
                    input$Type)},{
      if(input$Type == "BGC"){
        dat <- getDistDat1()
        session$sendCustomMessage("newCol1",dat[,.(NewID,Col)])
      }else{
        dat <- globalFeas$data
        dat[,Col := colour_values(NewSuit)]
        colDat <- dat[,.(SiteNo,Col)]
        setnames(colDat,c("NewID","Col"))
        session$sendCustomMessage("newCol1",colDat[,.(NewID,Col)])
      }
    },priority = 20)

    # observeEvent({c(input$Dist)},{
    #     colDat <- getDistDat1()
    #     leafletProxy("map1") %>%
    #         invokeMethod(data = colDat, method = "addHexTiles1", ~NewID,~Col,globalServer$Server,globalServer$Layer)
    # })
    observeEvent(input$Dist,{
        temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
        leafletProxy("map1") %>%
            fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# getDistDat2 <- reactive({
#     dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from future_sf where dist_code = '",input$Dist,
#                                  "' and scenario = '",input$col2_scn,"' and futureperiod = '",
#                                  input$col2_per,"' and gcm = '",input$col2_gcm,"'"))
#     dat <- as.data.table(dat)
#     dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
#     dat2 <- dat[,.N,by = .(NewID,bgc_pred)][order(-N), .SD[1], by = NewID]
#     dat2[,N := NULL]     
#     dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
#     dat2
# })
# 
# getDistDat3 <- reactive({
#     dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from future_sf where dist_code = '",input$Dist,
#                                  "' and scenario = '",input$col3_scn,"' and futureperiod = '",
#                                  input$col3_per,"' and gcm = '",input$col3_gcm,"'"))
#     dat <- as.data.table(dat)
#     dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
#     dat2 <- dat[,.N,by = .(NewID,bgc_pred)][order(-N), .SD[1], by = NewID]
#     dat2[,N := NULL]     
#     dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
#     dat2
# })

# output$map2 <- renderLeaflet({
#   temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
#   leaflet() %>%
#     fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4]) %>%
#     addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron") %>%
#     addHexMap()
# })
# 
# output$map3 <- renderLeaflet({
#   temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
#   leaflet() %>%
#     fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4]) %>%
#     addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron") %>%
#     addHexMap()
# })

# observeEvent({c(input$Dist)},{
#   colDat <- getDistDat2()
#   leafletProxy("map2") %>%
#     invokeMethod(data = colDat, method = "addHexTiles2", ~NewID,~Col,globalServer$Server,globalServer$Layer)
# })
# 
# observeEvent({c(input$Dist)},{
#   colDat <- getDistDat3()
#   leafletProxy("map3") %>%
#     invokeMethod(data = colDat, method = "addHexTiles3", ~NewID,~Col,globalServer$Server,globalServer$Layer)
# })

# observeEvent({c(input$col2_scn,input$col2_per,input$col2_gcm)},{
#   dat <- getDistDat2()
#   session$sendCustomMessage("newCol2",dat[,.(NewID,Col)])
# })
# 
# observeEvent({c(input$col3_scn,input$col3_per,input$col3_gcm)},{
#   dat <- getDistDat3()
#   session$sendCustomMessage("newCol3",dat[,.(NewID,Col)])
# })