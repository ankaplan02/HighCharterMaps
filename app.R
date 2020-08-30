library(dplyr)         # To manipulate data
require(tidyverse)
library(shiny)
require(highcharter)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
require(geojsonio)
library(geojsonR)
require(jsonlite)
require(usmap)
require(openxlsx)
require(tigris)
require(geojsonsf)

#setwd("C:/Users/AKaplan/Desktop/Practice_Git/esrd-hhr-hsa-plots-for-shiny-app")

# loads in usable saved data to plot 
geojsonaHRR <- readLines(file.path("data","HRR_Update2.geojson"), warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

#geojsonaHRR <- readLines(file.path("data","Hospital_Referral_Region.geojson"), warn = FALSE) %>%
#  paste(collapse = "\n") %>%
#  fromJSON(simplifyVector = FALSE)

HRRsf <- geojsonsf::geojson_sf(file.path("data", "HRR_Update2.geojson"))

#geojsonHSA <- readLines(file.path("data", "hsa_shp2.json"), warn = FALSE) %>%
#  paste(collapse = "\n") %>%
#  fromJSON(simplifyVector = FALSE)

#HSAsf <- geojsonsf::geojson_sf(file.path("data", "hsa_shp2.json"))
#HSAsf <- HSAsf %>% select("HSA")

#geojsonHSA <- readLines(file.path("data", "HSA_New.geojson"), warn = FALSE) %>%
#  paste(collapse = "\n") %>%
#  fromJSON(simplifyVector = FALSE)

HSAsf <- geojsonsf::geojson_sf(file.path("data", "HSA_New.geojson"))
HSAsf <- HSAsf %>% select("HSA")

HSAsf$HSA[nchar(HSAsf$HSA) == 1] <- paste("0",HSAsf$HSA[nchar(HSAsf$HSA) == 1], sep = "")
geojsonHSA <- geojson_list(HSAsf)



geojsonN <- readLines(file.path("data","Networks2.geojson"), warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

Netsf <- geojsonsf::geojson_sf(file.path("data","Networks2.geojson"))

#geojsonC <- readLines(file.path("data","counts2.json"), warn = FALSE) %>%
#  paste(collapse = "\n") %>%
#  fromJSON(simplifyVector = FALSE)

Countsf <- geojsonsf::geojson_sf(file.path("data","counts2.json"))

names(Countsf)[5] <- "code"
Countsf$State <- toupper(substring(Countsf$code, 4,5))

geojsonC <- readLines(file.path("data","county_new.geojson"), warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

#Countsf <- geojsonsf::geojson_sf(file.path("data","county_new.geojson"))

#names(Countsf)[2] <- "code"
#Countsf$State <- toupper(substring(Countsf$code, 4,5))


# adds thousands place in outcomes
lang <- getOption("highcharter.lang")
lang$thousandsSep <- ","
options(highcharter.lang = lang)

#loads in state initials, outcome data, and city-state variables 
#zips <- read.csv("data\\ZipHsaHrr18.csv", header = T)
load(file.path("data","Fake_Data_4_Maps.RData"))
load(file.path("data","Zip_County.RData"))
#Codes <- zips$zipcode18
colorlist<- rev(c("#2a4372","#b2324b","#969fa7","#40619d","#45ba8f","#4d1434","#66b1ce","#d6919d","#a5d1c3","#c0e1d4","#def1e8"))

HSAformap <- as.data.frame(cbind(as.character(ZipCounty$HSA93), as.character(ZipCounty$STATE)))
names(HSAformap) <- c("HSA", "HSAStateAbb")
HSAformap <- unique(HSAformap)
HSAformap <- HSAformap[!is.na(HSAformap$HSA),]
HSAsf <- merge(HSAsf, HSAformap, by = "HSA", all.x = T)

hrrcodes <- as.character(unique(ZipCounty$HRRNUM)[order(unique(ZipCounty$HRRNUM))])
hsacodes <- as.character(unique(ZipCounty$HSA93)[order(unique(ZipCounty$HSA93))])

networklist <- as.numeric(sapply(1:18, function(x) as.character(x)))
load(file.path("data","network2state.RData")); network2State <- as.data.frame(network2State)
names(network2State) <- c("Network", "State")
networkname <- paste0("Network ", networklist)

load(file.path("data","state_abb2name.RData"))
st <- as.data.frame(st); names(st) <- c("HSAStateAbb", "State")
#stor2 <- merge(stor2, st, by = "HSAstate", all.x = T)
statelist <- as.character(st[,2])
names(st) <- c("HRRStateAbb", "HRRstate")
stor2 <- merge(stor2,st, by.x = "HRRStateAbb", by.y = "HRRStateAbb", all.x = T)
#stor2 <- stor2[,-c(ncol(stor2))]
#names(stor2)[ncol(stor2)] <- "HRRstate"
names(stor2)[2] <- "State"

#HSAsf2 <- HSAsf
#HSAsf <- HSAsf2

forHSA <- unique(stor2[,c(5, 12, 3)])
names(forHSA)[1] <- "HSA"
forHSA <- forHSA[!is.na(forHSA$HSA),]
HSAsf <- merge(HSAsf, forHSA, by = "HSA")
HSAsf <- HSAsf[(as.character(HSAsf$HSAStateAbb) == as.character(HSAsf$StateAbb)),]

stor2$Network[stor2$HSADesc ==  "Fresno (Fresno), CA - Kings, CA"] <- 18
stor2$Network[stor2$HSADesc ==  "Inyo, CA - Mono, CA"] <- 18
hsascalif <- unique(stor2$HSA93[stor2$HSADesc %in% 
                                  c("Fresno (Fresno), CA - Kings, CA",
                                    "Inyo, CA - Mono, CA")])
HSAsf$Network[HSAsf$HSA %in% hsascalif] <- 18
# Kings county is in Network 18 BUT
# Fresno county is in 17
# but they share an HSA, so which one do I assign?

agelist<- as.character(names(table(stor2$AgeCat)))
racelist<-c("White","Black","Other")


# Define UI for application that draws a histogram
### start of UI
ui <- fluidPage( 
  
  tags$head(
    tags$style("*{font-family: Open Sans; font-size: 12pt}")
  ),
  
  tags$style(type = "text/css", "
      .irs-grid-text {font-family: 'Open Sans'; font-size: 10pt;} 
      .irs-grid-pol.small {height: 0px;} 
    "), # make slider input text bigger and remove minor ticks
  
  titlePanel("ESRD Mortality Rates (per 1,000) within Specified Regions"),
  
  fluidRow(
    column(5,
           
           wellPanel(
             
             #shinyjs::useShinyjs(),
             
             #id = "side-panel",
             
             h4("Define the Map for your Query"), 
             
             tags$div(title="",
                      radioButtons("datasrc", HTML(paste("Include Data From:", tags$sup("(?)"),":", sep = "")), c("Entire U.S.", "ESRD Networks", "Selected States/Territories"), 
                                   selected="Entire U.S.", inline=TRUE)
             ),
             conditionalPanel(
               condition = "input.datasrc == 'Selected States/Territories'", 
               pickerInput("states"," ", statelist, selected = NULL, 
                           #options = list(`actions-box` = TRUE,
                           #`none-selected-text` = "Select one or more states",
                           #`selected-text-format`= "count > 14",
                           #`count-selected-text` = "All States"), 
                           multiple = FALSE)),
             #conditionalPanel(
             #     condition = "input.datasrc == 'Entire U.S.'", 
             #     pickerInput("hrrcode"," ", hrrcodes, selected = hrrcodes, 
             #                 options = list(
             #                   `actions-box` = TRUE,
             #                   `none-selected-text` = "Select one or more Hospital Referral Regions",
             #                   `selected-text-format`= "count > 14",
             #                   `count-selected-text` = "All HRRs"), 
             #                 multiple = TRUE)),
             # 
             conditionalPanel(
               condition = "input.datasrc == 'ESRD Networks'",
               pickerInput("networks", label = "Network", choices = networklist, selected = NULL, 
                           #options = list(
                           #   `actions-box` = TRUE,
                           #   `none-selected-text` = "Select one or more Health Service Areas",
                           #   `selected-text-format`= "count > 14",
                           #   `count-selected-text` = "All HSAs"), 
                           multiple=FALSE)),
             
             #tags$div(title="Click here to slide through years",
             #          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #2a4372}")),
             #         sliderInput("year", HTML(paste("Years", tags$sup("(?)"),":", sep = "")), min=1998, max=2017, value=c(2014,2017),sep="", animate = FALSE, ticks = T)),
             
             radioButtons("age","Age:",c("All Ages","Selected Age Groups"),selected="All Ages",inline = TRUE),
             # "Selected Age Groups provides the drop down menu below #
             conditionalPanel(condition = "input.age == 'Selected Age Groups'", 
                              pickerInput("agegrp", " ", choices = agelist, selected = agelist, 
                                          options = list(`actions-box` = TRUE,
                                                         `none-selected-text` = "Select one or more age groups",
                                                         `selected-text-format`= "count > 14",
                                                         `count-selected-text` = "All Ages"), multiple = TRUE)),
             
             
             checkboxGroupInput("sex", "Sex:", choices = c("Male","Female"),selected = c("Male","Female"), inline = TRUE),
             
             
             pickerInput("race","Race:", racelist, selected = racelist, options = list(`actions-box` = TRUE,`selected-text-format`= "count > 5",
                                                                                       `count-selected-text` = "All Races"), multiple = TRUE),
             
             
             #checkboxGroupInput("eth", "Ethnicity:", choices = c("Hispanic or Latino","Not Hispanic or Latino","Unknown"), 
             #                    selected = c("Hispanic or Latino","Not Hispanic or Latino","Unknown"), inline = TRUE),
             
             #checkboxGroupInput("modal", "Treatment Modality:", choices = list("Hemodialysis"="Hemodialysis", "Peritoneal Dialysis"="Peritoneal Dialysis", "Transplant"="Transplant","Unknown"="Unknown"), 
             #                    selected = c("Hemodialysis","Peritoneal Dialysis","Transplant","Unknown"), inline=TRUE),
             
             # pickerInput("diag","Primary ESRD Diagnosis Group:", esrdcaulist, selected = esrdcaulist, options = list(`actions-box` = TRUE, `selected-text-format`= "count > 6",
             #                                                                                                       `count-selected-text` = "All Causes"), multiple = TRUE)
             actionButton("reset_input", "Reset")
             
           ),
           
           wellPanel(
             
             shinyjs::useShinyjs(),
             
             id = "side-panel2",
             
             h4("Display Options"),
             
             radioButtons("stra1"," ", choices = "Year"),
             
             actionButton("reset_input2", "Reset")
             
           )
    ),
    
    
    column(6,
           
           tabsetPanel(
             tabPanel("Chart",
                      fluidRow(
                        br(),
                        highchartOutput("highchart"),
                        textOutput("chartref"),
                        tags$head(tags$style("#chartref{color: grey; font-size: 12px;}")),
                        br(),
                        htmlOutput("text2")
                      )
             )
             
           )
    ) ) )



### start of server


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Updates the buttons for certain selections 
  observe({
    if (input$datasrc %in% c("Entire U.S.")){
      stra1choice = c("Network","State", "Health Service Area", "Hospital Referral Region", "County")
    }
    else if(input$datasrc == 'Selected States/Territories'){
      stra1choice = c("Health Service Area", "Hospital Referral Region", "County")
    }
    else if(input$datasrc == "ESRD Networks"){
      stra1choice = c("State", "Health Service Area", "Hospital Referral Region", "County")
    }
    updateRadioButtons(session, "stra1", label = "Display results by:",
                       choices = stra1choice,
                       selected = "State",
                       inline = TRUE)
  })
  
  #observe({
  #  if (input$stra1 == "State"){stra2choice = c("None")}
  #  else if(input$stra1 == "Age")      {stra2choice = c("Sex", "Race", "None")}
  #  else if(input$stra1 == "Sex")      {stra2choice = c("Age", "Race", "None")}
  #  else if(input$stra1 == "Race")     {stra2choice = c("Age", "Sex", "None")}
  
  #  updateRadioButtons(session, "stra2", label = "Secondarily display results by:",
  #                     choices = stra2choice,
  #                     selected = "None",
  #                     inline = TRUE)
  #})
  #this next step reacts to teh user inputs. changes secondary choices for display
  js <- c(
    "function(settings){",
    "  var datatable = settings.oInstance.api();",
    "  var table = datatable.table().node();",
    "  var caption = 'Suggested citation for this report: United States Renal Data System, 2019 Annual Data Report: Epidemiology of Kidney Disease in the United States. National Institutes of Health, National Institute of Diabetes and Digestive and Kidney Diseases, Bethesda, MD, 20892.'",
    "  $(table).append('<caption style=\"caption-side: bottom; font-size: 12px\">' + caption + '</caption>');",
    "}"
  )
  
  #manipulate data (summarize)
  getsub <- reactive({
    
    shiny::validate(
      need(input$agegrp, "Please select at least one age group"),
      need(input$sex, "Please select at least one sex"),
      need(input$race, "Please select at least one race")#,
      #need(input$eth, "Please select at least one ethnicity"),
      #need(input$modal, "Please select at least one modality"),
      #need(input$diag, "Please select at least one cause")
    ) 
    
    stor2 %>%
      { if (input$datasrc == "Entire U.S.") filter(.,) else if (input$datasrc == "ESRD Networks") filter(.,Network %in% input$networks) else if (input$datasrc == "Selected States/Territories") filter(., State %in% input$states)} %>%
      #filter(Year <= input$year[2] & Year >= input$year[1]) %>% 
      { if (input$age == "All Ages") filter(.,) else if (input$age == 'Selected Age Groups') filter(.,AgeCat %in% input$agegrp)} %>%
      #  else if (input$age == "18+") filter(.,Age %in% agelist1) else if (input$age == "65+") filter(.,Age %in% agelist2) 
      #  else filter(.,Age %in% input$agegrp)} %>%
      filter(Sex %in% input$sex) %>%
      #filter(Ethnicity %in% input$eth) %>%
      filter(Race %in% input$race) #%>%
    #filter(Modality %in% input$modal) %>%
    #filter(Cause %in% input$diag) 
  })
  # problem area above in AgeCat #
  
  #getCount <- reactive({Countsf})
  #getNet <- reactive({Netsf})
  
  output$highchart <- renderHighchart({
    
    if (input$datasrc=="Entire U.S.") { #trying to figure this out 
      # JUNE 8th 2020
      if(input$stra1=="State"){
        
        test <- getsub() %>% group_by_("Y", "State") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("State") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="State") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for state level
        # within the state... 
        
        highchart() %>%
          hc_add_series_map(map =usgeojson,df= hcdata, value = "perc", joinBy = c("woename","State"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "{point.State}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)      
        
      }
      else if(input$stra1=="Network"){
        
        #test<- getsub() %>%
        #  group_by(net,State) %>%
        #  dplyr::summarise(sum=n())
        test <- getsub() %>% group_by_("Y", "Network") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("Network") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="Network") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for Network
        #percentage adds up to simulated total percentage of "effected" or
        # percentage of diseased... 
        df <- merge(hcdata, network2State, by = "Network")
        names(df)[6] <- "name"
        #cool code here to save for later if problems arise#
        #labs2 <- which(!(df$State %in% "California") & df$n.y == df$n.y[df$Network %in% "18"])
        #df <- df[-labs2,] # got the weird duplicate entries that were given 
        # similar networks as to southern california... don't know why
        # something to do with the merging and HSA/HRR state labels
        # arizona was assigned to netowrk 18, so this was an example problem
        
        highchart() %>%
          hc_add_series_map(map =geojsonN, df= df, value = "perc", joinBy = c("name"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "ESRD Network {point.Network}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)      
        
      }
      else if(input$stra1=="Health Service Area"){
        
        test <- getsub() %>% group_by_("Y", "HSA93") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("HSA93", "HSADesc") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="HSA93") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for HSA
        df <- hcdata
        
        highchart() %>%
          hc_add_series_map(map = geojsonHSA,df= df, value = "perc", joinBy = c("HSA", "HSA93"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1,
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "HSA {point.HSA93} <br><br> {point.HSADesc}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538(colors = c("red", "white"))) %>%
          hc_title(text = "") %>%
          hc_mapNavigation(enabled=TRUE)  
      }
      else if(input$stra1=="Hospital Referral Region"){
        
        test <- getsub() %>% group_by_("Y", "HRRNUM", "HRRCITY") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("HRRNUM") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="HRRNUM") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop of HRR
        df <- hcdata
        
        highchart() %>%
          hc_add_series_map(map = geojsonaHRR,df= df, value = "perc", joinBy = "HRRNUM",
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "HRR {point.HRRNUM} <br><br> {point.HRRCITY}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)
      }
      
      else if(input$stra1=="County"){
        test <- getsub() %>% group_by_("Y", "FIPS", "County") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("FIPS") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="FIPS") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop of county
        df <- hcdata
        df$GEOID10 <- df$FIPS
        
        highchart() %>%
          hc_add_series_map(map = geojsonC, df= df, value = "perc", joinBy = c("fips","FIPS"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "FIPS {point.FIPS} <br><br> {point.County}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)
        #gg <- download_map_data(paste0("countries/us/us-all"))
        # due to rounding, we are missing 3%, but the map is so fine
        # that rounding results in 0. maybe if we do it 
        # per 1000 people (like a rate) then we wouldn't see this
      }
    }
    else if(input$datasrc == "Selected States/Territories"){
      if(input$stra1=="Health Service Area"){
        
        test <- getsub() %>% group_by_("Y", "HSA93") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("HSA93") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        
        hcdata<-inner_join(test,hcdata,by="HSA93") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for US level data
        df <- hcdata
        
        uniqDesc <- getsub() %>% select(HSA93, HSADesc) %>% unique()
        df <- merge(df, uniqDesc, by = "HSA93")
        #needed <- network2State[network2State$Network == input$networks,]
        #needed$State <- as.character(needed$State)
        #needed$State[grepl("California", needed$State)] <- "California"
        neededabb <- st$HRRStateAbb[st$HRRstate %in% input$states]
        neededabb <- as.character(neededabb)
        # this attempts to detect "HSAs" that don't belong based on their
        # state description
        # for instance some HSAs in orgen and nevada show up in 
        # northern california (exclusively orgen or nevada)
        hsadescripts <- strsplit(df$HSADesc, " - ")
        try <- lapply(hsadescripts, function(x){
          numch <- nchar(x); test <- substring(x,numch-1, numch);
          #flag <- sum(test %in% neededabb) ==0;
          return(test)
        })
        df <- df[!sapply(try, function(x) sum(x %in% neededabb) == 0),]
        # still need to have this match neededabb
        # omit those HSA's with labels that have both 
        #specified states outside of network or not desired state
        
        HSAsfSub <- HSAsf %>% filter(HSAStateAbb %in% neededabb)
        HSAsfSub <- HSAsfSub %>% filter(HSA %in% df$HSA93)
        geojsonHSAsub <- geojson_list(HSAsfSub)
        
        
        highchart() %>%
          hc_add_series_map(map = geojsonHSAsub,df= df, value = "perc", joinBy = c("HSA","HSA93"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "HSA {point.HSA93} <br><br> {point.HSADesc}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)  
      }
      else if(input$stra1=="Hospital Referral Region"){
        
        test <- getsub() %>% group_by_("Y", "HRRNUM", "HRRCITY","HRRStateAbb", "StateAbb") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("HRRNUM") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="HRRNUM") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for US level data
        df <- hcdata[as.character(hcdata$HRRStateAbb) == as.character(hcdata$StateAbb),]
        problems <- !(df$HRRStateAbb %in% st$HRRStateAbb[input$states %in% st$HRRstate]); 
        
        if(sum(problems) > 0){
          df <- df[df$HRRstate %in% input$states,]}
        
        HRRsfSub <- HRRsf %>% filter(substring(HRRCITY, 1,2) %in% st$HRRStateAbb[st$HRRstate %in%  input$states])
        geojsonHRRsub <- geojson_list(HRRsfSub)
        
        highchart() %>%
          hc_add_series_map(map = geojsonHRRsub,df= df, value = "perc", joinBy = "HRRNUM",
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "HRR {point.HRRNUM} <br><br> {point.HRRCITY}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)
      }
      else if(input$stra1=="County"){
        test <- getsub() %>% group_by_("Y", "FIPS", "County") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("FIPS", "StateAbb") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="FIPS") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for US level data
        df <- hcdata
        df$GEOID10 <- df$FIPS
        df <- as.data.frame(df)
        names(df)[2] <- "fips"
        
        stateABB <- (st$HRRStateAbb[which(st$HRRstate == input$states)])
        stateABB <- as.character(stateABB)
        #names(Countsf)[5] <- "code"
        #Countsf$State <- as.character(Countsf$State)
        CountsfSub <- Countsf[which(Countsf$fips %in% df$fips),]
        geojsonCsub <- geojson_list(CountsfSub)
        
        
        highchart() %>%
          hc_add_series_map(map = geojsonCsub, df=df, value = "perc", joinBy = c("fips"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "FIPS {point.fips} <br><br> {point.County}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)
        
        
      }
    }
    else if(input$datasrc == "ESRD Networks"){
      
      if(input$stra1=="Health Service Area"){
        
        #you stopped editing here, on June 27th 2020
        test <- getsub() %>% group_by_("Y", "HSA93") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("HSA93") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        
        hcdata<-inner_join(test,hcdata,by="HSA93") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for US level data
        df <- hcdata
        uniqDesc <- getsub() %>% select(HSA93, HSADesc) %>% unique()
        df <- merge(df, uniqDesc, by = "HSA93")
        needed <- network2State[network2State$Network == input$networks,]
        needed$State <- as.character(needed$State)
        needed$State[grepl("California", needed$State)] <- "California"
        neededabb <- st$HRRStateAbb[st$HRRstate %in% needed$State]
        neededabb <- as.character(neededabb)
        # this attempts to detect "HSAs" that don't belong based on their
        # state description
        # for instance some HSAs in orgen and nevada show up in 
        # northern california (exclusively orgen or nevada)
        hsadescripts <- strsplit(df$HSADesc, " - ")
        try <- lapply(hsadescripts, function(x){
          numch <- nchar(x); test <- substring(x,numch-1, numch);
          #flag <- sum(test %in% neededabb) ==0;
          return(test)
        })
        df <- df[!sapply(try, function(x) sum(x %in% neededabb) == 0),]
        # still need to have this match neededabb
        # omit those HSA's with labels that have both 
        #specified states outside of network or not desired state
        
        HSAsfSub <- HSAsf %>% filter(Network %in% input$networks)
        HSAsfSub <- HSAsfSub %>% filter(HSA %in% df$HSA93)
        geojsonHSAsub <- geojson_list(HSAsfSub)
        
        highchart() %>%
          hc_add_series_map(map = geojsonHSAsub, df= df, value = "perc", joinBy = c("HSA","HSA93"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "HSA {point.HSA93} <br><br> {point.HSADesc}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)  
      }
      else if(input$stra1=="Hospital Referral Region"){
        
        test <- getsub() %>% group_by_("Y", "HRRNUM", "HRRCITY","HRRStateAbb", "StateAbb") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("HRRNUM") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="HRRNUM") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2)  #divided by total pop for US level data
        df <- hcdata[as.character(hcdata$HRRStateAbb) == as.character(hcdata$StateAbb),]
        #next line tries to ensure that only HRR states of the certain network are chosen
        
        stateNet4Hrr <- as.character(network2State$State[network2State$Network == input$networks])
        if(sum(stateNet4Hrr %in% "Northern California") ==1 | sum(stateNet4Hrr %in% "Southern California") == 1){
          stateNet4Hrr[stateNet4Hrr == "Northern California" | stateNet4Hrr == "Southern California" ] <- "California"}
        state4Hrr <- as.character(st$HRRStateAbb[st$HRRstate %in% stateNet4Hrr])
        df <- df[df$HRRStateAbb %in% state4Hrr,]
        #problems <- !(df$HRRstate %in% input$states); 
        #if(sum(problems) > 0){
        #  df <- df[df$HRRstate %in% input$states,]}
        
        HRRsfSub <- HRRsf %>% filter(substring(HRRCITY, 1,2) %in% ZipCounty$STATE[ZipCounty$Network %in% input$networks])
        geojsonaHRRsub <- geojson_list(HRRsfSub)
        
        highchart() %>%
          hc_add_series_map(map = geojsonaHRRsub,df= df, value = "perc", joinBy = "HRRNUM",
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "HRR {point.HRRNUM} <br><br> {point.HRRCITY}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)
      }
      
      else if(input$stra1=="County"){
        
        test <- getsub() %>% group_by_("Y", "FIPS", "County") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("FIPS") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="FIPS") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for US level data
        df <- hcdata
        df$GEOID10 <- df$FIPS
        df <- as.data.frame(df)
        names(df)[2] <- "fips"
        
        statebigg <- as.character(network2State$State[which(network2State$Network == input$networks)])
        if(sum(grepl("California", x= statebigg)) == 0){
        stateabbs <- st$HRRStateAbb[st$HRRstate %in% statebigg]
        }else{
        stateabbs <- numeric(length(statebigg))
        if(length(stateabbs) == 2){
        stateabbs[grepl("California", x= statebigg)] <- "CA"
        stateabbs[!grepl("California", x= statebigg)] <- "HI"
        }else{
          stateabbs <- "CA"
        }
        }
        
        #stateabbs <- as.character(stateabbs)
        #names(Countsf)[5] <- "code"
        #print(sprintf("Initiating subsetting"))
        #Countsf$State <- as.character(Countsf$State)
        CountsfSub <- Countsf[which(Countsf$fips %in% df$fips),]
        print(sprintf("Made it through subsetting for network"))
        geojsonCsub <- geojson_list(CountsfSub)
        print(sprintf("Finished list of subsetted dat"))
        #names(df)[2] <- "fips"
        #CountsfSub <- Countsf %>% filter(toupper(substring(code, 4,5)) %in% ZipCounty$STATE[ZipCounty$Network %in% input$networks])
        #geojsonCsub <- geojson_list(CountsfSub)
        
        highchart() %>%
          hc_add_series_map(map = geojsonCsub, df= df, value = "perc", joinBy = c("fips"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "FIPS {point.fips} <br><br> {point.County}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE)
      
        }
      
      else if(input$stra1=="State"){
        test <- getsub() %>% group_by_("Y", "State") %>%
          dplyr::summarise(n = n())
        hcdata <- getsub() %>%
          group_by_("State") %>%
          dplyr::summarise(n=n()) %>%
          mutate(n=ifelse(n<=10,0,n)) 
        hcdata<-inner_join(test,hcdata,by="State") %>% filter(Y == 1)
        hcdata$perc <- round(hcdata$n.x/(hcdata$n.y) * 1000,2) #divided by total pop for US level data
        df <- hcdata
        #df$GEOID10 <- df$FIPS 
        df$State <- as.character(df$State)
        if(input$networks == 17){df$State[df$State == "California"]<- "Northern California"}
        if(input$networks == 18){df$State[df$State == "California"]<- "Southern California"}
        
        statebigg <- as.character(network2State$State[which(network2State$Network == input$networks)])
        if(sum(grepl("California", x= statebigg)) == 0){
          stateabbs <- st$HRRStateAbb[st$HRRstate %in% statebigg]
        }else{
          stateabbs <- numeric(length(statebigg))
          stateabbs[grepl("California", x= statebigg)] <- "CA"
          stateabbs[!grepl("California", x= statebigg)] <- "HI"
        }
        stateabbs <- as.character(stateabbs)
        #names(Netsf)[9] <- "code"
        #print(sprintf("Initiating subsetting map"))
        #places <- which(Netsf$code %in% stateabbs)
        #NetsfSub <- Netsf[places,]
        #print(sprintf("Finished map"))
        #geojsonNsub <- geojson_list(NetsfSub)
        #names(df)[2] <- "name"
        
        places <-which(Netsf$Network %in% input$networks)
        NetsfSub <- Netsf[places,]
        #print(sprintf("Finished map"))
        geojsonNsub <- geojson_list(NetsfSub)
        names(df)[2] <- "name"
        
        highchart() %>%
          hc_add_series_map(map = geojsonNsub,df=df, value = "perc", joinBy = c("name"),
                            name="Mortality Rate per 1,000", 
                            borderWidth = 0.1, 
                            tooltip = list(valueDecimals = 1,
                                           pointFormat = "{point.name}: {point.perc}",
                                           useHTML=TRUE, headerFormat = ""),
                            borderColor = "#FFFAFA", borderWidth = 0.1) %>%
          hc_add_theme(hc_theme_538()) %>%
          hc_title(text = "") %>% hc_colors(colorlist) %>%
          hc_mapNavigation(enabled=TRUE) 
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)