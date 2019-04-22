library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(rsconnect)
library(geojsonio)
library(lwgeom)
library(shinyBS)
library(shinyalert)
library(rgdal)
library(shinycssloaders)

 # read in GIS
gis1 <- geojson_read("zoning_gis.geojson", what="sp") %>%
  st_as_sf() %>%
  st_transform(crs=4326)

# read in table
uses_final1 <- read.csv("zoning_tab.csv", stringsAsFactors = F) %>%
  left_join(gis1, by="id") %>% data.frame() %>%
  select(id, plan_name.x, area_type, use1, use2, use3, use, plan_id, special_area_name, special_area_id, area_plan_name, plan_type, id_name) %>%
  rename(plan_name=plan_name.x)

parcel <- st_read(".","parcels") %>%
  st_transform(crs=4326) %>%
  mutate(parcel_id=paste("01-0278", parcel_id,sep="-"))
  
shinyApp(
ui <- dashboardPage(skin="black", 
  dashboardHeader(title="Zoning Dashboard",
                  titleWidth = 500),
  dashboardSidebar(width=300,box(width=12,background="black",p("Select from the buttons below to analyze TRPA zoning data at different scales; a user can analyze zoning data by planning areas, parcels, region-wide, or upload your own shapefile for a specific area. All zoning areas are included except shorezones. This dashboard is under development is not currently an official source of TRPA data."),
a("The app code can be found on Github here.",href="https://github.com/trpa-reid/zoning_dashboard")),
      radioButtons(inputId = "input_type",label="Select Zoning Analysis Type",   choices=c("Planning Areas","Regional Land Use", "Parcels", "Upload Shapefile Boundary"),selected = character(0)), 
      #bsTooltip(id = "filemap", title = "Upload a shapefile to analyze a specific area", placement = "right", trigger="hover"),
      uiOutput("ui"),
      uiOutput("ui2"),
      useShinyalert()
      ),
   dashboardBody(tags$head(tags$style(HTML('/* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #000000;
                              }'))),
     fluidRow(#tags$style(".small-box.bg-yellow { background-color: #FFFFFF !important;}"),
              valueBoxOutput("uses1", width=3) %>% withSpinner(color="#000000"),
              valueBoxOutput("uses2", width=3) %>% withSpinner(color="#000000")),
     fluidRow(
       box(dataTableOutput("pas_table") %>% withSpinner(color="#000000")),
              box(leafletOutput("map2", height=450) %>% withSpinner(color="#000000")))
  )),
server <- function(input, output) {
  map<-eventReactive(input$filemap,{
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    map <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    st_as_sf(map) %>% st_transform(crs=4326)
  })
  observe({
    shinyalert(
      title = "TRPA Zoning Dashboard",
     text= "The TRPA Zoning Dashboard is a data tool that allows planners, analysts, and members of the public to better understand which zoning uses are permitted throughout the Tahoe basin at different scales (parcel, planning area, and regional). This dashboard is under development is not currently an official source of TRPA data. To begin, select an analysis type button on the left sidebar.", type="info")
  })
  observeEvent(input$input_type, {
    # Show a modal when the button is pressed
    shinyalert(paste0("Please select the specific ",input$input_type," from the options on the left to create your analysis."), type = "info")
  })
  observeEvent(input$use_button, {
    # Show a modal when the button is pressed
    shinyalert(paste0("Please select ",input$use_button," from the dropdown on the left to create your analysis."), type = "info")
  })
  x <- reactive({
    req(input$input_type)
    input$input_type
  })
  x_button <- reactive({
    req(input$use_button)
    input$use_button
  })
  output$uses1 <- renderValueBox({
    if(x() == "Planning Areas"){
    valueBox(
      nrow(uses_final1 %>% filter(id_name %in% input$area_name) %>%
        filter(use %in% c("Allowed"))), "# Allowed Uses",
      color = "purple")
    } else if (x() == "Parcels") {
      valueBox(parcel %>% 
                 filter(parcel_id %in% input$parcel_name) %>% 
                 data.frame() %>% 
                 select(parcel_id), "Parcel ID",color = "orange")
    }else if (x() == "Upload Shapefile Boundary") {
      map <- map()
      valueBox(nrow(uses_final1 %>% left_join(
                    st_join(st_buffer(gis1,0), st_buffer(map,0), largest=TRUE) %>%
                    filter(!is.na(NAME)), by="id") %>% data.frame() %>% select(-geometry) %>% 
                    filter(!is.na(plan_id.y)) %>%
                    filter(use !="Not Allowed") %>%
                    distinct(id_name.x,plan_type.x,use3,use) %>%
                      filter(use=="Allowed")), "# Allowed Uses",color = "green")
    } else if(x() == "Regional Land Use" & x_button() == "Specific Uses") {
      valueBox(value =
                 paste0(round((uses_final1 %>% 
                 filter(use3 %in% input$use_name) %>%
                 filter(use %in% c("Allowed","Special Use")) %>%
                 summarise(number=n_distinct(plan_name)) %>% select(number) / n_distinct(uses_final1$plan_name)*100),1),"%"),
               subtitle= "% of all Planning Areas",
               color = "red")
    }else if(x() == "Regional Land Use" & x_button() == "Use Categories") {
      valueBox(value =
                 paste0(round((uses_final1 %>% 
                                 filter(use1 %in% input$use_cat_name) %>%
                                 filter(use %in% c("Allowed","Special Use")) %>%
                                 summarise(number=n_distinct(plan_name)) %>% select(number) / n_distinct(uses_final1$plan_name)*100),1),"%"),
               subtitle= "% of all Planning Areas",
               color = "red")
    }else if(x() == "Regional Land Use") {
      valueBox(value =
                 paste0(round((uses_final1 %>% 
                                 filter(use1 %in% input$use_cat_name) %>%
                                 filter(use %in% c("Allowed","Special Use")) %>%
                                 summarise(number=n_distinct(plan_name)) %>% select(number) / n_distinct(uses_final1$plan_name)*100),1),"%"),
               subtitle= "% of all Planning Areas",
               color = "red")
    } else if (x() == "") {
      valueBox("","",color = "orange")
    } else if (input$use_name=="") {
      valueBox("","",color = "red")
    }else if (input$use_name ==" ") {
      valueBox("","",color = "red")
    }  else {valueBox("","",color = "red")}
  })
output$uses2 <- renderValueBox({
    if(x() == "Planning Areas"){
    valueBox(
      nrow(uses_final1 %>% filter(id_name %in% input$area_name) %>%
             filter(use %in% c("Special Use"))), "# Special Uses",
      color = "purple")
    }else if (x() == "Upload Shapefile Boundary") {
      map <- map()
      valueBox(nrow(uses_final1 %>% left_join(
        st_join(st_buffer(gis1,0), st_buffer(map,0), largest=TRUE) %>%
          filter(!is.na(NAME)), by="id") %>% data.frame() %>% select(-geometry) %>% 
          filter(!is.na(plan_id.y)) %>%
          filter(use !="Not Allowed") %>%
          distinct(id_name.x,plan_type.x,use3,use) %>%
          filter(use=="Special Use")), "# Special Uses",color = "green")
    }else if (x() == "Parcels") {
      valueBox(nrow(st_join(st_buffer(parcel %>% filter(parcel_id %in% input$parcel_name),0), st_buffer(gis1,0), largest=TRUE) %>%
                      left_join(uses_final1, by="id") %>%
                      data.frame() %>%
                      filter(use %in% c("Allowed", "Special Use")) %>% 
                      distinct(parcel_id,use3, plan_name.x, plan_type.x, use)),"Number of Uses Allowed",color = "orange")
    }else if (x() == "Regional Land Use" & x_button() =="Specific Uses") {
      valueBox(value =
        uses_final1 %>% 
          filter(use3 %in% input$use_name) %>%
          filter(use %in% c("Allowed","Special Use")) %>%
          summarise(number=n_distinct(plan_name)) %>% select(number),
       subtitle= "# of Planning Areas",
        color = "red")
    } else if (x() == "Regional Land Use" & x_button()=="Use Categories") {
      valueBox(value =
                 uses_final1 %>% 
                 filter(use1 %in% input$use_cat_name) %>%
                 filter(use %in% c("Allowed","Special Use")) %>%
                 summarise(number=n_distinct(plan_name)) %>% select(number),
               subtitle= "# of Planning Areas",
               color = "red")
    } else if (x() == "") {
      valueBox("","",color = "purple")
      } else if (input$use_name=="") {
        valueBox("","",color = "red")
      }else if (input$parcel_name==" ") {
        valueBox("","",color = "green")
      } else{valueBox("","",color = "red")
    }
  })
  output$pas_table <- renderDataTable(server=FALSE,{
    if(x() == "Upload Shapefile Boundary"){
      map <- map()
      datatable(extensions = 'Buttons',
                uses_final1 %>% left_join(
                  st_join(st_buffer(gis1,0), st_buffer(map,0), largest=TRUE) %>%
                    filter(!is.na(NAME)), by="id") %>% data.frame() %>% select(-geometry) %>% 
                  filter(!is.na(plan_id.y)) %>%
                  filter(use !="Not Allowed") %>%
                  distinct(id_name.x,plan_type.x,use3,use),
                options=list(dom = 'Bfrtip',pageLength=5,buttons = c('csv','pdf')),
                colnames = c("Area Name","Plan Type","Use Type","Allowed"),rownames=F)
    }else if(x() == "Parcels"){
      datatable(extensions = 'Buttons',st_join(st_buffer(parcel %>% filter(parcel_id %in% input$parcel_name),0), st_buffer(gis1,0), largest=TRUE) %>%
                  left_join(uses_final1, by="id") %>%
                  data.frame() %>%
                  filter(use %in% c("Allowed", "Special Use")) %>% 
                  distinct(parcel_id,use3, id_name.y, plan_type.x, use) %>%
                  group_by(parcel_id,use3, id_name.y, plan_type.x) %>% 
                  summarise(`Use Type`=paste(use, collapse = " & ")),options=list(dom = 'Bfrtip',pageLength=5,buttons = c('csv','pdf')),
                colnames = c("parcel_id","Use Type","Area Name","Area Type","Allowed"),rownames=FALSE)
    } else if (x() == "Planning Areas"){
      datatable(extensions = 'Buttons',uses_final1 %>%
                  filter(id_name %in% input$area_name) %>%
                  filter(use %in% c("Allowed","Special Use")) %>% 
                  distinct(plan_name, area_type, use3, use ) %>%
                  group_by( plan_name,area_type,use3) %>% 
                  summarise(`Use Type`=paste(use, collapse = " & ")),
                options=list(dom = 'Bfrtip',pageLength=5,buttons = c('csv','pdf')), colnames = c("Area Name","Area Type","Use Type", "Allowed"),rownames=FALSE)
    } else if(x() == "Regional Land Use" & x_button() == "Specific Uses"){
      datatable(extensions = 'Buttons',
        uses_final1 %>% 
          filter(use %in% c("Allowed","Special Use") & use3 %in% input$use_name) %>% 
          filter(!is.na(use3)) %>% 
          data.frame() %>%
          select(plan_name, use3, use, plan_type) %>%
          distinct(plan_name, use3, use, plan_type) %>%
          group_by(plan_name, plan_type, use3) %>%
          summarise(`Use Type`=paste(use, collapse = " & ")) %>% filter(!is.na(plan_name)),
        options=list(dom = 'Bfrtip',pageLength=5,buttons = c('csv','pdf')),
        colnames = c("Area Name","Plan Type","Use Type","Allowed"),rownames=F)
    } else if(x() == "Regional Land Use" & x_button () =="Use Categories"){
      datatable(extensions = 'Buttons',
                uses_final1 %>% 
                  filter(use %in% c("Allowed","Special Use") & use1 %in% input$use_cat_name) %>% 
                  filter(!is.na(use1)) %>% 
                  data.frame() %>%
                  select(plan_name, use1, use, plan_type) %>%
                  distinct(plan_name, use1, use, plan_type) %>%
                  group_by(plan_name, plan_type, use1) %>%
                  summarise(`Use Type`=paste(use, collapse = " & ")) %>% filter(!is.na(plan_name)),
                options=list(dom = 'Bfrtip',pageLength=5,buttons = c('csv','pdf')),
                colnames = c("Area Name","Plan Type","Use Type","Allowed"),rownames=F)
    }   else {
      datatable(uses_final1 %>% filter(use=="test") %>% select(),rownames=F)
    }
  })
 output$map2 <- renderLeaflet({
   if(x() == "Parcels") {
     parcel %>% filter(parcel_id %in% input$parcel_name) %>%
       leaflet() %>% addPolygons(weight=1, fillOpacity=0.3, fillColor="#F2C614", color="white", popup=paste0(input$parcel_name),
                                 highlight = highlightOptions(
                                   weight = 3,
                                   fillOpacity = 0.5,
                                   color = "black",
                                   fillColor = "00C6F0",
                                   opacity = 1.0,
                                   bringToFront = TRUE,
                                   sendToBack = TRUE)) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }else if (x() == "Planning Areas"){
     gis1 %>% filter(id_name %in% input$area_name) %>% leaflet() %>% 
       addPolygons(weight=1, fillOpacity=0.5, fillColor="#7B02DF", color="white", popup=paste0(input$area_name),
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 0.5,
                     color = "black",
                     fillColor = "00C6F0",
                     opacity = 1.0,
                     bringToFront = TRUE,
                     sendToBack = TRUE)) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }else if (x() == "Upload Shapefile Boundary"){
     map <- map()
     leaflet(map) %>% addTiles() %>% addPolygons(weight=1, fillOpacity=0.5, fillColor="green", color="white") %>%
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   } else if(x() == "Regional Land Use" & x_button() =="Use Categories"){
     test9<-gis1 %>% left_join(
       uses_final1 %>% 
         filter(use1 %in% input$use_cat_name & use %in% c("Allowed","Special Use")) %>% 
         distinct(id) %>%
         mutate(tab="tab"), by="id") %>%
       filter(!is.na(tab))
     leaflet(test9) %>%
       addPolygons(weight=1, fillOpacity=0.5, fillColor="red", color="white", popup=paste0(test9$id_name),
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 0.5,
                     color = "black",
                     fillColor = "00C6F0",
                     opacity = 1.0,
                     bringToFront = TRUE,
                     sendToBack = TRUE)) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }else if(x() == "Regional Land Use" & x_button() == "Specific Uses"){
     test<-gis1 %>% left_join(
       uses_final1 %>% 
         filter(use3 %in% input$use_name & use %in% c("Allowed","Special Use")) %>% 
         distinct(id) %>%
         mutate(tab="tab"), by="id") %>%
       filter(!is.na(tab))
     leaflet(test) %>%
       addPolygons(weight=1, fillOpacity=0.5, fillColor="red", color="white", popup=paste0(test$id_name),
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 0.5,
                     color = "black",
                     fillColor = "00C6F0",
                     opacity = 1.0,
                     bringToFront = TRUE,
                     sendToBack = TRUE)) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }  else {
     leaflet() %>% addTiles()
   }
   })
 output$ui <- renderUI({
   if (is.null(input$input_type)){
     return(NULL)
   } else {
     switch(input$input_type,
            "Upload Shapefile Boundary" = fileInput(inputId = "filemap", label = "Select Shapefile",
                                                    multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
            "Planning Areas" = selectInput(selected=" ",inputId = "area_name",label="Select Planning Area:", multiple=T,  choices=sort(as.vector(c(unique(as.character(gis1$id_name))," ")))),
            "Regional Land Use" = radioButtons(inputId = "use_button",label="Select Use Type",   choices=c("Use Categories","Specific Uses"),selected = character(0)),
            "Parcels" = selectInput(selected=" ",inputId = "parcel_name",label="Select Parcel:", multiple=T,  choices=sort(as.vector(c(unique(parcel$parcel_id)," ")))))
    }
  })
 output$ui2 <- renderUI({
   if (is.null(input$use_button)){
     return(NULL)
   } else if (input$input_type != "Regional Land Use"){
    return(NULL)
   }else {
     switch(input$use_button,
 "Specific Uses"=selectInput(selected=" ",inputId = "use_name",label="Select Zoning Use:", multiple=T,  choices=sort(as.vector(c(unique(uses_final1$use3)," ")))),
 "Use Categories"=selectInput(selected=" ",inputId = "use_cat_name",label="Select Zoning Use Category:", multiple=T,  choices=sort(as.vector(c(unique(uses_final1$use1)," ")))))
   }
 })
  }
)

