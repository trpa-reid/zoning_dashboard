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
  dashboardHeader(title="TRPA Zoning Dashboard",titleWidth = 500),
  dashboardSidebar(
      radioButtons(inputId = "input_type",label="Select Zoning Analysis Type",   choices=c("Planning Areas","Regional", "Parcels", "Upload Shapefile Boundary")), 
      #bsTooltip(id = "input_type", title = "Select the scale of analysis", placement = "right", trigger="hover"),
      uiOutput("ui"),
      box(width=12,background="black",p("Select from the buttons above to analyze TRPA zoning data at different scales; a user can analyze zoning data by planning areas, parcels, or region-wide. All zoning areas are included except shorezones."))
      ),
   dashboardBody(tags$head(tags$style(HTML('/* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #000000;
                              }'))),
     fluidRow(valueBoxOutput("uses1", width=4),
              valueBoxOutput("uses2", width=2)),
     fluidRow(
       box(dataTableOutput("pas_table")),
              box(leafletOutput("map", height=550)))
  )),
server <- function(input, output) {
  observe({
    showModal(modalDialog(
      title = "TRPA Zoning Dashboard",
      "The TRPA Zoning Dashboard is a data tool that allows planners, analysts, and members of the public to better understand which zoning uses are permitted throughout the Tahoe basin at different scales (parcel, planning area, and regional). The dashboard is still under development and will be updated as new data becomes available. For questions and comments please contact Reid Haefer at rhaefer@trpa.org.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  map <- reactive({
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    map <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    map <- st_as_sf(map) %>% st_transform(crs=4326)
  })
  w<-reactive({
    input$input_type
  })
  output$uses1 <- renderValueBox({
    if(w() == "Planning Areas"){
    valueBox(
      nrow(uses_final1 %>% filter(id_name %in% input$area_name) %>%
        filter(use %in% c("Allowed"))), "# Allowed Uses",
      color = "purple")
    } else if (w() == "Parcels") {
      valueBox(parcel %>% 
                 filter(parcel_id %in% input$parcel_name) %>% 
                 data.frame() %>% 
                 select(parcel_id), "parcel_id",color = "green")
    }else if (w() == "Upload Shapefile Boundary") {
      req(input$filemap)
      valueBox(nrow(uses_final1 %>% left_join(
                    st_join(st_buffer(gis1 %>% st_transform(crs=4326),0), st_buffer(map <- map(),0), largest=TRUE) %>%
                    filter(!is.na(NAME)), by="id") %>% data.frame() %>% select(-geometry) %>% 
                    filter(!is.na(plan_id.y)) %>%
                    filter(use !="Not Allowed") %>%
                    distinct(id_name.x,plan_type.x,use3,use) %>%
                      filter(use=="Allowed")), "# Allowed Uses",color = "green")
    } else if( w()== "Regional") {
      valueBox("","",color = "red")
    } else if (w() == "") {
      valueBox("","",color = "purple")
    } else if (input$use_name=="") {
      valueBox("","",color = "red")
    }else if (input$use_name ==" ") {
      valueBox("","",color = "red")
    }  else {valueBox("","",color = "red")}
  })
  z<-reactive({
    input$input_type
  })
output$uses2 <- renderValueBox({
    if(z() == "Planning Areas"){
    valueBox(
      nrow(uses_final1 %>% filter(id_name %in% input$area_name) %>%
             filter(use %in% c("Special Use"))), "# Special Uses",
      color = "purple")
    }else if (w() == "Upload Shapefile Boundary") {
      req(input$filemap)
      valueBox(nrow(uses_final1 %>% left_join(
        st_join(st_buffer(gis1 %>% st_transform(crs=4326),0), st_buffer(map <- map(),0), largest=TRUE) %>%
          filter(!is.na(NAME)), by="id") %>% data.frame() %>% select(-geometry) %>% 
          filter(!is.na(plan_id.y)) %>%
          filter(use !="Not Allowed") %>%
          distinct(id_name.x,plan_type.x,use3,use) %>%
          filter(use=="Special Use")), "# Special Uses",color = "green")
    }else if (z() == "Parcels") {
      valueBox(nrow(st_join(st_buffer(parcel %>% filter(parcel_id %in% input$parcel_name),0), st_buffer(gis1,0), largest=TRUE) %>%
                      left_join(uses_final1, by="id") %>%
                      data.frame() %>%
                      filter(use %in% c("Allowed", "Special Use")) %>% 
                      distinct(parcel_id,use3, plan_name.x, plan_type.x, use)),"Number of Uses Allowed",color = "green")
    }else if (z() == "Regional") {
      valueBox(value =
        uses_final1 %>% 
          filter(use3 %in% input$use_name) %>%
          filter(use %in% c("Allowed","Special Use")) %>%
          summarise(number=n_distinct(plan_name)) %>% select(number),
       subtitle= "# of Planning Areas",
        color = "red")
    } else if (z() == "") {
      valueBox("","",color = "purple")
      } else if (input$use_name=="") {
        valueBox("","",color = "red")
      }else if (input$use_name==" ") {
        valueBox("test","test",color = "red")
      }else if (input$parcel_name==" ") {
        valueBox("","",color = "green")
      } else{valueBox("","",color = "red")
    }
  })
  y<-reactive({
    input$input_type
  })
  output$pas_table <- renderDataTable({
    if(y() == "Upload Shapefile Boundary"){
      req(input$filemap)
      map <- map()
      datatable(extensions = 'Buttons',
                uses_final1 %>% left_join(
                  st_join(st_buffer(gis1 %>% st_transform(crs=4326),0), st_buffer(map,0), largest=TRUE) %>%
                    filter(!is.na(NAME)), by="id") %>% data.frame() %>% select(-geometry) %>% 
                  filter(!is.na(plan_id.y)) %>%
                  filter(use !="Not Allowed") %>%
                  distinct(id_name.x,plan_type.x,use3,use),
                options=list(dom = 'Bfrtip',pageLength=8,buttons = c('csv','pdf')),
                colnames = c("Area Name","Plan Type","Use Type","Allowed"))
    }else if(y() == "Regional"){
      datatable(extensions = 'Buttons',
        uses_final1 %>% 
          filter(use %in% c("Allowed","Special Use") & use3 %in% input$use_name) %>% 
          filter(!is.na(use3)) %>% 
          data.frame() %>%
          select(plan_name, use3, use, plan_type) %>%
          distinct(plan_name, use3, use, plan_type) %>%
          group_by(plan_name, plan_type, use3) %>%
          summarise(`Use Type`=paste(use, collapse = " & ")) %>% filter(!is.na(plan_name)),
        options=list(dom = 'Bfrtip',pageLength=8,buttons = c('csv','pdf')),
        colnames = c("Area Name","Plan Type","Use Type","Allowed"))
    }  else if(y()=="Parcels"){
      datatable(extensions = 'Buttons',st_join(st_buffer(parcel %>% filter(parcel_id %in% input$parcel_name),0), st_buffer(gis1,0), largest=TRUE) %>%
                  left_join(uses_final1, by="id") %>%
                  data.frame() %>%
                  filter(use %in% c("Allowed", "Special Use")) %>% 
                  distinct(parcel_id,use3, id_name.y, plan_type.x, use) %>%
                  group_by(parcel_id,use3, id_name.y, plan_type.x) %>% 
                  summarise(`Use Type`=paste(use, collapse = " & ")),options=list(dom = 'Bfrtip',pageLength=8,buttons = c('csv','pdf')),
                colnames = c("parcel_id","Use Type","Area Name","Area Type","Allowed"))
    } else if (y() == "Planning Areas"){
    datatable(extensions = 'Buttons',uses_final1 %>%
                filter(id_name %in% input$area_name) %>%
                filter(use %in% c("Allowed","Special Use")) %>% 
                distinct(plan_name, area_type, use3, use ) %>%
                group_by( plan_name,area_type,use3) %>% 
                summarise(`Use Type`=paste(use, collapse = " & ")),
              options=list(dom = 'Bfrtip',pageLength=8,buttons = c('csv','pdf')), colnames = c("Area Name","Area Type","Use Type", "Allowed"),
              rownames=F)
    } else {
      datatable(uses_final1 %>% filter(use=="test") %>% select())
    }
  })
  x<-reactive({
    input$input_type
    })
 output$map <- renderLeaflet({
   if(x() == "Regional"){
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
                     sendToBack = TRUE),  
                  #label = ~test$id_name,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.DE, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }  else if (x() == "Parcels") {
     parcel %>% filter(parcel_id %in% input$parcel_name) %>%
        leaflet() %>% addPolygons(weight=1, fillOpacity=0.3, fillColor="#229F03", color="white", popup=paste0(input$parcel_name),
                                  highlight = highlightOptions(
                                    weight = 3,
                                    fillOpacity = 0.5,
                                    color = "black",
                                    fillColor = "00C6F0",
                                    opacity = 1.0,
                                    bringToFront = TRUE,
                                    sendToBack = TRUE),  
                                  # # Add label info when mouseover
                                  label = input$parcel_name,
                                  labelOptions = labelOptions(
                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.DE, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }else if (x() == "Planning Areas"){
     gis1 %>% filter(id_name %in% input$area_name) %>% leaflet() %>% 
       addPolygons(weight=1, fillOpacity=0.5, fillColor="#03F", color="white", popup=paste0(input$area_name),
                   highlight = highlightOptions(
                     weight = 3,
                     fillOpacity = 0.5,
                     color = "black",
                     fillColor = "00C6F0",
                     opacity = 1.0,
                     bringToFront = TRUE,
                     sendToBack = TRUE),  
                   # # Add label info when mouseover
                   label = input$area_name,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.DE, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }else if (x() == "Upload Shapefile Boundary"){
     req(input$filemap)
     map <- map()
     leaflet(map) %>% addTiles() %>% addPolygons()
   } else {
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
           "Regional" = selectInput(selected=" ",inputId = "use_name",label="Select Zoning Use:", multiple=T,  choices=sort(as.vector(c(unique(uses_final1$use3)," ")))),
      "Parcels" = selectInput(selected=" ",inputId = "parcel_name",label="Select Parcel:", multiple=T,  choices=sort(as.vector(c(unique(parcel$parcel_id)," ")))))
    }
  })
  }
)

