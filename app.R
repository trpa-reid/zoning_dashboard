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
#gis1 <- geojson_read("zoning_gis.geojson", what="sp") %>%
 # st_as_sf() %>%
  #st_transform(crs=4326) %>%
#gis1$acres <- st_area(gis1)
#gis1$acres<- round(units::set_units(gis1$acres, "acre"),1)

# read in sde GIS layer
gis1 <- st_read(".","zoning_gis") %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>%
  rename(id=ZONING_ID,
         plan_name=PLAN_NAME,
         plan_type=PLAN_TYPE,
         special_area_name=SPECIAL_AR,
         special_area_id= SPECIAL__1) %>% 
  mutate(special_area_id=case_when(is.na(special_area_id) ~ "none", TRUE ~ as.character(special_area_id)),
         id_name = str_replace_all(paste(plan_name, special_area_name, sep=" "), "NA","")) %>%
  st_zm(drop = T, what = "ZM")
gis1$acres <- st_area(gis1)
gis1$acres<- round(units::set_units(gis1$acres, "acre"),1)

# read in sde tabular layer

tab<-read_csv("uses.csv") %>%
  rename(use=Use_Status,
        use1= Category,
        use2=Subcategory,
        use3=Use_Type,
        density_num=Density,
        density_unit=Unit,
        id=Zoning_ID) %>% mutate(use_final=use3) %>% left_join(gis1 %>% mutate(id=as.character(id)), by= "id") %>% data.frame()

# new use and density data set
#tab<-read_csv("use_density_final.csv") %>%
 # rename(id=old_id) %>%
  #left_join(gis1 %>% mutate(id=as.character(id)), by= "id") %>% data.frame() %>%
  #filter(!is.na(data_type1)) %>%
  #select(id, plan_name, use,use1, use2, use3, use_final, density_num, density_unit, special_area_name, special_area_id, area_plan_name, plan_type, id_name) %>%
  #mutate(area_type="test")

# read in table
#uses_final1 <- read.csv("zoning_tab.csv", stringsAsFactors = F) %>%
 # left_join(gis1, by="id") %>% data.frame() %>%
  #select(id, plan_name.x, area_type, use1, use2, use3, use, plan_id, special_area_name, special_area_id, area_plan_name, plan_type, id_name) %>%
  #rename(plan_name=plan_name.x)

parcel <- st_read(".","parcels") %>%
  st_transform(crs=4326) %>%
  mutate(parcel_id=paste("01-0278", parcel_id,sep="-"))
  
shinyApp(
ui <- dashboardPage(skin="black", 
  dashboardHeader(title="Zoning Dashboard",
                  titleWidth = 500),
  dashboardSidebar(width=300,box(width=12,background="black",p("Select from the options below to analyze TRPA zoning data at different scales. All zoning areas are included except shorezones. This dashboard is under development is not currently an official source of TRPA data."),
a("The app code is found on Github here.",href="https://github.com/trpa-reid/zoning_dashboard")),
      radioButtons(inputId = "input_type",label="Select Zoning Analysis Type",   choices=c("Click Any Point on Map","Select Planning Areas","Select Regional Land Use", "Select Parcel", "Upload Shapefile Boundary"),selected = "Click Any Point on Map"), 
     # bsTooltip(id = "input_type", title = "Upload a shapefile to analyze a specific area", placement = "right", trigger="hover"),
      uiOutput("ui"),
      uiOutput("ui2"),
      useShinyalert()
      ),
   dashboardBody(tags$head(tags$style(HTML('/* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #000000;
                              }'))),
     fluidRow(#tags$style(".small-box.bg-yellow { background-color: #FFFFFF !important;}"),
              valueBoxOutput("uses1", width=2) %>% withSpinner(color="#000000"),
              valueBoxOutput("uses2", width=2) %>% withSpinner(color="#000000")),
     fluidRow(
       box(leafletOutput("map2", height=600) %>% withSpinner(color="#000000")),
       box(dataTableOutput("pas_table") %>% withSpinner(color="#000000")))
  )),
server <- function(input, output) {
  map<-eventReactive(input$filemap,{
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    map <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    st_as_sf(map) %>% st_transform(crs=4326) %>% select(-everything())
  })
  observe({
    shinyalert(
      title = "TRPA Zoning Dashboard",
     text= "The TRPA Zoning Dashboard is a data tool that allows planners, analysts, and members of the public to better understand which zoning uses are permitted throughout the Tahoe basin at different scales (parcel, planning area, and regional). This dashboard is under development is not currently an official source of TRPA data. To begin, select an analysis type button on the left sidebar.", type="info")
  })
  observeEvent(input$input_type, {
    # Show a modal when the button is pressed
    shinyalert(paste0("Select the analysis options from the sidebar on the left."), type = "info")
  })
  observeEvent(input$use_button, {
    # Show a modal when the button is pressed
    shinyalert(paste0("Select ",input$use_button," from the dropdown on the left to create your analysis."), type = "info")
  })
  x <- reactive({
    req(input$input_type)
    input$input_type
  })
  event <-reactive({ 
    req(input$map2_shape_click)
    input$map2_shape_click$id
  })
  x_button <- reactive({
    req(input$use_button)
    input$use_button
  })
  output$uses1 <- renderValueBox({
    if(x() == "Select Planning Areas"){
    valueBox(
      nrow(tab %>% filter(id_name %in% input$area_name) %>%
        filter(use %in% c("Allowed"))), "# Allowed Uses",
      color = "purple")
    } else if(x() == "Click Any Point on Map"){
      valueBox(
        nrow(tab %>% filter(id == as.character(event())) %>%
               filter(use %in% c("Allowed"))), "# Allowed Uses",
        color = "purple")
    } else if (x() == "Select Parcel") {
      valueBox(parcel %>% 
                 filter(parcel_id %in% input$parcel_name) %>% 
                 data.frame() %>% 
                 select(parcel_id), "Parcel ID",color = "orange")
    }else if (x() == "Upload Shapefile Boundary") {
      map <- map() 
      valueBox(nrow(tab %>% left_join(
        st_join(st_buffer(gis1,0), st_buffer(map %>% st_transform(crs=4326) %>% mutate(reid="reid"),0), largest=TRUE) %>%
          filter(!is.na(reid)), by="id") %>% data.frame()  %>% 
          filter(!is.na(PLAN_ID.y)) %>%
                    filter(use =="Allowed") %>%
                    distinct(use3)), "# Allowed Uses",color = "green")
    } else if(x() == "Select Regional Land Use" & x_button() == "Specific Uses") {
      valueBox(value =
                 paste0(round((tab %>% 
                 filter(use3 %in% input$use_name) %>%
                 summarise(number=n_distinct(id)) %>% select(number) / n_distinct(tab$id)*100),1),"%"),
               subtitle= "% of All Planning Areas",
               color = "red")
    }else if(x() == "Select Regional Land Use" & x_button() == "Use Categories") {
      valueBox(value =
                 paste0(round((tab %>% 
                                 filter(use2 %in% input$use_cat) %>%
                                 summarise(number=n_distinct(id)) %>% select(number) / n_distinct(tab$id)*100),1),"%"),
               subtitle= "% of all Planning Areas",
               color = "red")
    }else if(x() == "Select Regional Land Use" & x_button() == "Use Groups") {
      valueBox(value =
                 paste0(round((  nrow(
                   tab %>% 
                     filter( use1 %in% input$use_group) %>% 
                     distinct(id, use1, use, density_num, density_unit, plan_name, special_area_id, id_name) %>%
                     group_by(id, use1,plan_name, special_area_id, id_name) %>%
                     summarise(`Use Type`=paste(unique(use), collapse = " & "))
                 ) / n_distinct(tab$id)*100),1),"%"),
               subtitle= "% of all Planning Areas",
               color = "red")
    }else if(x() == "Select Regional Land Use") {
      valueBox(value =
                 paste0(round((tab %>% 
                                 filter(use1 %in% input$use_group) %>%
                                 summarise(number=n_distinct(id)) %>% select(number) / n_distinct(tab$id)*100),1),"%"),
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
    if(x() == "Select Planning Areas"){
    valueBox(
      nrow(tab %>% filter(id_name %in% input$area_name) %>%
             filter(use %in% c("Special Use"))), "# Special Uses",
      color = "purple")
    }else if(x() == "Click Any Point on Map"){
      valueBox(
        nrow(tab %>% filter(id == as.character(event())) %>%
               filter(use %in% c("Special Use"))), "# Special Uses",
        color = "purple")
    }else if (x() == "Upload Shapefile Boundary") {
      map <- map()
      valueBox(nrow(tab %>% left_join(
        st_join(st_buffer(gis1,0), st_buffer(map %>% st_transform(crs=4326) %>% mutate(reid="reid"),0), largest=TRUE) %>%
          filter(!is.na(reid)), by="id") %>% data.frame()  %>% 
          filter(!is.na(PLAN_ID.y)) %>%
          filter(use =="Special Use") %>%
          distinct(use3)), "# Special Uses",color = "green")
    }else if (x() == "Select Parcel") {
      valueBox(nrow(st_join(st_buffer(parcel %>% filter(parcel_id %in% input$parcel_name),0), st_buffer(gis1,0), largest=TRUE) %>%
                      left_join(tab, by="id") %>%
                      data.frame() %>%
                      filter(use %in% c("Allowed", "Special Use")) %>% 
                      distinct(parcel_id,use3, plan_name.x, plan_type.x, use)),"Number of Uses Allowed",color = "orange")
    }else if (x() == "Select Regional Land Use" & x_button() =="Specific Uses") {
      valueBox(value =
        tab %>% 
          filter(use3 %in% input$use_name) %>%
          summarise(number=n_distinct(plan_name)) %>% select(number),
       subtitle= "# of Planning Areas",
        color = "red")
    }else if (x() == "Select Regional Land Use" & x_button() =="Use Categories") {
      valueBox(value =
                 tab %>% 
                 filter(use2 %in% input$use_cat) %>%
                 summarise(number=n_distinct(plan_name)) %>% select(number),
               subtitle= "# of Planning Areas",
               color = "red")
    } else if (x() == "Select Regional Land Use" & x_button()=="Use Groups") {
      valueBox(value =
                 nrow(
                   tab %>% 
                 filter( use1 %in% input$use_group) %>% 
                 distinct(id, use1, use, density_num, density_unit, plan_name, special_area_id, id_name) %>%
                 group_by(id, use1,plan_name, special_area_id, id_name) %>%
                 summarise(`Use Type`=paste(unique(use), collapse = " & "))
                 ),
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
                tab %>% left_join(
                  st_join(st_buffer(gis1,0), st_buffer(map %>% st_transform(crs=4326) %>% mutate(reid="reid"),0), largest=TRUE) %>%
                    filter(!is.na(reid)), by="id") %>% data.frame()  %>% 
                  filter(!is.na(PLAN_ID.y)) %>%
                  distinct(PLAN_ID.y,use3,use),
                options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')),
                colnames = c("Area Name","Use Type","Allowed"),rownames=F)
    }else if (x() == "Click Any Point on Map"){
      datatable(extensions = 'Buttons',
                tab %>%
                  filter(id == as.character(event())) %>%
                  distinct(plan_name, special_area_id, use3, use,density_num, density_unit ),
                options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')), 
                colnames = c("Area Name","Special Area","Use Type", "Allowance", "Density","Density Unit"),rownames=FALSE)
    }else if(x() == "Select Parcel"){
      datatable(extensions = 'Buttons',
                st_join(st_buffer(parcel %>% filter(parcel_id %in% input$parcel_name),0), st_buffer(gis1,0), largest=TRUE) %>%
                  left_join(tab, by="id") %>%
                  data.frame() %>% 
                  select(parcel_id, plan_name.x, special_area_id.x, use3, use, density_num, density_unit),
                options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')),
                colnames = c("Parcel ID","Plan Name", "Special Area","Use", "Allowance" ,"Density", "Density Unit"),rownames=FALSE)
    } else if (x() == "Select Planning Areas"){
      datatable(extensions = 'Buttons',
                tab %>%
                  filter(id_name %in% input$area_name) %>%
                  distinct(plan_name, use3, use,density_num, density_unit ),
                options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')), 
                colnames = c("Area Name","Use Type", "Allowance", "Density","Density Unit"),rownames=FALSE)
    } else if(x() == "Select Regional Land Use" & x_button() == "Specific Uses"){
      datatable(extensions = 'Buttons',
        tab %>% 
          filter(use3 %in% input$use_name) %>% 
          filter(!is.na(use3)) %>% 
          data.frame() %>%
          select(id_name, use3, use, plan_type, id,density_num, density_unit) %>%
          distinct(id_name, use3, use, plan_type, id, density_num,density_unit) %>%
          group_by(id_name, plan_type, use3, id, density_num,density_unit) %>%
          summarise(`Use Type`=paste(use, collapse = " & ")) %>% 
          filter(!is.na(id_name)) %>%
          left_join(data.frame(gis1) %>% select(id,acres), by="id") %>%
          select(-id),
        options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')),
        colnames = c("ID","Plan Name","Plan Type","Land Use","Density","Density Unit","Use Allowed","Acres"),rownames=F)
    } else if(x() == "Select Regional Land Use" & x_button() == "Use Categories"){
      datatable(extensions = 'Buttons',
                tab %>% 
                  filter( use2 %in% input$use_cat) %>% 
                  filter(!is.na(use2)) %>% 
                  data.frame() %>%
                  distinct(id_name, use2, use3,use, id,density_num, density_unit) %>%
                  group_by(id_name, use2, use3, id, density_num, density_unit) %>%
                  summarise(`Use Type`=paste(use, collapse = " & ")) %>% 
                  left_join(data.frame(gis1) %>% select(id,acres), by="id") %>%
                  ungroup() %>%
                  select(-id),
                options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')),
                colnames = c("Plan Name","Land Use", "Use Category","Density","Density Unit","Use Allowed","Acres"),rownames=F)
      }else if(x() == "Select Regional Land Use" & x_button () =="Use Groups"){
      datatable(extensions = 'Buttons',
                tab %>% 
                  filter( use1 %in% input$use_group) %>% 
                  distinct(id, use1, use, density_num, density_unit, plan_name, special_area_id, id_name,use3) %>%
                  group_by(id, use1,plan_name, special_area_id, id_name,use3, density_num, density_unit) %>%
                  summarise(`Use Type`=paste(unique(use), collapse = " & ")) %>%
                  select(plan_name, special_area_id, use1,use3, `Use Type`, density_num, density_unit) %>%
                  left_join(data.frame(gis1) %>% select(id,acres), by="id") %>%
                  ungroup() %>%
                  select(-id),
                options=list(dom = 'Bfrtip',pageLength=10,buttons = c('csv','pdf')),
                colnames = c("Plan Name","Special Area","Use Group","Specific Use","Use Allowed","Acres"),rownames=F)
    }   else {
      datatable(tab %>% filter(use=="test") %>% select(),rownames=F)
    }
  })
 output$map2 <- renderLeaflet({
   if(x() == "Select Parcel") {
     parcel %>% filter(parcel_id %in% input$parcel_name) %>%
       leaflet() %>%  addPolygons(weight=1, fillOpacity=0.3, fillColor="#F2C614", color="white", popup=paste0(input$parcel_name),
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
   }else if(x() == "Click Any Point on Map"){
     test<-gis1  %>% left_join(
       tab %>% 
         distinct(id) %>%
         mutate(tab="tab"), by="id") %>%
       filter(!is.na(tab))
     leaflet(test) %>%
       addPolygons(layerId = ~id, stroke=T,fillOpacity=0,weight=1, color="white", opacity=0, 
                   popup=paste("<b>PlANNING AREA NAME:</b>", test$plan_name, "<br>",
                               "<b>SPECIAL AREA:</b>",test$special_area_id),
                   highlight = highlightOptions(
         weight = 1,
         fillOpacity = 0,
         color = "red",
         opacity = 0,
         bringToFront = TRUE,
         sendToBack = TRUE)) %>% 
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
       addLayersControl(
         baseGroups = c("Satellite","OSM",  "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE))
   }else if (x() == "Select Planning Areas"){
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
   } else if(x() == "Select Regional Land Use" & x_button() =="Use Groups"){
     test9<-gis1 %>% left_join(
       tab %>% 
         filter(use1 %in% input$use_group) %>% 
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
   } else if(x() == "Select Regional Land Use" & x_button() =="Use Categories"){
     test9<-gis1 %>% left_join(
       tab %>% 
         filter(use2 %in% input$use_cat) %>% 
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
   } else if(x() == "Select Regional Land Use" & x_button() == "Specific Uses"){
     test<-gis1 %>% left_join(
       tab %>% 
         filter(use3 %in% input$use_name) %>% 
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
   }  else {
     switch(input$input_type,
            "Click Any Point on Map",
            "Upload Shapefile Boundary" = fileInput(inputId = "filemap", label = "Select Shapefile",
                                                    multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
            "Select Planning Areas" = selectInput(selected=" ",inputId = "area_name",label="Select Planning Area:", multiple=T,  choices=sort(as.vector(c(unique(as.character(gis1$id_name))," ")))),
            "Select Regional Land Use" = radioButtons(inputId = "use_button",label="Select Level of Analysis",   choices=c("Specific Uses", "Use Categories","Use Groups"),selected = character(0)),
            "Select Parcel" = selectInput(selected=" ",inputId = "parcel_name",label="Select Parcel:", multiple=T,  choices=sort(as.vector(c(unique(parcel$parcel_id)," ")))))
    }
  })
 output$ui2 <- renderUI({
   if (is.null(input$use_button)){
     return(NULL)
   } else if (input$input_type != "Select Regional Land Use"){
    return(NULL)
   }else {
     switch(input$use_button,
 "Specific Uses"=selectInput(selected=" ",inputId = "use_name",label="Select Zoning Use:", multiple=T,  choices=sort(as.vector(c(unique(tab$use3)," ")))),
 "Use Groups"=selectInput(selected=" ",inputId = "use_group",label="Select Zoning Use Group:", multiple=T,  choices=sort(as.vector(c(unique(tab$use1)," ")))),
 "Use Categories"=selectInput(selected=" ",inputId = "use_cat",label="Select Zoning Use Category:", multiple=T,  choices=sort(as.vector(c(unique(tab$use2)," ")))))
   }
 })
  }
)

