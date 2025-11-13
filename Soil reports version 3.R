library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(shinyjs)
library(tidyverse)
library(dplyr)
library(flextable)
library(gdtools)
library(jsonlite)
library(viridis)
library(firebase)
library(httr)



# Increase max file upload size 
options(shiny.maxRequestSize = 30*1024^4)

firebase_config(
  api_key = "AIzaSyBLz93hGuvCGoWuxDr4ShmPNHQZphoiZeE",
  auth_domain = "soil-sampling-point.firebaseapp.com",
  project_id = "soil-sampling-point",
  app_id = "1:601055699938:web:458199ff64edce13f98fbb",
  overwrite = TRUE
)



# Read-in shapefile function
Read_Shapefile <- function(shp_path) {
  infiles <- shp_path$datapath # get the location of files
  dir <- unique(dirname(infiles)) # get the directory
  outfiles <- file.path(dir, shp_path$name) # create new path name
  name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name 
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
  x <- read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
  return(x)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  
  # CSS styling
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap');
      body {
        font-family: 'Poppins', sans-serif;
        background-color: white;
        color: #008B8B;
      }
      h1,
      h3 {
        font-family: 'Poppins', sans-serif;
        font-weight: 600;
        color: #008B8B;
        text-align: center;
      }
      h4 {
        font-family: 'Poppins', sans-serif;
        font-weight: 600;
        color: #008B8B;
        text-align: left;
      }
      h5 {
        font-family: 'Poppins', sans-serif;
        color: #000000;
        text-align: left;
      }
      #download_button {
      padding-left: 5%;
      padding-right: 5%;
      padding-bottom: 5%;
      }
      .shiny-input-container {
        background-color: #F5FFFA;
        color: black;
      }
      .well {
        background-color:#F5FFFA;
        border-color:#008B8B; 
                    }
      #warning {
             font-family: 'Poppins', sans-serif;
        font-weight: 600;
        color: 'red';
        text-align: center;       
      }"))
  ),
  
  fluidRow(img(src = "FCT Logo (Full Colour).jpg", width="25%",align="left")),  
  
  
  
  # Application title
  titlePanel(title= h1("FCT data"), windowTitle = "FCT data"),
  
  hr(),
  hr(),
  
  useFirebase(), 
  
  uiOutput("main")
  
)


server <- function(input, output) {
  
  f <- FirebaseEmailPassword$new(persistence = "local")
  
  
  
  # showTab(inputId = "tabs","Import data")
  #  hideTab(inputId = "tabs","Soil report tables")
  #  hideTab(inputId = "tabs","Farm Carbon Calculator results")
  #  hideTab(inputId = "tabs","names_to_parcelid")
  #  showTab(inputId = "tabs","Map")
  
  
  
  soil_map_choices <- data.frame(Label = c("Total carbon yield (t/ha)","Mean SOC (t/ha)",  "Mean SOM (%)",
                                           "P concentration (mg/l)","P index","K concentration (mg/l)",
                                           "K index", "Mg concentration (mg/l)" ,"Mg index"),
                                 Variable = c("Total_carbon_yield", "mean_SOC",
                                              "mean_SOM", "P_concentration","P_index",
                                              "K_concentration","K_index","Mg_concentration",
                                              "Mg_index"))
  
  observeEvent(input$start_again,{
    
    refresh()
    
    # hideTab(inputId = "tabs","Soil report tables")
    #  hideTab(inputId = "tabs","Farm Carbon Calculator results")
    #  hideTab(inputId = "tabs","names_to_parcelid")
    
    #  show("add_data_from_firestore")
    #  output$invalid_farm_warning <- renderText({""})
    #  output$soil_obs_warning <- renderText({""})
    #  hide("lab_data_input")
    #  hide(id = "Import_data")
    #  hide(id="soil_data_head")
    #  hide(id="data_head")
    #  hide(id="crop_type")
    #  hide(id="crop_type_text")
    #  hide(id="text_div_crops")
  })
  
  
  observe({
    output$main <- renderUI({
      
      if (is.null(f$get_signed_in())) {
        
        wellPanel(
          #fluidRow(column(offset = 4, width=4,textOutput("password_warning"))),
          fluidRow(column(offset = 4, width=4, textInput("email_signin", "Your email"))),
          fluidRow(column(offset = 4, width=4, passwordInput("password_signin", "Your password"))),
          fluidRow(column(offset=8,width=4,actionButton("signin", "Sign in"))))
        
      } else if (f$get_signed_in()$success == TRUE) {
        
        sidebarLayout(
          sidebarPanel(
            useShinyjs(),
            # Input: Select a file ---
            fileInput(inputId = "shp", label = h5("Upload shapefile (.shp, .dbf, .sbn, .sbx, .shx, .prj)"), multiple = TRUE,
                      accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj')),
            fileInput(inputId = "jsonreport", label = h5("Upload report .json"),
                      accept = c('.json'))
          ),
          
          
          mainPanel(
            useShinyjs(),
            tabsetPanel( id = "tabs", type = "tabs",
                         tabPanel("Import data",
                                  br(),
                                  
                                  div(id = "add_data_from_firestore",
                                      wellPanel(fluidRow(column(width=8,h4("Get sampled points from database"))),
                                                fluidRow(column(width=8, checkboxInput("pre_update", "Tick this box if the data you need was sampled before 10/10/2024", value = FALSE, width = NULL))),
                                                fluidRow(column(width=8, checkboxInput("stirleyfarm_box", "Tick this box if the data you need is for Stirley Farm", value = FALSE, width = NULL))),
                                                fluidRow(column(width=4,textInput("project_name",h5("Enter project name"), value = "")),
                                                         column(width=4,textInput("project_year",h5("Enter project year"), value = "")),
                                                         column(width=4,textInput("farm_name",h5("Enter farm name"), value = ""))),
                                                div(id = "warning",fluidRow(textOutput("soil_obs_warning")),
                                                    fluidRow(textOutput("invalid_farm_warning"))),
                                                fluidRow(actionButton("soil_obs_data","Get data")))),
                                  br(),
                                  hidden(div(id = "soil_data_head",
                                             fluidRow(column(width=4,actionButton("start_again", "Start again"))),
                                             br(),
                                             fluidRow(column(width=4,downloadButton("soil_obs_data_download","Download soil sampling point data"))),
                                             br(),
                                             leafletOutput("sampling_map"),
                                             br(),
                                             fluidRow(column(width=4,downloadButton("soil_obs_field_data_download","Download field level soil observation data"))),
                                             br(),
                                             br(),
                                             fluidRow(h4("Preview of field-level soil sampling data:")),
                                             br(),
                                             tableOutput("soil_obs_table"),
                                             br())),
                                         hidden(div(id = "lab_data_warning",textOutput("lab_warning"))),
                                         hidden(div(id = "lab_data_input",
                                                    actionButton("Add_lab_data", h5("Get lab data from database for this farm")))),
                                  hidden(div(id = "Import_data", fluidRow(a(href="csv_template.csv", "Download template csv file for data input", download=NA, target="_blank")),
                                                    fluidRow(h5("Or access the google sheets version here to",a(href="https://docs.google.com/spreadsheets/d/1HEjLOdmbzphYvK1i2VA-uYFpMSrJLIDVrt4C4SGAaM0/edit?gid=0#gid=0", "make a copy", target="_blank"))))),
                                  br(),
                                  hidden(div(id = "data_head", fluidRow(h4("Preview of the uploaded lab data joined to soil observation data:")),
                                             br(),
                                             tableOutput("fileLoaded"))),
                                  br(),
                                  
                                  hidden(div(id = "crop_type",
                                             fluidRow(actionButton("add_crop_type", "Click to assign crop type to fields")))),
                                  hidden(div(id = "crop_type_text",fluidRow(h4("Please assign crop type to fields:")),
                                             uiOutput("Assign_cropping_type"),
                                             uiOutput("submit_crop_types"))),
                                  hidden(div(id='text_div_crops',
                                             fluidRow( textOutput("text_crops")))),
                                  br()),
                         tabPanel("Soil report tables",
                                  br(),
                                  div(id = "please_upload", fluidRow(h4("Please upload csv to view soil tables and download"))),
                                  br(),
                                  hidden(div(id = "Preview",
                                             fluidRow(id="download_button",column(width=4,downloadButton("report", "Download word document of soil report tables"))),
                                             br(),
                                             fluidRow(id="download_button_Duchy",column(width=4,downloadButton("Duchy_report", "Download word document of soil report tables - Duchy version"))),
                                             br(),
                                             fluidRow(h3("Preview of soil report tables")))),
                                  br(),
                                  fluidRow(uiOutput("flextable_preview")),
                                  br()),
                         tabPanel("Farm Carbon Calculator results",
                                  br(),
                                  div(id = "please_upload_json", fluidRow(h4("Please upload report json to view carbon footprint results and download"))),
                                  
                                  hidden(div(id="json_preview",
                                             div(id="sc_download_button",fluidRow(downloadButton("sc_report", "Download word document of soil and carbon report tables")),
                                                 br(),
                                                 fluidRow(downloadButton("sc_report_Duchy", "Download Duchy version of soil and carbon report tables"))),
                                             br(),
                                             tableOutput("jsonfileLoaded"),
                                             br(),
                                             br(),
                                             plotOutput("carbon_balance_barplot"),
                                             br(),
                                             uiOutput("cbaltable"),
                                             uiOutput("seqtable"),
                                             uiOutput("carbon_balance_flextable")))
                         ),
                         tabPanel("Field names to parcel ID", value = "names_to_parcelid",
                                  br(),
                                  hidden(div(id = "parcelIDsbutton",
                                             fluidRow(h4("Please upload a csv and shapefile, and then click to match field names to parcel ID.")),
                                             fluidRow(actionButton("field_names_parcelid", "Click to add parcel IDs")),
                                             br(),
                                             fluidRow(actionButton("parcelid_already_loaded", "Click if the uploaded csv file already contains parcel IDs")))),
                                  hidden(div(id='parcel_ids_text',
                                             fluidRow(h4("If the uploaded csv file doesn't contain parcel IDs,
                                        select the correct parcel ID for each field name below to
                                        plot the soil analysis data on the map.")))),
                                  br(),
                                  fluidRow(uiOutput("field_names")),
                                  br(),
                                  hidden(div(id='text_div',
                                             fluidRow( textOutput("text")))),
                                  hidden(div(id='text_div2',
                                             fluidRow( wellPanel(textOutput("text_already_loaded"))))),
                                  fluidRow(uiOutput("submit_field_names")),
                                  br()),
                         tabPanel("Map", value = "map_tab",
                                  br(),
                                  div(id = "Import_shp", fluidRow(h4("Please upload a shapefile")),
                                      fluidRow(a(href="https://environment.data.gov.uk/rpa", "Click here to download field boundaries from RPA", target="_blank"))),
                                  br(),
                                  hidden(div(id = "soilmapbutton",
                                             fluidRow(column(width = 6,wellPanel(actionButton("soil_data_map", "Click to show\nsoil data map"))),
                                                      column(width = 6,wellPanel(selectInput("soil_data_variable", "Select a variable to show on the map",
                                                                                             choices = list("Total carbon yield" = "Total_carbon_yield",
                                                                                                            "Mean SOC" = "mean_SOC",
                                                                                                            "Mean SOM" = "mean_SOM",
                                                                                                            "P concentration" = "P_concentration",
                                                                                                            "P index" = "P_index",
                                                                                                            "K concentration" = "K_concentration",
                                                                                                            "K index" = "K_index",
                                                                                                            "Mg concentration" = "Mg_concentration",
                                                                                                            "Mg index" = "Mg_index"),
                                                                                             selected = "Total_carbon_yield")))))),
                                  # br(),
                                  #  fluidRow(id="download_map_button",downloadButton("map_download", "Download map")))),
                                  hidden(div(id = "mapbutton",
                                             fluidRow(actionButton("field_map", "Click to show map")))),
                                  br(),
                                  leafletOutput("map"),
                                  br(),
                                  br(),
                                  br())
            )
          ))
        
      } 
      
    })
    
    observeEvent(input$signin, {
      
      f$sign_in(input$email_signin, input$password_signin)
      
      # if (is.null(f$get_signed_in())) {
      #    output$password_warning <- renderText({"Invalid username or password"})
      #  }
    })
  })
  
  
  
  
  
  observeEvent(input$soil_obs_data, {
    
    hide(id="crop_type_text")
    
    
    if (input$project_name != "" | input$project_year != "" | input$farm_name != "") {
      
      
      if (input$pre_update == FALSE) {
        
        
        
        queryUrl <- paste0("https://firestore.googleapis.com/v1/projects/soil-sampling-point/databases/(default)/documents:runQuery")
        
        
        req_project_name <- ifelse(input$project_name != "",paste0('"where": {"fieldFilter": {"field": {"fieldPath": "project"},"op": "EQUAL","value": {"stringValue": "',toupper(trimws(input$project_name)),'"}}}'),"")
        req_project_year <- ifelse(input$project_year != "",paste0('"where": {"fieldFilter": {
         "field": {
           "fieldPath": "projectYear"
         },
         "op": "EQUAL",
         "value": {
           "stringValue": "',gsub("\\D+", "", input$project_year),'"
         }
                }}'),"")
        req_farm_name <- ifelse(input$farm_name != "",paste0('"where": {"fieldFilter": {
         "field": {
           "fieldPath": "farmName"
         },
         "op": "EQUAL",
         "value": {
           "stringValue": "',toupper(trimws(input$farm_name)),'"
         }
                }}'),"")
        
        where_list <- paste0(req_project_name,ifelse(req_project_name != "" & (req_project_year != "" | req_farm_name != ""),",",""),
                             req_project_year,ifelse(req_project_year != "" & req_farm_name != "",",",""),
                             req_farm_name)
        
        queryBody <- paste0('{
      "structuredQuery": {
      "from": [{ "collectionId": "samplePoint" }],',where_list,'
         
      }}')
        
      } 
      else {
        
        queryUrl <- paste0("https://firestore.googleapis.com/v1/projects/soil-sampling-point/databases/(default)/documents:runQuery")
        
        
        req_project_name <- ifelse(input$project_name != "",paste0('"where": {"fieldFilter": {"field": {"fieldPath": "project"},"op": "EQUAL","value": {"stringValue": "',input$project_name,'"}}}'),"")
        req_project_year <- ifelse(input$project_year != "",paste0('"where": {"fieldFilter": {
         "field": {
           "fieldPath": "projectYear"
         },
         "op": "EQUAL",
         "value": {
           "stringValue": "',gsub("\\D+", "", input$project_year),'"
         }
                }}'),"")
        req_farm_name <- ifelse(input$farm_name != "",paste0('"where": {"fieldFilter": {
         "field": {
           "fieldPath": "farmName"
         },
         "op": "EQUAL",
         "value": {
           "stringValue": "',input$farm_name,'"
         }
                }}'),"")
        
        where_list <- paste0(req_project_name,ifelse(req_project_name != "" & (req_project_year != "" | req_farm_name != ""),",",""),
                             req_project_year,ifelse(req_project_year != "" & req_farm_name != "",",",""),
                             req_farm_name)
        
        queryBody <- paste0('{
      "structuredQuery": {
      "from": [{ "collectionId": "samplePoint" }],',where_list,'
         
      }}')
      }
      
      response <- POST(
        url = queryUrl,
        body = queryBody,
        add_headers("Authorization" = paste("Bearer", paste(f$get_signed_in()$response$stsTokenManager$accessToken)),
                    "Content-Type" = "application/json")
      )
      results <- fromJSON(rawToChar(response$content))
      
      soil_obs <<- data.frame(project = results$document$fields$project$stringValue,
                              Project.year = results$document$fields$projectYear$stringValue,
                              Farm.name = results$document$fields$farmName$stringValue,
                              Field.name = results$document$fields$fieldName$stringValue,
                              Latitude = results$document$fields$latitude$stringValue,
                              Longitude = results$document$fields$longitude$stringValue,
                              VESS.top = results$document$fields$VESSTop$stringValue,
                              VESS.bottom = results$document$fields$VESSBottom$stringValue,
                              Earthworms = results$document$fields$earthworms$stringValue,
                              Infiltration = results$document$fields$infiltration$stringValue,
                              Lab.sample = results$document$fields$labSample$booleanValue,
                              Ag.stability = results$document$fields$agS$booleanValue,
                              Aggregate.stability.5.mins..1. = results$document$fields$agStability5mins1$stringValue,
                              Aggregate.stability.5.mins..2. = results$document$fields$agStability5mins2$stringValue,
                              Aggregate.stability.5.mins..3. = results$document$fields$agStability5mins3$stringValue,
                              Aggregate.stability.120.mins..1. = results$document$fields$agStability120mins1$stringValue,
                              Aggregate.stability.120.mins..2. = results$document$fields$agStability120mins2$stringValue,
                              Aggregate.stability.120.mins..3. = results$document$fields$agStability120mins3$stringValue,
                              Timestamp = results$document$fields$createdAt$timestampValue
      )
      
      
      if (nrow(soil_obs) > 0) {
        
        
        mapping_points <- data.frame(Latitude = as.numeric(soil_obs$Latitude),
                                     Longitude = as.numeric(soil_obs$Longitude))
        output$sampling_map <- renderLeaflet(
          leaflet(mapping_points
                  # options = leafletOptions(minZoom = 15)
          ) %>% 
            # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
            #  setView(lng = -4.2026458, lat = 56.4906712, zoom = 5)
            addTiles() %>%
            addCircleMarkers(
              radius = 3,
              color = "red",
              stroke = FALSE, fillOpacity = 1
            )
        )
        
        
        show("lab_data_input")
       # show(id = "Import_data")
        show(id="soil_data_head")
        # showTab(inputId = "tabs","Import data")
        # showTab(inputId = "tabs","Soil report tables")
        # showTab(inputId = "tabs","Farm Carbon Calculator results")
        # showTab(inputId = "tabs","names_to_parcelid")
        # showTab(inputId = "tabs","Map")
        
        
        hide(id = "add_data_from_firestore")
        
        for (i in 1:nrow(soil_obs)) {
          soil_obs$Aggregate.stability.5.mins[i] <- mean(as.numeric(soil_obs$Aggregate.stability.5.mins..1.[i]),as.numeric(soil_obs$Aggregate.stability.5.mins..2.[i]),
                                                         as.numeric(soil_obs$Aggregate.stability.5.mins..3.[i]),na.rm=TRUE)
          soil_obs$Aggregate.stability.120.mins[i] <- mean(as.numeric(soil_obs$Aggregate.stability.120.mins..1.[i]),as.numeric(soil_obs$Aggregate.stability.120.mins..2.[i]),
                                                           as.numeric(soil_obs$Aggregate.stability.120.mins..3.[i]),na.rm=TRUE)
          
        }
        
        
        
        soil_obs_field_level <- group_by(soil_obs, Field.name, Project.year) %>% summarise(VESS_top_av = mean(as.numeric(VESS.top),na.rm=TRUE),
                                                                                            VESS_bottom_av = mean(as.numeric(VESS.bottom),na.rm=TRUE),
                                                                                            Worms_av = mean(as.numeric(Earthworms),na.rm=TRUE),
                                                                                            Ag_stability_5mins_av = mean(Aggregate.stability.5.mins,na.rm=TRUE),
                                                                                            Ag_stability_120mins_av = mean(as.numeric(Aggregate.stability.120.mins),na.rm=TRUE))
        colnames(soil_obs_field_level)[1] <- "Field_name"
        soil_obs_field_level <- soil_obs_field_level[order(soil_obs_field_level$Project.year),]
        soil_obs_field_level <- soil_obs_field_level[order(soil_obs_field_level$Field_name),]
        soil_obs_field_level$Field_name <- toupper(soil_obs_field_level$Field_name)
        
        if (input$stirleyfarm_box == TRUE) {
          soil_obs_field_level$Field_name <- as.numeric(soil_obs_field_level$Field_name)
          soil_obs_field_level <- soil_obs_field_level[order(soil_obs_field_level$Field_name),]
          soil_obs_field_level$Field_name <- as.character(soil_obs_field_level$Field_name)
        }
        
        soil_obs_field_level <<- soil_obs_field_level
        
        
        output$soil_obs_table <- renderTable({
          show("soil_data_head")
          head(soil_obs_field_level)
        })
        
        
        
        
        
        output$soil_obs_data_download <- downloadHandler(
          
          filename = paste0("Sampled_points_data_",soil_obs$Farm.name[1],".csv"), 
          content = function(file) {
            
            write.csv(soil_obs, file, row.names = FALSE)
            
          })  
        
        
        
        output$soil_obs_field_data_download <- downloadHandler(
          filename = paste0("Sampled_points_field_level_data_",soil_obs$Farm.name[1],".csv"),
          content = function(file) {
            write.csv(soil_obs_field_level, file, row.names = FALSE)}
        )
        
        
        observeEvent(input$Add_lab_data,{
          hide("lab_data_input")
                  # GET LAB RESULTS
        
        queryBody2 <- paste0('{
      "structuredQuery": {
      "from": [{ "collectionId": "labResults" }],',where_list,'
         
      }}')
      
      
      responseLab <<- POST(
        url = queryUrl,
        body = queryBody2,
        add_headers("Authorization" = paste("Bearer", paste(f$get_signed_in()$response$stsTokenManager$accessToken)),
                    "Content-Type" = "application/json")
      )
      resultsLab <- fromJSON(rawToChar(responseLab$content))
      
      soil_obs_lab <<- data.frame( #project = resultsLab$document$fields$project$stringValue,
                                   Farm_name = resultsLab$document$fields$farmName$stringValue,
                                   projectYear = resultsLab$document$fields$projectYear$stringValue,
                         
                              Field_name = resultsLab$document$fields$Field_name$stringValue,
                              Area_ha = resultsLab$document$fields$areaHa$stringValue,
                              Parcel_ID = resultsLab$document$fields$parcelID$stringValue,
                              SOM_0_10 = resultsLab$document$fields$SOM_0_10$stringValue,
                              SOM_10_30 = resultsLab$document$fields$SOM_10_30$stringValue,
                              SOM_30_50 = resultsLab$document$fields$SOM_30_50$stringValue,
                              BD_0_10 = resultsLab$document$fields$BD_0_10$stringValue,
                              BD_10_30 = resultsLab$document$fields$BD_10_30$stringValue,
                              BD_30_50 = resultsLab$document$fields$BD_30_50$stringValue,
                              SOC_0_10 = resultsLab$document$fields$SOC_0_10$stringValue,
                              SOC_10_30 = resultsLab$document$fields$SOC_10_30$stringValue,
                              SOC_30_50 = resultsLab$document$fields$SOC_30_50$stringValue,
                              P_concentration = resultsLab$document$fields$P_concentration$stringValue,
                              K_concentration = resultsLab$document$fields$K_concentration$stringValue,
                              Mg_concentration = resultsLab$document$fields$Mg_concentration$stringValue,
                              P_index = resultsLab$document$fields$P_index$stringValue,
                              K_index = resultsLab$document$fields$K_index$stringValue,
                              Mg_index = resultsLab$document$fields$Mg_index$stringValue,
                              pH = resultsLab$document$fields$pH$stringValue,
                              Ag_stability_5mins1 = resultsLab$document$fields$Ag_stability_5mins1$stringValue,
                              Ag_stability_5mins2 = resultsLab$document$fields$Ag_stability_5mins2$stringValue,
                              Ag_stability_5mins3 = resultsLab$document$fields$Ag_stability_5mins3$stringValue,
                              Ag_stability_120mins1 = resultsLab$document$fields$Ag_stability_120mins1$stringValue,
                              Ag_stability_120mins2 = resultsLab$document$fields$Ag_stability_120mins2$stringValue,
                              Ag_stability_120mins3 = resultsLab$document$fields$Ag_stability_120mins3$stringValue,
                              Timestamp = resultsLab$document$fields$createdAt$timestampValue
      )
      
      if (nrow(soil_obs_lab) > 0) {
        
        soil_obs_lab$projectYear <- as.integer(soil_obs_lab$projectYear)
        
      soil_obs_lab <-  soil_obs_lab %>% mutate_at(c("Area_ha","SOM_0_10" , "SOM_10_30", "SOM_30_50" , "BD_0_10" , "BD_10_30" ,            
                                  "BD_30_50" , "SOC_0_10", "SOC_10_30", "SOC_30_50" , "P_concentration" , "K_concentration" ,"Mg_concentration",
                                  "P_index" , "K_index", "Mg_index"  , "pH" ,"Ag_stability_5mins1" ,  "Ag_stability_5mins2" ,  "Ag_stability_5mins3",   "Ag_stability_120mins1", "Ag_stability_120mins2", "Ag_stability_120mins3"), as.numeric)
        

        field_data <- soil_obs_lab
        field_data <- field_data[order(field_data$Field_name),]
     #   field_data$Field_name <- toupper(field_data$Field_name)
        
     #   if (sum(field_data$Field_name == soil_obs_field_level$Field_name) == nrow(field_data)) {
          
          hide(id = "Import_data")
          show(id = "crop_type")
          
          field_data$projectYear <- as.integer(field_data$projectYear)
          
          for (i in 1:nrow(field_data)) {
            field_data$projectYear[i] <- ifelse(is.na(field_data$projectYear[i]),1,field_data$projectYear[i])
          }
          field_data <- field_data[order(field_data$projectYear),]
          
          if (sum(field_data$Ag_stability_5mins1 != rep("",nrow(field_data)),na.rm=TRUE) > 0) {
            extra_ag_results <- field_data[,c("Ag_stability_5mins1",	"Ag_stability_5mins2",
                                              "Ag_stability_5mins3",	"Ag_stability_120mins1",	
                                              "Ag_stability_120mins2",	"Ag_stability_120mins3")]
            for (m in 1:nrow(soil_obs_field_level)) {
              soil_obs_field_level$Ag_stability_5mins_av[m] <- mean(as.numeric(field_data[m,c("Ag_stability_5mins1",	"Ag_stability_5mins2",
                                                                                              "Ag_stability_5mins3")]),na.rm=TRUE)
              soil_obs_field_level$Ag_stability_120mins_av[m] <- mean(as.numeric(field_data[m,c("Ag_stability_120mins1",	"Ag_stability_120mins2",
                                                                                                "Ag_stability_120mins3")]),na.rm=TRUE)
            }
            
          }
          
          
          field_data <- left_join(field_data,soil_obs_field_level, by = "Field_name")
          
          
          
          
          show(id = "data_head")
          output$fileLoaded <- renderTable({
            
            
            head(field_data)
            
          })
          
          
          
          
          field_data$mean_SOM <- numeric(nrow(field_data))
          for (i in 1:nrow(field_data)) {
            if (!is.na(as.numeric(field_data$SOM_0_10[i])) & !is.na(as.numeric(field_data$SOM_10_30[i])) & !is.na(as.numeric(field_data$SOM_30_50[i]))) {
              field_data$mean_SOM[i] <- sum(as.numeric(field_data$SOM_0_10[i])*10,
                                            as.numeric(field_data$SOM_10_30[i])*20,
                                            as.numeric(field_data$SOM_30_50[i])*20)/50
            } else if (is.na(as.numeric(field_data$SOM_0_10[i])) & !is.na(as.numeric(field_data$SOM_10_30[i])) & !is.na(as.numeric(field_data$SOM_30_50[i]))) {
              field_data$mean_SOM[i] <- sum(as.numeric(field_data$SOM_10_30[i])*20,
                                            as.numeric(field_data$SOM_30_50[i])*20, na.rm=TRUE)/40
            } else if ((!is.na(as.numeric(field_data$SOM_0_10[i])) & is.na(as.numeric(field_data$SOM_10_30[i])) & !is.na(as.numeric(field_data$SOM_30_50[i])))
                       | (!is.na(as.numeric(field_data$SOM_0_10[i])) & !is.na(as.numeric(field_data$SOM_10_30[i])) & is.na(as.numeric(field_data$SOM_30_50[i])))) {
              field_data$mean_SOM[i] <- sum(as.numeric(field_data$SOM_0_10[i])*10,
                                            as.numeric(field_data$SOM_10_30[i])*20,
                                            as.numeric(field_data$SOM_30_50[i])*20, na.rm=TRUE)/30
            } else if ((is.na(as.numeric(field_data$SOM_0_10[i])) & is.na(as.numeric(field_data$SOM_10_30[i])) & !is.na(as.numeric(field_data$SOM_30_50[i])))|
                       (is.na(as.numeric(field_data$SOM_0_10[i])) & !is.na(as.numeric(field_data$SOM_10_30[i])) & is.na(as.numeric(field_data$SOM_30_50[i])))){
              field_data$mean_SOM[i] <- sum(as.numeric(field_data$SOM_0_10[i])*10,
                                            as.numeric(field_data$SOM_10_30[i])*20,
                                            as.numeric(field_data$SOM_30_50[i])*20, na.rm=TRUE)/20
            } else if (!is.na(as.numeric(field_data$SOM_0_10[i])) & is.na(as.numeric(field_data$SOM_10_30[i])) & is.na(as.numeric(field_data$SOM_30_50[i]))) {
              field_data$mean_SOM[i] <- as.numeric(field_data$SOM_0_10[i])
            } else {
              field_data$mean_SOM[i] <- NA
            }
            
          }
          
          field_data$mean_SOM <- round(field_data$mean_SOM,2)
          
          
          
          field_data$P_index <- factor(field_data$P_index, labels = c(0:2,"2-","2+",3:9), levels = c(0:2,"2-","2+",3:9))
          field_data$K_index <- factor(field_data$K_index, labels = c(0:2,"2-","2+",3:9), levels = c(0:2,"2-","2+",3:9))
          field_data$Mg_index <- factor(field_data$Mg_index, labels = c(0:9), levels = c(0:9))
          
          indices <- read.csv("data/P_K_Mg_indices.csv")
          
          colnames(indices) <- c("Index", "Analysis",  "Lower_value_mgperl", "Upper_value_mgperl")    
          
          
          field_nutrients <- field_data[,c("Field_name", "P_index", "P_concentration","K_index",
                                           "K_concentration", "Mg_index",
                                           "Mg_concentration", "pH","mean_SOM")]
          
          
          
          #show(id = "crop_type")
          
          
          
          indices$Range <- ifelse((!is.na(indices$Index) & indices$Index == "9"), 
                                  paste0(indices$Lower_value_mgperl," and above"),
                                  ifelse((indices$Index == " "|is.na(indices$Index))," ",
                                         paste0(indices$Lower_value_mgperl," - ",
                                                indices$Upper_value_mgperl)))
          
          
          # Loop through rows in data and assign an Interpretation for pH
          field_nutrients$pH_meaning <- character(nrow(field_nutrients))
          
          for (i in 1:nrow(field_nutrients)) { 
            
            if (!is.na(field_nutrients$pH[i]) & field_nutrients$pH[i] < 5.5) {
              field_nutrients$pH_meaning[i] <- "Strongly acidic"
            } else if (!is.na(field_nutrients$pH[i]) & 
                       field_nutrients$pH[i] >= 5.5 &
                       field_nutrients$pH[i] < 6.5) {
              field_nutrients$pH_meaning[i] <- "Slightly acidic"
            } else if (!is.na(field_nutrients$pH[i]) & 
                       field_nutrients$pH[i] >= 6.5 &
                       field_nutrients$pH[i] < 7.5) 
            {field_nutrients$pH_meaning[i] <- "Neutral"
            } else if (!is.na(field_nutrients$pH[i]) & 
                       field_nutrients$pH[i] >= 7.5 &
                       field_nutrients$pH[i] < 8.5) {
              field_nutrients$pH_meaning[i] <- "Slightly alkaline"
            } else if (!is.na(field_nutrients$pH[i]) & 
                       field_nutrients$pH[i] >= 8.5
            ) {
              field_nutrients$pH_meaning[i] <- "Strongly alkaline"
            } else {
              field_nutrients$pH_meaning[i] <- ""
            }
            
          }
          
          
          
          # SOC calculation
          
          field_som_bd <- field_data[,c("Field_name", "SOM_0_10", "SOM_10_30", 
                                        "SOM_30_50", "BD_0_10", "BD_10_30",
                                        "BD_30_50", "SOC_0_10", "SOC_10_30", 
                                        "SOC_30_50")] %>% mutate_at(c("SOM_0_10", "SOM_10_30", 
                                       "SOM_30_50", "BD_0_10", "BD_10_30",
                                       "BD_30_50", "SOC_0_10", "SOC_10_30", 
                                       "SOC_30_50"), as.numeric)
          ii <<- field_som_bd
          colnames(field_som_bd) <- c("Field_name", "SOM0_10cm", "SOM10_30cm", 
                                      "SOM30_50cm","BD0_10gcm", "BD10_30gcm",
                                      "BD30_50gcm","SOC0_10cm", "SOC10_30cm", "SOC30_50cm")
          
          
          
          
          field_som_bd$SOC0_10cm <- ifelse(is.na(field_som_bd$SOC0_10cm) & !is.na(field_som_bd$SOM0_10cm), field_som_bd$SOM0_10cm/1.72, field_som_bd$SOC0_10cm)
          field_som_bd$SOC10_30cm <- ifelse(is.na(field_som_bd$SOC10_30cm) & !is.na(field_som_bd$SOM10_30cm), field_som_bd$SOM10_30cm/1.72, field_som_bd$SOC10_30cm)
          field_som_bd$SOC30_50cm <- ifelse(is.na(field_som_bd$SOC30_50cm) & !is.na(field_som_bd$SOM30_50cm), field_som_bd$SOM30_50cm/1.72, field_som_bd$SOC30_50cm)
          
          #  field_som_bd <-  field_som_bd %>%
          #    mutate(mean_SOC = rowMeans(select(.,c(SOC0_10cm, SOC10_30cm, SOC30_50cm)),na.rm = TRUE)) 
          
          
          field_som_bd$mean_SOC <- numeric(nrow(field_som_bd))
          for (i in 1:nrow(field_som_bd)) {
            if (!is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & !is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & !is.na(as.numeric(field_som_bd$SOC30_50cm[i]))) {
              field_som_bd$mean_SOC[i] <- sum(as.numeric(field_som_bd$SOC0_10cm[i])*10,
                                              as.numeric(field_som_bd$SOC10_30cm[i])*20,
                                              as.numeric(field_som_bd$SOC30_50cm[i])*20)/50
            } else if (is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & !is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & !is.na(as.numeric(field_som_bd$SOC30_50cm[i]))) {
              field_som_bd$mean_SOC[i] <- sum(as.numeric(field_som_bd$SOC10_30cm[i])*20,
                                              as.numeric(field_som_bd$SOC30_50cm[i])*20, na.rm=TRUE)/40
            } else if ((!is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & !is.na(as.numeric(field_som_bd$SOC30_50cm[i])))
                       | (!is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & !is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & is.na(as.numeric(field_som_bd$SOC30_50cm[i])))) {
              field_som_bd$mean_SOC[i] <- sum(as.numeric(field_som_bd$SOC0_10cm[i])*10,
                                              as.numeric(field_som_bd$SOC10_30cm[i])*20,
                                              as.numeric(field_som_bd$SOC30_50cm[i])*20, na.rm=TRUE)/30
            } else if ((is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & !is.na(as.numeric(field_som_bd$SOC30_50cm[i])))|
                       (is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & !is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & is.na(as.numeric(field_som_bd$SOC30_50cm[i])))){
              field_som_bd$mean_SOC[i] <- sum(as.numeric(field_som_bd$SOC0_10cm[i])*10,
                                              as.numeric(field_som_bd$SOC10_30cm[i])*20,
                                              as.numeric(field_som_bd$SOC30_50cm[i])*20, na.rm=TRUE)/20
            } else if (!is.na(as.numeric(field_som_bd$SOC0_10cm[i])) & is.na(as.numeric(field_som_bd$SOC10_30cm[i])) & is.na(as.numeric(field_som_bd$SOC30_50cm[i]))) {
              field_som_bd$mean_SOC[i] <- as.numeric(field_som_bd$SOC0_10cm[i])
            } else {
              field_som_bd$mean_SOC[i] <- NA
            }
            
          }
          
          field_som_bd$mean_SOC <- round(field_som_bd$mean_SOC,2)
          
          
          
          #replace all NA values with zero
          field_som_bd <-   field_som_bd %>% replace(is.na(.), 0)
          
          
          
          
          field_som_bd$carbon_yield0_10tperha <- 10*field_som_bd$BD0_10gcm*field_som_bd$SOC0_10cm
          field_som_bd$carbon_yield10_30tperha <- 20*field_som_bd$BD10_30gcm*field_som_bd$SOC10_30cm
          field_som_bd$carbon_yield30_50tperha <- 20*field_som_bd$BD30_50gcm*field_som_bd$SOC30_50cm
          
          field_som_bd$Total_carbon_yield <- field_som_bd$carbon_yield0_10tperha+field_som_bd$carbon_yield10_30tperha+
            field_som_bd$carbon_yield30_50tperha
          
          field_som_bd[,c(2:15)] <- round(field_som_bd[,c(2:15)],2)
          
          field_data <<- left_join(field_data,field_som_bd[,c("Field_name","mean_SOC","Total_carbon_yield")],by="Field_name")
          
          
          table2 <- field_som_bd[,c("Field_name","SOM0_10cm","SOM10_30cm","SOM30_50cm",
                                    "BD0_10gcm","BD10_30gcm","BD30_50gcm")] 
          
          table3 <- field_som_bd[,c("Field_name", "carbon_yield0_10tperha", "carbon_yield10_30tperha", "carbon_yield30_50tperha", "Total_carbon_yield" )]
          
          
          
          ## VESS
          
          table4 <- field_data[,c("Field_name","VESS_top_av", "VESS_bottom_av", "Worms_av"
                                  ,"Ag_stability_5mins_av", "Ag_stability_120mins_av")] 
          
          table4[,c(2:6)] <- round(table4[,c(2:6)],2)
          
          vess_table <<- table4
          field_nutrients_table <<- field_nutrients[,c("Field_name", "P_index", "P_concentration","K_index",
                                                       "K_concentration", "Mg_index",
                                                       "Mg_concentration", "pH")]
          
          
          
          
          
          output$flextable_preview <- renderUI(
            
            verticalLayout(
              flextable(field_nutrients_table) %>% 
                set_header_labels("Field_name" = "Field",
                                  "cropping_type" = "Cropping type",
                                  "P_index" = "P index",
                                  "P_concentration" = "P concentration (mg/l)",
                                  "K_index" = "K index",
                                  "K_concentration" = "K concentration (mg/l)",
                                  "Mg_index" = "Mg index",
                                  "Mg_concentration" = "Mg concentration (mg/l)",
                                  "pH" = "pH value",
                                  "pH_meaning" = "pH",
                                  "optimum_pH" = "Optimum pH") %>%
                merge_v(j = "Field_name", part = "body") %>%
                autofit(add_w = 0.1,add_h = 0.1,
                        part = c("body", "header"), unit = "in",hspans = "none") %>% 
                set_table_properties(layout = "autofit") %>%
                bg(i = ~ P_index == "0" , 
                   j = "P_index", 
                   bg = "#f88379", part = "body") %>%
                bg(i = ~ K_index == "0" , 
                   j = "K_index", 
                   bg = "#f88379", part = "body") %>%
                bg(i = ~ Mg_index == "0" , 
                   j = "Mg_index", 
                   bg = "#f88379", part = "body") %>%
                bg(i = ~ P_index == "1" , 
                   j = "P_index", 
                   bg = "#ffe135", part = "body") %>%
                bg(i = ~ K_index == "1" , 
                   j = "K_index", 
                   bg = "#ffe135", part = "body") %>%
                bg(i = ~ Mg_index == "1" , 
                   j = "Mg_index", 
                   bg = "#ffe135", part = "body") %>%
                bg(i = ~ P_index == "2-" | P_index == "2+" | P_index == "2" , 
                   j = "P_index", 
                   bg = "#74c2bb", part = "body") %>%
                bg(i = ~ K_index == "2-" | K_index == "2+" , 
                   j = "K_index", 
                   bg = "#74c2bb", part = "body") %>%
                bg(i = ~ Mg_index == "2" , 
                   j = "Mg_index", 
                   bg = "#74c2bb", part = "body") %>%
                bg(i = ~ P_index == "3" | P_index == "4"   , 
                   j = "P_index", 
                   bg = "#83bae3", part = "body") %>%
                bg(i = ~ K_index == "3"| K_index == "4"  , 
                   j = "K_index", 
                   bg = "#83bae3", part = "body") %>%
                bg(i = ~ Mg_index == "3" | Mg_index == "4" , 
                   j = "Mg_index", 
                   bg = "#83bae3", part = "body") %>%
                bg(i = ~ P_index == "5" | P_index == "6" | P_index == "7" | P_index == "8" | P_index == "9" , 
                   j = "P_index", 
                   bg = "#f88379", part = "body") %>%
                bg(i = ~ K_index == "5"| K_index == "6" | K_index == "7" | K_index == "8" | K_index == "9"  , 
                   j = "K_index", 
                   bg = "#f88379", part = "body") %>%
                bg(i = ~ Mg_index == "5" | Mg_index == "6" | Mg_index == "7" | Mg_index == "8" | Mg_index == "9" , 
                   j = "Mg_index", 
                   bg = "#f88379", part = "body")%>%
                font(fontname = ("Poppins"), part = "all") %>%
                fontsize(size = 9, part = "body") %>%
                fontsize(size = 10, part = "header") %>%
                theme_box() %>%
                align(align = "center", part="header") %>%
                htmltools_value(),
              hr(),
              flextable(table2) %>% 
                set_header_labels("Field_name" = "Field","SOM0_10cm" = "0 - 10cm", "SOM10_30cm" = "10 - 30cm",
                                  "SOM30_50cm" = "30 - 50cm","BD0_10gcm" = "0 - 10cm","BD10_30gcm" = "10 - 30cm",
                                  "BD30_50gcm" = "30 - 50cm" ) %>%
                add_header_row(top = TRUE, values = c(" ","Soil Organic Matter (%)", "Bulk density (g/cm3)"), colwidths = c(1,3,3)) %>%
                font(fontname = ("Poppins"), part = "all") %>%
                fontsize(size = 9, part = "body") %>%
                fontsize(size = 10, part = "header") %>%
                merge_v(j = "Field_name", part = "body") %>%
                align(align = "center", part="all") %>%
                autofit(add_w = 0.1,add_h = 0.1,
                        part = c("body", "header"), unit = "in",hspans = "none") %>% 
                set_table_properties(layout = "autofit") %>%
                theme_box() %>%
                align(align = "center", part="header") %>%
                htmltools_value(),
              hr(),
              flextable(table3) %>% 
                set_header_labels("Field_name" = "Field","carbon_yield0_10tperha" = "Carbon yield tonnes per ha 0 - 10cm",
                                  "carbon_yield10_30tperha" = "Carbon yield tonnes per ha 10 - 30cm",
                                  "carbon_yield30_50tperha" = "Carbon yield tonnes per ha 30 - 50cm",
                                  "Total_carbon_yield" = "Total carbon yield tonnes per ha for 0 - 50cm") %>%
                font(fontname = ("Poppins"), part = "all") %>%
                fontsize(size = 9, part = "body") %>%
                fontsize(size = 10, part = "header") %>%
                align(align = "center", part="all") %>%
                merge_v(j = "Field_name", part = "body") %>%
                autofit(add_w = 0.1,add_h = 0.1,
                        part = c("body", "header"), unit = "in",hspans = "none") %>% 
                set_table_properties(layout = "autofit") %>%
                theme_box() %>%
                align(align = "center", part="header") %>%
                htmltools_value(),
              hr(),
              flextable(table4) %>% 
                set_header_labels("Field_name" = "Field","VESS_top_av" = "VESS top", "VESS_bottom_av" = "VESS bottom", "Worms_av" = "Worms", 
                                  "Ag_stability_5mins_av" = "Ag (5mins)", "Ag_stability_120mins_av" = "Ag (120mins)") %>%
                font(fontname = ("Poppins"), part = "all") %>%
                fontsize(size = 9, part = "body") %>%
                fontsize(size = 10, part = "header") %>%
                align(align = "center", part="all") %>%
                merge_v(j = "Field_name", part = "body") %>%
                autofit(add_w = 0.1,add_h = 0.1,
                        part = c("body", "header"), unit = "in",hspans = "none") %>% 
                set_table_properties(layout = "autofit") %>%
                theme_box() %>%
                align(align = "center", part="header") %>%
                htmltools_value()
              
              
            ))
          
          
          show(id = "parcelIDsbutton")
          hide(id = "please_upload")  
          show(id = "Preview")
          
          
          
          
          
          
          observeEvent(input$add_crop_type, {
            hide("crop_type")
            show("crop_type_text")
            output$Assign_cropping_type <- renderUI({
              tagList(
                lapply(1:length(field_data$Field_name),function(i){
                  fluidRow(column(offset=3, width=6, div(
                    wellPanel(selectInput(paste0("crop_",field_data$Field_name[i]), label = paste(field_data$Field_name[i]), choices = list("Continuous grass or grass-clover swards",
                                                                                                                                            "Grass with an occasional barley crop",
                                                                                                                                            "Grass with an occasional wheat or oat crop",
                                                                                                                                            "Continuous arable cropping"), selected = "Continuous grass or grass-clover swards")))))
                } ) )
            })
            
            
            output$submit_crop_types <- renderUI({
              fluidRow(column(width=4,wellPanel(actionButton("submit_crops", "Submit")))) 
            }) 
            
          })
          
          
          
          observeEvent(input$submit_crops, {
            
            crop_types_submitted <- character(length(field_data$Field_name))
            
            for (i in 1:length(field_data$Field_name)) {
              crop_types_submitted[i] <- input[[paste0("crop_",field_data$Field_name[i])]]
            }
            
            
            show('text_div_crops')
            output$text_crops <- renderText({"Crop types successfully submitted, thank you."})
            
            for (i in 1:nrow(field_nutrients)) {
              field_nutrients$mean_SOM_0_30[i] <- sum(as.numeric(field_data$SOM_0_10[i])*10, as.numeric(field_data$SOM_10_30[i])*20, na.rm=TRUE)/30
            }
            field_nutrients$cropping_type <- crop_types_submitted
            field_nutrients <- field_nutrients[,c("Field_name","cropping_type", "P_index", "P_concentration","K_index",
                                                  "K_concentration", "Mg_index",
                                                  "Mg_concentration", "pH","mean_SOM","mean_SOM_0_30")]
            field_nutrients$optimum_pH <- character(nrow(field_nutrients))
            for (i in 1:nrow(field_nutrients)) { 
              field_nutrients$optimum_pH[i] <- ifelse(field_nutrients$mean_SOM_0_30[i] < 10 
                                                      & field_nutrients$cropping_type[i] == "Continuous arable cropping",6.5,
                                                      ifelse(field_nutrients$mean_SOM_0_30[i] < 10 
                                                             & field_nutrients$cropping_type[i] == "Grass with an occasional barley crop",6.2,
                                                             ifelse(field_nutrients$mean_SOM_0_30[i] < 10 
                                                                    & (field_nutrients$cropping_type[i] == "Grass with an occasional wheat or oat crop" |
                                                                         field_nutrients$cropping_type[i] == "Continuous grass or grass-clover swards") ,6,
                                                                    ifelse(field_nutrients$mean_SOM_0_30[i] > 20 
                                                                           & field_nutrients$cropping_type[i] == "Continuous arable cropping",5.8,
                                                                           ifelse(field_nutrients$mean_SOM_0_30[i] > 20 
                                                                                  & field_nutrients$cropping_type[i] == "Grass with an occasional barley crop",5.5,
                                                                                  ifelse(field_nutrients$mean_SOM_0_30[i] > 20 
                                                                                         & (field_nutrients$cropping_type[i] == "Grass with an occasional wheat or oat crop" |
                                                                                              field_nutrients$cropping_type[i] == "Continuous grass or grass-clover swards") ,5.3,
                                                                                         ifelse((field_nutrients$mean_SOM_0_30[i] <= 20 & field_nutrients$mean_SOM_0_30[i] >= 10)
                                                                                                & field_nutrients$cropping_type[i] == "Continuous arable cropping",6,
                                                                                                ifelse((field_nutrients$mean_SOM_0_30[i] <= 20 & field_nutrients$mean_SOM_0_30[i] >= 10)
                                                                                                       & field_nutrients$cropping_type[i] == "Grass with an occasional barley crop",5.7,
                                                                                                       ifelse((field_nutrients$mean_SOM_0_30[i] <= 20 & field_nutrients$mean_SOM_0_30[i] >= 10)
                                                                                                              & (field_nutrients$cropping_type[i] == "Grass with an occasional wheat or oat crop" |
                                                                                                                   field_nutrients$cropping_type[i] == "Continuous grass or grass-clover swards") ,5.5, NA)))))))))
            }
            
            field_nutrients <- field_nutrients[,c("Field_name","cropping_type", "P_index", "P_concentration","K_index",
                                                  "K_concentration", "Mg_index",
                                                  "Mg_concentration", "pH","mean_SOM","optimum_pH")]  
            
            
            
            
            
            # Loop through rows in data and assign an Interpretation for pH
            field_nutrients$pH_meaning <- character(nrow(field_nutrients))
            
            for (i in 1:nrow(field_nutrients)) { 
              
              if (!is.na(field_nutrients$pH[i]) & field_nutrients$pH[i] < 5.5) {
                field_nutrients$pH_meaning[i] <- "Strongly acidic"
              } else if (!is.na(field_nutrients$pH[i]) & 
                         field_nutrients$pH[i] >= 5.5 &
                         field_nutrients$pH[i] < 6.5) {
                field_nutrients$pH_meaning[i] <- "Slightly acidic"
              } else if (!is.na(field_nutrients$pH[i]) & 
                         field_nutrients$pH[i] >= 6.5 &
                         field_nutrients$pH[i] < 7.5) 
              {field_nutrients$pH_meaning[i] <- "Neutral"
              } else if (!is.na(field_nutrients$pH[i]) & 
                         field_nutrients$pH[i] >= 7.5 &
                         field_nutrients$pH[i] < 8.5) {
                field_nutrients$pH_meaning[i] <- "Slightly alkaline"
              } else if (!is.na(field_nutrients$pH[i]) & 
                         field_nutrients$pH[i] >= 8.5
              ) {
                field_nutrients$pH_meaning[i] <- "Strongly alkaline"
              } else {
                field_nutrients$pH_meaning[i] <- ""
              }
              
            }
            
            
            
            # SOC calculation
            
            field_som_bd <- field_data[,c("Field_name", "SOM_0_10", "SOM_10_30", 
                                          "SOM_30_50", "BD_0_10", "BD_10_30",
                                          "BD_30_50", "SOC_0_10", "SOC_10_30", 
                                          "SOC_30_50")] %>% mutate_at(c("SOM_0_10", "SOM_10_30", 
                                         "SOM_30_50", "BD_0_10", "BD_10_30",
                                         "BD_30_50", "SOC_0_10", "SOC_10_30", 
                                         "SOC_30_50"), as.numeric)
            ii <<- field_som_bd
            colnames(field_som_bd) <- c("Field_name", "SOM0_10cm", "SOM10_30cm", 
                                        "SOM30_50cm","BD0_10gcm", "BD10_30gcm",
                                        "BD30_50gcm","SOC0_10cm", "SOC10_30cm", "SOC30_50cm")
            
            
            
            
            field_som_bd$SOC0_10cm <- ifelse(is.na(field_som_bd$SOC0_10cm) & !is.na(field_som_bd$SOM0_10cm), field_som_bd$SOM0_10cm/1.72, field_som_bd$SOC0_10cm)
            field_som_bd$SOC10_30cm <- ifelse(is.na(field_som_bd$SOC10_30cm) & !is.na(field_som_bd$SOM10_30cm), field_som_bd$SOM10_30cm/1.72, field_som_bd$SOC10_30cm)
            field_som_bd$SOC30_50cm <- ifelse(is.na(field_som_bd$SOC30_50cm) & !is.na(field_som_bd$SOM30_50cm), field_som_bd$SOM30_50cm/1.72, field_som_bd$SOC30_50cm)
            
            field_som_bd <-  field_som_bd %>%
              mutate(mean_SOC = rowMeans(select(.,c(SOC0_10cm, SOC10_30cm, SOC30_50cm)),na.rm = TRUE)) 
            
            
            #replace all NA values with zero
            field_som_bd <-   field_som_bd %>% replace(is.na(.), 0)
            
            
            
            
            field_som_bd$carbon_yield0_10tperha <- 10*field_som_bd$BD0_10gcm*field_som_bd$SOC0_10cm
            field_som_bd$carbon_yield10_30tperha <- 20*field_som_bd$BD10_30gcm*field_som_bd$SOC10_30cm
            field_som_bd$carbon_yield30_50tperha <- 20*field_som_bd$BD30_50gcm*field_som_bd$SOC30_50cm
            
            field_som_bd$Total_carbon_yield <- field_som_bd$carbon_yield0_10tperha+field_som_bd$carbon_yield10_30tperha+
              field_som_bd$carbon_yield30_50tperha
            
            field_som_bd[,c(2:15)] <- round(field_som_bd[,c(2:15)],2)
            
            field_data <<- left_join(field_data,field_som_bd[,c("Field_name","mean_SOC","Total_carbon_yield")],by="Field_name")
            
            
            table2 <- field_som_bd[,c("Field_name","SOM0_10cm","SOM10_30cm","SOM30_50cm",
                                      "BD0_10gcm","BD10_30gcm","BD30_50gcm")] 
            
            table3 <- field_som_bd[,c("Field_name", "carbon_yield0_10tperha", "carbon_yield10_30tperha", "carbon_yield30_50tperha", "Total_carbon_yield" )]
            
            
            
            ## VESS
            
            
            table4 <- field_data[,c("Field_name","VESS_top_av", "VESS_bottom_av", "Worms_av"
                                    ,"Ag_stability_5mins_av", "Ag_stability_120mins_av")] 
            table4$cropping_type <- crop_types_submitted
            table4[,c(2:6)] <- round(table4[,c(2:6)],2)
            
            
            
            table4 <- table4[,c("Field_name","cropping_type","VESS_top_av", "VESS_bottom_av", "Worms_av"
                                ,"Ag_stability_5mins_av", "Ag_stability_120mins_av")] 
            vess_table <<- table4
            
            field_nutrients_table <<- field_nutrients[,c("Field_name","cropping_type", "P_index", "P_concentration","K_index",
                                                         "K_concentration", "Mg_index",
                                                         "Mg_concentration", "pH","optimum_pH")]
            
            
            
            output$flextable_preview <- renderUI(
              
              verticalLayout(
                flextable(field_nutrients_table) %>% 
                  set_header_labels("Field_name" = "Field",
                                    "cropping_type" = "Cropping type",
                                    "P_index" = "P index",
                                    "P_concentration" = "P concentration (mg/l)",
                                    "K_index" = "K index",
                                    "K_concentration" = "K concentration (mg/l)",
                                    "Mg_index" = "Mg index",
                                    "Mg_concentration" = "Mg concentration (mg/l)",
                                    "pH" = "pH value",
                                    "pH_meaning" = "pH",
                                    "optimum_pH" = "Optimum pH") %>%
                  merge_v(j = "Field_name", part = "body") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  bg(i = ~ P_index == "0" , 
                     j = "P_index", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ K_index == "0" , 
                     j = "K_index", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ Mg_index == "0" , 
                     j = "Mg_index", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ P_index == "1" , 
                     j = "P_index", 
                     bg = "#ffe135", part = "body") %>%
                  bg(i = ~ K_index == "1" , 
                     j = "K_index", 
                     bg = "#ffe135", part = "body") %>%
                  bg(i = ~ Mg_index == "1" , 
                     j = "Mg_index", 
                     bg = "#ffe135", part = "body") %>%
                  bg(i = ~ P_index == "2-" | P_index == "2+" | P_index == "2" , 
                     j = "P_index", 
                     bg = "#74c2bb", part = "body") %>%
                  bg(i = ~ K_index == "2-" | K_index == "2+" , 
                     j = "K_index", 
                     bg = "#74c2bb", part = "body") %>%
                  bg(i = ~ Mg_index == "2" , 
                     j = "Mg_index", 
                     bg = "#74c2bb", part = "body") %>%
                  bg(i = ~ P_index == "3" | P_index == "4"   , 
                     j = "P_index", 
                     bg = "#83bae3", part = "body") %>%
                  bg(i = ~ K_index == "3"| K_index == "4"  , 
                     j = "K_index", 
                     bg = "#83bae3", part = "body") %>%
                  bg(i = ~ Mg_index == "3" | Mg_index == "4" , 
                     j = "Mg_index", 
                     bg = "#83bae3", part = "body") %>%
                  bg(i = ~ P_index == "5" | P_index == "6" | P_index == "7" | P_index == "8" | P_index == "9" , 
                     j = "P_index", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ K_index == "5"| K_index == "6" | K_index == "7" | K_index == "8" | K_index == "9"  , 
                     j = "K_index", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ Mg_index == "5" | Mg_index == "6" | Mg_index == "7" | Mg_index == "8" | Mg_index == "9" , 
                     j = "Mg_index", 
                     bg = "#f88379", part = "body")%>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>%
                  fontsize(size = 10, part = "header") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value(),
                hr(),
                flextable(table2) %>% 
                  set_header_labels("Field_name" = "Field","SOM0_10cm" = "0 - 10cm", "SOM10_30cm" = "10 - 30cm",
                                    "SOM30_50cm" = "30 - 50cm","BD0_10gcm" = "0 - 10cm","BD10_30gcm" = "10 - 30cm",
                                    "BD30_50gcm" = "30 - 50cm" ) %>%
                  add_header_row(top = TRUE, values = c(" ","Soil Organic Matter (%)", "Bulk density (g/cm3)"), colwidths = c(1,3,3)) %>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>%
                  fontsize(size = 10, part = "header") %>%
                  align(align = "center", part="all") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value(),
                hr(),
                flextable(table3) %>% 
                  set_header_labels("Field_name" = "Field","carbon_yield0_10tperha" = "Carbon yield tonnes per ha 0 - 10cm",
                                    "carbon_yield10_30tperha" = "Carbon yield tonnes per ha 10 - 30cm",
                                    "carbon_yield30_50tperha" = "Carbon yield tonnes per ha 30 - 50cm",
                                    "Total_carbon_yield" = "Total carbon yield tonnes per ha for 0 - 50cm") %>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>%
                  fontsize(size = 10, part = "header") %>%
                  align(align = "center", part="all") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value(),
                hr(),
                flextable(table4) %>%  
                  set_header_labels("Field_name" = "Field","cropping_type" = "Cropping type","VESS_top_av" = "VESS top", "VESS_bottom_av" = "VESS bottom", "Worms_av" = "Worms", 
                                    "Ag_stability_5mins_av" = "Ag (5mins)", "Ag_stability_120mins_av" = "Ag (120mins)") %>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>%
                  fontsize(size = 10, part = "header") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  bg(i = ~ VESS_top_av >= 3.5 & VESS_top_av <= 5, 
                     j = "VESS_top_av", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ VESS_bottom_av >= 3.5 & VESS_bottom_av <= 5, 
                     j = "VESS_bottom_av", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ Ag_stability_5mins_av >= 3 & Ag_stability_5mins_av <= 4,
                     j = "Ag_stability_5mins_av", 
                     bg = "#f88379", part = "body") %>%
                  bg(i = ~ Ag_stability_120mins_av >= 3 & Ag_stability_120mins_av <= 4,
                     j = "Ag_stability_120mins_av", 
                     bg = "#f88379", part = "body") %>%
                  
                  bg(i = ~ VESS_top_av >= 2.5 & VESS_top_av <= 3.49, 
                     j = "VESS_top_av", 
                     bg = "#ffe135", part = "body") %>%
                  bg(i = ~ VESS_bottom_av >= 2.5 & VESS_bottom_av <= 3.49, 
                     j = "VESS_bottom_av", 
                     bg = "#ffe135", part = "body") %>%
                  bg(i = ~ Ag_stability_5mins_av >= 1 & Ag_stability_5mins_av <= 2.99,
                     j = "Ag_stability_5mins_av", 
                     bg = "#ffe135", part = "body") %>%
                  bg(i = ~ Ag_stability_120mins_av >= 1.5 & Ag_stability_120mins_av <= 2.99,
                     j = "Ag_stability_120mins_av", 
                     bg = "#ffe135", part = "body") %>%
                  
                  bg(i = ~ VESS_top_av >= 1 & VESS_top_av <= 2.49, 
                     j = "VESS_top_av", 
                     bg = "#74c2bb", part = "body") %>%
                  bg(i = ~ VESS_bottom_av >= 1 & VESS_bottom_av <= 2.49, 
                     j = "VESS_bottom_av", 
                     bg = "#74c2bb", part = "body") %>%
                  bg(i = ~ Ag_stability_5mins_av >= 0 & Ag_stability_5mins_av <= 0.99,
                     j = "Ag_stability_5mins_av", 
                     bg = "#74c2bb", part = "body") %>%
                  bg(i = ~ Ag_stability_120mins_av >= 0 & Ag_stability_120mins_av <= 1.49,
                     j = "Ag_stability_120mins_av", 
                     bg = "#74c2bb", part = "body") %>%
                  
                  bg(i = ~ Worms_av >= 0 & Worms_av <= 3 & cropping_type == "Continuous arable cropping", 
                     j = "Worms_av", 
                     bg = "#f88379", part = "body") %>%
                  
                  bg(i = ~ Worms_av >= 0 & Worms_av <= 9 & (cropping_type == "Grass with an occasional barley crop" |
                                                              cropping_type == "Grass with an occasional wheat or oat crop" | cropping_type == "Continuous grass or grass-clover swards"), 
                     j = "Worms_av", 
                     bg = "#f88379", part = "body") %>%
                  
                  bg(i = ~ Worms_av >= 4 & Worms_av <= 8 & cropping_type == "Continuous arable cropping", 
                     j = "Worms_av", 
                     bg = "#ffe135", part = "body") %>%
                  
                  bg(i = ~ Worms_av >= 10 & Worms_av <= 19 & (cropping_type == "Grass with an occasional barley crop" |
                                                                cropping_type == "Grass with an occasional wheat or oat crop" | cropping_type == "Continuous grass or grass-clover swards"), 
                     j = "Worms_av", 
                     bg = "#ffe135", part = "body") %>%
                  
                  bg(i = ~ Worms_av >= 9 & cropping_type == "Continuous arable cropping", 
                     j = "Worms_av", 
                     bg = "#74c2bb", part = "body") %>%
                  
                  bg(i = ~ Worms_av >= 20 & (cropping_type == "Grass with an occasional barley crop" |
                                               cropping_type == "Grass with an occasional wheat or oat crop" | cropping_type == "Continuous grass or grass-clover swards"), 
                     j = "Worms_av", 
                     bg = "#74c2bb", part = "body") %>%
                  
                  theme_box() %>%
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value()
                
                
              ))
            
            
          })
          
          
          
          
          
          
          output$report <- downloadHandler(
            
            filename = "Soil_report_tables.doc",
            content = function(file) {
              res <- rmarkdown::render(
                "Ideal_table2024.Rmd",
                params = list(
                  soil_data = field_data,
                  crop_types = field_nutrients_table,
                  vess_crop_types = vess_table
                ))
              file.rename(res, file)
            })  
          
          output$Duchy_report <- downloadHandler(
            
            filename = "Soil_report_tables.doc",
            content = function(file) {
              res <- rmarkdown::render(
                "Duchy_Ideal_table2024.Rmd",
                params = list(
                  soil_data = field_data,
                  crop_types = field_nutrients_table
                ))
              file.rename(res, file)
            })
          
          
          
          # Read json once user submits files
          observeEvent(input$jsonreport, {
            
            hide(id = "please_upload_json")
            show(id = "json_preview")
            json_report <- jsonlite::fromJSON(input$jsonreport$datapath) 
            
            report_details <<- as.data.frame(json_report$emissions)
            
            report_summary <- data.frame(Report_name = json_report[,"title"],
                                         Report_period_start = json_report$reporting_period$start,
                                         Report_period_end = json_report$reporting_period$end,
                                         Farm_area_ha = sum(as.numeric(json_report$farm_areas$areas$Cultivated),as.numeric(json_report$farm_areas$areas$Grass),as.numeric(json_report$farm_areas$areas$`Non-cropping`),na.rm=TRUE),
                                         Total_emissions = sum(as.numeric(report_details$calculator_output$positive$value),na.rm=TRUE),
                                         Total_offset = sum(as.numeric(report_details$calculator_output$negative$value),na.rm=TRUE),
                                         Carbon_balance = sum(sum(as.numeric(report_details$calculator_output$positive$value),na.rm=TRUE), 
                                                              sum(as.numeric(report_details$calculator_output$negative$value),na.rm=TRUE),na.rm=TRUE),
                                         Scope_1_emissions = sum(as.numeric(report_details$calculator_output$scope_1$value),na.rm=TRUE),
                                         Scope_2_emissions = sum(as.numeric(report_details$calculator_output$scope_2$value),na.rm=TRUE),
                                         Scope_3_emissions = sum(as.numeric(report_details$calculator_output$scope_3$value),na.rm=TRUE),
                                         Methane = sum(sum(as.numeric(report_details$calculator_output$scope_1_CH4$value),na.rm=TRUE),
                                                       sum(as.numeric(report_details$calculator_output$scope_2_CH4$value),na.rm=TRUE),
                                                       sum(as.numeric(report_details$calculator_output$scope_3_CH4$value),na.rm=TRUE)),
                                         Nitrous_oxide  = sum(sum(as.numeric(report_details$calculator_output$scope_1_N2O$value),na.rm=TRUE),
                                                              sum(as.numeric(report_details$calculator_output$scope_2_N2O$value),na.rm=TRUE),
                                                              sum(as.numeric(report_details$calculator_output$scope_3_N2O$value),na.rm=TRUE)))
            
            emissions_by_type <- data.frame(report_details$section,report_details$calculator_output$positive$value) %>% 
              group_by(report_details.section) %>% 
              summarise(total_emissions_by_section = sum(report_details.calculator_output.positive.value, na.rm=TRUE)) %>% 
              pivot_wider(names_from = "report_details.section", values_from = "total_emissions_by_section")
            
            report_summary$Fuels <- ifelse(!is.null(emissions_by_type$fuels),emissions_by_type$fuels,0)
            report_summary$Livestock <- ifelse(!is.null(emissions_by_type$livestock),emissions_by_type$livestock,0)
            report_summary$Land_use <- ifelse(!is.null(emissions_by_type$sequestration),emissions_by_type$sequestration,0)
            report_summary$Waste <- ifelse(!is.null(emissions_by_type$waste),emissions_by_type$waste,0)
            report_summary$Distribution <- ifelse(!is.null(emissions_by_type$distribution),emissions_by_type$distribution,0)
            report_summary$Processing <- ifelse(!is.null(emissions_by_type$processing),emissions_by_type$processing,0)
            report_summary$Chemicals <- ifelse(!is.null(emissions_by_type$inputs),emissions_by_type$inputs,0)
            report_summary$Crops <- ifelse(!is.null(emissions_by_type$crops),emissions_by_type$crops,0)
            report_summary$Inventory <- ifelse(!is.null(emissions_by_type$inventory),emissions_by_type$inventory,0)
            report_summary$Materials <- ifelse(!is.null(emissions_by_type$materials),emissions_by_type$materials,0)
            
            report_summary$Inventory_and_materials <- sum(report_summary$Inventory,report_summary$Materials, na.rm=TRUE)
            report_summary <- report_summary[,c("Report_name","Report_period_start", "Report_period_end","Farm_area_ha",
                                                "Total_emissions" ,"Total_offset" , "Carbon_balance", "Scope_1_emissions",
                                                "Scope_2_emissions","Scope_3_emissions","Methane", "Nitrous_oxide" ,
                                                "Fuels", "Livestock", "Land_use" ,"Waste", "Distribution","Processing","Chemicals",
                                                "Crops","Inventory_and_materials")]
            
            
            neg_emissions_by_type <- data.frame(report_details$emissions_category$name,report_details$calculator_output$negative$value) %>% 
              group_by(report_details.emissions_category.name) %>% 
              summarise(total_negative_emissions_by_section = sum(report_details.calculator_output.negative.value, na.rm=TRUE))# %>% 
            
            seq_CS <- neg_emissions_by_type[grepl("countryside stewardship",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Countryside_stewardship <- sum(seq_CS$total_negative_emissions_by_section)
            
            seq_hedgerows <- neg_emissions_by_type[grepl("hedgerow",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_hedgerows <- sum(seq_hedgerows$total_negative_emissions_by_section)
            
            seq_woodland <- neg_emissions_by_type[grepl("woodland",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_woodland <- sum(seq_woodland$total_negative_emissions_by_section)
            
            seq_other <- neg_emissions_by_type[!grepl("countryside stewardship",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE)&
                                                 !grepl("hedgerow",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE)&
                                                 !grepl("woodland",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_other <- sum(seq_other$total_negative_emissions_by_section)
            
            
            
            output$jsonfileLoaded <- renderTable({
              report_summary
            })
            
            output$carbon_balance_barplot <- renderPlot({
              
              Palette <<- c("#88C906", "#00ACA6", "#644A3A", "#F5D000", "#FD8125", "#FF1900", "#E25DA9", "#A383E5", "#0053F0", "#00AAD3", "#01BE66", "#D1F179")
              
              carbon_balance <- data.frame(Type = c("Total emissions", "Total offset", "Carbon balance"),
                                           Emissions = c(report_summary$Total_emissions,report_summary$Total_offset,report_summary$Carbon_balance))
              carbon_balance$Type <- factor(carbon_balance$Type, levels = c("Total emissions", "Total offset", "Carbon balance"))
              
              cols <- Palette[1:3]
              ggplot(carbon_balance, aes(fill=Type, y=Emissions, x=Type)) +
                geom_bar(position='dodge', stat='identity') +
                ggtitle('Carbon balance') +
                xlab('') +
                ylab('Emissions (tCO2e)') +
                scale_fill_manual('Type', values=cols) +
                theme(text = element_text(family = "Poppins", size = 20),
                      panel.background = element_rect(fill = '#BBD1CF50'))
            })
            
            
            crops_tonnes <- report_details[report_details$section == "crops" & report_details$user_input$quantity$description == "Tonnes harvested",]
            total_crops_tonnes <- sum(crops_tonnes$user_input$quantity$value)
            carbon_balance_table <- data.frame("Metric" = c("Carbon Balance",
                                                            "Carbon Balance per hectare",
                                                            "Carbon balance per tonne crops"),
                                               "Value" = c( paste(round(report_summary$Carbon_balance,2),"tonnes of CO2e"),
                                                            paste(round((report_summary$Carbon_balance/report_summary$Farm_area_ha),2),"tonnes CO2e per hectare"),
                                                            paste(ifelse(total_crops_tonnes > 0, round(report_summary$Carbon_balance/total_crops_tonnes,2),0),"tonnes of CO2e per tonne of product"))
            )
            
            output$carbon_balance_flextable <- renderUI(
              verticalLayout(
                flextable(carbon_balance_table) %>% 
                  fontsize(size = 10, part = "header") %>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>% 
                  bg( bg="#F5D00080", part = "header") %>%
                  bg(bg="#00ACA620", part = "body") %>%
                  align(align = "center", part="all") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value()
                
              ))
            
            
            cbal_table <- data.frame(Emissions_Type = c("Fuels", "Livestock", "Land use" ,"Waste", "Distribution","Processing","Chemicals",
                                                        "Crops","Inventory and materials", "Total"),
                                     Emissions_value = c(report_summary$Fuels, report_summary$Livestock, report_summary$Land_use ,
                                                         report_summary$Waste, report_summary$Distribution,report_summary$Processing,report_summary$Chemicals,
                                                         report_summary$Crops,report_summary$Inventory_and_materials,
                                                         sum(report_summary$Fuels, report_summary$Livestock, report_summary$Land_use ,
                                                             report_summary$Waste, report_summary$Distribution,report_summary$Processing,report_summary$Chemicals,
                                                             report_summary$Crops,report_summary$Inventory_and_materials,na.rm=TRUE)))
            cbal_table$Emissions_value <- round(cbal_table$Emissions_value,2)
            
            cbal_table$percentage <- numeric(nrow(cbal_table))
            
            for (i in 1:(nrow(cbal_table)-1)) {
              cbal_table$percentage[i] <- (cbal_table$Emissions_value[i]/sum(cbal_table$Emissions_value[1:(nrow(cbal_table)-1)],na.rm=TRUE))*100  
            }
            cbal_table$percentage[nrow(cbal_table)] <- 100
            cbal_table$percentage <- round(cbal_table$percentage,2)
            
            output$cbaltable <- renderUI(
              verticalLayout(
                flextable(cbal_table) %>% 
                  set_header_labels("Emissions_Type" = "Emissions Type","Emissions_value"  = "Emissions value (tCO2e)",
                                    "percentage" = "Percentage of emissions (%)") %>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>%
                  fontsize(size = 10, part = "header") %>%
                  bg( bg="#00ACA680", part = "header") %>%
                  bg( i = c(1:9), bg="#00ACA620", part = "body") %>%
                  bg( i = 10, bg="#BBD1CF30", part = "body") %>%
                  align(align = "center", part="all") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value()
                
              ))
            
            
            seq_table <- data.frame(Seq_Type = c("Hedgerows","Woodland", "Countryside Stewardship","Other", "Total"),
                                    Seq_value = c(report_summary$Seq_hedgerows,report_summary$Seq_woodland,report_summary$Countryside_stewardship,
                                                  report_summary$Seq_other,sum(report_summary$Seq_hedgerows,report_summary$Seq_woodland,report_summary$Countryside_stewardship,
                                                                               report_summary$Seq_other,na.rm=TRUE)))
            seq_table$percentage <- numeric(nrow(seq_table))
            
            for (i in 1:4) {
              seq_table$percentage[i] <- (seq_table$Seq_value[i]/sum(seq_table$Seq_value[1:4],na.rm=TRUE))*100  
            }
            seq_table$percentage[5] <- 100
            seq_table$percentage <- round(seq_table$percentage,2)
            
            
            seq_table$Seq_value <- round(seq_table$Seq_value,2)
            output$seqtable <- renderUI(
              verticalLayout(
                flextable(seq_table) %>% 
                  set_header_labels("Seq_Type" = "Sequestration Type","Seq_value"  = "Sequestration value (tCO2e)","percentage"="Percentage of sequestration (%)" ) %>%
                  font(fontname = ("Poppins"), part = "all") %>%
                  fontsize(size = 9, part = "body") %>%
                  fontsize(size = 10, part = "header") %>%
                  bg( bg="#88C90680", part = "header") %>%
                  bg( i = c(1:4), bg="#00ACA620", part = "body") %>%
                  bg( i = 5, bg="#BBD1CF30", part = "body") %>%
                  align(align = "center", part="all") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value(),
                hr()
                
              ))
            
            
            
            output$sc_report <- downloadHandler(
              
              filename = "Soil_and_carbon_report_tables.doc",
              content = function(file) {
                res <- rmarkdown::render(
                  "Soil and carbon.Rmd",
                  params = list(
                    soil_data = field_data,
                    report_summary = report_summary,
                    report_details = report_details,
                    crop_types = field_nutrients_table,
                    vess_crop_types = vess_table
                    
                  ))
                file.rename(res, file)
              }) 
            
            
            
            output$sc_report_Duchy <- downloadHandler(
              
              filename = "Soil_and_carbon_report_tables.doc",
              content = function(file) {
                res <- rmarkdown::render(
                  "Soil and carbon_Duchy.Rmd",
                  params = list(
                    soil_data = field_data,
                    report_summary = report_summary,
                    report_details = report_details,
                    crop_types = field_nutrients_table
                    
                  ))
                file.rename(res, file)
              })  
            
            
          })  
          
      #  }
        
      #  else {
      #    output$lab_warning <- renderText({"Field names in uploaded csv do not match field names for selected soil sampling points"})
      #  }
      } else {
        output$lab_warning <- renderText({"No lab results for this farm"})
        show("lab_data_warning")
        show("Import_data")
      }
       
          
        })
        
        

      
      
      }     
      else {
        output$soil_obs_warning <- renderText({"Please enter the farm name for the sampled points you would like to download"})
      }
    }
  
 else {
  output$invalid_farm_warning <- renderText({"Sorry, no results for this search"})
}
  })
  
  
  
  observeEvent(input$field_names_parcelid, {
    if (!is.null(input$file1) & !is.null(input$shp)) {
      hide(id = "parcelIDsbutton")
      show(id='parcel_ids_text')
      hide(id = "mapbutton")
      show(id = "soilmapbutton")
      field_names <<- field_data$Field_name 
      parcel_ids <<- user_shp$parcel_id
      parcel_ids_forlist <- data.frame(ID = parcel_ids, num = c(1:length(parcel_ids)))
      parcel_choices <- setNames(as.list(parcel_ids_forlist$num),parcel_ids_forlist$ID)
      parcel_choices[["No parcel ID"]] <- length(parcel_ids)+1
      
      output$field_names <- renderUI({
        tagList(
          lapply(1:length(field_names),function(i){
            fluidRow(column(offset=3, width=6, div(
              wellPanel(selectInput(paste(field_names[i]), label = paste(field_names[i]), choices = parcel_choices, selected = length(parcel_ids)+1)))))
          } ) )
      })
      
      output$submit_field_names <- renderUI({
        fluidRow(column(width=4,wellPanel(actionButton("submit", "Submit")))) 
      }) 
      
      observeEvent(input$submit, {
        
        
        field_parcel_ids <- data.frame(Field_name = field_names, parcel_id = character(length(field_names)))
        
        for (i in 1:nrow(field_parcel_ids)) {
          
          if(as.numeric(input[[paste0(field_names[i])]]) < length(parcel_ids)+1) {
            field_parcel_ids$parcel_id[i] <-  parcel_ids[[as.numeric(input[[paste0(field_names[i])]])]]
          } else {
            field_parcel_ids$parcel_id[i] <- NA
          } }
        
        
        # Add data to display to parcel ids
        observeEvent(input$soil_data_variable, {
          csv_parcel_ids <- left_join(field_data[,c("Field_name",paste(input$soil_data_variable))], field_parcel_ids, by = "Field_name")
          # If the same parcel_id has been assigned to more than one field, then there will be duplicate rows with
          # different values of P_index. In order to not just overwrite, bring the duplicates together by calculating the mean
          # (doesn't strictly make sense for P. Index, but could be adapted)
          # csv_parcel_ids <- group_by(csv_parcel_ids, parcel_id) %>% summarise(assign(input$soil_data_variable, mean(get(input$soil_data_variable), na.rm=TRUE)))
          
          polygons_with_csv <- left_join(user_shp, csv_parcel_ids, by = "parcel_id")
          
          field_polygons <- sf::st_transform(polygons_with_csv, '+proj=longlat +datum=WGS84')
          if (input$soil_data_variable %in% c("Total_carbon_yield", "mean_SOC",
                                              "mean_SOM", "P_concentration",
                                              "K_concentration","Mg_concentration")) {
            bin_pal <- colorBin('YlGnBu', polygons_with_csv[[input$soil_data_variable]])
          } else {
            bin_pal <- colorFactor(viridis(12),polygons_with_csv[[input$soil_data_variable]])
          }
          
          labels = paste0("<strong>",soil_map_choices[soil_map_choices$Variable == input$soil_data_variable,"Label"],"</strong><br/>",
                          polygons_with_csv[[input$soil_data_variable]]) %>% lapply(htmltools::HTML)
          observeEvent(input$soil_data_map, {
            output$map <- renderLeaflet(
              leaflet(
                # options = leafletOptions(minZoom = 15)
              ) %>% 
                # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
                #  setView(lng = -4.2026458, lat = 56.4906712, zoom = 5)
                addTiles() %>%
                addPolygons(data = field_polygons, 
                            color = 'black', stroke = TRUE,  weight = 0.4, fillOpacity = 1,
                            fillColor = ~bin_pal(polygons_with_csv[[input$soil_data_variable]]), 
                            highlightOptions = highlightOptions(weight = 2, color = 'black'),
                            label = labels) %>%
                addLegend(data = field_polygons, pal = bin_pal, title = soil_map_choices[soil_map_choices$Variable == input$soil_data_variable,"Label"],
                          values = ~(polygons_with_csv[[input$soil_data_variable]]), position = 'bottomright')
            ) 
          })
        })
        
        
        show('text_div')
        output$text <- renderText({"Field names successfully submitted, thank you."})
      })
    }  
  })
  
  
  
  
  observeEvent(input$parcelid_already_loaded, {
    if (!is.null(input$file1) & !is.null(input$shp)) {
      hide(id = "mapbutton")
      show(id = "soilmapbutton")
      hide(id = "parcelIDsbutton")
      show('text_div2')
      output$text_already_loaded <- renderText({"Go to Map tab"})
      #updateTabsetPanel(session,"maps",selected = "map_tab")
      
      
      observeEvent(input$soil_data_variable, {
        csv_parcel_ids <- field_data[,c("Field_name",paste(input$soil_data_variable),"Parcel_ID")]
        colnames(csv_parcel_ids)[3] <- "parcel_id"
        csv_parcel_ids$parcel_id <- as.character(csv_parcel_ids$parcel_id)
        # If the same parcel_id has been assigned to more than one field, then there will be duplicate rows with
        # different values of P_index. In order to not just overwrite, bring the duplicates together by calculating the mean
        # (doesn't strictly make sense for P. Index, but could be adapted)
        # csv_parcel_ids <- group_by(csv_parcel_ids, parcel_id) %>% summarise(assign(input$soil_data_variable, mean(get(input$soil_data_variable), na.rm=TRUE)))
        
        polygons_with_csv <- left_join(user_shp, csv_parcel_ids, by = "parcel_id")
        
        field_polygons <- sf::st_transform(polygons_with_csv, '+proj=longlat +datum=WGS84')
        if (input$soil_data_variable %in% c("Total_carbon_yield", "mean_SOC",
                                            "mean_SOM", "P_concentration",
                                            "K_concentration","Mg_concentration")) {
          bin_pal <- colorBin('YlGnBu', polygons_with_csv[[input$soil_data_variable]])
        } else {
          bin_pal <- colorFactor(viridis(12),polygons_with_csv[[input$soil_data_variable]])
        }
        
        labels = paste0("<strong>",soil_map_choices[soil_map_choices$Variable == input$soil_data_variable,"Label"],"</strong><br/>",
                        polygons_with_csv[[input$soil_data_variable]]) %>% lapply(htmltools::HTML)
        observeEvent(input$soil_data_map, {
          output$map <- renderLeaflet(
            leaflet(
              # options = leafletOptions(minZoom = 15)
            ) %>% 
              # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
              #  setView(lng = -4.2026458, lat = 56.4906712, zoom = 5)
              addTiles() %>%
              addPolygons(data = field_polygons, 
                          color = 'black', stroke = TRUE,  weight = 0.4, fillOpacity = 1,
                          fillColor = ~bin_pal(polygons_with_csv[[input$soil_data_variable]]), 
                          highlightOptions = highlightOptions(weight = 2, color = 'black'),
                          label = labels) %>%
              addLegend(data = field_polygons, pal = bin_pal, title = soil_map_choices[soil_map_choices$Variable == input$soil_data_variable,"Label"],
                        values = ~(polygons_with_csv[[input$soil_data_variable]]), position = 'bottomright')
          ) 
        })
      })
      
      
      
      
      
    } else {
      output$field_names <- renderUI({ 
      })
    }
  })
  
  
  
  
  
  
  
  
  
  
  # Read shapefile once user submits files
  observeEvent(input$shp, {
    
    hide(id = "Import_shp")
    show(id = "mapbutton")
    user_shp <<- Read_Shapefile(input$shp)
    
  }) 
  
  
  
  
  
  
  observeEvent(input$field_map, {
    req(input$shp) 
    ## Transform polygons layer
    field_polygons <- sf::st_transform(user_shp, '+proj=longlat +datum=WGS84')
    
    output$map <- renderLeaflet(
      leaflet(
        # options = leafletOptions(minZoom = 15)
      ) %>% 
        # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
        #  setView(lng = -4.2026458, lat = 56.4906712, zoom = 5)
        addTiles() %>%
        addPolygons(data = field_polygons, fillColor = "pink",
                    color = 'black', stroke = TRUE,  weight = 0.4, fillOpacity = 1) #%>%  
      
      # setView(lat = st_coordinates(st_centroid(polygons))[1,2], lng= st_coordinates(st_centroid(polygons))[1,1], zoom = 13)
      # ADD POINT DATA
      # points = sf::st_read('data/collisions.shp')
      
      # ADD HEAT MAP %>% 
      # leaflet.extras::addHeatmap(data = points, blur = 20, max = 0.05, radius = 12)
    )
    
  })  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
