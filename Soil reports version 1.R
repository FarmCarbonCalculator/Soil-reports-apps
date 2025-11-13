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

# Increase max file upload size 
options(shiny.maxRequestSize = 30*1024^2)


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
      .popover-title {
        color: #008B8B;
        font-weight: 600;
        font-size: 15px;
                    }
      .popover-content {
        font-size: 15px;
                    }"))
  ),
  
  fluidRow(img(src = "FCT Logo (Full Colour).jpg", width="25%",align="left")),
  
  # Application title
  titlePanel(title= h1("FCT data input"), windowTitle = "FCT data input"),
  
  hr(),
  hr(),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),  
      # Input: Select a file ----
      fileInput("file1", h5("Upload .csv file"),
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput(inputId = "shp", label = h5("Upload shapefile (.shp, .dbf, .sbn, .sbx, .shx, .prj)"), multiple = TRUE, 
                accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj')),
      fileInput(inputId = "jsonreport", label = h5("Upload report .json"), 
                accept = c('.json'))
    ),
    
    
    mainPanel(
      useShinyjs(),  
      tabsetPanel( id = "tabs", type = "tabs",
                   tabPanel("Imported data preview", 
                            br(),
                            div(id = "Import_data", fluidRow(h4("Please upload a csv file")),
                                fluidRow(a(href="csv_template.csv", "Download template csv file for data input", download=NA, target="_blank")),
                                fluidRow(h5("Or access the google sheets version here to",a(href="https://docs.google.com/spreadsheets/d/1OzIGFI_iD1qpuYDsqtDUBAnVxQ563_6LhQtahb-aroY/edit#gid=0", "make a copy", target="_blank")))),
                            
                            hidden(div(id = "add_data_from_firestore",
                                       wellPanel(fileInput(inputId = "soil_obs_data", label = h5("Import soil observation data (.csv)"), multiple = FALSE, 
                                                           accept = '.csv')))), 
                            br(),
                            hidden(div(id = "data_head", fluidRow(h4("First 6 rows of the data you have uploaded:")))),
                            br(),
                            tableOutput("fileLoaded"),
                            br(),
                            hidden(div(id = "crop_type",
                                       fluidRow(actionButton("add_crop_type", "Click to assign crop type to fields")))),
                            hidden(div(id = "crop_type_text",fluidRow(h4("Please assign crop type to fields:")))),
                            uiOutput("Assign_cropping_type"),
                            br(),
                            hidden(div(id='text_div_crops',
                                       fluidRow( textOutput("text_crops")))),
                            uiOutput("submit_crop_types"),
                            br()),
                   tabPanel("Soil report tables",
                            br(),
                            div(id = "please_upload", fluidRow(h4("Please upload csv to view soil tables and download"))),
                            br(),
                            hidden(div(id = "choose_header", 
                                       fluidRow(wellPanel(selectInput("header", label = h4("Please select template"), 
                                                                      choices = list("Tidy data" = 1,
                                                                                     "Duchy" = 2
                                                                                     #"Emma Jan 24" = 3
                                                                      ), selected = 1))))),
                            br(),
                            hidden(div(id = "Preview",
                                       fluidRow(id="download_button",downloadButton("report", "Download word document of soil report tables")),
                                       br(),
                                       fluidRow(h3("Preview of soil report tables")))),
                            br(),
                            fluidRow(uiOutput("flextable_preview")),
                            br()),
                   tabPanel("Farm Carbon Calculator results",
                            br(),
                            div(id = "please_upload_json", fluidRow(h4("Please upload report json to view carbon footprint results and download"))),
                            
                            hidden(div(id="json_preview",
                                       #div(id="sc_download_button",fluidRow(downloadButton("sc_report", "Download word document of soil and carbon report tables"))),
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
    )
  )
)


server <- function(input, output) {
  
  
  
  soil_map_choices <- data.frame(Label = c("Total carbon yield (t/ha)","Mean SOC (t/ha)",  "Mean SOM (%)",
                                           "P concentration (mg/l)","P index","K concentration (mg/l)",
                                           "K index", "Mg concentration (mg/l)" ,"Mg index"),
                                 Variable = c("Total_carbon_yield", "mean_SOC",
                                              "mean_SOM", "P_concentration","P_index",
                                              "K_concentration","K_index","Mg_concentration",
                                              "Mg_index"))
  
  
  
  # Read csv once user submits files
  observeEvent(input$file1, {
    
    hide(id = "Import_data")
    show(id = "add_data_from_firestore")
    
    field_data <- read.csv(input$file1$datapath)
    
    
    field_data <- field_data %>%
      # mutate(mean_SOM = round(rowMeans(select(.,c(SOM_0_10, SOM_10_30, 
      #                                      SOM_30_50)),na.rm = TRUE),2)) %>%
      mutate(VESS_top_av = rowMeans(select(.,c(VESS_top1, VESS_top2, VESS_top3)),na.rm = TRUE)) %>%
      mutate(VESS_bottom_av = rowMeans(select(.,c(VESS_bottom1,VESS_bottom2,VESS_bottom3)),na.rm = TRUE)) %>%
      mutate(Worms_av = rowMeans(select(.,c(Worms1, Worms2 ,Worms3)),na.rm = TRUE)) %>%
      #mutate(Infiltration_av = rowMeans(select(.,c(Infiltration1 , Infiltration2, Infiltration3)))) %>%
      mutate(Ag_stability_5mins_av = rowMeans(select(.,c(Ag_stability_5mins1, Ag_stability_5mins2, Ag_stability_5mins3)),na.rm = TRUE)) %>%
      mutate(Ag_stability_120mins_av = rowMeans(select(.,c(Ag_stability_120mins1, Ag_stability_120mins2, Ag_stability_120mins3)),na.rm = TRUE))
    
    
    
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
    
    
    field_nutrients <<- field_data[,c("Field_name", "P_index", "P_concentration","K_index",
                                      "K_concentration", "Mg_index",
                                      "Mg_concentration", "pH","mean_SOM")]
    
    
    output$fileLoaded <- renderTable({
      
      show(id = "data_head")
      
      head(field_data)
      
    })
    
    show(id = "crop_type")
    
    
    
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
                                  "SOC_30_50")]
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
    table4$Worms_av <- round(table4$Worms_av,0)
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
          set_header_labels("Field_name" = "Field","VESS_top_av" = "VESS top", "VESS_bottom_av" = "VESS bottom", "Worms_av" = "Worms", 
                            "Ag_stability_5mins_av" = "Ag (5mins)", "Ag_stability_120mins_av" = "Ag (120mins)") %>%
          font(fontname = ("Poppins"), part = "all") %>%
          fontsize(size = 9, part = "body") %>%
          fontsize(size = 10, part = "header") %>%
          align(align = "center", part="all") %>%
          autofit(add_w = 0.1,add_h = 0.1,
                  part = c("body", "header"), unit = "in",hspans = "none") %>% 
          set_table_properties(layout = "autofit") %>%
          theme_box() %>%
          align(align = "center", part="header") %>%
          htmltools_value()
        
        
      ))
    
    
    show(id = "parcelIDsbutton")
    hide(id = "please_upload")  
    show(id = "choose_header")
    show(id = "Preview")
    
    
    
    
    
    
    observeEvent(input$add_crop_type, {
      hide("crop_type")
      show("crop_type_text")
      output$Assign_cropping_type <- renderUI({
        tagList(
          lapply(1:length(field_data$Field_name),function(i){
            fluidRow(column(offset=3, width=6, div(
              wellPanel(selectInput(paste0("crop_",field_data$Field_name[i]), label = paste(field_data$Field_name[i]), choices = list("Continuous arable cropping",
                                                                                                                                      "Grass with an occasional barley crop",
                                                                                                                                      "Grass with an occasional wheat or oat crop",
                                                                                                                                      "Continuous grass or grass-clover swards"), selected = "Continuous grass or grass-clover swards")))))
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
      
      
      field_nutrients$cropping_type <- crop_types_submitted
      field_nutrients <- field_nutrients[,c("Field_name","cropping_type", "P_index", "P_concentration","K_index",
                                            "K_concentration", "Mg_index",
                                            "Mg_concentration", "pH","mean_SOM")]
      field_nutrients$optimum_pH <- character(nrow(field_nutrients))
      for (i in 1:nrow(field_nutrients)) { 
        field_nutrients$optimum_pH[i] <- ifelse(field_nutrients$mean_SOM[i] < 10 
                                                & field_nutrients$cropping_type[i] == "Continuous arable cropping",6.5,
                                                ifelse(field_nutrients$mean_SOM[i] < 10 
                                                       & field_nutrients$cropping_type[i] == "Grass with an occasional barley crop",6.2,
                                                       ifelse(field_nutrients$mean_SOM[i] < 10 
                                                              & (field_nutrients$cropping_type[i] == "Grass with an occasional wheat or oat crop" |
                                                                   field_nutrients$cropping_type[i] == "Continuous grass or grass-clover swards") ,6,
                                                              ifelse(field_nutrients$mean_SOM[i] > 20 
                                                                     & field_nutrients$cropping_type[i] == "Continuous arable cropping",5.8,
                                                                     ifelse(field_nutrients$mean_SOM[i] > 20 
                                                                            & field_nutrients$cropping_type[i] == "Grass with an occasional barley crop",5.5,
                                                                            ifelse(field_nutrients$mean_SOM[i] > 20 
                                                                                   & (field_nutrients$cropping_type[i] == "Grass with an occasional wheat or oat crop" |
                                                                                        field_nutrients$cropping_type[i] == "Continuous grass or grass-clover swards") ,5.3,
                                                                                   ifelse((field_nutrients$mean_SOM[i] <= 20 & field_nutrients$mean_SOM[i] >= 10)
                                                                                          & field_nutrients$cropping_type[i] == "Continuous arable cropping",6,
                                                                                          ifelse((field_nutrients$mean_SOM[i] <= 20 & field_nutrients$mean_SOM[i] >= 10)
                                                                                                 & field_nutrients$cropping_type[i] == "Grass with an occasional barley crop",5.7,
                                                                                                 ifelse((field_nutrients$mean_SOM[i] <= 20 & field_nutrients$mean_SOM[i] >= 10)
                                                                                                        & (field_nutrients$cropping_type[i] == "Grass with an occasional wheat or oat crop" |
                                                                                                             field_nutrients$cropping_type[i] == "Continuous grass or grass-clover swards") ,5.5, NA)))))))))
      }
      
      
      
      
      
      
      
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
                                    "SOC_30_50")]
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
      table4$Worms_av <- round(table4$Worms_av,0)
      
      
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
    
    
    
    
    
    
    
    
    
    
  }) 
  
  
  
  
  # Read shapefile once user submits files
  observeEvent(input$shp, {
    
    hide(id = "Import_shp")
    show(id = "mapbutton")
    user_shp <<- Read_Shapefile(input$shp)
    
  }) 
  
  
  # Read json once user submits files
  observeEvent(input$jsonreport, {
    
    hide(id = "please_upload_json")
    show(id = "json_preview")
    json_report <- jsonlite::fromJSON(input$jsonreport$datapath) 
    
    report_details <- as.data.frame(json_report$emissions)
    
    report_summary <- data.frame(Report_name = json_report[,1],
                                 Report_period_start = as.character(json_report[,9][1]),
                                 Report_period_end = as.character(json_report[,9][2]),
                                 Farm_area_ha = sum(as.numeric(json_report$farm_areas$areas[1]),as.numeric(json_report$farm_areas$areas[2]),as.numeric(json_report$farm_areas$areas[3]),na.rm=TRUE),
                                 Total_emissions = sum(as.numeric(report_details$calculator_output$emissions$value),na.rm=TRUE),
                                 Total_offset = sum(as.numeric(report_details$calculator_output$negative_emissions$value),na.rm=TRUE),
                                 Carbon_balance = sum(sum(as.numeric(report_details$calculator_output$emissions$value),na.rm=TRUE), 
                                                      sum(as.numeric(report_details$calculator_output$negative_emissions$value),na.rm=TRUE),na.rm=TRUE),
                                 Scope_1_emissions = sum(as.numeric(report_details$calculator_output$scope_1_emissions$value),na.rm=TRUE),
                                 Scope_2_emissions = sum(as.numeric(report_details$calculator_output$scope_2_emissions$value),na.rm=TRUE),
                                 Scope_3_emissions = sum(as.numeric(report_details$calculator_output$scope_3_emissions$value),na.rm=TRUE),
                                 Methane = sum(as.numeric(report_details$calculator_output$methane_emissions$value),na.rm=TRUE),
                                 Nitrous_oxide  = sum(as.numeric(report_details$calculator_output$nitrogen_out_n2o$value),na.rm=TRUE))
    
    emissions_by_type <- data.frame(report_details$section,report_details$calculator_output$emissions$value) %>% 
      group_by(report_details.section) %>% 
      summarise(total_emissions_by_section = sum(report_details.calculator_output.emissions.value, na.rm=TRUE)) %>% 
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
    
    
    neg_emissions_by_type <- data.frame(report_details$emissions_category$name,report_details$calculator_output$negative_emissions$value) %>% 
      group_by(report_details.emissions_category.name) %>% 
      summarise(total_negative_emissions_by_section = sum(report_details.calculator_output.negative_emissions.value, na.rm=TRUE))# %>% 
    
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
    
    
    carbon_balance_table <- data.frame("Metric" = c("Carbon Balance",
                                                    "Carbon Balance per hectare",
                                                    "Carbon balance per tonne"),
                                       "Value" = c( paste(round(report_summary$Carbon_balance,2),"tonnes of CO2e"),
                                                    paste(round((report_summary$Carbon_balance/report_summary$Farm_area_ha),2),"tonnes CO2e per hectare"),
                                                    paste("tonnes of CO2e per tonne of product"))
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
    
    for (i in 1:4) {
      cbal_table$percentage[i] <- (cbal_table$Emissions_value[i]/sum(cbal_table$Emissions_value[1:9],na.rm=TRUE))*100  
    }
    cbal_table$percentage[10] <- 100
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
    
    
    
    
    #    output$sc_report <- downloadHandler(
    #       
    #       filename = "Soil_and_carbon_report_tables.doc",
    #       content = function(file) {
    #         res <- rmarkdown::render(
    #           "Soil and carbon.Rmd",
    #           params = list(
    #             soil_data = read.csv(input$file1$datapath),
    #             report_summary = report_summary,
    #             crop_types = field_nutrients_table
    #             
    #           ))
    #         file.rename(res, file)
    #       })
    
    
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
      
    } else {
      output$field_names <- renderUI({ 
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
  
  
  
  
  
  
  
  
  #    output$map_download <- downloadHandler(
  #      
  #      filename = "Soil_report_map.doc",
  #      content = function(file) {
  #        res <- rmarkdown::render(
  #          "Map_report.Rmd",
  #          params = list(
  #            polygons_with_csv = polygons_with_csv,
  #            variable = input$soil_data_variable,
  #            label =  soil_map_choices[soil_map_choices$Variable == input$soil_data_variable,"Label"],
  #            map_lat = input$map_center$lat,
  #            map_lon = input$map_center$lng,
  #            zoom = input$map_zoom
  #          ))
  #        file.rename(res, file)
  #      }) 
  
  
  observeEvent(input$header, { 
    
    
    if (input$header == 1) {
      
      output$report <- downloadHandler(
        
        filename = "Soil_report_tables.doc",
        content = function(file) {
          res <- rmarkdown::render(
            "Ideal_table.Rmd",
            params = list(
              soil_data = read.csv(input$file1$datapath),
              crop_types = field_nutrients_table,
              vess_crop_types = vess_table
            ))
          file.rename(res, file)
        })  
    } else if (input$header == 2 & nrow(field_data) <=3) {
      
      output$report <- downloadHandler(
        
        filename = "Soil_report_tables.doc",
        content = function(file) {
          res <- rmarkdown::render(
            "Duchy_report_1_3_fields.Rmd",
            params = list(
              soil_data = read.csv(input$file1$datapath),
              crop_types = field_nutrients_table
            ))
          file.rename(res, file)
        })  
    }  else if (input$header == 2 & nrow(field_data) >3 & nrow(field_data) <=6) {
      
      output$report <- downloadHandler(
        
        filename = "Soil_report_tables.doc",
        content = function(file) {
          res <- rmarkdown::render(
            "Duchy_report_3_6_fields.Rmd",
            params = list(
              soil_data = read.csv(input$file1$datapath),
              crop_types = field_nutrients_table
            ))
          file.rename(res, file)
        })  
    }  else if (input$header == 2 & nrow(field_data) > 6 & nrow(field_data) <= 9) {
      
      output$report <- downloadHandler(
        
        filename = "Soil_report_tables.doc",
        content = function(file) {
          res <- rmarkdown::render(
            "Duchy_report_6_9_fields.Rmd",
            params = list(
              soil_data = read.csv(input$file1$datapath),
              crop_types = field_nutrients_table
            ))
          file.rename(res, file)
        })  
    } else if (input$header == 2 & nrow(field_data) > 9 & nrow(field_data) <= 12) {
      
      output$report <- downloadHandler(
        
        filename = "Soil_report_tables.doc",
        content = function(file) {
          res <- rmarkdown::render(
            "Duchy_report_9_12_fields.Rmd",
            params = list(
              soil_data = read.csv(input$file1$datapath),
              crop_types = field_nutrients_table
            ))
          file.rename(res, file)
        })  
    }
    #  else if (input$header == 3) {
    #    
    #    output$report <- downloadHandler(
    #      
    #      filename = "Soil_report_tables.doc",
    #      content = function(file) {
    #        res <- rmarkdown::render(
    #          "Ideal_table_new_Jan24.Rmd",
    #          params = list(
    #            soil_data = read.csv(input$file1$datapath),
    #            crop_types = field_nutrients_table
    #          ))
    #        file.rename(res, file)
    #      })  
    #  }
    
    
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
