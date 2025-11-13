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
library(stringr)
library(readr)
library(stringi)




# Increase max file upload size 
options(shiny.maxRequestSize = 30*1024^2)





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
        color: #ff0000;
        text-align: left;       
      }"))
  ),
  
  fluidRow(img(src = "FCT Logo (Full Colour).jpg", width="25%",align="left")),  
  
  
  
  # Application title
  titlePanel(title= h1("FCC report"), windowTitle = "FCC report"),
  
  hr(),
  hr(),
  
  uiOutput("main"),
  
)


server <- function(input, output) {
  
  observeEvent(input$start_again,{
    
    refresh()

  })
  
  
  observe({
    output$main <- renderUI({

        
        sidebarLayout(
          sidebarPanel(
            useShinyjs(),
            fileInput(inputId = "jsonreport", label = h5("Upload report .json"),
                      accept = c('.json'))
          ),
          
          
          mainPanel(
            useShinyjs(),
            tabsetPanel( id = "tabs", type = "tabs",
                         tabPanel("Farm Carbon Calculator results",
                                  br(),
                                  div(id = "please_upload_json", fluidRow(h4("Please upload report json to view carbon footprint results and download"))),
                                  
                                  hidden(div(id="json_preview",
                                             div(id="warning",textOutput("inventory_warning")),
                                             div(id="sc_download_button",fluidRow(downloadButton("sc_report", "Download word document of carbon report tables")),
                                                 br(),
                                             tableOutput("jsonfileLoaded"),
                                             br(),
                                             br(),
                                             plotOutput("carbon_balance_barplot"),
                                             br(),
                                             uiOutput("cbaltable"),
                                             uiOutput("seqtable"),
                                             uiOutput("carbon_balance_flextable")))
                         ))))
            
          )
        
      
      
    })

  })
  
  
  
  
  

          # Read json once user submits files
          observeEvent(input$jsonreport, {
            

            json_report <<- jsonlite::fromJSON(input$jsonreport$datapath) 
            
            json_json <<- read_file(input$jsonreport$datapath)
            
            report_details <<- as.data.frame(json_report$emissions)
            
            inventory_indices <- grep("inventory", report_details$section, perl = TRUE)
            
            if (length(inventory_indices) > 0 & anyNA(report_details$calculator_output$positive$value[inventory_indices])) {
              output$inventory_warning <- renderText({"An inventory item's emissions are not appearing, please edit the JSON text"})
              #json_string <- as.character(json_report)
              #materials_bit <- str_extract(json_string, '"materials":[ (.+) ]")
            } else {
              output$inventory_warning <- renderText({""})
            }
            
            
            
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
            
            #report_summary$Inventory_and_materials <- sum(report_summary$Inventory,report_summary$Materials, na.rm=TRUE)
            report_summary <- report_summary[,c("Report_name","Report_period_start", "Report_period_end","Farm_area_ha",
                                                "Total_emissions" ,"Total_offset" , "Carbon_balance", "Scope_1_emissions",
                                                "Scope_2_emissions","Scope_3_emissions","Methane", "Nitrous_oxide" ,
                                                "Fuels", "Livestock", "Land_use" ,"Waste", "Distribution","Processing","Chemicals",
                                                "Crops","Inventory", "Materials")]
            
            
            neg_emissions_by_type <- data.frame(report_details$emissions_category$name,report_details$calculator_output$negative$value) %>% 
              group_by(report_details.emissions_category.name) %>% 
              summarise(total_negative_emissions_by_section = sum(report_details.calculator_output.negative.value, na.rm=TRUE))# %>% 
            
            seq_CS <- neg_emissions_by_type[grepl("countryside stewardship",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Countryside_stewardship <- sum(seq_CS$total_negative_emissions_by_section)
            
            seq_hedgerows <- neg_emissions_by_type[grepl("hedgerow",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_hedgerows <- sum(seq_hedgerows$total_negative_emissions_by_section)
            
            seq_woodland <- neg_emissions_by_type[grepl("woodland",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_woodland <- sum(seq_woodland$total_negative_emissions_by_section)
            
            seq_perm_wetland <- neg_emissions_by_type[grepl("Permanent Wetland",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_perm_wetland <- sum(seq_perm_wetland$total_negative_emissions_by_section)
            
            seq_other <- neg_emissions_by_type[!grepl("countryside stewardship",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE)&
                                                 !grepl("Permanent Wetland",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE)&
                                                 !grepl("hedgerow",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE)&
                                                 !grepl("woodland",neg_emissions_by_type$report_details.emissions_category.name,ignore.case=TRUE),]
            report_summary$Seq_other <- sum(seq_other$total_negative_emissions_by_section)
         
            rs <<- report_summary
            
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
                                                        "Crops","Inventory","Materials", "Total"),
                                     Emissions_value = c(report_summary$Fuels, report_summary$Livestock, report_summary$Land_use ,
                                                         report_summary$Waste, report_summary$Distribution,report_summary$Processing,report_summary$Chemicals,
                                                         report_summary$Crops,report_summary$Inventory,report_summary$Materials,
                                                         sum(report_summary$Fuels, report_summary$Livestock, report_summary$Land_use ,
                                                             report_summary$Waste, report_summary$Distribution,report_summary$Processing,report_summary$Chemicals,
                                                             report_summary$Crops,report_summary$Inventory,report_summary$Materials,na.rm=TRUE)))
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
                  bg( i = c(1:10), bg="#00ACA620", part = "body") %>%
                  bg( i = 11, bg="#BBD1CF30", part = "body") %>%
                  align(align = "center", part="all") %>%
                  autofit(add_w = 0.1,add_h = 0.1,
                          part = c("body", "header"), unit = "in",hspans = "none") %>% 
                  set_table_properties(layout = "autofit") %>%
                  theme_box() %>%
                  align(align = "center", part="header") %>%
                  htmltools_value()
                
              ))
            
            
            seq_table <- data.frame(Seq_Type = c("Hedgerows","Woodland","Permanent wetland", "Countryside Stewardship","Other", "Total"),
                                    Seq_value = c(report_summary$Seq_hedgerows,report_summary$Seq_woodland,report_summary$Seq_perm_wetland,report_summary$Countryside_stewardship,
                                                  report_summary$Seq_other,sum(report_summary$Seq_hedgerows,report_summary$Seq_woodland,report_summary$Seq_perm_wetland,report_summary$Countryside_stewardship,
                                                                               report_summary$Seq_other,na.rm=TRUE)))
            seq_table$percentage <- numeric(nrow(seq_table))
            
            for (i in 1:5) {
              seq_table$percentage[i] <- (seq_table$Seq_value[i]/sum(seq_table$Seq_value[1:5],na.rm=TRUE))*100  
            }
            seq_table$percentage[6] <- 100
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
                  bg( i = c(1:5), bg="#00ACA620", part = "body") %>%
                  bg( i = 6, bg="#BBD1CF30", part = "body") %>%
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
              
              filename = "Carbon_report_tables.doc",
              content = function(file) {
                res <- rmarkdown::render(
                  "FCC_report.Rmd",
                  params = list(
                    report_summary = report_summary,
                    report_details = report_details
                    
                  ))
                file.rename(res, file)
              }) 
            
          hide("please_upload_json")
          show("json_preview")
            
          })
          
          
         
}


# Run the application 
shinyApp(ui = ui, server = server)
