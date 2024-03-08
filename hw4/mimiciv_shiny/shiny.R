library(ggplot2)
library(shiny)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(forcats)

# load dataset 
mimic_icu_cohort <- readRDS(
  "~/203b-lecture/203b-hw/hw4/mimiciv_shiny/mimic_icu_cohort.rds")

demographvar <- c("race", "insurance", "marital_status", 
                  "gender", "age_at_intime")
labevents <- c("creatinine", "potassium", "sodium", "chloride", 
               "bicarbonate", "hematocrit", "white blood cell count",
               "glucose")
vital <- c('heart rate', 'systolic non-invasive blood pressure', 
           'diastolic non-invasive blood pressure', 
           'body temperature in Fahrenheit', 'respiratory rate')

allvar <- c(demographvar, labevents, vital)

subject_id <- mimic_icu_cohort$subject_id



ui <- fluidPage(
  
  # App title ----
  titlePanel("Exploring the ICU Cohort Data"),
  tabsetPanel(
    tabPanel("Patient characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Variable", "Variable of interest", 
                             choices = allvar)
               ),
               mainPanel(
                 plotOutput("demoboxplot")
               ))),
    tabPanel("Patient's ADT and ICU stay information",
             sidebarLayout(
               sidebarPanel(
                 selectInput("SubjectID", "Variable of interest", 
                             choices = subject_id)
               ),
               mainPanel(
                 plotOutput("ADT")
             )))))

server <- function(input, output, session) {
  
  output$demoboxplot <- renderPlot({
    var <- input$Variable
    
    if (var %in% demographvar) {
      if (is.numeric(mimic_icu_cohort[[var]])) {
        hist(mimic_icu_cohort[[var]], main = var, xlab = var, ylab = "Count")
      } else {
        count <- table(mimic_icu_cohort[[var]])
        barplot(count, main = var, xlab = var, ylab = "Count")
      }
      
    }else if (var %in% c(labevents, vital)) {
      boxplot(mimic_icu_cohort[[var]], main = var, 
              ylab = "Value", horizontal = TRUE)
    }
  })

  
  output$ADT <- renderPlot({
    req(input$SubjectID)  
    sid <- input$SubjectID
    
  selected_data <- mimic_icu_cohort %>% 
      filter(subject_id == sid)   
    
    p <- ggplot(data = selected_data) +
    geom_segment(data = selected_data, aes(x = intime, xend = outtime,
                                          y = "ICU stay", 
                                          yend = "ICU stay",
                                          color = "blue",
                                          linewidth = 3)) +
    geom_segment(data = selected_data, aes(x = admittime, xend = dischtime,
                                           y= "ADT",
                                           yend = "ADT",
                                           color = "red",
                                           linewidth = 3)) +
      labs(
        title = paste("Patient", sid, selected_data$gender, 
                      selected_data$anchor_age, "years old",
                      str_to_lower(selected_data$race[1])),
        x = "Calendar Time",
        y = ""
      ) +
      theme_light() +
      theme(legend.position = "bottom", legend.box = "vertical")
    
    # Print the plot
    print(p)
  })
}
    


shinyApp(ui = ui, server = server)
