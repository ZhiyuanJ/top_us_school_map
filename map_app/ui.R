library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(patchwork)


source(paste0(here::here(), "/helper.R"))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  #CSS style for the us
  tags$style("#controler {opacity:0.85;
                          margin: auto;
                          padding:0 20px 20px 20px;}"),
  
  navbarPage(
    #Title
    "Top US High School", id = "main",
    
    #Map Tab
    tabPanel("Map", icon = icon(name = "map", class = "fa-solid", lib = "font-awesome"),
             leafletOutput("mainMap", height=1000),
             absolutePanel(id = "controler", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 150, left = 40 , right = "auto", bottom = "auto",
                           width = 350, height = "auto",
                           h2("School Filter", align = "center"), 
                           selectInput("mapState", "States", 
                                       choices = c(structure(state_abb, names =  state_name)), 
                                       multiple = TRUE), 
                           selectInput("mapTown", "Town/City (Select State First)",
                                       choices = "", selected = "", multiple = TRUE),
                           
                           fluidRow(
                             column(6, checkboxGroupInput("pubPri_input", "Public/Private:",
                                                          choiceNames =
                                                            list("Public", "Private"),
                                                          choiceValues =
                                                            list("Public", "Private"),
                                                          selected = list("Public", "Private"))),
                             #column(6, selectizeInput("category_input", "School Category", choices = category_list, multiple = TRUE,
                             #                          options = list(maxItems = 3, placeholder = 'Max 3')))
                             column(6, selectizeInput("category_input", "School Category", choices = category_list, multiple = FALSE))
                             
                             
                           ),
                           selectInput("program_input", "Program Offer", choices = c(structure( c("No Preference","AP Offered", "IB Offered", "Gifted Prog. Offered"),
                                                                                                names = c("No Preference","AP", "IB", "Gifted"))))
             )
    ),
    
    
    #Data Table Tab
    tabPanel("Data Table", icon = icon(name = "table", class = "fa-solid", lib = "font-awesome"),
             h1("Data Tables"),
             p("Here are the Schools You filtered from the Map"),
             DT::dataTableOutput("mainTable")
             
    ),
    
    #School-Level Analysis
    tabPanel("School Analysis", icon = icon(name = "magnifying-glass-chart", class = "fa-solid", lib = "font-awesome"),
             h1(textOutput("school_test")),
             radioButtons("nation_state", label = h3("National/State"),
                          choices = list("National" = 1, "State" = 2), 
                          selected = 1, inline = TRUE),
             actionButton("school_output","Show Plot"),
             fluidPage(
               column(4, plotOutput("tuition_school_dist")),
               column(4, plotOutput("test_school_dist")),
               column(4, plotOutput("poll_school_dist"))
             )
    ),
    
    
    # State-level Tab
    tabPanel("State Analysis", icon = icon(name = "chess-rook", class = "fa-solid", lib = "font-awesome"),
             sidebarLayout(
               sidebarPanel(
                 h3("State Standing"),
                 selectizeInput("statState", "States:", 
                                choices = c(structure(state_abb, names =  state_name)), 
                                options = list(
                                  placeholder = 'All',
                                  onInitialize = I('function() { this.setValue(""); }')
                                ),
                                multiple = FALSE),
                 actionButton("update", "Show Plot")
                 
               ),
               
               mainPanel(
                 plotOutput("statePlot"),
                 em("The graphs above present the perceived happiness and safeness information voted by students. The dashed line represents the mean from all states, while the dashed line (if applicable) represents the mean from selected state."),
                 div(),
                 em("The graphs below present the teacher-student ratio and graduation rate information, separated by public or private school as the difference is significiant."),
                 br(), br(),
                 em("Remark: When some information in a state is limited in a way that generating graph(s) is not feasible, only the overall information would be presented in the corresponding graph(s).")
               )
             ),
    ),
    
    
    #Introduction Tab
    navbarMenu("About",icon = icon(name = "question", class = "fa-solid", lib = "font-awesome"),
               tabPanel("Introduction", includeMarkdown("Introduction.md")),
               tabPanel("About the Creator", includeMarkdown("Authors.md")) 
    )
  )
))
