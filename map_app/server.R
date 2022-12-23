library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(patchwork)

source(paste0(here::here(), "/helper.R"))


tidy_df <- readRDS(paste0(here::here(),"/tidy_df.Rds"))
uni_df <- readRDS(paste0(here::here(),"/uni_df.Rds"))


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  observe({
    updateSelectInput(session, "mapTown",
                      choice = tidy_df %>% filter(state %in% input$mapState) %>% select(town) %>% pull())
  })
  
  observeEvent(input$to_school,{
    updateTabsetPanel(session = session, inputId = "main", selected = "School Analysis")
    removeModal()
  })
  
  global_values<-reactiveValues()
  
  #Filtering For final data set
  map_df<- reactive({
    if(length(input$mapState)==0){
      tidy_df %>%
        filter(pubPri %in% input$pubPri_input) %>%
        find_category(., input$category_input) %>%
        find_program(., input$program_input)
      
    }else if(length(input$mapTown)>0){
      tidy_df %>%
        filter(state%in% input$mapState) %>%
        filter(pubPri %in% input$pubPri_input) %>%
        filter(town %in% input$mapTown)%>%
        find_category(., input$category_input) %>%
        find_program(.,input$program_input)
      
    }else{
      tidy_df %>%
        filter(state %in% input$mapState) %>%
        filter(pubPri %in% input$pubPri_input) %>%
        find_category(., input$category_input) %>%
        find_program(., input$program_input)
    }
    
  })
  
  #Map Tab
  observeEvent(input$mainMap_marker_click, {
    req(input$mainMap_marker_click)
    
    global_values$school_name<- info_puller(df = map_df(),input$mainMap_marker_click, info_title = "name")
    global_values$state <- info_puller(df = map_df(), input$mainMap_marker_click, info_title = "state")
    
    showModal(modalDialog(size = "l",
                          
                          h1(info_puller(df = map_df(),input$mainMap_marker_click, info_title = "name")),
                          p(if_else(info_puller(df = map_df(),input$mainMap_marker_click, info_title = "introduction") %>% is.na(), "Please Visit the Website for more information",
                                    info_puller(df = map_df(),input$mainMap_marker_click, info_title = "introduction"))),
                          
                          tags$br(),
                          HTML(make_callable(df = map_df(), input$mainMap_marker_click)),
                          
                          
                          tags$address(paste0("Address: ", info_puller(df = map_df(),input$mainMap_marker_click, info_title = "address") )),
                          #tags$br(),
                          fluidRow(
                            
                            #Display the Popular University:
                            column(6, h3("Where the Alumni Go:"),
                                   tags$em("Historical result, base on Alumni report"),
                                   if(uni_df %>% 
                                      filter(name ==info_puller(df = map_df(),input$mainMap_marker_click, info_title = "name")) %>% 
                                      is.na() %>% sum()>0){
                                     p("Seems the data are missing, sorry about that.")
                                   }else{
                                     renderTable( uni_df %>% 
                                                    filter(name ==info_puller(df = map_df(),input$mainMap_marker_click, info_title = "name") ) %>%
                                                    select(-name,-rank) )}),
                            #Display other info
                            column(6,
                                   
                                   h3("School Tages"),
                                   tags$code(category_maker(df = map_df(), clicker = input$mainMap_marker_click )),
                                   h3("Academic Performance"),
                                   tags$code(academic_maker_1(df = map_df(), clicker = input$mainMap_marker_click )),
                                   tags$code(academic_maker_2(df = map_df(), clicker = input$mainMap_marker_click )),
                                   h5(rate_score_maker(df = map_df(), clicker = input$mainMap_marker_click )),
                                   
                                   h3("Other Performance"),
                                   fluidRow(column(6, if(info_puller(df = map_df(),input$mainMap_marker_click, info_title = "Diversity") %>% is.na()){
                                     h3("The Diversity Data For this School is Not Avaliable.")
                                   }else{
                                     renderPlot(diversity_base + gghighlight(Diversity ==info_puller(df = map_df(),input$mainMap_marker_click, info_title = "Diversity"),
                                                                             use_group_by = FALSE, label_key = Diversity),
                                                width = "auto", height = 200)
                                   }),
                                   
                                   column(6, if(info_puller(df = map_df(),input$mainMap_marker_click, info_title = "ClubsActivities") %>% is.na()){
                                     h3("The Club Data For this School is Not Avaliable.")
                                   }else{
                                     renderPlot(club_base + gghighlight(ClubsActivities ==info_puller(df = map_df(),input$mainMap_marker_click, info_title = "ClubsActivities"),
                                                                        use_group_by = FALSE,label_key = ClubsActivities),
                                                width = "auto", height = 200)
                                   })
                                   
                                   )
                                   
                                   
                                   
                                   
                            )),
                          
                          
                          #Make the Website and quit button
                          footer = tagList(
                            actionButton("to_school", "To School Analysis"),
                            actionButton("school_url", "Website",
                                         onclick =
                                           paste0("window.open('https://",info_puller(df = map_df(),input$mainMap_marker_click, info_title = "website") , "', '_blank')"),
                                         icon = icon("globe", class = "fa-shape fa-solid")), #A url Button
                            
                            modalButton("Dismiss", icon = icon("circle-xmark", class = "fa-shape fa-solid"))),
                          easyClose = TRUE
    ))
    
  })
  
  
  
  output$mainMap <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(lng = map_df()$lng, lat = map_df()$lat, popup = map_df()$name,
                        icon = awesomeIcons(
                          icon = "education",
                          iconColor = "white",
                          library = "glyphicon",
                          markerColor = if_else(map_df()$name == "Duke University", "darkblue", if_else(map_df()$pubPri == "Private", "red", "blue"))
                        )) %>%
      addLegend(colors = c("red", "blue"), labels = c("Private", "Public"), position = "topright")
    
  )
  
  #Data Table Tab
  output$mainTable<- DT::renderDataTable(map_df() %>% select(-introduction,-tel_num,-lat,-lng) %>%
                                           rename("private_or_public" = pubPri, 
                                                  "graduation_rate" = grad_rate, 
                                                  "College Prep" = CollegePrep, 
                                                  "Club & Activities" = ClubsActivities) %>% 
                                           datatable(escape = FALSE, selection = "none") )
  
  #School Analysis Tab
  output$school_test <- renderText(if(length(global_values$school_name)== 0 ){
    "Please Select A School From the Map Tab"
  }else{
    global_values$school_name
  })
  
  tuition_base <- eventReactive(input$school_output, {
    
    if(length(global_values$school_name) == 0){
      stop(safeError("Please Select a School from the Map Tab First."))
    }
    
    if(input$nation_state == 1 && (map_df() %>% filter(name == global_values$school_name) %>% select(pubPri) %>% pull()) =="Private" ){
      nation_tuition_base +labs(title = "National School Tuition Distribution ")+
        geom_vline(aes(xintercept = (map_df() %>% filter(name == global_values$school_name) %>% select(tuition) %>% pull())), color = "red")
    }else if(input$nation_state == 1 && (map_df() %>% filter(name == global_values$school_name) %>% select(pubPri) %>% pull()) =="Public" ){
      nation_tuition_base +labs(title = "National School Tuition Distribution ",
                                caption = "National Data Displayed Due to Public School",)
    }else if(input$nation_state == 2 & global_values$state %in% c("ID", "WY", "NE", "VT", "MS")  ){
      nation_tuition_base +labs(caption = "National Data Displayed Due to insufficient Data",
                                title = "National School Tuition Distribution ")
    }else if(input$nation_state == 2 & (map_df() %>% filter(name == global_values$school_name) %>% select(pubPri) %>% pull()) =="Public" ){
      nation_tuition_base +labs(caption = "National Data Displayed Due to Public School",
                                title = "National School Tuition Distribution ")
    }else if(input$nation_state == 2 & global_values$school_name %in% c("Midland School","Primoris Academy",
                                                                        "Score At The Top Learning Center & School",
                                                                        "Lancaster Country Day School","School of the Woods")){
      nation_tuition_base +labs(caption = "National Data Displayed Due to insufficient Data",
                                title = "National School Tuition Distribution ")
    }else{
      make_state_base(global_values$state, "tuition")+
        geom_vline(aes(xintercept = (map_df() %>% filter(name == global_values$school_name) %>% select(tuition) %>% pull())), color = "red")+
        labs(title = "State Level School Tuition Distribution", caption = "Red Line Represent the Selected School")
    }
  })
  
  test_base <- eventReactive(input$school_output, {
    
    if(length(global_values$school_name) == 0){
      stop(safeError("Please Select a School from the Map Tab First."))
    }
    
    if(input$nation_state == 1){
      
      nation_test_base +labs(title = "National School Test Score Distribution ")+
        geom_vline(data = subset(map_df() %>% 
                                   gather("test", "score", c(ACT,SAT)), test == "ACT"), 
                   aes(xintercept = (map_df() %>% filter(name == global_values$school_name) %>% select(ACT) %>% pull())  ), color = "red")+
        geom_vline(data = subset(map_df() %>% 
                                   gather("test", "score", c(ACT,SAT)), test == "SAT"), 
                   aes(xintercept =  (map_df() %>% filter(name == global_values$school_name) %>% select(SAT) %>% pull()) ), color = "red")
      
    }else if(input$nation_state == 2 & global_values$state %in% c("ID", "WY", "NE", "VT","MS")  ){
      nation_test_base +labs(caption = "National Data Displayed Due to insufficient Data",
                             title = "National School Test Score Distribution ")
    }else if(input$nation_state == 2 & global_values$school_name %in% (map_df() %>% filter(is.na(SAT)| is.na(ACT)) %>%select(name) %>% pull() ) ){
      nation_test_base +labs(caption = "National Data Displayed Due to insufficient Data",
                             title = "National School Test Score Distribution ")
    }else{
      make_state_base(global_values$state, "test")+
        geom_vline(data = subset(map_df() %>% 
                                   gather("test", "score", c(ACT,SAT)), test == "ACT"), 
                   aes(xintercept = (map_df() %>% filter(name == global_values$school_name) %>% select(ACT) %>% pull())  ), color = "red")+
        geom_vline(data = subset(map_df() %>% 
                                   gather("test", "score", c(ACT,SAT)), test == "SAT"), 
                   aes(xintercept =  (map_df() %>% filter(name == global_values$school_name) %>% select(SAT) %>% pull()) ), color = "red")+
        labs(title = "State Level School Test Score Distribution", caption = "Red Line Represent the Selected School")
    }
  })
  
  poll_base <- eventReactive(input$school_output, {
    
    if(length(global_values$school_name) == 0){
      stop(safeError("Please Select a School from the Map Tab First."))
    }
    
    if(input$nation_state == 1){
      nation_poll_base +labs(title = "National School Student Poll about feel Happy and Safe Percentage")+
        geom_vline(data = (tidy_df %>% rename("Feel Happy" = happy_poll,"Feel Safe" = safe_poll) %>% 
                             gather("poll", "score", c("Feel Happy", "Feel Safe")) %>% filter( poll == "Feel Happy")), 
                   aes(xintercept = (map_df() %>% filter(name == global_values$school_name) %>% select(happy_poll) %>% pull())  ), color = "red")+
        geom_vline(data = (tidy_df %>% rename("Feel Happy" = happy_poll,"Feel Safe" = safe_poll) %>% 
                             gather("poll", "score", c("Feel Happy", "Feel Safe")) %>% filter( poll == "Feel Safe")), 
                   aes(xintercept =  (map_df() %>% filter(name == global_values$school_name) %>% select(safe_poll) %>% pull()) ), color = "red")
      
    }else if(input$nation_state == 1 & global_values$school_name %in% (map_df() %>% filter(is.na(happy_poll)| is.na(safe_poll)) %>%select(name) %>% pull() ) ){
      nation_poll_base +labs(caption = "National Data Displayed Due to insufficient Data",
                             title = "National School Student Poll about feel Happy and Safe Percentage")
    }else if(input$nation_state == 2 & global_values$state %in% c("ID", "WY", "NE", "VT","MS")  ){
      nation_poll_base +labs(caption = "National Data Displayed Due to insufficient Data",
                             title = "National School Student Poll about feel Happy and Safe Percentage")
    }else if(input$nation_state == 2 & global_values$school_name %in% (map_df() %>% filter(is.na(happy_poll)| is.na(safe_poll)) %>%select(name) %>% pull() ) ){
      nation_poll_base +labs(caption = "National Data Displayed Due to insufficient Data",
                             title = "National School Student Poll about feel Happy and Safe Percentage")
    }else{
      make_state_base(global_values$state, "poll")+
        geom_vline(data = (tidy_df %>% rename("Feel Happy" = happy_poll,"Feel Safe" = safe_poll) %>% 
                             gather("poll", "score", c("Feel Happy", "Feel Safe")) %>% filter( poll == "Feel Happy")), 
                   aes(xintercept = (map_df() %>% filter(name == global_values$school_name) %>% select(happy_poll) %>% pull())  ), color = "red")+
        geom_vline(data = (tidy_df %>% rename("Feel Happy" = happy_poll,"Feel Safe" = safe_poll) %>% 
                             gather("poll", "score", c("Feel Happy", "Feel Safe")) %>% filter( poll == "Feel Safe")), 
                   aes(xintercept =  (map_df() %>% filter(name == global_values$school_name) %>% select(safe_poll) %>% pull()) ), color = "red")+
        labs(title = "State Level School Student Poll about feel Happy and Safe Percentage", caption = "Red Line Represent the Selected School")
    }
  })
  
  observeEvent(input$school_output,{
    output$tuition_school_dist <- renderPlot(tuition_base())
    output$test_school_dist <- renderPlot(test_base())
    output$poll_school_dist <- renderPlot(poll_base())
  })
  #State Statistics Tab
  
  # default graph
  values = reactiveValues()
  
  # happy
  values$happy = ggplot() +
    geom_density(data = tidy_df %>% select(happy_poll) %>% drop_na(), aes(x = happy_poll, color = "All")) +
    geom_vline(xintercept = mean(tidy_df$happy_poll, na.rm = TRUE), linetype = "dashed") +
    theme_minimal() +
    labs(x = "Happy Poll %")
  
  values$happyOut = ggplot() +
    geom_density(data = tidy_df %>% select(happy_poll) %>% drop_na() %>% mutate(state = "All"), aes(x = happy_poll, color = state)) +
    geom_vline(xintercept = mean(tidy_df$happy_poll, na.rm = TRUE), linetype = "dashed") +
    theme_minimal() +
    labs(x = "Happy Poll %")
  
  # safe
  values$safe = ggplot() +
    geom_density(data = tidy_df %>% select(safe_poll) %>% drop_na(), aes(x = safe_poll, color = "All")) +
    geom_vline(xintercept = mean(tidy_df$safe_poll, na.rm = TRUE), linetype = "dashed") +
    theme_minimal() +
    labs(x = "Safe Poll %")
  
  values$safeOut = ggplot() +
    geom_density(data = tidy_df %>% select(safe_poll) %>% drop_na() %>% mutate(state = "All"), aes(x = safe_poll, color = state)) +
    geom_vline(xintercept = mean(tidy_df$safe_poll, na.rm = TRUE), linetype = "dashed") +
    theme_minimal() +
    labs(x = "Safe Poll %")
  
  # teacher student ratio data frame
  values$ratio = tidy_df %>%
    select(pubPri, state, teacher_student_ratio) %>%
    drop_na() %>%
    group_by(pubPri, state) %>%
    summarise(ratio = mean(teacher_student_ratio), .groups = "drop")
  
  values$gratio = tidy_df %>%
    select(pubPri, teacher_student_ratio) %>%
    drop_na() %>%
    group_by(pubPri) %>%
    summarise(ratio = mean(teacher_student_ratio), .groups = "drop") %>%
    mutate(state = "All") %>%
    ggplot(aes(x = pubPri, y = ratio)) +
    geom_bar(aes(fill = state),position = "dodge", stat = "identity") +
    labs(x = "Public or Private", y = "mean teacher-student ratio")
  
  # average graduation rate
  values$grad = tidy_df %>%
    select(pubPri, state, grad_rate) %>%
    drop_na() %>%
    group_by(pubPri, state) %>%
    summarise(grad_rate = mean(grad_rate), .groups = "drop")
  
  values$ggrad = tidy_df %>%
    select(pubPri, grad_rate) %>%
    drop_na() %>%
    group_by(pubPri) %>%
    summarise(grad_rate = mean(grad_rate), .groups = "drop") %>%
    mutate(state = "All") %>%
    ggplot(aes(x = pubPri, y = grad_rate)) +
    geom_bar(aes(fill = state),position = "dodge", stat = "identity") +
    labs(x = "Public or Private", y = "mean graduation rate")
  
  observeEvent(input$update, {
    state = input$statState
    # happy poll graph
    if(input$statState != ""){
      if(!(input$statState %in% c("ID", "WY", "NE", "VT", "MS"))){
        subset = tidy_df %>%
          filter(state == input$statState)
        hmean = mean(subset$happy_poll, na.rm = TRUE)
        values$happyOut = values$happy +
          geom_density(data = subset, aes(x = happy_poll, color = state)) +
          geom_vline(xintercept = hmean, linetype = "solid")
      }
      else{
        values$happyOut = values$happy + labs(caption = "Not enough schools in this state.")
      }
    }
    
    # safe poll graph
    if(input$statState != ""){
      if(!(input$statState %in% c("ID", "WY", "NE", "VT", "MS"))){
        subset = tidy_df %>%
          filter(state == input$statState)
        smean = mean(subset$safe_poll, na.rm = TRUE)
        values$safeOut = values$safe +
          geom_density(data = subset, aes(x = safe_poll, color = state)) +
          geom_vline(xintercept = smean, linetype = "solid")
      }
      else{
        values$safeOut = values$safe + labs(caption = "Not enough schools in this state.")
      }
    }
    
    # teacher student ratio graph
    if(input$statState != ""){
      if(nrow(values$ratio %>% filter(state == input$statState)) == 2){
        values$gratio = tidy_df %>%
          select(pubPri, teacher_student_ratio) %>%
          drop_na() %>%
          group_by(pubPri) %>%
          summarise(ratio = mean(teacher_student_ratio)) %>%
          mutate(state = "All") %>%
          rbind(values$ratio %>% filter(state == input$statState)) %>%
          ggplot(aes(x = pubPri, y = ratio)) +
          geom_bar(aes(fill = state), position = "dodge", stat = "identity") +
          labs(x = "Public or Private", y = "mean teacher-student ratio")
      }
      else{
        values$gratio = tidy_df %>%
          select(pubPri, teacher_student_ratio) %>%
          drop_na() %>%
          group_by(pubPri) %>%
          summarise(ratio = mean(teacher_student_ratio)) %>%
          mutate(state = "All") %>%
          ggplot(aes(x = pubPri, y = ratio)) +
          geom_bar(aes(fill = state),position = "dodge", stat = "identity") +
          labs(x = "Public or Private", y = "mean teacher-student ratio", caption = "Not enough ratio information in this state.")
      }
    }
    
    # graduation rate
    if(input$statState != ""){
      if(nrow(values$grad %>% filter(state == input$statState)) == 2){
        values$ggrad = tidy_df %>%
          select(pubPri, grad_rate) %>%
          drop_na() %>%
          group_by(pubPri) %>%
          summarise(grad_rate = mean(grad_rate)) %>%
          mutate(state = "All") %>%
          rbind(values$grad %>% filter(state == input$statState)) %>%
          ggplot(aes(x = pubPri, y = grad_rate)) +
          geom_bar(aes(fill = state), position = "dodge", stat = "identity") +
          labs(x = "Public or Private", y = "mean graduation rate")
      }
      else{
        values$ggrad = tidy_df %>%
          select(pubPri, grad_rate) %>%
          drop_na() %>%
          group_by(pubPri) %>%
          summarise(grad_rate = mean(grad_rate)) %>%
          mutate(state = "All") %>%
          ggplot(aes(x = pubPri, y = grad_rate)) +
          geom_bar(aes(fill = state),position = "dodge", stat = "identity") +
          labs(x = "Public or Private", y = "mean graduation rate", caption = "Not enough rate information in this state.")
      }
    }
  })
  
  output$statePlot = renderPlot({
    values$happyOut + values$safeOut + values$gratio + values$ggrad
  })
  
  
})