library(tidyverse)
require(gghighlight)

tidy_df <- readRDS(paste0(here::here(),"/tidy_df.Rds"))
uni_df <- readRDS(paste0(here::here(),"/uni_df.Rds"))

#Use to help pulling the information from the df while using clickable marker
info_puller <-function(df, clicker, info_title){
  input_lat = clicker$lat %>% as.numeric()
  input_lng = clicker$lng %>% as.numeric()
  
  return(df %>% filter(lat == input_lat, lng == input_lng) %>% 
         select(all_of(info_title)) %>% pull())
}


category_list<- c("Not Specify","Magnet","Boarding","Catholic",
              "All-Girls","Episcopal","Quaker", 
              "All-Boys","Charter","Jewish",
              "Montessori","Methodist",
              "Christian (General)","Alternative",
              "Other","Muslim","Lutheran","Presbyterian")

state_maping <- tibble(abbv = state.abb, full = state.name) %>% 
  filter(!(abbv %in% c("AK", "WV", "SD", "ND", "MT"))) %>% 
  add_row(abbv = "DC", full = "Washington, D.C.")

state_abb <- state_maping %>% select(abbv) %>% pull()
state_name <- state_maping %>% select(full) %>% pull()

#Identify School category (Filtering from nested list)
find_category <- function(input_df, elements_to_find){
  
  if(elements_to_find == "Not Specify"){
    return(input_df)
  }else{
  return(input_df %>% 
           mutate(Flag = (map_lgl(category, {function (x, target) target %in% x}, elements_to_find))) %>% 
    filter(Flag) %>% 
    select(-Flag))}
}
#Identify Program Offered (Filtering from nested list)
find_program <- function(input_df, elements_to_find){
  
  if(elements_to_find == "No Preference"){
    return(input_df)
  }else{
    return(input_df %>% 
             mutate(Flag = (map_lgl(programs, {function (x, target) target %in% x}, elements_to_find))) %>% 
             filter(Flag) %>% 
             select(-Flag))}
}
#Make a callable html expression for the phone number
make_callable <- function(df, clicker){

  phone_number = info_puller(df ,clicker, info_title = "telephone")
  phone_digits = info_puller(df ,clicker, info_title = "tel_num")
  
  callable = paste0("<a href='tel:", phone_digits,"'>Call The School: ",phone_number,"</a>")
  return(callable)
}

category_maker <- function(df, clicker){
  category <- info_puller(df, clicker, "category")  %>% unlist() %>% str_c(collapse = " ")
  type <- info_puller(df, clicker, "pubPri") %>% unlist()
  programs_offered <- info_puller(df, clicker, "programs") %>% unlist()%>% str_c(collapse = " ")
  
  if(programs_offered %>% is.na()){
    programs_offered<-""
  }
  
  result <- paste(type, category, programs_offered, collapse = "/")
  
  #return(paste(type, " ", category," ",programs_offered))
  return(result)
}

academic_maker_1 <- function(df, clicker){
  tsr <- info_puller(df, clicker, "teacher_student_ratio") 
  grad_rate <- info_puller(df, clicker, "grad_rate")
  
  if(is.na(tsr)){
    tsr <- paste0("Teacher Student Ratio:"," No Data")
  }else{
    tsr <- tsr %>% as.character %>% paste0("Teacher Student Ratio: 1 : ",.)
  }
  
  if(is.na(grad_rate)){
    grad_rate <- paste0("Graduation Rate: ","No Data")
  }else{
    grad_rate <- grad_rate %>% magrittr::multiply_by(100) %>% as.character() %>% paste0("Graduation Rate: ",.,"%")
  }
  
  
  return(paste(tsr," / ", grad_rate))
}

academic_maker_2 <- function(df, clicker){
  sat_score <- info_puller(df, clicker, "SAT") 
  act_score <- info_puller(df, clicker, "ACT") 
  
  if(is.na(sat_score)){
    sat_score <- paste("Average SAT: ","No Data")
  }else{
    sat_score <- sat_score %>% as.character %>% paste("Average SAT: ",.)
  }
  
  if(is.na(act_score)){
    act_score <- paste("Average ACT: ","No Data")
  }else{
    act_score <- act_score %>% as.character() %>% paste("Average ACT: ",.)
  }
  
  return(paste(sat_score,"/ ", act_score))
}

rate_score_maker <- function(df, clicker){
  num_people <- info_puller(df, clicker, "peopleRated") 
  mean_score <- info_puller(df, clicker, "meanRating") 
  
  result <- str_c("Review Score: ",mean_score,"/5",", Based on ",num_people," students' review")
  return(result)
}

diversity_base <- tidy_df %>% filter(!is.na(Diversity)) %>% ggplot(aes(x = Diversity))+
  geom_bar() +labs(x = "Diversity", y = "")+
  theme_minimal()+
  theme(axis.text.y = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

club_base <- 
  tidy_df %>% filter(!is.na(ClubsActivities)) %>% ggplot(aes(x = ClubsActivities))+
  geom_bar() + 
  theme_minimal()+
  theme(axis.text.y = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x = "Club Activities", y = "")

nation_tuition_base <- tidy_df %>% filter(pubPri == "Private") %>% 
  ggplot(aes(x = tuition))+
  geom_density(na.rm = TRUE)+theme_minimal()+
  theme(axis.text.y = element_blank(),panel.grid.minor = element_blank())+
  labs(y = "")

nation_test_base <- tidy_df  %>%
  gather("test", "score", c(ACT, SAT))%>% 
  ggplot(aes(x = score))+
  geom_density(na.rm = TRUE)+
  facet_wrap(.~test, scales = "free")+
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),  panel.background= element_blank(),
        axis.ticks.y = element_blank())+
  labs(y = "")

nation_poll_base <- tidy_df %>%
  rename("Feel Happy" = happy_poll,"Feel Safe" = safe_poll) %>% 
  gather("poll", "score", c("Feel Happy", "Feel Safe")) %>% 
  ggplot(aes(x = score))+
  geom_density(na.rm = TRUE)+
  facet_wrap(.~poll, scales = "free")+
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),  panel.background= element_blank(),
        axis.ticks.y = element_blank())+
  labs(y = "")

make_state_base <- function(state_input, type){
  result <- if(type == "tuition"){
    tidy_df %>%filter(state == state_input ) %>% 
      filter(pubPri == "Private") %>% 
      ggplot(aes(x = tuition))+
      geom_density(na.rm = TRUE)+theme_minimal()+
      theme(axis.text.y = element_blank(),panel.grid.minor = element_blank())+
      labs(y = "")
  } else if(type == "test"){
    tidy_df %>% filter(state == state_input) %>%  
      gather("test", "score", c(ACT, SAT))%>% 
      ggplot(aes(x = score))+
      geom_density(na.rm = TRUE)+
      facet_wrap(.~test, scales = "free")+
      theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),  panel.background= element_blank(),
            axis.ticks.y = element_blank())+
      labs(y = "")
  }else{
    tidy_df %>% filter(state == state_input) %>% 
      rename("Feel Happy" = happy_poll,"Feel Safe" = safe_poll) %>% 
      gather("poll", "score", c("Feel Happy", "Feel Safe")) %>% 
      ggplot(aes(x = score))+
      geom_density(na.rm = TRUE)+
      facet_wrap(.~poll, scales = "free")+
      theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),  panel.background= element_blank(),
            axis.ticks.y = element_blank())+
      labs(y = "")
  }
  return(result)
}