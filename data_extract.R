library(tidyverse)
library(rvest)

#List of all Niche Pages
niche_files<- list.files("niche") %>% 
  paste0("niche/",.)

#Helper Function
simple_return <- function(cache, tager){
  result <- cache %>% html_element(tager) %>% 
    html_text()
  return(result)
}

#Input an url, output information in form of tibble
extra_info <- function(url){
  cache <- read_html(url)
  
  #School Name
  name <-  cache %>% simple_return(".postcard__title")
  
  #School type (Public Prive)
  #type <- cache %>% simple_return(., ".postcard__genus--current")
  type_list <- cache %>% simple_return(., ".postcard-fact:nth-child(2)") %>% str_split(., ", ")
  pubPri <- type_list[[1]][1]
  other_type <- type_list[[1]][-1]
  
  #School grades (school category, k12, 9-12 6-12 etc).
  #grade <- cache %>% simple_return(., ".postcard-fact:nth-child(3)")
  
  #Simple Intro
  intro <-cache %>% simple_return(., ".bare-value")
  if(is.na(intro)){
    intro <- cache %>% simple_return(".premium-paragraph__text") 
  }
  
  #prgram offered Only want to keep three tags: AP IB Gited Program
  tag_list <- cache %>% html_elements(".search-tags__wrap__list") %>% 
    html_text2() %>% 
    str_split("\n") %>% .[[1]]
  
  #Address
  
  #Address has weird behavior 
  addr <- cache %>% html_elements(".profile__address--compact") %>% 
    html_text()
  
  #  Township, State
  town <-   cache %>% simple_return(".postcard-fact:nth-child(4)") %>% 
    str_extract(".+(?=,)")
  state <- cache %>% simple_return(".postcard-fact:nth-child(4)") %>% 
    str_extract("(?<=,\\s)\\w+")
  
  
  #website 
  website <- cache %>% simple_return(.,".fact-with-icon--website .fact-with-icon__content")
  
  #tel
  tel <- cache %>% simple_return(.,".fact-with-icon--telephone .fact-with-icon__content")
  
  #Nich Rating
  rate_list <- cache %>% html_elements(".ordered__list__bucket__item") %>% 
    html_text()
  #Tuition
  tuition <- cache %>% simple_return(.,".scalar--one .scalar__value")
  
  #Financial Aid (dirty)
  # aid_list <- cache %>% html_element("#tuition .profile__bucket--2") %>% 
  #   html_text2() %>% 
  #   str_split("\n") %>% .[[1]]
  
  #graduate Rate
  grade_rate <- cache %>% simple_return("#academics .scalar .scalar__value")
  #SAT ACT dirty 
  acad_list <- cache %>% html_elements(.,"#academics .scalar--three .scalar__value") %>% 
    html_text2() %>% 
    str_split("\n")
  
  #Popular uni and counts go to 
  pop_uni_list <- cache %>% html_elements(".popular-entity__name") %>% 
    html_text2()
  uni_attend_list <- cache %>% html_elements(".popular-entity-descriptor") %>% 
    html_text2()
  
  #Safe and happy poll
  safe_happy_list <- cache %>% html_elements("#culture-safety .poll__single__percent__label") %>% 
    html_text2()
  
  #Student num
  student_num <- cache %>% simple_return(.,"#students .scalar__value")
  
  #Teacher:Student
  ts_ratio <- cache %>% simple_return(.,"#teachers .scalar__value span")
  
  #Geo location Three Online Schools has no geo location
  geo_loc <- cache%>% 
    html_elements("head") %>% 
    html_elements("meta") %>% 
    html_attr("content") %>% .[c(21, 22)]
  
  #Niche comment distribution (Five to one)
  comm_dist <- cache %>% html_elements(".review__chart__item__total") %>% 
    html_text2()
  
  #Apply Deadline
  # ddl <- cache %>% html_element("#applying .scalar--three:nth-child(1) .scalar__value span") %>% 
    # html_text2()
  #Application fee
  # app_fee <- cache %>% simple_return("#applying .scalar--three:nth-child(2) .scalar__value span")
  #Interview ?
  # interview <- cache %>% simple_return("#applying :nth-child(3) .scalar__value span")
  #Recommended Test
  # recomen_test <- cache %>% simple_return(".scalar--two .scalar__value span")
  
  result = tibble(
    name = name,
    pubPri = pubPri,
    category = list(other_type),
    #grades = grade,
    introduction = intro,
    programs = lst(tag_list),
    address = addr,
    town = town %>% str_to_title(),
    state = state,
    website = website,
    telephone = tel,
    tel_num = tel %>% str_extract_all("\\d") %>% flatten() %>% paste(collapse=""),
    rates = lst(rate_list),
    tuition = tuition,
    #aid = lst(aid_list),
    graduation_rate = grade_rate,
    academic = lst(acad_list),
    popular_uni = lst(pop_uni_list),
    pop_uni_attend = lst(uni_attend_list),
    safe_poll = safe_happy_list[1],
    happy_poll = safe_happy_list[2],
    population = student_num %>% as.numeric,
    teacher_student_ratio = ts_ratio,
    lat = geo_loc[1] %>% as.numeric(),
    lng = geo_loc[2]%>% as.numeric(),
    comment_dist = lst(comm_dist)
    # deadline = ddl,
    # application_fee = app_fee,
    # interview = interview,
    # test = recomen_test
  )
  return(result)
  
}

#A mini rought data frame for building and testing the shiny app

#There online schools will have NA for geo information and hence generate warnings
dirty_df <- niche_files[] %>% purrr::map_dfr(extra_info) 

saveRDS(dirty_df, file = "dirty_df.Rds")