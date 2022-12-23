library(stringr)
library(tidyverse)
require(data.table)

dirty_df <- readRDS("dirty_df.Rds")
letter_levels <- c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-")

#Make the school name uniquely identify
non_unique_school<- c("Canterbury School", "Episcopal High School", "Highland Park High School","Jesuit High School",
                      "St. Andrew's Episcopal School", "Troy High School", "Veritas Collegiate Academy","University High School")

double_duplicate_school <- c("Veritas Collegiate Academy-VA", "University High School-CA")
dirty_df <- dirty_df %>% 
  mutate(name = if_else(name %in% non_unique_school,      paste0(name,"-",state)  ,name )) %>% 
  mutate(name = if_else(name %in% double_duplicate_school,paste0(name," at ",town),name ))

# Add missing Address
idx = which(str_detect(dirty_df$address,"\\#"))
dirty_df[idx,] %>% select(name,address,website)
dirty_df$address[idx[1]] <- "9888 Bissonnet, Suite-285, Houston, TX 77036"
dirty_df$address[idx[2]] <- "25 NE 2nd Street, Room 5601, Miami, FL 33132"
dirty_df$address[idx[4]] <- "1906 College Heights Blvd Ste 71031, Bowling Green, KY 42101"
dirty_df$address[idx[5]] <- "11011 SW 104 Street, Portable T-301, Miami, FL 33176"
dirty_df$address[idx[6]] <- "11380 NW 27th Avenue, Room 1111, Miami, FL 33167"
dirty_df$address[idx[7]] <- "19735 Colima Rd Ste 2, Rowland Heights, CA 91748"
dirty_df$address[idx[8]] <- "1715 High School Rd #240, Jackson, WY 83002"
dirty_df$address[idx[9]] <- "1164 N Virginia St, Reno, NV 89503"

# Clean the tuition values
idx1 <- which(str_detect(dirty_df$teacher_student_ratio,"\\$"))
idx2 <- which(is.na(dirty_df$tuition))

dirty_df$tuition[intersect(idx1,idx2)] <- dirty_df$teacher_student_ratio[intersect(idx1,idx2)]

dirty_df <- dirty_df %>% 
  mutate(tuition = ifelse(pubPri == "Public",0, tuition %>%str_remove("\\$") %>% 
                            str_remove(",") %>% 
                            as.numeric()))

#Clean Graduation Rate and Academic Performance
acad_record <- dirty_df %>% select(graduation_rate, academic) %>%
  unnest_wider(academic, names_sep = "_") 
n_observ = dim(dirty_df)[1]
GSA <- matrix(NA,nrow = n_observ, ncol = 3)
colnames(GSA) <- c("grad_rate","SAT","ACT")

for (i in 1:n_observ){
  if(length(acad_record$academic_1[[i]]) == 1){
    if(!is.null(acad_record$academic_1[[i]])) GSA[i,1] = as.numeric(str_remove(acad_record$academic_1[[i]],"%"))/100
    if(!is.null(acad_record$academic_2[[i]])) GSA[i,2] = as.numeric(acad_record$academic_2[[i]][1])
    if(!is.null(acad_record$academic_3[[i]])) GSA[i,3] = as.numeric(acad_record$academic_3[[i]][1])
  }
  else{
    if(!is.null(acad_record$graduation_rate[[i]])) GSA[i,1] = as.numeric(str_remove(acad_record$graduation_rate[[i]],"%"))/100
    if(!is.null(acad_record$academic_1[[i]])) GSA[i,2] = as.numeric(acad_record$academic_1[[i]][1])
    if(!is.null(acad_record$academic_2[[i]])) GSA[i,3] = as.numeric(acad_record$academic_2[[i]][1])
  }
}

dirty_df <- cbind(dirty_df,GSA) %>% as_tibble() %>% select(-academic, -graduation_rate)
dirty_df<- dirty_df %>% mutate(SAT = ifelse(SAT <100, NA, SAT))

#Clean the programs Offered
program_tags <- c("AP Offered","IB Offered", "Gifted Prog. Offered")
AIG = rep(NA,n_observ) %>% as.matrix()

for (i in 1:n_observ){
  idx = rep(FALSE,3)
  if ("AP Offered" %in% dirty_df$programs[[i]]){
    idx[1] = TRUE
  }
  if("IB Offered" %in% dirty_df$programs[[i]]){
    idx[2] = TRUE
  }
  if("Gifted Prog. Offered" %in% dirty_df$programs[[i]]){
    idx[3] = TRUE
  }
  if(any(idx)!=FALSE){
    AIG[i] = list(program_tags[idx])
  }
}

dirty_df<-dirty_df %>% select(-programs) %>% bind_cols(as.matrix(AIG) %>% as_tibble() %>% rename(programs = V1))


#Prepare the comment section
comment_rate <- dirty_df %>%
  select(comment_dist) %>%
  unnest_wider(comment_dist, names_sep = "_") %>%
  mutate(five = str_split(comment_dist_1, " "), four = str_split(comment_dist_2, " "),
         three = str_split(comment_dist_3, " "), two = str_split(comment_dist_4, " "), 
         one = str_split(comment_dist_5, " ")) %>%
  unnest_wider(five, names_sep = "") %>%
  unnest_wider(four, names_sep = "") %>%
  unnest_wider(three, names_sep = "") %>%
  unnest_wider(two, names_sep = "") %>%
  unnest_wider(one, names_sep = "") %>%
  select(five1, four1, three1, two1, one1) %>%
  rename(five = five1, four = four1, three = three1, two = two1, one = one1) %>%
  mutate(five = as.integer(five), four = as.integer(four), three = as.integer(three), two = as.integer(two), one = as.integer(one)) %>%
  mutate(peopleRated = five + four + three + two + one) %>%
  mutate(meanRating = round((5*five + 4*four + 3*three + 2*two + one)/peopleRated, 2)) %>% select(c(6,7))
dirty_df <- dirty_df %>% select(-comment_dist) %>% bind_cols(comment_rate)


#Clean Niche Rating
niche_rating = dirty_df %>%
  select(rates) %>%
  unnest_wider(rates, names_sep = "_") %>%
  # Academic
  mutate(Academics = ifelse(str_detect(rates_1, "unavailable"), NA, 
                            str_replace(str_sub(str_replace(rates_1, "Academicsgrade", ""), 2), " minus", "-"))) %>%
  # Diversity
  mutate(Diversity = ifelse(str_detect(rates_2, "unavailable"), NA, 
                            str_replace(str_sub(str_replace(rates_2, "Diversitygrade", ""), 2), " minus", "-"))) %>%
  # Teachers
  mutate(Teachers = ifelse(str_detect(rates_3, "unavailable"), NA, 
                           str_replace(str_sub(str_replace(rates_3, "Teachersgrade", ""), 2), " minus", "-"))) %>%
  # College Prep
  mutate(CollegePrep = ifelse(str_detect(rates_4, "unavailable"), NA, 
                              str_replace(str_sub(str_replace(rates_4, "College Prepgrade", ""), 2), " minus", "-"))) %>%
  # Clubs & Activities
  mutate(ClubsActivities = ifelse(str_detect(rates_5, "unavailable"), NA, 
                                  str_replace(str_sub(str_replace(rates_5, "Clubs & Activitiesgrade", ""), 2), " minus", "-"))) %>%
  # one outlier handling
  mutate(CollegePrep = ifelse(CollegePrep %like% "grade", NA, CollegePrep)) %>%
  mutate(ClubsActivities = ifelse(ClubsActivities %like% "grade", NA, ClubsActivities)) %>%
  select(Academics, Diversity, Teachers, CollegePrep, ClubsActivities)

dirty_df<-dirty_df %>% select(-rates) %>% bind_cols(niche_rating)
dirty_df$Academics <- factor(dirty_df$Academics, level = letter_levels)
dirty_df$Diversity <- factor(dirty_df$Diversity, level = letter_levels)
dirty_df$Teachers <- factor(dirty_df$Teachers, level = letter_levels)
dirty_df$CollegePrep <- factor(dirty_df$CollegePrep, level = letter_levels)
dirty_df$ClubsActivities <- factor(dirty_df$ClubsActivities, level = letter_levels)


# Clean the two poll data
dirty_df$safe_poll <- dirty_df$safe_poll%>%
  str_remove("%") %>%
  {suppressWarnings(as.numeric(.)/100)}

dirty_df$happy_poll <- dirty_df$happy_poll%>%
  str_remove("%") %>%
  {suppressWarnings(as.numeric(.)/100)} 

#Remove three online schools
dirty_df<-dirty_df %>% filter(!is.na(lat))

# Student:Teacher Ratio Cleaning
dirty_df$teacher_student_ratio <- lapply(dirty_df$teacher_student_ratio, 
                                         function(x) replace(x, 
                                                             !grepl(":", x), 
                                                             NA)) %>%
  sub("\\:.*", "", .) %>%
  as.numeric()

dirty_df$teacher_student_ratio <- lapply(dirty_df$teacher_student_ratio, 
                                         function(x) ifelse(x > 100, NA, x)) %>%
  as.numeric()

#Drop wired observation
dirty_df<-dirty_df %>% filter(name != "Rockingham County Schools") %>% filter( name != "Mt. Everest Academy")


#Both uni had been cleanned in the helper script and stored seperately
tidy_df <- dirty_df %>% select(-popular_uni, -pop_uni_attend)


#Get the Uni Attender Data
unnest_uni_df <- dirty_df  %>% 
  select(name, popular_uni, pop_uni_attend) %>%
  rename("uni" = popular_uni, "attend" = pop_uni_attend) %>%
  unnest_wider(uni, names_sep = "_") %>%
  unnest_wider(attend, names_sep = "_")

uni_df <- inner_join(unnest_uni_df %>% select(name, contains("uni")) %>%
                       pivot_longer(-1, names_to = c("drop", "rank"), values_to = "University", names_sep = "_") %>%
                       select(-drop),
                     unnest_uni_df %>% select(name, contains("attend")) %>%
                       pivot_longer(-1, names_to = c("drop", "rank"), values_to = "attend", names_sep = "_") %>%
                       select(-drop),
                     by = c("name", "rank"))%>% 
  mutate("Attender #" = str_remove_all(attend, "\\D*")%>% as.integer(),
         rank = as.integer(rank)) %>% select(-attend) 


saveRDS(tidy_df, file = "tidy_df.Rds")
saveRDS(uni_df, file = "uni_df.Rds")
