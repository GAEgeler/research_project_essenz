#load till data

#autor: gian-andrea egeler
#state: february 2021

# load functions
source("R/config.R")
source("R/01_function_read_data.R")
source("R/02_function_house_keeping.R")
source("R/03_function_intervention_mni.R") 


#prepare folder structure
if(!dir.exists(here::here("processed data/"))){
  dir.create(here::here("processed data/"))
  message("new directory for processed data was created")
}

## ----load data 2019--------------------
# first define path resp. where the data is
path_ = here::here("raw data/2019", "Essenz_4Betriebe_01082019-30112019_Kasse.xlsx")

# concatenate data
# takes around: 5min, very slow
# warnings can be ignored, due to id_variable, read functions expects a numerical number (however there were also letters in it)
sellings19 <- path_ %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names()

# diselect city port
pattrn <- grep("682 CityPort", sellings19, invert = TRUE, value = TRUE)
 
sellings19 <- pattrn %>% 
  purrr::map_df(~ read_till_data_19(path_, .x), .id = "sheet") 


## ----load data 2020--------------------

# first define path resp. where the data is
# watch that the path ends with a /
path_ = here::here("raw data/2020/")

# set pattern to loop trough
patt_till = "Kassenexport.csv"
patt_info = "Menudatenexport.csv" # 743 had some parsing failures, had to check and save it again => 



# challenge loading data is, there are always two files, which correspond to each other (resp. need to be merged together)
sellings20 <- read_till_data_2020(path_)



## ----house keeping---------------------
# first clean data a bit
# set variables for house keeping
patt = c("article")
sellings19 <- house_keeping(sellings19)

# set variables for house keeping
patt = c("article", "meal_description", "meal_line", "meal_content_veg", "meal_content_vga") # attention card_num of powerinn contains some points
sellings20 <- house_keeping(sellings20)



## ----filter data-----------------------
# filter data according lunch meals
# set variables for filtering
patt = c("article") # ,"article_description", "meal_line", "meal_content_light"
# set filter date: from until
filter_date = c("2019-08-19", "2019-11-02") #kw 34 bis kw 44

sellings_19 <- filter_data(sellings19)


# set variables for filtering
patt = c("article") 
# set filter date
filter_date = c("2020-08-17", "2020-10-31") #kw 34 bis kw 44
sellings_20 <- filter_data(sellings20)

#double check if all meal lines are there for 2019
message("All meal lines that were considered for the analyses")
sort(unique(sellings_19$article))

# meal lines 2020 
message("All meal lines that were considered for the analyses")
sort(unique(sellings_20$article))

# most of the NA's are buffet
# ggplot2::ggplot(sellings_20, aes(x = meal_line, y = ..count..)) +
#   geom_bar(stat = "count")
# 
# ggplot2::ggplot(sellings_19, aes(x = article, y = ..count..)) +
#   geom_bar(stat = "count")

## ----edit data-------------------------
### 2019
# add some variables as meal content 
sell19_agg <- edit_data(sellings_19)

#double check if all meals are there
message("The meal lines were summarized to three meal contents")
unique(sell19_agg$meal_content)


### 2020
# add some variables 
sellings_20 <- edit_data(sellings_20)

#double check if all meals are there
message("The meal lines were summarized to three meal contents")
unique(sellings_20$meal_content)


## ----insert mni information------------
# insert mni information
sell_20 <- mni_label(sellings_20)



## ----merge information field documentation----
# define path & sheet
path = here::here("raw data/field_documentation/Versuchsdesign_13.09.2020.xlsx")
sheet = "final_long"

#load field documentation
field_doc <- info_experiment(path, sheet)

#merge information back to
sell20_agg <- sell_20 %>% 
  dplyr::left_join(., field_doc, by = c("date", "canteen_name"))



## ----combinde data 2019 & 2020---------
# merge data from 2019 and 2020
#https://stackoverflow.com/questions/25923392/select-columns-based-on-string-match-dplyrselect
patt = c("trans_date", "shop_id", "article", "canteen_name", "meal_content", "meal_line", "year") # only selected variables were further processed

sell19 <- sell19_agg %>% 
  dplyr::mutate(year = 2019) %>% 
  dplyr::select(tidyselect::matches(patt)) 

sell20 <- sell20_agg %>% 
  dplyr::mutate(year = 2020) %>% 
  dplyr::select(tidyselect::matches(patt))

sellings_tot <- bind_rows(sell19, sell20) %>% 
  dplyr::mutate(date = lubridate::date(trans_date))



## ----edit data: import survey information (e.g. cardnum, age etc.)----
# set directory for loading badge number
path_bd = here::here("raw data/badge_nr/Daten_t0_t1_alle Betriebe_bereinigt_V2.xlsx")

# load data
survey_info <- read_survey_info(path_bd)

# merg01_read_survey_info_2020.R about the card_num into the data set sellings_20
# sime things to keep it mind: 
#1) delete points (see house keeping) 
#2) delete K in string only for control canteen (Kontroll) plus there is one person containing an X in the number: K00000X
#3) delete 3000 in canteen "Commit"

# check if cardnum is in card_num_nr before joining
commit <- sell20_agg %>% 
  #only canteen Commit
  dplyr::filter(canteen_name == "Commit") %>% 
  # fist to charachter then subset the string resp start with the fifth position
  dplyr::mutate(card_num_nr = stringr::str_sub(card_num, start = 5)) %>% 
  # change back to numeric
  dplyr::mutate(card_num_nr = as.numeric(card_num_nr))


sell20_survey <- sell20_agg %>% 
  #deelete special characters
  dplyr::mutate(card_num_nr = stringr::str_replace_all(.$card_num, "[[:punct:]]", "")) %>%  
  dplyr::mutate(card_num_nr = stringr::str_remove_all(.$card_num_nr, "K|X")) %>% 
  dplyr::mutate(card_num_nr = as.numeric(.$card_num_nr)) %>% 
  dplyr::filter(canteen_name != "Commit")
  

# bind them togheter and filter out only time of the field experiment
sell20_survey <- dplyr::bind_rows(sell20_survey, commit)  %>% 
  dplyr::filter(date < "2020-10-01")

# merge information from selling data and information about the person
sell20_ind <- sell20_survey %>% 
  dplyr::inner_join(., survey_info[, -1], by = c("card_num_nr")) # c("a" = "b") is not working, why?


# seems that only 100 can be merged
length(unique(sell20_ind$card_num))

#remove all unused data sets
rm(list = c("sellings_20", "field_doc", 
            "sellings20", "path_", 
            "path", "buffet", "filter_match", 
            "meat", "sheet", "stopwords_manual", 
            "vegi", "till_filter", "sell19", 
            "sell20", "survey_info", "path_bd", 
            "sell20_survey", "sellings_NA", "sell_20", 
            "sell19", "pattrn", "patt_info", "patt_till", 
            "patt", "sellings_19", "sellings19", "commit"))
