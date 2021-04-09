## function to filter till data

# author: gian-andrea egler (august 2020)

# packages
source("R/config.R")

house_keeping <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set
  #' @return clean data
  #' @description function which prepares data for filtering
  #' @export
  
  # house keeping
  
  for(clean in patt){
    
    # check if more variable names in pattern than skip the rest
    # important for data from 2019
    # however is not working for data from 2020
    # if(clean != "article"){next}
    
    data[, clean][[1]] <- stringi::stri_trans_general(data[, clean][[1]],"Latin-ASCII") # replace all umlaute
    data[, clean][[1]] <- stringr::str_to_lower(data[, clean][[1]]) # all to lower capitals
    data[, clean][[1]] <- stringr::str_replace(data[, clean][[1]], '\\*', '') # replace all asterisks
    data[, clean][[1]] <- stringr::str_replace_all(data[, clean][[1]], "[[:punct:]]", " ") # remove all punctuation
    data[, clean][[1]] <- stringr::str_replace_all(data[, clean][[1]], "[^[:alnum:]]", " ") #remove all non-alphanumeric characters
    data[, clean][[1]] <- tm::removeNumbers(data[, clean][[1]]) ## remove numbers
    data[, clean][[1]] <- tm::removeWords(data[, clean][[1]], tm::stopwords("german")) ## remove stop words
    data[, clean][[1]] <- tm::removeWords(data[, clean][[1]], words = stopwords_manual) # remove words form a document, see config.R
    data[, clean][[1]] <- tm::stripWhitespace(data[, clean][[1]]) # strip withe space (double white spaces are collapsed to one)
    data[, clean][[1]] <- stringr::str_trim(data[, clean][[1]], side = "both") # remove white space
    
    message(clean, " was 'cleaned'")
    
  }
  
  return(data) 
}


filter_data <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set (df)
  #' @return filtered data frame, most filter criteria from here
  #' https://zfv.ch/de/microsites/abb-restaurant-power-inn/menuplan
  #' https://zfv.ch/de/microsites/restaurant-allegra/menuplan
  #' https://zfv.ch/de/microsites/restaurant-daellebach/menuplan
  #' @description function which prepares data and filter only the 
  #' interested articles types
  #' @export
  
  #set filter criteria only for article
  #\\< x \\> points to the start and to the end of a word (seems not to work): https://stackoverflow.com/questions/7227976/using-grep-in-r-to-find-strings-as-whole-words-but-not-strings-as-part-of-words
  # look up for a better solution grep exact match: https://stackoverflow.com/questions/46153832/exact-match-with-grepl-r?rq=1
  # see config file for filter_match
  filter_list = paste0("^", filter_match,"$", collapse = "|") # paste single strings together
  
  
  # filter article
  df <- data %>% 
    # filter(grepl(filter_list, data$article)) # seems not all to filter as i like (str_detect) either
    dplyr::filter(stringr::str_detect(.$article, filter_list))
  
  # filter time stamp: sellings between 10:00 and 15:00
  # does not make a big difference
  df <- df %>% 
    dplyr::filter(., lubridate::hour(.$trans_date)>= 10 & lubridate::hour(.$trans_date)<=15)
  
  # filter date: sellings between kw34 and kw44
  df <- df %>% 
    dplyr::filter(., lubridate::date(.$trans_date) >= filter_date[1] & 
             lubridate::date(.$trans_date) <= filter_date[2])
  
  # drop all the tills, which are not needed, till_filter is in the config file
  df <- df %>% 
    dplyr::filter(stringr::str_detect(.$till, till_filter, negate = TRUE))
  
  
  return(df)
  
  message("data was filtered successfully according the string: ", filter_list)
}


edit_data <- function(data){
  
  #' @author gian-andrea egeler
  #' @param data data set (df)
  #' @return edited data frame 
  #' @description function which edits the data frame, adds new variables etc.
  #' @export 
  
  # source info about meal content from config.R file
  source("R/config.R")
  
  
  
  #skip this step in case we have the selling from 2020
  #add an error catch if the name of 2019 is not entered correcly
  if(deparse(substitute(data)) == "sellings_19"){
    
    # new variable date
    df <- data %>% 
      dplyr::mutate(date = lubridate::date(.$trans_date))
    
    # new variable meal_content
    df <- df %>% 
      dplyr::mutate(meal_content = stringr::str_replace_all(.$article, meat, "meat")) %>% 
      dplyr::mutate(meal_content = stringr::str_replace_all(.$meal_content, vegi, "ovo-lakto-vegetarian")) %>% 
      dplyr::mutate(meal_content = stringr::str_replace_all(.$meal_content, buffet, "buffet"))
    
    # change names of canteen (all lower capitals without umlaute)
    df <- df %>% 
      dplyr::mutate(canteen_name = dplyr::case_when(shop_id == 714 ~ "Prompt",
                                      shop_id == 743 ~ "Kontroll",
                                      shop_id == 745 ~ "Commit",
                                      TRUE ~ NA_character_))
    
    
  }else{
    
    # merge information from content_vga and content_veg together & add according
    # config_file the meal content
    df <- data %>% 
      dplyr::mutate(meal_content = stringr::str_replace_all(.$article, buffet, "buffet")) %>%
      dplyr::mutate(meal_content = stringr::str_replace_all(.$meal_content, meat, "meat")) %>% 
      dplyr::mutate(meal_content = stringr::str_replace_all(.$meal_content, vegi, "ovo-lakto-vegetarian")) %>% 
      dplyr::mutate(meal_content = dplyr::case_when(.$meal_content_veg == "vegetarisch" ~ "ovo-lakto-vegetarian",
                                      .$meal_content_vga == "vegan" ~ "vegan",
                                      TRUE ~ meal_content))
    
    
    # change names of canteen (all lower capitals without umlaute)
    df <- df %>% 
      dplyr::mutate(canteen_name = dplyr::case_when(shop_id == 714 ~ "Prompt",
                                      shop_id == 743 ~ "Kontroll",
                                      shop_id == 745 ~ "Commit",
                                      TRUE ~ NA_character_))
    
  }
  
  return(df)
}



message("functions:\n
      - house_keeping
      - filter_data
      - edit_data
      \nare ready")
