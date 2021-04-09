## function to add mni-label

# packages

mni_label <- function(data){
  
  #' @author gian-andrea egeler
  #' @param data data set (df)
  #' @return edited data frame 
  #' @description function which adds mni-label information in the data frame
  #' according ZfV upb cut off is at <=2683
  #' according ZfV ebp cut off is between <=20|>=0
  #' @export
  
  # load funtion paste_na(, sep = ",")
  source("R/function_paste_na.R")
  
  df <- data %>% 
    # check cut off values
    dplyr::mutate(mni_ubp = dplyr::case_when(UBP_orig <=2683 ~ "erde",
                               TRUE ~ NA_character_),
           mni_ebp = dplyr::case_when(EBP_orig <=20 & EBP_orig >=0 ~ "herz",
                                 TRUE ~ NA_character_)) %>% 
    # paste both information into new variable 
    dplyr::mutate(mni_label = paste_na(mni_ubp, mni_ebp)) %>% 
    # new variable which checks if there is a mni_label
    # 0 = no mni label, 1 = mni_label present
    dplyr::mutate(mni_present = dplyr::if_else(!is.na(mni_label), 1, 0))
    
  return(df)
}


info_experiment <- function(path, sheet){
  
  #' @author gian-andrea egeler
  #' @param path path with the excel file
  #' @param sheet which excel sheet needs to be read 
  #' @return information about the field documentation 
  #' @description function which adds information of the interventions
  #' aka (field documentation) 
  #' @export
  
  # laod data of the field documentation
  info <- readxl::read_xlsx(path = path, sheet = sheet, trim_ws = TRUE) %>% 
    dplyr::mutate(date = lubridate::as_date(date)) %>% 
    dplyr::rename(canteen_name = mensa) %>% 
    # change umlaute to normal vocals
    dplyr::mutate(canteen_name = stringi::stri_trans_general(.$canteen_name,"Latin-ASCII")) %>% 
    # trim all white spaces, seems not to work (bekause its inside the string) 
    # mutate(comments = stringi::stri_trim_both(.$comments))
    # drop all NA in intervention (are all weekends)
    tidyr::drop_na(intervention) %>% 
    dplyr::select(-traditional, veggie, comments)
  
  return (info)    
}

message("Functions\n
        - mni_label
        - info_experiment 
        \nare ready")