## functions to load till data


#define function
read_till_data_19 <- function(path, sheet){
  
  #' @author gian-andrea egeler
  #' @param path path of the data file
  #' @param sheet sheet names, thus the loop works correct
  #' @return data frame 
  #' @description function which loads data from zfv till in xlsx format, 
  #' thus filters some only the importand variable names
  #' @export
  
  # define read_xlsx
  df <- readxl::read_xlsx(path = path, sheet = sheet, col_names = TRUE, trim_ws = TRUE) %>% 
    dplyr::rename(id_ = ID, article_id = ARTICLE_ID, transaction_id = TRANSACTION_ID, shop_id = Zugeordnete_KST, till = TILL_DESCRIPTION, 
           trans_date = TRANS_DATE, tota_amaount = TOTAL_AMOUNT, 
           pricelevel = PRICELEVEL_DESCRIPTION, card_num = CARD_NUM, price = PRICE, article = ARTICLE_DESCRIPTION, 
           turnover = Betrag_Brutto, canteen = PROFIT_CENTER_DESCRIPTION) %>% 
    dplyr::select(id_, article_id, trans_date, transaction_id, shop_id, till, pricelevel, card_num, price, article, turnover, canteen) %>% 
    # bring it into the right format
    dplyr::mutate(trans_date = lubridate::as_datetime(.$trans_date)) 
  
  # print if sheet_name is read successfully
  for (name_sheet in sheet) {
    message("sheet ", name_sheet," was read successfully")
  }  
  
  return(df)
}


read_till_data_2020 <- function(path){
  
  #' @author gian-andrea egeler
  #' @param path path of the data file
  #' @return data frame
  #' @description function which loads data from zfv 2020 and merges data from
  #' another source
  #' @details id_ and transaction_id should be the same
  #' @details article = contains some article descriptions and meal lines
  #' meal_descriptions contains information about the meal content 
  #' @export
  
  # prepare data lists for loop (city_port is excluded)
  list_till <- list.files(path = path, pattern = patt_till)[-1]  
  list_info <- list.files(path = path, pattern = patt_info)[-1]  
  
  # empty tibble
  df_ <- tibble::tibble() # as_tibble gives an error
  
  for (cant in 1:length(list_till)) {
    df_till <- readr::read_delim(file = paste0(path, list_till[cant]), col_names = T,
                          trim_ws = T, delim = ";", escape_double = FALSE,
                          col_types = readr::cols(CARD_NUM = readr::col_character()),
                          locale = readr::locale(encoding = "latin1")) %>% 
      dplyr::rename(id_ = ID, article_id = ARTICLE_ID, article_code = ARTICLE_CODE, 
             transaction_id = TRANSACTION_ID, shop_id = Zugeordnete_KST, 
             till = TILL_DESCRIPTION, 
             trans_date = TRANS_DATE, total_amount = TOTAL_AMOUNT, 
             pricelevel = PRICELEVEL_DESCRIPTION, card_num = CARD_NUM, 
             price = PRICE, article = ARTICLE_DESCRIPTION, 
             turnover = Betrag_Brutto, canteen = PROFIT_CENTER_DESCRIPTION) %>% 
      dplyr::select(id_, article_id, article_code, trans_date, transaction_id, shop_id,
             till, pricelevel, card_num, price, total_amount, article, turnover, canteen) %>% 
      # bring it into the right format
      dplyr::mutate(date = lubridate::date(.$trans_date))
    
    message("iNFO: data ", list_till[cant], " is beeing processed")
    
    #escape_double made the trick => somehow problems to read date format while importing the csv
    df_info <- readr::read_delim(file = paste0(path, list_info[cant]), col_names = T, 
                          trim_ws = T, delim = ";", escape_double = FALSE,
                          locale = readr::locale(encoding = "latin1")) %>% 
      dplyr::rename(date = Datum, article_code = Artikelcode, 
             meal_description = Menubeschrieb,
             meal_line = MenutypeBeschrieb,
             meal_content_veg = Menuattribut_Veg, 
             meal_content_vga = Menuattribut_Vga, EBP = MenuMNI_EBP, 
             UBP = MenuMNI_UBP, EBP_orig = MenuMNI_EBP_abs,
             UBP_orig = MenuMNI_UBP_abs) %>% 
      dplyr::select(date, article_code, meal_description, meal_line, 
             meal_content_veg, meal_content_vga,
             EBP, UBP, EBP_orig, UBP_orig) %>% 
      # change for merging to date format  
      dplyr::mutate(date = lubridate::as_date(date))
    
    
    message("iNFO: data ", list_info[cant], " is beeing processed")
    
    df_full <-  df_till %>% 
      #merge information from df_info and df_till
      dplyr::left_join(., df_info, by = c("date", "article_code")) %>% 
      dplyr::mutate(source_id = list_till[cant])
    
    message("iNFO: data ", list_till[cant], " and ", list_info[cant], " were merged together")
    
    
    # concatenate data frames together
    df_ =  dplyr::bind_rows(df_, df_full)
    
  }
  
  return(df_)
  
}



#define function
read_survey_info <- function(path){
  
  #' @author gian-andrea egeler
  #' @param path path of the data file
  #' @return data frame with card number of those which filled out the survey
  #' @description function which loads the individual 
  #' card number with information
  #' about the person behind the card number
  #' @export
  
  df <- readxl::read_xlsx(path = path, col_names = T, trim_ws = T, na = "#NULL!") %>% 
    # gender and age are missing at the moment
    dplyr::select(STARTED, Badgenummer, EF04_01, ET01, EF17, EF06, EF07, EF18, EF16, EF10_01, Betrieb) %>%
    dplyr::rename(date = STARTED, card_num_nr = Badgenummer, diet = ET01, age = EF04_01, gender = EF17,
           educ = EF06, income = EF07, office = EF18, home_o = EF16, 
           shop_survey = Betrieb,
           eating_ou = EF10_01) %>%
    dplyr::mutate(gender =  dplyr::case_when(gender == 1 ~ "f",
                              gender == 2 ~ "m",
                              gender == 3 ~ "divers",
                              TRUE ~ NA_character_)) %>%
    dplyr::mutate(shop_survey =  dplyr::case_when(shop_survey == 1 ~ "Prompt",
                                   shop_survey == 2 ~ "Commit",
                                   shop_survey == 3 ~ "Kontroll",
                                   shop_survey == 4 ~ "andere",
                                   TRUE ~ NA_character_)) %>% 
    dplyr::mutate(diet =  dplyr::case_when(diet == 1 ~ "vegan",
                            diet == 2 ~ "ovo-lakto_vegetarisch",
                            diet == 3 ~ "pescetarisch",
                            diet == 4 ~ "flexitarisch",
                            diet == 5 ~ "fleisch-liebhaber",
                            TRUE ~ NA_character_)) %>% 
    dplyr::mutate(educt =  dplyr::case_when(educ == 1 ~ "kein Schluabschluss",
                             educ == 2 ~ "obligatorische Sekundarstufe",
                             educ == 3 ~ "Matura",
                             educ == 4 ~ "Universität",
                             TRUE ~ NA_character_),
           income =  dplyr::case_when(income == 1 ~ "CHF 4999 oder weniger",
                              income == 2 ~ "CHF 5000 – CHF 6999",
                              income == 3 ~ "CHF 7000 – CHF 8999",
                              income == 4 ~ "CHF 9000 – CHF 10999",
                              income == 5 ~ "CHF 11000 – CHF 13999",
                              income == 6 ~ "CHF 14000 oder mehr", 
                              income == 7 ~ "keine Angabe",
                              TRUE ~ NA_character_)) %>% 
    # change "nie" to 0
    dplyr:: mutate(office =  dplyr::if_else(office == 1,0, office),
           home_o =  dplyr::if_else(home_o == 1, 0, home_o)) %>% 
    # drop na's in card_num
    tidyr::drop_na(card_num_nr)
  
  return(df)
  
}




message("functions:\n
        - read_till_data_19
        - 'read_till_data_2020'
        - read_survey_info
        \nare ready")



