# function to plot intervention per canteen

# state: december 2020

#source mytheme information
source("R/config.R")

plot_intervention <- function(data, 
                              canteen_pr = NULL,
                              save = FALSE, 
                              width = NULL, 
                              height = NULL){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param canteen string containing which canteen
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return barplot with ggplot
  #' @export
  
  #prepare data
  df <- data %>%
    # add some new variable which contains also "information" about post intervention e.g. nachhaltigkeitswoche 26. bis 30. Oktober 2020 
    dplyr::mutate(condit = dplyr::case_when(date > "2020-09-30" &
                                            date < "2020-10-26" ~ "post",
                                            date > "2020-10-25" ~ "nachhaltigkeitswoche",
                              TRUE ~ intervention)) %>% 
    dplyr::filter(canteen_name == toString(canteen_pr)) %>% 
    dplyr::group_by(canteen_name, condit, mni_label) %>% 
    dplyr::summarise(tot_sold = n()) %>% 
    dplyr::mutate(pct = tot_sold / sum(tot_sold)) %>% 
    ungroup()
  
  # add text and xlab
  df_t <- df %>% 
    dplyr::group_by(condit) %>%
    dplyr::summarise(tot = sum(tot_sold)) %>% 
    ungroup() %>%
    # add new labels to condition, and change to factor to reorder xlab (more or less )
    dplyr::mutate(condit_lab = factor(condit, levels = def_lev, labels = def_lev_new)) %>% 
    dplyr::mutate(xlab_ = paste0("(n = ", tot,
                          ")"),
           xlab = paste(condit_lab, xlab_, sep = "\n")) %>% 
    dplyr::left_join(., df, by = "condit") %>% 
    dplyr::mutate(mni_label = dplyr::if_else(is.na(mni_label), "kein MNI", mni_label)) %>%
    dplyr::mutate(xlab = forcats::fct_reorder(xlab, as.numeric(.$condit_lab))) # rearrange factor for plots
  
    
  #plot
  # order factor according condit is not working
  p <- ggplot2::ggplot(df_t, ggplot2::aes(x = xlab,
                        y = pct, fill = factor(mni_label, levels = old_labs,
                                               labels = new_labs))) +
    ggplot2::geom_bar(stat = "identity", position = position_stack(), width = .6) +
    ggplot2::scale_fill_manual(values = pal) +
    scale_y_origin(labels = scales::percent) +
    ggplot2::guides(fill = guide_legend("MNI-Label")) +
    ggplot2::geom_text(aes(label = if_else(pct > 0.02, scales::percent(pct, accuracy = 1), "")), 
              position = position_stack(reverse = F, vjust = .5), size = 22 * converter) +
    ggplot2::labs(y = "Anteil Menüverkäufe in Prozent", x = "") +
    # coord_flip() +
    mytheme
    # theme(legend.position = "bottom")
  
  if(save == TRUE){
    ggplot2::ggsave(filename = here::here("plots",
                                          paste0("intervention_",
                                                 canteen_pr,
                                                 "_", 
                                                 format(Sys.Date(), "%Y_%m_%d"),
                                                 ".pdf")),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = cairo_pdf)
    
    message("plot ", canteen_pr,
                            "_", 
                            format(Sys.Date(), "%Y_%m_%d"),
                            ".pdf",
            " has been saved")
    
    ggplot2::ggsave(filename = here::here("plots",
                                          paste0("intervention_",
                                                 canteen_pr,
                                                 "_", 
                                                 format(Sys.Date(), "%Y_%m_%d"),
                                                 ".png")),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png")
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".png",
            " has been saved")
    
  }else{
    print(p)
  }
}


plot_mni_cut <- function(data,
                         var = NULL,
                         save = FALSE, 
                         width = NULL, 
                         height = NULL){
  
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param var containing the variables (as string) to group data frame
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return barplot with ggplot
  #' @export

  
  #prepare data
  ds_p <- data %>% 
    # group data
    group_by_at(vars(card_num, var, mni_cut)) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    #drop missings in gender (-2)
    drop_na(var) %>% 
    #drop tot_sold are not in that interessted
    select(-tot_sold) %>% 
    group_by_at(vars(var, mni_cut)) %>% 
    summarise(share = n()) %>% 
    mutate(pct = share / sum(share)) %>% 
    ungroup()
  
  
  #txt
  ds_p %<>% 
    group_by_at(vars(var)) %>% 
    summarise(tot_t = sum(share)) %>% 
    ungroup() %>% 
    left_join(., ds_p, by = c("gender")) %>% 
    mutate(xlab_ = paste0("(n = ", tot_t, ")"),
           xlab = paste(if_else(var == "f", "Frau", "Mann"), xlab_, sep = "\n")) #not generic!
  
  #overall
  ds_t <- ds_p %>% 
    group_by(mni_cut) %>% 
    summarise(tot_t = sum(share)) %>% 
    mutate(pct = tot_t / sum(tot_t)) %>% 
    ungroup() %>% 
    #see pckg glue for dynamic programming: https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
    mutate(!!var := "Alle") %>%  
    mutate(xlab_ = paste0("(n =", sum(tot_t), ")"),
           xlab = paste(.[[var]], xlab_, sep = "\n")) %>% 
    bind_rows(., ds_p)
  
  
  #plot itself
  pal = c("MNI-Selten-Wähler" = "#CCC2CD",
          "MNI-Gelegenheitswähler" = "#DBE8D9",
          "MNI-Regelmässig-Wähler" = "#DAF3FF",
          "MNI-Stammwähler" = "#7A92B3"
  )
  
  
  p <- ggplot(ds_t, aes(y = pct, x = forcats::fct_rev(xlab), fill = mni_cut)) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(aes(label = scales::percent(pct, accuracy = 1)), position = position_stack(vjust = .5), size =  22 * converter)+
    scale_fill_manual(values = pal) +
    scale_y_origin(labels = scales::percent) +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "", reverse = TRUE, nrow = 1)) +
    coord_flip() +
    mytheme +
    theme(legend.position = "bottom")
  
  
  if(save == TRUE){
    ggplot2::ggsave(filename = here::here("plots",
                                          paste0("intervention_",
                                                 canteen_pr,
                                                 "_", 
                                                 format(Sys.Date(), "%Y_%m_%d"),
                                                 ".pdf")),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = cairo_pdf)
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".pdf",
            " has been saved")
    
    ggplot2::ggsave(filename = here::here("plots",
                                          paste0("intervention_",
                                                 canteen_pr,
                                                 "_", 
                                                 format(Sys.Date(), "%Y_%m_%d"),
                                                 ".png")),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png")
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".png",
            " has been saved")
    
  }else{
    print(p)
  }
  
}

message("function(s):\n 
        - plot_intervention 
        . plot_mni_cut (not ready!)
        \nare ready!")