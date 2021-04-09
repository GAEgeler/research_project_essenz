# set some configurations

#set filter for tills, which are dropped for the analyses--------
# pay attention to lower and upper capitals
till_filter = c("Oase Restaurant+|Giardino+")


#set filter for meal_lines----------
filter_match = c("traditional", "veggie", "global", "exquisit", "classic", "spezial", "soul", "bowl", 
                 "salatteller", "salatteller klein", "salatteller gross", "salatbecher", "tagesmenu veggie",
                 "vegi", "tagesmenu", "tagesmenu traditional", "hot green market", "free choice buffet") # are they sufficient?

stopwords_manual = c("g", "dl", "ta")


#set meal_content:------------
#meals with meat
meat = c("classic|traditional|^tagesmenu traditional$|exquisit|spezial|^tagesmenu$") # spezial is fitnesteller

#vegi meals
vegi = c("soul|vegi|veggie|^tagesmenu veggie$")

#define buffet
buffet = c("bowl|^salatteller$|^salatteller klein$|^salatteller gross$|salatbecher|global|^hot green market$|^free choice buffet$")

#set mytheme--------------

# configurations concernig the plots

#load packages
library(extrafont)
# loadfonts(device = "win", quiet = TRUE) # attention works only if you work with 
# rstudio offline

mytheme <- ggplot2::theme_bw()+ # definve theme for plot
  ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"),
        axis.text.x = ggplot2::element_text(size=22),
        axis.text.y = ggplot2::element_text(size=22, face = "plain"),
        legend.text = ggplot2::element_text(size = 22),
        legend.title = ggplot2::element_text(size =22),
        strip.text = ggplot2::element_text(size=30),
        panel.spacing = ggplot2::unit(1, "lines"), # space between panels 
        axis.title.y = ggplot2::element_text(size = 22, margin = ggplot2::margin(t = 0, r = 22, b = 0, l = 0)),
        axis.title.x = ggplot2::element_text(size = 22,  margin = ggplot2::margin(t = 22, r = 0, b = 0, l = 0)),
        plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b=15),size = 22),
        plot.caption = ggplot2::element_text(margin = ggplot2::margin(t=15), face="italic", size=22),
        # text = element_text(family = "Akkurat"), #noch anpassen mit Calibri
        legend.key = ggplot2::element_rect(color = "white", size = 6, fill = "white"), # see for that part the funktion draw_key_ploygon3
        legend.key.size = ggplot2::unit(1.5, "cm"),
        legend.margin = ggplot2::margin(-0.5, 0, 0.05, 0, "cm"),
        plot.margin = ggplot2::unit(c(t = 0, r = 0, b = 0, l = 0),"cm")) 


#set coord expansion
#source: https://stackoverflow.com/questions/61969752/force-the-origin-to-start-at-0-without-margin-between-data-and-x-axis-in-new-ggp
scale_y_origin <- function(...) {
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), ...)
}

scale_x_origin <- function(...) {
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02)), ...)
}

scale_x_origin_dis <- function(...) {
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.02, 0.02)), ...)
}


#geom_text size "converter": https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size
converter = (25.4/72.27)
