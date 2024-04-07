## Frontmatter ---------------
##
## Script name: DuBois.R
## Author: Zane Wolf
## Date Created: 4/2
## Description: Tidy Tuesday's WEB Du Bois Challenge - creating a waffle chart using the Pan-African flag and associated colors and Jefferies font

##
## Notes:
##   
##
## ___________________________

# HOUSEKEEPING ---------------

# packages
library(tidyverse)
library(tidytuesdayR)
library(treemap)
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 
#library(janitor)
library(treemapify)
library(scales)
library(waffle)
library(fontawesome)
library(hrbrthemes)
library(emojifont)
library(showtext)




# GLOBAL SETTINGS ------------

# set global plotting theme - not using gg
theme_set(theme_minimal(base_size = 12, base_family = "Jefferies"))

theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(.5, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey30"),
  plot.title= element_text(size = 18, face = 'bold'),
  plot.subtitle = element_text(size = 12, color  ='red'),
  plot.caption = element_text(size = 6, margin = margin(t=15)),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(50,25,10,25)
)

# global variables
webdb_colors  <- c("#918b8f","#e0b9aa","#d2ad61","#c6b398","#858a79","#a2434d")

 
pan_colors2 <- c("#e4141e","#00863d","#000000","#E6A222","#224DE6","#c2c5bb")
pan_colors <- c("#c2c5bb","#224DE6","#E6A222","#00863d","#333330","#e4141e")

# FUNCTIONS ------------------


my_waffle <- function(x, rows = 5, use_glyph = 'square', glyph_size = 6,
                      title = 'Waffle chart') {
  
  len <- sum(x)
  waffles <- seq(len) - 1
  nms <- if(is.null(names(x))) seq_along(x) else names(x)
  df <- data.frame(xvals = waffles %/% rows,
                   yvals = 1 - (waffles %% rows),
                   fill = factor(rep(nms, times = x)))
  
  
  ggplot(df, aes(xvals, yvals, color = fill)) +
    geom_text(label = fontawesome(paste('fa', use_glyph, sep = '-')), 
              family = 'fontawesome-webfont', size = glyph_size) +
    coord_flip()+
    lims(x  = c(min(df$xvals) - 1, max(df$xvals) + 1),
         y  = c(min(df$yvals) - 1, max(df$yvals) + 1)) + 
    theme_void(base_size = 16, base_family = "Jefferies") +
    labs(title = title, color = NULL) +
    theme(
          plot.margin = margin(25, 25, 20, 25),  
          plot.title= element_text(size = 18, face = 'bold',margin=margin(l=20)),
          plot.subtitle = element_text(size = 12, color  ='grey30', margin=margin(t=10,l=20)),
          plot.caption = element_text(size = 8, margin = margin(t=15)),)
} 
# IMPORT DATA ----------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 14)

dubois_week10 <- tuesdata$dubois_week10 |> 
  mutate(Occupation = toupper(Occupation),
         labels = paste0(Occupation, " (",as.character(Percentage), "%)"),
         Number = round(Percentage*3.291,digits=0)) |> 
  arrange(Number)



# TREEMAP --------------------


# vp <- viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
#                width = unit(1, "npc"), height = unit(1, "npc"),
#                default.units = "npc", just = "centre",
#                gp = gpar(), clip = "inherit", mask = "inherit",
#                xscale = c(0, 1), yscale = c(0, 1),
#                angle = 0,
#                layout = NULL,
#                layout.pos.row = NULL, layout.pos.col = NULL,
#                name = NULL)
# 
# # 
# treemap(dubois_week10,
#         index="Occupation",
#         vSize="Percentage",
#         type="index",
#         title="PERCENTAGE",
#         palette =webdb_colors,
#         fontfamily.labels  ="VTC Du Bois Trial",
#         fontfamily.title  ="VTC Du Bois Trial Wide",
#         fontsize.labels = 13,
#         fontsize.title = 20,
#         border.col=c("white"),
#         border.lwds = 1
# )
# 
# 
# 
# ggplot(dubois_week10, aes(area = Percentage, fill = Occupation, label=Occupation)) +
#   geom_treemap()+
#   scale_fill_manual(values=pan_colors)+
#   geom_treemap_text(
#     color="black", 
#     place="center", 
#     size='12',
#     family = "Bohemian Typewriter"
#   )+
#   labs(title="Tidy Tuesday Week 11",subtitle="Illustrating The Careers Of 300 African American Graduates of Atlanta University as of 1900")

## WAFFLE CHART -----


my_waffle(dubois_week10$Number,
       rows=33, 
       use_glyph ="child", 
       glyph_size=14)+
  scale_color_manual(values=pan_colors, labels=dubois_week10$labels)+
  theme(legend.position = "bottom")+
  guides(colour = guide_legend(reverse=T))+
  labs(
    title=toupper("Illustrating The Careers Of 300 African American Graduates of Atlanta University as of 1900"),
    subtitle=toupper('"The university was founded in 1867. It has instructed 6000 negro students. It has graduated 330 negros among whom are:"'),
    caption="\n\nSource: Tidy Tuesday Week 14: 2024 Du Bois Visualization Challenge · Graphic: Zane Wolf")
  


