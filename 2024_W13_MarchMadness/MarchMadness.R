## Frontmatter---------------------------
##
## Script name: MarchMadness.R
##
## Description: Tidy Tuesday NCAA March Madness script, with two plots
## - heatmap visualizing percent of people picking each team to win throughout rounds
## - barbell plot showcasing total games, wins, and losses per team
##
## Author: Zane Wolf
##
## Date Created: 3/28/2024
##
## Notes: 
##   
##
## ____________________________________

# HOUSEKEEPING ---------------
library(tidyverse)
library(hrbrthemes)
library(paletteer)
library(ggthemes)
library(prismatic)
library(scales)
library(png)
library(grid)
library(magick)

# GLOBAL SETTINGS ------------

# custom plot settings, will be applied to every plot
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))

theme_update(
  axis.ticks = element_line(color="grey92"),
  axis.ticks.length = unit(.5, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size=12),
  legend.text = element_text(color="grey30"),
  plot.title= element_text(size = 18, face = 'bold'),
  plot.subtitle = element_text(size = 12, color  ='grey30'),
  plot.caption = element_text(size=9, margin = margin(t=15)),  
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(25,25,10,25)
)


# IMPORT DATA ----------------
tuesdata <- tidytuesdayR::tt_load('2024-03-26')
team_results <- tuesdata$`team-results`
public_picks <- tuesdata$`public-picks`

logo <- magick::image_read("C:/Users/Zane/Documents/Projects/Dataviz/TidyTuesday/March_Madness/NCAA-March-Madness-logo.png")

img <- grid::rasterGrob(logo,interpolate=T)

# EDA ------------------------
# if I were doing data exploration, that section would be here. However,I have a relatively good idea of the data I want to focus on from the data dictionaries and just looking at the dataframe, so I'm being lazy. JT would be so proud. 

# PLOTS ----------------------
## Heatmap -----

# tidy up the data
df_public  <- public_picks |> 
  pivot_longer(!c(TEAM, YEAR, TEAMNO), names_to="round", values_to="percent")  |>
  mutate(#TEAM=fct_rev(factor(TEAM)), #use either this or the reorder function in the ggplot line to sort teams. Remove both to see default sorting, then add one and the other to determine what they do
         round=factor(round, levels=c('R64','R32','S16','E8','F4','FINALS')),
         percent=as.numeric(str_split_i(percent,"%",1))) |> 
  arrange(-percent)



df_public |> 
  ggplot()+
  
  # try adding a - in front of 'percent' in the reorder function, see what happens
  geom_tile(aes(round,reorder(TEAM,percent), fill=percent),color="white",lwd=0.5,linetype=1)+
  
  # change color palette and define ticks for legend
  scale_fill_paletteer_c("ggthemes::Classic Blue",
                         limits=c(0,100),
                         breaks=c(0,25,50,75,100),
                         labels=c(0,25,50,75,100))+
  
  # customize x tick labels
  scale_x_discrete(labels=c("Round of 64","Round of 32", "Sweet 16", "Elite 8", "Final Four", "Finals"))+
  
  # make the legend more aesthetically pleasing
  guides(fill = guide_colourbar(
    title = "Percent", 
    title.position  ='top', 
    title.hjust= 0.5, 
    barwidth = unit(10, 'lines'),
    barheight = unit(.5, 'lines')
  ))+
  
  # add title and subtitles, remove axis labels
  labs(
    title = 'March Madness: Place Your Bets',
    subtitle= str_wrap('The percent of people who picked each team to win the game in each round',40),
    x = element_blank(),
    y = element_blank()
  )+
  
  # move legend and angle x axis labels so they are easier to read
  theme(legend.position = 'top',
        axis.text.x = element_text(angle=45, hjust=1, size=12), 
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(color='#26456E'))+
  coord_cartesian(clip='off')+
  
  # add logo 
  annotation_custom(grob=img,ymin=79,ymax=69,xmin=2,xmax=9)
  
  

## RECYCLING BIN ----
# this is code that I no longer need but don't want to delete, so i just move it to the bottom and hide it so it doesn't clutter up the script

# 
# df_combo <- team_results |>
#   select(TEAM, R64,R32,S16,E8,F4,F2,CHAMP, GAMES, W,L) |>
#   pivot_longer(!c(TEAM,CHAMP), names_to="round", values_to="num")  |>
#   left_join(y=df_public,by=c("TEAM", "round"), relationship="many-to-many") |>
#   drop_na()
# 
# ## Barbell Plot ----
# 
# pal_base <- c("#C3C3C3", "#1C6AA8")
# pal_dark <- clr_darken(pal_base, .25)
# 
# grey_base <- "grey50"
# grey_dark <- "grey25"
# 
# 
# title <- paste0(
#   "title, <b>bold <b style='color:",
#   pal_dark[1], ";'>lowest loss</b> and <b style='color:", pal_dark[2], ";'>highest win</b> rates</b>"
# )
# 
# caption <- paste0(
#   "<b>Source: kicker.de<br><br></b> Teams are ranked by ..."
# )
# 
# # callout <- paste0(
# #   "<b>Frankfurt</b> is the only team listed<br>that <b style='color:", pal_dark[1],
# #   ";'>lost more games (37.3%)</b><br>than <b style='color:", pal_dark[2], ";'>they have won (36.8%)</b>"
# # )
# 
# df_public_teams <- unique(reorder(df_public$TEAM,df_public$percent))
# 
# df_team_factors <- factor(df_team, levels=df_public_teams)
# 
# df_wl <- team_results |> 
#   select(c(TEAM, GAMES,W, L, CHAMP))  |> 
#   add_row(TEAM="Duquesne", GAMES=0, W=0, L=0, CHAMP=0) |> 
#   add_row(TEAM="McNeese St.", GAMES=0, W=0, L=0, CHAMP=0) |> 
#   add_row(TEAM="Samford", GAMES=0, W=0, L=0, CHAMP=0) |> 
#   add_row(TEAM="Grambling St.", GAMES=0, W=0, L=0, CHAMP=0) |> 
#   add_row(TEAM="Wagner", GAMES=0, W=0, L=0, CHAMP=0) |> 
#   add_row(TEAM="Stetson", GAMES=0, W=0, L=0, CHAMP=0) |> 
#   pivot_longer(
#     cols  = -c(TEAM,GAMES,CHAMP),
#     names_to  = "type",
#     values_to  ="result"
#   ) |> 
#   mutate(share = result/GAMES) |> 
#   mutate(is_smaller = if_else(row_number() == 1, 0, 1), .by = TEAM) |> 
#   filter(TEAM %in% df_public_teams)  |> 
#   mutate(TEAM=factor(TEAM, levels=df_team_factors)) |> 
#   arrange(TEAM)
# 
# n <- length(unique(df_wl$TEAM)) - 1
# 
# 
# df_wl |> 
#   ggplot(aes(x = result, y = fct_rev(TEAM))) +
#   
#   ## dumbbell segments
#   stat_summary(
#     geom = "linerange", fun.min = min, fun.max = max,
#     linewidth = c(rep(.8, n), 1.2),
#     color = c(rep(grey_base, n), grey_dark)
#   ) +
#   
#   ## dumbbell points
#   ## white point to overplot line endings
#   geom_point(
#     aes(x = result), size = 6, shape = 21, stroke = 1, color = "white", fill = "white"
#   ) +
#   ## semi-transparent point fill
#   geom_point(
#     aes(x = result, fill = type), size = 6, shape = 21, stroke = 1, color = "white", alpha = .75
#   ) +
#   ## point outline
#   geom_point(
#     aes(x = result), size = 6, shape = 21, stroke = 1, color = "white", fill = NA
#   ) +
  
  ## result labels
  # geom_text(
  #   aes(label = result,#scales::percent(result, accuracy = 1, prefix = "    ", suffix = "%    "),
  #       x = result, hjust = ifelse(type=="L",2,-1),color = type),
  #   fontface = c(rep("plain", n*2), rep("bold", 2)),
  #   family = "sans", size = 4.2
  # ) +
  
  ## legend labels
  # annotate(
#   geom = "text", x = c(.18, .60), y = n + 1.8,
#   label = c("matches lost", "matches won"), family = "sans",
#   fontface = "bold", color = pal_base, size = 5, hjust = .5
# ) +
# 
## call-out Eintracht
# geom_richtext(
#   aes(x = .46, y = 3.3, label = callout), stat = "unique",
#   family = "sans", size = 4, lineheight = 1.2,
#   color = grey_base, hjust = 0, vjust = 1.03, fill = NA, label.color = NA
# ) +
# annotate(
#   geom = "curve", x = .51, xend = .43, y = 3.3, yend = 4, curvature = .35, 
#   angle = 60, color = grey_base, linewidth = .4, 
#   arrow = arrow(type = "closed", length = unit(.08, "inches"))
# ) +

# coord_cartesian(clip = "off") +
#   scale_x_continuous(expand = expansion(add = c(.035, .05))) +
#   scale_y_discrete(expand = expansion(add = c(.35, 1))) +
#   scale_color_manual(values = pal_dark) +
#   scale_fill_manual(values = pal_base) +
#   labs(x = element_blank(), y = element_blank()) +
#   theme(axis.text.y = element_text(face = c(rep("plain", n), "bold"), size=14))
