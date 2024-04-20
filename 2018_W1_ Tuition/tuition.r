## Frontmatter ---------------
##
## Script name: 
## Author: Zane Wolf
## Date Created: 
## Description:
##
## Notes:
##   
##
## ___________________________

# HOUSEKEEPING ---------------

# clean up 
rm(list=ls())
cat("\014")  

# packages
library(tidyverse)
library(readxl)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(paletteer)
library(ggforce)
library(formattable)
library(gt)
library(gtExtras)
library(patchwork)# library(janitor)


# GLOBAL SETTINGS ------------

# set global plotting theme
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))

theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(.5, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey15"),
  plot.title= element_text(size = 18, color="grey15",face = 'bold', margin = margin(b=5)),
  plot.subtitle = element_text(size = 12, color  ='grey15', margin = margin(b=15)),
  plot.caption = element_text(size = 9, margin = margin(t=15)),
  axis.text=element_text(size=9,face = 'bold'),
  axis.title=element_text(face="bold",color  ='grey15'),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(25,25,10,25)
)

# global variables

# FUNCTIONS ------------------

range <- data$perc_change |> sort()
domain <- seq(1,4,along.with=range)

scaleLineWidth  <- function(value){
  domain[match(value,range)]
}

# IMPORT DATA ----------------
data <- read_xlsx('us_avg_tuition.xlsx')

# PLOT -----------------------
### Density Plot ----

data <- data |> pivot_longer(!State,names_to="Year",values_to = "Tuition")

ggplot(data, aes(x = Tuition, y = fct_rev(as.factor(Year)), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5,rel_min_height = 0.01) +
  scale_fill_paletteer_c("grDevices::Inferno") +
  labs(title = 'Average US State Tuition Creeps to the Right',
       subtitle  =str_wrap("The distribution of state tuitions have become increasingly flattened, positively skewed, and shifted to the right over time as states have increased their tuition.",75),
       y=element_blank(),
       x="Tuition (USD)") +
  coord_cartesian(clip = "off") + # To avoid cut off
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "#d6ccc2")
    # panel.spacing = unit(0.1, "lines"),
    # strip.text.x = element_text(size = 8)
  )

### Line Plot LARGE ----

data  <- data |> 
  group_by(State) |> 
  mutate(perc_change = (Tuition - lag(Tuition))/lag(Tuition),
         # lineScale = scale(perc_change)[,1],
         year_1=lubridate::ymd(as.numeric(str_split_i(Year,"-",1)),truncated=2L),
         lineWidth = ifelse(is.na(perc_change),1,scaleLineWidth(perc_change)))  |> 
  ungroup()

data |> 
  subset(State %in% c("Alaska","Georgia","California","Massachusetts","Kansas")) |> 
  ggplot(aes(x=year_1,y=Tuition,group=State))+
  geom_point(size=2,color="red")+
  geom_line(aes(linewidth=lineWidth))+
  facet_wrap(~State,ncol=3)

data |> 
  subset(State %in% c("Alaska","Georgia","California","Massachusetts","Kansas")) |> 
  ggplot(aes(x = year_1, y = Tuition)) +
  ggforce::geom_link2(aes(group=State, color = perc_change), size = 4, n = 500, lineend = "round") +
  facet_wrap(~State,ncol=3)+
  scale_color_paletteer_c("ggthemes::Red")

# formattable(data, 
#             align = c("l",rep("r", NCOL(State) - 1)),
#             list(`State` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
#                  `% Change` = color_bar("#FA614B"), 
#                  `Improvement` = percent))

data_table  <- data |> 
  group_by(State) |> 
  mutate(total_change=1+(last(Tuition)-first(Tuition))/first(Tuition),
         tuition_all = list(Tuition),
         first_tuition=round(first(Tuition),2),
         last_tuition=round(last(Tuition),2))  |> 
  select(State,total_change,tuition_all, first_tuition, last_tuition) |>
  distinct() |>
  as_tibble()

data_table |> 
  # arrange(desc(total_change)) |>
  mutate(color = "") |>
  # filter(State %in% c("Hawaii", "Colorado", "Georgia", "Arizona", "Nevada","Iowa", "Missouri", "Montana", "Maryland", "Ohio"))|>
  gt() |> 
  cols_label(
    State = 'State',
    first_tuition = "2004-05",
    last_tuition = "2015-16",
    total_change = "Total \u0394", 
    tuition_all = "Trends",
    color = ""
  ) |> 
  fmt_percent(columns = total_change)  |> 
  fmt_currency(columns = c(first_tuition,last_tuition)) |> 
  cols_width(
    last_tuition ~ px(110),
    total_change ~ px(110)
  ) |>
  gtExtras::gt_plt_sparkline(
    column = tuition_all,
    fig_dim = c(20, 40),
    palette = c("black", "black", "purple", "green", "lightgrey"),
    same_limit = TRUE,
    type="shaded"
    ) |> 
  cols_move_to_start(columns=c(State,first_tuition,last_tuition,total_change,color,tuition_all)) |> 
  data_color(
    columns = total_change,
    target_columns = color,
    method = "numeric",
    palette = "RColorBrewer::OrRd",
    domain = range(data_table$total_change)
  ) |>
  opt_vertical_padding(scale = 0.65) |> 
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    column_labels.font.weight="bold"
  ) |> 
  tab_style(
    style="font-weight:bold;color:#555555;",
    location=cells_body(
      columns=State
    )
  )  |> 
  tab_footnote(
    footnote = "Source: Tidy Tuesday 2018 US State Tuition · Graphic: Zane Wolf",
    locations = NULL
  ) 
  # tab_style(
  #   style="margin-right:25px;",
  #   location=cells_body(
  #     columns=total_change
  #   )
  # ) 
  # 
full_table |> gtsave(filename="C:/Users/Zane/Documents/Projects/Dataviz/01_Ongoing_Projects/TidyTuesday/TT_US_Tuition/TT_US_Tuition/Table.png")


### Line Plot Broken ----
chunk <- 17
n <- nrow(data_table)

r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(data_table,r)

part1 <- d[[1]]

p1 <- d[[1]] |> 
  # arrange(desc(total_change)) |>
  mutate(color = "") |>
  # filter(State %in% c("Hawaii", "Colorado", "Georgia", "Arizona", "Nevada","Iowa", "Missouri", "Montana", "Maryland", "Ohio"))|>
  gt() |> 
  cols_label(
    State = 'State',
    first_tuition = "2004-05",
    last_tuition = "2015-16",
    total_change = "Total \u0394", 
    tuition_all = "Trends",
    color = ""
  ) |> 
  fmt_percent(columns = total_change)  |> 
  fmt_currency(columns = c(first_tuition,last_tuition)) |> 
  cols_width(
    last_tuition ~ px(110),
    total_change ~ px(110)
  ) |>
  gtExtras::gt_plt_sparkline(
    column = tuition_all,
    fig_dim = c(20, 40),
    palette = c("black", "black", "purple", "green", "lightgrey"),
    same_limit = TRUE,
    type="shaded"
  ) |> 
  cols_move_to_start(columns=c(State,first_tuition,last_tuition,total_change,color,tuition_all)) |> 
  data_color(
    columns = total_change,
    target_columns = color,
    method = "numeric",
    palette = "RColorBrewer::OrRd",
    domain = range(data_table$total_change)
  ) |>
  opt_vertical_padding(scale = 0.65) |> 
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    column_labels.font.weight="bold"
  ) |> 
  tab_style(
    style="font-weight:bold;color:#555555;",
    location=cells_body(
      columns=State
    )
  )  |> 
  tab_footnote(
    footnote = "Source: Tidy Tuesday 2018 US State Tuition · Graphic: Zane Wolf",
    locations = NULL
  )

p2 <- d[[2]] |> 
  # arrange(desc(total_change)) |>
  mutate(color = "") |>
  # filter(State %in% c("Hawaii", "Colorado", "Georgia", "Arizona", "Nevada","Iowa", "Missouri", "Montana", "Maryland", "Ohio"))|>
  gt() |> 
  cols_label(
    State = 'State',
    first_tuition = "2004-05",
    last_tuition = "2015-16",
    total_change = "Total \u0394", 
    tuition_all = "Trends",
    color = ""
  ) |> 
  fmt_percent(columns = total_change)  |> 
  fmt_currency(columns = c(first_tuition,last_tuition)) |> 
  cols_width(
    last_tuition ~ px(110),
    total_change ~ px(110)
  ) |>
  gtExtras::gt_plt_sparkline(
    column = tuition_all,
    fig_dim = c(20, 40),
    palette = c("black", "black", "purple", "green", "lightgrey"),
    same_limit = TRUE,
    type="shaded"
  ) |> 
  cols_move_to_start(columns=c(State,first_tuition,last_tuition,total_change,color,tuition_all)) |> 
  data_color(
    columns = total_change,
    target_columns = color,
    method = "numeric",
    palette = "RColorBrewer::OrRd",
    domain = range(data_table$total_change)
  ) |>
  opt_vertical_padding(scale = 0.65) |> 
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    column_labels.font.weight="bold"
  ) |> 
  tab_style(
    style="font-weight:bold;color:#555555;",
    location=cells_body(
      columns=State
    )
  )  |> 
  tab_footnote(
    footnote = "Source: Tidy Tuesday 2018 US State Tuition · Graphic: Zane Wolf",
    locations = NULL
  )


p3 <- d[[3]] |> 
  # arrange(desc(total_change)) |>
  mutate(color = "") |>
  # filter(State %in% c("Hawaii", "Colorado", "Georgia", "Arizona", "Nevada","Iowa", "Missouri", "Montana", "Maryland", "Ohio"))|>
  gt() |> 
  cols_label(
    State = 'State',
    first_tuition = "2004-05",
    last_tuition = "2015-16",
    total_change = "Total \u0394", 
    tuition_all = "Trends",
    color = ""
  ) |> 
  fmt_percent(columns = total_change)  |> 
  fmt_currency(columns = c(first_tuition,last_tuition)) |> 
  cols_width(
    last_tuition ~ px(110),
    total_change ~ px(110)
  ) |>
  gtExtras::gt_plt_sparkline(
    column = tuition_all,
    fig_dim = c(20, 40),
    palette = c("black", "black", "purple", "green", "lightgrey"),
    same_limit = TRUE,
    type="shaded"
  ) |> 
  cols_move_to_start(columns=c(State,first_tuition,last_tuition,total_change,color,tuition_all)) |> 
  data_color(
    columns = total_change,
    target_columns = color,
    method = "numeric",
    palette = "RColorBrewer::OrRd",
    domain = range(data_table$total_change)
  ) |>
  opt_vertical_padding(scale = 0.65) |> 
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    column_labels.font.weight="bold"
  ) |> 
  tab_style(
    style="font-weight:bold;color:#555555;",
    location=cells_body(
      columns=State
    )
  )  |> 
  tab_footnote(
    footnote = "Source: Tidy Tuesday 2018 US State Tuition · Graphic: Zane Wolf",
    locations = NULL
  )

p1 |> gtsave(filename="C:/Users/Zane/Documents/Projects/Dataviz/01_Ongoing_Projects/TidyTuesday/TT_US_Tuition/TT_US_Tuition/Table_p1.png")
p2 |> gtsave(filename="C:/Users/Zane/Documents/Projects/Dataviz/01_Ongoing_Projects/TidyTuesday/TT_US_Tuition/TT_US_Tuition/Table_p2.png")
p3 |> gtsave(filename="C:/Users/Zane/Documents/Projects/Dataviz/01_Ongoing_Projects/TidyTuesday/TT_US_Tuition/TT_US_Tuition/Table_p3.png")
