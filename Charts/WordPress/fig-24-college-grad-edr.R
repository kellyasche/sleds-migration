library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(shiny)
library(shinycssloaders)
library(ggiraph)
library(kableExtra)
library(rmapshaper)
library(cowplot)
library(DT)
library(htmlwidgets)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(systemfonts)
reset_font_cache()
library(ggtext)
library(ggforce)
library(tigris)
library(janitor)
library(ggalluvial)





rm(list = ls())

theme_bar <- theme_bw() +
  theme(panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(1, "lines"),
        legend.margin = margin(0,0,0,0),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        text = element_text(family = "Arial"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(margin = margin(l = 2)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Arial"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Arial"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique() %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc) ,
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))
color.ruca <- c("Entirely rural" = "#009933", "Town/rural mix" = "#99CC33", "Urban/town/rural mix" = "#CC9966", "Entirely urban" = "#754C29", "Minnesota" = "black")

color.pr <- c("Northwest" = "#4575b4","Northeast" = "grey", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59", "Minnesota" = "black")

color.edr <- c("EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365", "Minnesota" = "black")

color.six <- c("#009933", "#4575b4", "grey", "#fee090", "#fc8d59", "#d73027")

mn_counties <- st_read("Data/Shapefiles/county shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE) %>%
  rename(countyfp = FIPS_CODE)

wf.states.color = c("Meaningful workforce SW" = "#d73027", "Meaningful workforce MN" = "#fc8d59", "Attending ps" = "#fee090",  "Not meaningful" = "#e0f3f8", "No MN emp record" = "#91bfdb")

states.color = c("Meaningful emp SW" = "#d73027", "Meaningful emp MN" = "#fc8d59", "Attending ps" = "#fee090",  "Not meaningful, not attending ps" = "#e0f3f8", "No MN emp record, not attending ps" = "#91bfdb")

updated.states.color = c("Meaningful WF - SW" = "#d73027", "Meaningful WF - MN" = "#fc8d59", "Attending ps" = "#fee090",  "Not meaningful, not attending ps" = "#e0f3f8", "No MN emp record" = "#91bfdb")

mn_edr <- st_read("Data/Shapefiles/RDO shapefiles/rdo.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE)  %>%
  select(RDCNUM, geometry) %>%
  rename(edr = 1)

names(mn_edr)

us <- st_read("Data/Shapefiles/US shapefiles/cb_2018_us_state_500k.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE) %>%
  select(STUSPS, geometry) %>%
  rename(edr = 1)

names(us)

geometry = mn_edr %>%
  rbind(us)


# Prep data ---------------------------------------------------------------

original <- read_csv("Data/SLEDS/Masters/Post secondary/Master-post-secondary-location.csv")

names(original)

data <- original %>%
  filter(Graduated == "Y") %>%
  mutate(ps.location = ifelse(State %in% c("SD", "ND", "IA"), State,
                              ifelse(State == "MN", edr, "Far away"))) %>%
  group_by(ps.location) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n),
         ps.location = fct_reorder(ps.location, pct))

data.edr <- data %>%
  filter(!ps.location %in% c("SD", "ND", "IA", "Far away")) %>%
  mutate(edr = str_sub(ps.location, 5,6),
         edr = trimws(edr, which = "both")) %>%
  left_join(mn_edr, by = c("edr"))

data.border.states <- data %>%
  filter(ps.location %in% c("SD", "ND", "IA")) %>%
  left_join(us, by = c("ps.location" = "edr"))

# Create edr chart ------------------------------------------------------------
names(data.edr)

plot.edr <- ggplot(data.edr) +
  geom_sf_interactive(color = "grey85", aes(geometry = geometry, fill = pct, data_id = n, tooltip = paste("EDR location of college(s): ", ps.location, "\nNumber of SW high school grads: ", comma(n), "\nProportion of individuals: ", percent(pct, accuracy = .1), sep = ""))) +
  geom_sf_text(aes(label = percent(pct, accuracy = 1), geometry = geometry), fill = "transparent", color = "black", size = 2.5) +
  scale_fill_fermenter(type = "seq", palette = "PuBu", direction = 1, labels = scales::percent) +
  theme_sf +
  labs(title= "Percent of Southwest high school students by\ncollege graduation EDR location", x ="", y = "", subtitle = "A large percentage of Southwest students graduate from\ncolleges located across the central part of Minnesota.") +
  theme(legend.position = "none")


# Create border state chart -----------------------------------------------
names(data.border.states)

plot.border.state <- ggplot(data.border.states, aes(reorder(ps.location, -pct), pct)) +
  geom_col_interactive(aes(data_id = n, tooltip = paste("State of college location: ", ps.location, "\nNumber of individuals: ", comma(n), "\nPercent of individuals: ", percent(pct, accuracy = .1), sep = ""))) +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "black", size = 4) +
  labs(x="", y = "", title = "Percent of Southwest high schoolers that graduate\nfrom a border state college", subtitle = "Nearly 20% of Southwest high schoolers that graduate college do\nso from a border state campus\n")+
  scale_y_continuous(labels=scales::percent) +
  theme_bar+
  theme(legend.position = "bottom",
        legend.box.margin = margin(-20, 0, 0, 0),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


# Join plots --------------------------------------------------------------

plot.row.1.2 <- plot_grid(plot.edr + theme(legend.position = "none",
                                           plot.title = element_blank(),
                                           plot.subtitle = element_blank()),
                          plot.border.state + theme(legend.position = "none",
                                                    plot.title = element_blank(),
                                                    plot.subtitle = element_blank()))


title <- ggdraw() +
  draw_label("Percent of Southwest high schoolers that graduate college by\ncampus location",
             fontface = "bold",
             x = 0,
             hjust = 0,
             fontfamily = "Avenir",
             size = 14) +
  draw_label("Colleges located in South Dakota, central Minnesota and the twin cities are major draws for\nSouthwest high school students",
             x = 0,
             y = .25,
             hjust = 0,
             fontfamily = "Avenir",
             size = 10) +
  theme(plot.margin = margin(-35,0,0,0))

plot <- plot_grid(title, plot.row.1.2,
          ncol = 1,
          rel_heights = c(0.3, 1))

i.plot <- girafe(ggobj = plot,
                 options = list(opts_selection(type = "none")))

i.plot

withr::with_dir("Charts/WordPress", saveWidget(i.plot, file="fig-24-college-grad-edr.html"))

