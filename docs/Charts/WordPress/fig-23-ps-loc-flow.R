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

# Prep data ---------------------------------------------------------------

original <- read_csv("Data/SLEDS/Masters/After analysis/Master-meaningful-emp-sw.csv") 

ps.grad.location <- read_csv("Data/SLEDS/Masters/Post secondary/Master-post-secondary-location.csv") %>%
  rename(ps.grad = 2,
         ps.state = 7,
         ps.county = 11,
         ps.edr = 14,
         ps.pr = 15)

master <- original %>%
  left_join(ps.grad.location[,c(1,2,7,11,14,15)], by = "PersonID") 

flow <- master %>%
  select(grad.year.5, grad.year.10, ps.grad.InstitutionSector, ps.grad, ps.state, ps.pr) %>%
  mutate(ps.loc = ifelse(ps.pr == "Southwest", "Southwest MN", ps.pr),
         ps.loc = ifelse(!is.na(ps.pr) & ps.pr != "Southwest", "MN - outside Southwest", ps.loc),
         ps.loc = ifelse(ps.grad.InstitutionSector == "Never attended ps", "No college", ps.loc),
         ps.loc = ifelse(ps.grad.InstitutionSector == "Did not grad", "Some college", ps.loc),
         ps.loc = ifelse(ps.state %in% c("SD", "ND", "IA"), "Border State", ps.loc),
         ps.loc = ifelse(is.na(ps.loc), "Far far away", ps.loc)) %>%
  select(grad.year.5, ps.loc) %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(grad.year.5 != "After 2019") %>%
  filter(ps.loc != "Far far away") %>%
  filter(ps.loc != "Multiple") %>%
  group_by(grad.year.5, ps.loc) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(grad.year.5 = str_replace(grad.year.5, "Meaningful emp SW", "Meaningful workforce SW"),
         grad.year.5 = str_replace(grad.year.5, "Meaningful emp MN", "Meaningful workforce MN"),
         grad.year.5 = str_replace(grad.year.5, "No MN emp record, not attending ps", "No MN emp record"),
         grad.year.5 = str_replace(grad.year.5, "Not meaningful, not attending ps", "Not meaningful workforce"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful workforce SW", "Meaningful workforce MN", "Not meaningful workforce", "No MN emp record"),
         ps.loc = fct_relevel(ps.loc, "No college", "Some college", "Southwest MN", "MN - outside Southwest", "Border State", "Far far away"))


# Create chart ------------------------------------------------------------

plot <- ggplot(data = flow, aes(axis1 = ps.loc, axis2 = grad.year.5, y = n)) +
  geom_alluvium(aes(fill = grad.year.5)) +
  geom_stratum(aes(fill = grad.year.5)) +
  geom_text(stat = "stratum", 
            aes(label = paste(after_stat(stratum), "\nN = ", comma(after_stat(count)), sep = "")), size = 3) +
  geom_text(stat = "flow", aes(label = comma(n), color = grad.year.5), nudge_x = -.2, size = 2.5) +
  labs(x="", y = "", color="", title = "Flow of post secondary grad location by workforce\nparticipation", subtitle = "Very few Southwest high school graduates attend a college in\nSouthwest") +
  scale_y_continuous(labels=scales::comma)+
  theme_void() +
  scale_fill_manual(values = c("grey75", "#d73027", "grey75", "grey75")) +
  scale_color_manual(values = c("black", "grey75", "grey75", "grey75")) +
  theme(legend.position = "none",
        text = element_text(size = 14,
                            family = "Avenir"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

i.plot <- girafe(ggobj = plot,
                 options = list(opts_selection(type = "none")))

i.plot

withr::with_dir("Charts/WordPress", saveWidget(i.plot, file="fig-23-ps-loc-flow.html"))
