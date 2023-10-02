# library -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(ggiraph)
library(rmapshaper)
library(cowplot)
library(RColorBrewer)
library(extrafont)
library(lubridate)
library(systemfonts)
reset_font_cache()
library(ggtext)
library(readxl)
library(ggforce)


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
        text = element_text(family = "Calibri"),
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
        text = element_text(family = "Calibri"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Calibri"),
        plot.title.position = "plot")

regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique() %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  rename(mif = `MIF Region`) %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc) ,
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"),
         mif = ifelse(is.na(mif), "TC", mif),
         mif = as.factor(mif),
         mif = fct_relevel(mif, "NW", "NE", "WC", "EC", "SW", "SE", "TC"))

color.ruca <- c("Entirely rural" = "#009933", "Town/rural mix" = "#99CC33", "Urban/town/rural mix" = "#CC9966", "Entirely urban" = "#754C29")

color.pr <- c("Northwest" = "#4575b4","Northeast" = "grey", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59")

color.edr <- c("EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365")

color.pr.edr <- c ("Northwest" = "#4575b4","Northeast" = "#e0f3f8", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59", "Minnesota" = "black", "EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365")

color.six <- c("#009933", "#4575b4", "grey", "#fee090", "#fc8d59", "#d73027")

mn_counties <- st_read("Data/Shapefiles/County shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  rename(countyfp = 4)


# Prep data ---------------------------------------------------------------

master.states <- read_csv("Data/SLEDS/annual-states.csv") 


seven.states <- master.states %>%
  mutate(states = ifelse(county.match == "Meaningful emp - match", 1, 99),
         states = ifelse(edr.match == "Meaningful emp - match" & county.match != "Meaningful emp - match", 2, states),
         states = ifelse(region.match == "Meaningful emp - match" & county.match != "Meaningful emp - match" & edr.match != "Meaningful emp - match", 3, states),
         states = ifelse(state.match == "Meaningful emp - match" & county.match != "Meaningful emp - match" & edr.match != "Meaningful emp - match" & region.match != "Meaningful emp - match", 4, states),
         states = ifelse(state.match == "After 2019", 0, states),
         states = ifelse(ps.attend == "ps.attend" & !(states %in% c(0,1,2,3,4)), 5, states),
         states = ifelse(ps.attend == "ps.not.attend" & state.match == "MN emp record - not meaningful", 6, states),
         states = ifelse(ps.attend == "ps.not.attend" & state.match == "No MN emp record", 7, states),
         states = ifelse(states == 0, "After 2019", states),
         states = ifelse(states == "1", "Meaningful emp County", states),
         states = ifelse(states == "2", "Meaningful emp EDR", states),
         states = ifelse(states == "3", "Meaningful emp SW", states),
         states = ifelse(states == "4", "Meaningful emp MN", states),
         states = ifelse(states == "5", "Attending ps", states),
         states = ifelse(states == "6", "Not meaningful, not attending ps", states),
         states = ifelse(states == "7", "No MN emp record, not attending ps", states),
         states = as.factor(states),
         states = fct_relevel(states, "Meaningful emp County", "Meaningful emp EDR", "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps", "After 2019"),
         five.states = ifelse(states %in% c("Meaningful emp County", "Meaningful emp EDR", "Meaningful emp SW"), "Meaningful emp SW", as.character(states)),
         five.states = str_replace(five.states, "Not meaningful, not attending ps", "Not meaningful"),
         five.states = str_replace(five.states, "No MN emp record, not attending ps", "No MN emp record"),
         five.states = as.factor(five.states),
         five.states = fct_relevel(five.states, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful", "No MN emp record", "After 2019"))

prop.change.states <- seven.states %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.0", "grad.year.1", "grad.year.2", "grad.year.3", "grad.year.4", "grad.year.5", "grad.year.6", "grad.year.7", "grad.year.8", "grad.year.9", "grad.year.10", "grad.year.11")) %>%
  group_by(grad.year, five.states) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(five.states != "After 2019") %>%
  group_by(grad.year) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  group_by(five.states) %>%
  mutate(change.prop = (pct - pct[grad.year == "grad.year.0"]) / pct[grad.year == "grad.year.0"]) %>%
  ungroup() %>%
  mutate(data_id = seq(n())) 

names(prop.states)
# Create chart ------------------------------------------------------------

ggplot(prop.change.states, aes(as.numeric(grad.year)-1, change.prop, color = five.states)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(size = 2) +
  geom_label_repel(data = filter(prop.change.states, five.states != "Attending ps" & grad.year == "grad.year.11"), aes(x = as.numeric(grad.year)-1, y = change.prop, label = paste(five.states, "\n", percent(change.prop, accuracy = .1))), show.legend = FALSE) +
  geom_label_repel(data = filter(prop.change.states, five.states == "Attending ps" & grad.year == "grad.year.9"), aes(x = as.numeric(grad.year)-1, y = change.prop, label = paste(five.states, "\n", percent(change.prop, accuracy = .1))), show.legend = FALSE) +
  labs(x="Grad Year", y = "Proportion of individuals", color="", title = "Percent share by year(s) after graduating high school")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  theme_line +
  scale_color_manual(values = brewer.pal(n = 6, "RdYlBu"),
                    guide = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

ggsave(filename = "Charts/Research team/five-states-proportion-change.pdf", device = cairo_pdf, dpi = "print", width = 12, height = 6)



