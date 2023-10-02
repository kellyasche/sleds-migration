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
library(ggsankey)


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



# Prep another dataset ----------------------------------------------------

original <- read_csv("Data/SLEDS/Masters/Master-meaningful-emp-sw.csv") 

ps.location.original <- read_csv("Data/SLEDS/Master-ps-location.csv")

ps.institutionsector <- ps.location.original %>%
  select(grad.year.1, grad.year.5, grad.year.10, InstitutionName, ps.state, ps.planning.region,  InstitutionSector, ps.states) %>%
  mutate(new.InstitutionSector = ifelse(ps.planning.region == "Southwest" & InstitutionSector %in% c(4,5,6,7,8,9), "Southwest, 2-year", InstitutionSector),
         new.InstitutionSector = ifelse(ps.planning.region == "Southwest" & InstitutionSector %in% c("1", "2", "3"), "Southwest, 4-year", new.InstitutionSector),
         new.InstitutionSector = ifelse(ps.planning.region %in% c("Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southeast") & new.InstitutionSector %in% c("4", "5", "6", "7", "8", "9"), "MN, 2-year", new.InstitutionSector),
         new.InstitutionSector = ifelse(ps.planning.region %in% c("Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southeast") & new.InstitutionSector %in% c("1", "2", "3"), "MN, 4-year", new.InstitutionSector),
         new.InstitutionSector = ifelse(is.na(new.InstitutionSector), "Outside MN", new.InstitutionSector))

data <- ps.institutionsector %>%
  select(grad.year.1, grad.year.5, grad.year.10, new.InstitutionSector) 


data.5 <- data %>%
  select(grad.year.5, new.InstitutionSector) %>%
  filter(grad.year.5 != "After 2019") %>%
  filter(grad.year.5 != "Attending ps")

flow.5 <- data.5 %>%
  make_long(new.InstitutionSector, grad.year.5) %>%
  mutate(node = fct_relevel(node, "No MN emp record, not attending ps", "Not meaningful, not attending ps", "Attending ps", "Meaningful emp MN","Meaningful emp SW", "Outside MN", "MN, 4-year", "MN, 2-year", "Southwest, 4-year", "Southwest, 2-year"))

data.10 <- data %>%
  select(grad.year.10, new.InstitutionSector) %>%
  filter(grad.year.10 != "After 2019") %>%
  filter(grad.year.10 != "Attending ps")

flow.10 <- data.10 %>%
  make_long(new.InstitutionSector, grad.year.10) %>%
  mutate(node = fct_relevel(node, "No MN emp record, not attending ps", "Not meaningful, not attending ps", "Attending ps", "Meaningful emp MN","Meaningful emp SW", "Outside MN", "MN, 4-year", "MN, 2-year", "Southwest, 4-year", "Southwest, 2-year")) 

names(flow)


# Create chart 5 ----------------------------------------------------------

ggplot(flow.5, aes(x = x,
                   next_x = next_x,
                   node = node,
                   next_node = next_node,
                   fill = factor(node),
                   label = factor(node))) +
  geom_sankey(flow.alpha = 0.5,
              node.color = "black",
              show.legend = FALSE) +
  geom_sankey_label() +
  theme_bar +
  labs(x = "", y = "", title = "Flow of post-secondary sector to states of\nemployment", subtitle = "MN 4-year colleges not beneficial to Southwest") +
  scale_fill_manual(values = brewer.pal(n = 9, "RdYlBu")) +
  theme_sankey() +
  theme(legend.position = "none",
        text = element_text(size = 18, family = "Calibri"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

ggsave(filename = "Charts/Research team/ps-institutionsector-emp-states-flow-5.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 6.5)
# Create chart 10 ------------------------------------------------------------

ggplot(flow.10, aes(x = x,
                    next_x = next_x,
                    node = node,
                    next_node = next_node,
                    fill = factor(node),
                    label = factor(node))) +
  geom_sankey(flow.alpha = 0.5,
              node.color = "black",
              show.legend = FALSE) +
  geom_sankey_label() +
  theme_bar +
  labs(x = "", y = "", title = "Flow of post-secondary institutional sectors to states of employment", subtitle = "Significant proportion of students traveling out of Minnesota for college and not returning") +
  scale_fill_manual(values = brewer.pal(n = 9, "RdYlBu")) +
  theme_sankey() +
  theme(legend.position = "none",
        text = element_text(size = 18, family = "Calibri"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

ggsave(filename = "Charts/Research team/ps-institutionsector-emp-states-flow.10.pdf", device = cairo_pdf, dpi = "print", width = 12, height = 6.5)
