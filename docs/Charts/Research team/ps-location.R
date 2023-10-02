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


# Prep county boundary ----------------------------------------------------

us_county_boundaries <- st_read("Data/Shapefiles/US county shapefiles/cb_2018_us_county_500k.shp", quiet = TRUE)


# Prep post-secondary location data ---------------------------------------

states.original <- read_csv("Data/SLEDS/Masters/Master-meaningful-emp-sw.csv")

post.sec.original <- read_csv("Data/SLEDS/Masters/Master-post-secondary-location.csv") %>%
  select(PersonID, InstitutionName, InstitutionSector, City, State, FIPS, countyfp, Name, Dem_Desc, edr, planning.region) %>%
  rename(ps.city = City,
         ps.state = State,
         ps.state.fps = FIPS,
         ps.countyfp = countyfp,
         ps.county = Name,
         ps.Dem_Desc = Dem_Desc,
         ps.edr = edr,
         ps.planning.region = planning.region) %>%
  mutate(ps.state.fps = formatC(ps.state.fps, width = 2, flag = "0"))

states.ps.location <- states.original %>%
  left_join(post.sec.original, by = "PersonID") %>%
  mutate(geoid = paste(ps.state.fps, ps.countyfp, sep = ""))

ps.grad.location <- states.ps.location %>%
  filter(!is.na(InstitutionName)) %>%
  mutate(ps.states = ifelse(ps.planning.region == "Southwest", "Southwest MN", ps.planning.region),
         ps.states = ifelse(ps.states %in% c("Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southeast"), "MN - outside Southwest", ps.states),
         ps.states = ifelse(ps.state %in% c("SD", "ND", "IA"), "Border states", ps.states),
         ps.states = ifelse(is.na(ps.states), "Far far away", ps.states),
         ps.states = fct_relevel(ps.states, "Southwest MN", "MN - outside Southwest", "Border states", "Far far away"))

ps.grad.location.prop <- ps.grad.location %>%
  select(grad.year.1, grad.year.5, grad.year.10, ps.states) %>%
  pivot_longer(names_to = "grad.year", values_to = "emp.states", 1:3) %>%
  filter(emp.states != "After 2019") %>%
  group_by(ps.states, grad.year, emp.states) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year, ps.states) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.1", "grad.year.5", "grad.year.10")) %>%
  group_by(grad.year, ps.states) %>%
  arrange(desc(emp.states)) %>%
  mutate(yloc = ((1 - sum(prop[emp.states != "Meaningful emp SW"])) / 2) + sum(prop[emp.states != "Meaningful emp SW"]),
         cumsum = cumsum(prop),
         yloc.2 = ((cumsum - lag(cumsum)) / 2) + lag(cumsum),
         yloc.2 = ifelse(is.na(yloc.2), cumsum/2, yloc.2)) %>%
  ungroup() %>%
  mutate(emp.states = fct_relevel(emp.states, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"))

data <- ps.grad.location.prop

write_csv(ps.grad.location, "Data/SLEDS/Master-ps-location.csv")


# Create chart ------------------------------------------------------------

names(ps.grad.location.prop)

ggplot(data = filter(data, grad.year %in% c("grad.year.5", "grad.year.10")), aes(ps.states, prop, fill = emp.states)) +
  facet_wrap(~grad.year) +
  geom_bar(stat = "identity", position = position_stack()) +
  labs(x="Post-secondary location", y = "", title = "Proportion of post-secondary location by employment state", subtitle = "Attending post-secondary outside of Minnesota increases likeliness of having no MN employment record") +
  geom_label(data = filter(data, grad.year %in% c("grad.year.5", "grad.year.10"), ps.states == "Southwest MN", emp.states == "Meaningful emp SW"), aes(y = yloc, label = "Significantly\nlarger"), show.legend = FALSE) +
  geom_label(data = filter(data, grad.year %in% c("grad.year.10"), ps.states %in% c("Border states", "Far far away"), emp.states == "No MN emp record, not attending ps"), aes(y = yloc.2, label = "Significantly\nlarger"), show.legend = FALSE) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = brewer.pal(n = 7, "RdYlBu"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(filename = "Charts/Research team/ps-location.pdf", device = cairo_pdf, dpi = "print", width = 12, height = 6.5)
