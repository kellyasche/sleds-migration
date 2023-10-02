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
library(sjPlot)


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

meaningful.emp.sw <- read_csv("Data/SLEDS/Masters/Master-meaningful-emp-sw.csv") %>%
  mutate(grad.year.1 = fct_relevel(grad.year.1, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"),
         grad.year.10 = fct_relevel(grad.year.10, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"))

meaningful.emp.sw.ps.recode <- meaningful.emp.sw %>%
  select(grad.year.1, grad.year.5, grad.year.10, ps.grad.InstitutionSector) %>%
  mutate(ps.grad.InstitutionSector = as.factor(ps.grad.InstitutionSector),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector %in% c("1", "2", "3"), "4-year, any", as.character(ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector == "11", "2-year, not public", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector == "4", "2-year, public", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector == "10", "Multiple", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector %in% c("Did not grad", "Never attended ps"), "None", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = fct_relevel(new.ps.grad.InstitutionSector, "2-year, public", "2-year, not public", "4-year, any", "Multiple", "None"))



data <- meaningful.emp.sw.ps.recode %>%
  select(grad.year.1, grad.year.5, grad.year.10, new.ps.grad.InstitutionSector) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:3) %>%
  filter(states != "After 2019") %>%
  group_by(grad.year, new.ps.grad.InstitutionSector, states) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year, new.ps.grad.InstitutionSector) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.1", "grad.year.5", "grad.year.10")) %>%
  group_by(grad.year, new.ps.grad.InstitutionSector) %>%
  arrange(desc(states)) %>%
  mutate(yloc = ((1 - sum(prop[states != "Meaningful emp SW"])) / 2) + sum(prop[states != "Meaningful emp SW"]),
         cumsum = cumsum(prop),
         yloc.2 = ((cumsum - lag(cumsum)) / 2) + lag(cumsum)) %>%
  ungroup() 

# Create chart ------------------------------------------------------------
ggplot(data = filter(data, grad.year %in% c("grad.year.5", "grad.year.10")), aes(new.ps.grad.InstitutionSector, prop, fill = states)) +
  facet_wrap(~grad.year) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_label(data = filter(data, grad.year %in% c("grad.year.5", "grad.year.10") & new.ps.grad.InstitutionSector == "2-year, public" & states == "Meaningful emp SW"), aes(y = yloc, label = paste("Largest\nproportion\n", percent(prop, accuracy = 1), sep = "")), show.legend = FALSE, color = "white") +
  geom_label(data = filter(data, grad.year == "grad.year.10"), aes(x = 3.5, y = .2, label = "No MN emp record grows considerably"), fill = "#91bfdb", color = "white") +
  geom_label(data = filter(data, grad.year == "grad.year.10"), aes(x = 3, y = .65, label = "Meaningful emp MN grows considerably"), fill = "#fc8d59", color = "white") +
  labs(x="", y = "", title = "Proportion of college institution sector graduates by state", subtitle = "2-year public colleges help local employment, while having no MN employment record grows") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = brewer.pal(n = 6, "RdYlBu"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(filename = "Charts/Research team/ps-grad-institution-sector.pdf", device = cairo_pdf, dpi = "print", width = 12, height = 6.5)
