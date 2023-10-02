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




meaningful.emp.sw <- read_csv("Data/SLEDS/Masters/Master-meaningful-emp-sw.csv") %>%
  mutate(grad.year.1 = fct_relevel(grad.year.1, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"),
         grad.year.10 = fct_relevel(grad.year.10, "Meaningful emp SW", "Meaningful emp MN", "Attending ps", "Not meaningful, not attending ps", "No MN emp record, not attending ps"))

data <- meaningful.emp.sw %>%
  select(grad.year.1, grad.year.5, grad.year.10, total.cte.courses.taken) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:3) %>%
  select(grad.year, states, total.cte.courses.taken) %>%
  filter(states != "After 2019") %>%
  drop_na() %>%
  group_by(grad.year, states) %>%
  mutate(q1 = quantile(total.cte.courses.taken, .25),
         q3 = quantile(total.cte.courses.taken, .75),
         iqr = IQR(total.cte.courses.taken),
         low.outlier = q1 - 1.5*iqr,
         high.outlier = q3 + 1.5*iqr) %>%
  filter(total.cte.courses.taken > low.outlier,
         total.cte.courses.taken < high.outlier) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.1", "grad.year.5", "grad.year.10"))

# Create chart ------------------------------------------------------------

labels <- data %>%
  group_by(grad.year, states) %>%
  summarize(Min = quantile(total.cte.courses.taken, .0),
            Q1 = quantile(total.cte.courses.taken, .25),
            Median = median(total.cte.courses.taken),
            Q3 = quantile(total.cte.courses.taken, .75),
            Max = quantile(total.cte.courses.taken, 1),
            Mean = mean(total.cte.courses.taken)) %>%
  ungroup()

ggplot(data, aes(grad.year, total.cte.courses.taken, fill = states)) +
  geom_boxplot(position = position_dodge(width = .7)) +
  annotate(geom = "label",
           x = 2,
           y = 25, 
           label = "Consistently higher") +
 annotate(geom = "segment",
          x = 2, 
          xend = .8, 
          y = 23, 
          yend = 11, 
          arrow = arrow(length = unit(.25, "cm"))) +
  annotate(geom = "segment",
           x = 2, 
           xend = 1.8, 
           y = 23, 
           yend = 10, 
           arrow = arrow(length = unit(.25, "cm"))) +
  annotate(geom = "segment",
           x = 2, 
           xend = 2.6, 
           y = 23, 
           yend = 9, 
           arrow = arrow(length = unit(.25, "cm"))) +
  labs(x="", y = "", color="", title = "Boxplots of number of CTE courses taken by state", subtitle = "Meaningful employment in Southwest typically associated with higher number of CTE courses taken")+
  scale_y_continuous(labels=scales::comma,
                     limits = c(0, 30))+
  scale_fill_manual(values = brewer.pal(n = 6, "RdYlBu"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "bottom",
        text = element_text(size = 18))


ggsave(filename = "Charts/Research team/total-cte-courses.pdf", device = cairo_pdf, dpi = "print", width = 12, height = 6.5)
