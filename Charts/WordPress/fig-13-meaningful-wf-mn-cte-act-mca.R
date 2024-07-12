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

# Prep ps loc data ---------------------------------------------------------------

original <- read_csv("Data/SLEDS/Masters/After analysis/Master-meaningful-emp-sw.csv") 

ps.grad.location <- read_csv("Data/SLEDS/Masters/Post secondary/Master-post-secondary-location.csv") %>%
  rename(ps.grad = 2,
         ps.state = 7,
         ps.county = 11,
         ps.edr = 14,
         ps.pr = 15)

master <- original %>%
  left_join(ps.grad.location[,c(1,2,7,11,14,15)], by = "PersonID") 


# Prep CTE n data ---------------------------------------------------------

data <- master %>%
  select(grad.year.1, grad.year.5, grad.year.10, total.cte.courses.taken) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:3) %>%
  select(grad.year, states, total.cte.courses.taken) %>%
  filter(states != "After 2019") %>%
  filter(states != "Attending ps") %>%
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
  mutate(grad.year = str_replace(grad.year, "grad.year.5", "5 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.10", "10 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.1", "1 year after hs grad"),
         grad.year = fct_relevel(grad.year, "1 year after hs grad", "5 years after hs grad", "10 years after hs grad"),
         states = str_replace(states, "No MN emp record, not attending ps", "No MN emp record"),
         states = str_replace(states, "Meaningful emp SW", "Meaningful WF - SW"),
         states = str_replace(states, "Meaningful emp MN", "Meaningful WF - MN"),
         states = fct_relevel(states, "Meaningful WF - SW", "Meaningful WF - MN", "Not meaningful, not attending ps", "No MN emp record")) %>%
  filter(grad.year == "5 years after hs grad") %>%
  filter(states != "Not meaningful, not attending ps") %>%
  mutate(fill = ifelse(states == "Meaningful WF - MN", "Yes", "No"))


# Create CTE N chart ------------------------------------------------------

cte.n.plot <- ggplot(data, aes(states, total.cte.courses.taken, fill = states)) +
  geom_boxplot(position = position_dodge(width = .7)) +
  stat_summary(fun.y = "mean", geom = "point", color = "#1a9850", position = position_dodge(width = .7), size = 3, label = mean, show.legend = FALSE) +
  labs(x="", y = "", color="", title = "Boxplots of number of CTE courses taken by state", subtitle = "Meaningful employment in Southwest typically associated with higher number of CTE\ncourses taken")+
  scale_y_continuous(labels=scales::comma)+
  scale_fill_manual(values = c("grey75", "#fc8d59", "grey75"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "none",
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 25, hjust = 1))

cte.n.plot


# Prep CTE achievement data -----------------------------------------------

data <- master %>%
  select(grad.year.1, grad.year.5, grad.year.10, cte.achievement) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:3) %>%
  filter(states != "After 2019") %>%
  filter(states != "Attending ps") %>%
  group_by(grad.year, cte.achievement, states) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year, cte.achievement) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.1", "grad.year.5", "grad.year.10")) %>%
  group_by(grad.year, cte.achievement) %>%
  arrange(desc(states)) %>%
  mutate(yloc = ((1 - sum(prop[states != "Meaningful emp SW"])) / 2) + sum(prop[states != "Meaningful emp SW"]),
         cumsum = cumsum(prop),
         yloc.2 = ((cumsum - lag(cumsum)) / 2) + lag(cumsum)) %>%
  ungroup() %>%
  mutate(grad.year = str_replace(grad.year, "grad.year.10", "10 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.5", "5 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.1", "1 year after hs grad"),
         grad.year = fct_relevel(grad.year, "1 year after hs grad", "5 years after hs grad", "10 years after hs grad"),
         states = fct_relevel(states, "Meaningful emp SW", "Meaningful emp MN", "Not meaningful, not attending ps", "No MN emp record, not attending ps")) %>%
  filter(states == "Meaningful emp MN") %>%
  mutate(cte.achievement = str_replace(cte.achievement, "CTE Concentrator or Completor", "CTE Conc/Comp"),
         cte.achievement = fct_relevel(cte.achievement, "No CTE", "CTE Participant", "CTE Conc/Comp")) %>%
  filter(grad.year == "5 years after hs grad") 


# Create CTE achievement plot ---------------------------------------------


cte.achievement.plot <- ggplot(data = data, aes(cte.achievement, prop)) +
  geom_bar(stat = "identity", position = position_stack(), fill = ifelse(data$cte.achievement %in% c("No CTE", "CTE Participant"), "#fc8d59", "grey75")) +
  geom_label(aes(label = percent(prop, accuracy = .1)), show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x="", y = "", title = "Proportion of CTE Achievement that have meaningful workforce\nparticipation in Southwest", subtitle = "CTE achievement is associated with higher levels of meaningful employment in\nSouthwest") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = brewer.pal(n = 6, "RdYlBu"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 25, hjust = 1))

cte.achievement.plot


# Prep ACT data -----------------------------------------------------------

data <- master %>%
  select(grad.year.1, grad.year.5, grad.year.10, took.ACT) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:3) %>%
  filter(states != "After 2019") %>%
  filter(states != "Attending ps") %>%
  group_by(grad.year, took.ACT, states) %>%
  summarize(n = n()) %>%
  ungroup()  %>%
  group_by(grad.year, took.ACT) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.1", "grad.year.5", "grad.year.10")) %>%
  group_by(grad.year, took.ACT) %>%
  arrange(desc(states)) %>%
  mutate(yloc = ((1 - sum(prop[states != "Meaningful emp SW"])) / 2) + sum(prop[states != "Meaningful emp SW"]),
         cumsum = cumsum(prop),
         yloc.2 = ((cumsum - lag(cumsum)) / 2) + lag(cumsum)) %>%
  ungroup() %>%
  mutate(grad.year = str_replace(grad.year, "grad.year.10", "10 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.5", "5 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.1", "1 year after hs grad"),
         grad.year = fct_relevel(grad.year, "1 year after grad", "5 years after grad", "10 years after grad"),
         states = fct_relevel(states, "Meaningful emp SW", "Meaningful emp MN", "Not meaningful, not attending ps", "No MN emp record, not attending ps")) %>%
  filter(states == "Meaningful emp MN") %>%
  filter(grad.year != "1 year after grad") %>%
  mutate(took.ACT = ifelse(took.ACT == "No", "Did not\ntake ACT", "Took ACT")) %>%
  filter(grad.year == "5 years after hs grad") 


# Create ACT chart --------------------------------------------------------
names(data)

act.plot <- ggplot(data = data, aes(took.ACT, prop)) +
  geom_bar_interactive(stat = "identity", position = position_stack(), fill = ifelse(data$took.ACT == "Took ACT", "#fc8d59", "grey75"), aes(data_id = n, tooltip = paste("Years after high school: ", grad.year, "\nWorkforce participation state: ", states, "\n", took.ACT, "\nNumber of individuals: ", comma(n), "\nProportion of individuals: ", percent(prop, accuracy = .1), sep = ""))) +
  geom_label(aes(label = percent(prop, accuracy = 1)), show.legend = FALSE, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  labs(x="", y = "", title = "Proportion of individuals who did and did not take ACT that have\nmeaningful workforce participation in Southwest", subtitle = "Not taking the ACT was associated with higher proportions of individuals having\nmeaningful workforce participation in Southwest\n") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = brewer.pal(n = 6, "RdYlBu"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "none") 

act.plot


# Prep MCA scores ---------------------------------------------------------

data <- master %>%
  select(grad.year.5, MCA.R, MCA.M, MCA.S) %>%
  pivot_longer(names_to = "mca", values_to = "score", 2:4) %>%
  filter(grad.year.5 != "After 2019") %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(!is.na(score)) %>%
  group_by(grad.year.5, mca, score) %>%
  summarize(n = n()) %>%
  ungroup()  %>%
  group_by(mca, score) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(grad.year.5 == "Meaningful emp MN") 


# Create MCA plot ---------------------------------------------------------
names(data)

mca.plot <- ggplot(data = data, aes(mca, prop, fill = as.factor(score), group = as.factor(score))) +
  geom_bar_interactive(stat = "identity", position = position_dodge(), aes(data_id = n, tooltip = paste("Years after high school: ", "5 years after high school", "\nWorkforce participation state: ", grad.year.5, "\nMCA test: ", mca, "\nMCA score: ", score, "\nNumber of individuals: ", comma(n), "\nPercentage of individuals: ", percent(prop, accuracy = .1), sep = ""))) +
  geom_label(position = position_dodge(width = .9), aes(label = percent(prop, accuracy = 1)), show.legend = FALSE, color = "white") +
  geom_hline(yintercept = 0, color = "black") +
  labs(x="", y = "", title = "Proportion of individuals within each MCA - reading test score that\nhave meaningful workforce participation in Southwest", subtitle = "A higher proportion of individuals with lower test scores have meaningful workforce\nparticipation in Southwest\n") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("grey55", "grey75", "#fdae61", "#f46d43")) +
  theme_bar+
  theme(legend.position = "bottom") 

mca.plot


# Combine all the damn charts ---------------------------------------------


plot.row.3.4 <- plot_grid(act.plot + theme(legend.position = "none",
                                           plot.title = element_blank(),
                                           plot.subtitle = element_blank()), 
                          mca.plot + theme(plot.title = element_blank(),
                                           plot.subtitle = element_blank()))

title <- ggdraw() +
  draw_label("Proportion of individuals with meaningful workforce participation\noutside SW but still in MN by ACT, and MCA scores",
             fontface = "bold",
             x = 0,
             hjust = 0,
             fontfamily = "Avenir",
             size = 14) +
  draw_label("A higher proportion of individuals that engage a bit in CTE, take the ACT, and perform well on\nMCA test have meaningful workforce participation in SW",
             x = 0,
             y = .22,
             hjust = 0,
             fontfamily = "Avenir",
             size = 10) +
  theme(plot.margin = margin(-35,0,0,0))

plot <- plot_grid(title, plot.row.3.4,
          ncol = 1,
          rel_heights = c(0.3, 1, 1))

i.plot <- girafe(ggobj = plot,
                 options = list(opts_selection(type = "none")))

i.plot

withr::with_dir("Charts/WordPress", saveWidget(i.plot, file="fig-13-meaningful-wf-mn-cte-act-mca.html"))
