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

updated.states.color = c("Meaningful emp SW" = "#d73027", "Meaningful emp MN" = "#fc8d59", "Attending ps" = "#fee090",  "Not meaningful, not attending ps" = "#e0f3f8", "No MN emp record" = "#91bfdb")

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

data <- master %>%
  select(grad.year.5, grad.year.10, ps.grad.InstitutionSector, ps.grad, ps.state, ps.pr) %>%
  mutate(ps.loc = ifelse(ps.pr == "Southwest", "Southwest MN", ps.pr),
         ps.loc = ifelse(!is.na(ps.pr) & ps.pr != "Southwest", "MN - outside Southwest", ps.loc),
         ps.loc = ifelse(ps.grad.InstitutionSector == "Never attended ps", "No college", ps.loc),
         ps.loc = ifelse(ps.grad.InstitutionSector == "Did not grad", "Some college", ps.loc),
         ps.loc = ifelse(ps.state != "MN" & !is.na(ps.state), "Out-of-state", ps.loc)) %>%
  select(grad.year.5, grad.year.10, ps.loc) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:2) %>%
  filter(states != "After 2019") %>%
  filter(states != "Attending ps") %>%
  group_by(grad.year, ps.loc) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.5", "grad.year.10")) %>%
  mutate(grad.year = str_replace(grad.year, "grad.year.5", "5 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.10", "10 years after hs grad"),
         grad.year = fct_relevel(grad.year, "5 years after hs grad", "10 years after hs grad"),
         ps.loc = fct_relevel(ps.loc, "No college", "Some college", "Southwest MN", "MN - outside Southwest", "Out-of-state"),
         data_id = seq(n())) %>%
  filter(grad.year == "5 years after hs grad") %>%
  mutate(higher.prop = ifelse(ps.loc == "Southwest MN", "Higher prop - SW",
                              ifelse(ps.loc == "MN - outside Southwest", "Higher prop - MN, not SW",
                                     ifelse(ps.loc %in% c("No college", "Out-of-state"), "Higher prop - no MN emp record", "No higher prop"))),
         higher.prop = fct_relevel(higher.prop, "Higher prop - SW", "Higher prop - MN, not SW", "Higher prop - no MN emp record", "No higher prop"))


# Create ps loc chart ------------------------------------------------------------

ps.loc.plot <- ggplot(data, aes(ps.loc, prop, fill = higher.prop)) +
  geom_bar_interactive(stat = "identity", aes(data_id = prop, tooltip = paste("Years after graduating hs: ", grad.year, "\nLocation of college graduated from: ", ps.loc, "\nNumber of individuals: ", comma(n), "\nPercent of individuals: ", percent(prop, accuracy = .1), sep = ""))) +
  geom_label(aes(label = percent(prop, accuracy = 1)), show.legend = FALSE, color = "black", size = 4, position = position_dodge(width = .9)) +
  geom_text(aes(y = prop / 2, label = paste("N =\n", comma(n), sep = "")), show.legend = FALSE, size = 3) +
  labs(x="", y = "", title = "Proportion of individuals with meaningful workforce\nparticipation in Southwest by the institution sector", subtitle = "A significantly larger proportion of individuals graduating from a 2-year college\nhave meaningful workforce participation in Southwest, but it isn't the largest\ncontributor\n") +
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, .3)) +
  scale_fill_manual(values = c("#d73027", "#fc8d59", "#91bfdb", "grey75"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 25, hjust = 1))

ps.loc.plot

# Prep highest cred data ---------------------------------------------------------------

data.highest.cred <- master %>%
  select(grad.year.5, grad.year.10, highest.cred.level) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:2) %>%
  filter(states != "After 2019") %>%
  filter(states != "Attending ps") %>%
  mutate(highest.cred.level = str_replace(highest.cred.level, "Bachelor degree", "Bachelor degree or higher"),
         highest.cred.level = str_replace(highest.cred.level, "Master degree or higher", "Bachelor degree or higher"),
         highest.cred.level = str_replace(highest.cred.level, "Associate degree", "Associate degree or less"),
         highest.cred.level = str_replace(highest.cred.level, "Less than Associate Degree", "Associate degree or less")) %>%
  group_by(grad.year, highest.cred.level) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = str_replace(grad.year, "grad.year.5", "5 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.10", "10 years after hs grad"),
         grad.year = fct_relevel(grad.year, "5 years after hs grad", "10 years after hs grad"),
         highest.cred.level = fct_relevel(highest.cred.level, "No college", "Some college", "Associate degree or less", "Bachelor degree or higher")) %>%
  filter(grad.year != "10 years after hs grad") %>%
  mutate(higher.prop = ifelse(highest.cred.level == "Associate degree or less", "Higher prop - SW",
                              ifelse(highest.cred.level == "Bachelor degree or higher", "Higher prop - MN, not SW",
                                     ifelse(highest.cred.level %in% c("No college", "Out-of-state"), "Higher prop - no MN emp record", "No higher prop"))),
         higher.prop = fct_relevel(higher.prop, "Higher prop - SW", "Higher prop - MN, not SW", "Higher prop - no MN emp record", "No higher prop"),
         data_id = seq(n()))



# Create highest cred chart ------------------------------------------------------------
names(data.highest.cred)

plot.highest.cred <- ggplot(data = data.highest.cred, aes(highest.cred.level, prop, fill = higher.prop)) +
  geom_bar_interactive(stat = "identity", position = position_stack(), aes(data_id = prop, tooltip = paste("Years after graduating hs: ", grad.year, "\nHighest credential earned: ", highest.cred.level, "\nNumber of individuals: ", comma(n), "\nPercent of individuals: ", percent(prop, accuracy = .1), sep = ""))) +
  geom_label(aes(label = percent(prop, accuracy = 1)), show.legend = FALSE, color= "black", size = 4) +
  geom_text(aes(y = prop / 2, label = paste("N = ", comma(n), sep = "")), show.legend = FALSE, size = 3) +
  labs(x="", y = "", title = "Proportion of individuals with meaningful workforce participation in\nSouthwest highest credential earned", subtitle = "A signifincantly larger proportion of individuals with an associate degree or less have\nmeaningful worforce participation in Southwest") +
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, .3)) +
  scale_fill_manual(values = c("#d73027", "#fc8d59", "#91bfdb", "grey75"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1))

plot.highest.cred



# Prep ps type data -------------------------------------------------------

meaningful.emp.sw.ps.recode <- master %>%
  select(grad.year.1, grad.year.5, grad.year.10, ps.grad.InstitutionSector) %>%
  mutate(ps.grad.InstitutionSector = as.factor(ps.grad.InstitutionSector),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector %in% c("1", "2", "3"), "4-year", as.character(ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector %in% c("4", "11"), "2-year", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector == "10", "Multiple", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector %in% c("Never attended ps"), "No college", as.character(new.ps.grad.InstitutionSector)),
         new.ps.grad.InstitutionSector = str_replace(new.ps.grad.InstitutionSector, "Did not grad", "Some college"),
         new.ps.grad.InstitutionSector = fct_relevel(new.ps.grad.InstitutionSector, "No college", "Some college", "2-year", "4-year", "Multiple"))


data.sector <- meaningful.emp.sw.ps.recode %>%
  select(grad.year.5, grad.year.10, new.ps.grad.InstitutionSector) %>%
  pivot_longer(names_to = "grad.year", values_to = "states", 1:2) %>%
  filter(states != "After 2019") %>%
  filter(states != "Attending ps") %>%
  group_by(grad.year, new.ps.grad.InstitutionSector) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = fct_relevel(grad.year, "grad.year.5", "grad.year.10")) %>%
  mutate(grad.year = str_replace(grad.year, "grad.year.5", "5 years after hs grad"),
         grad.year = str_replace(grad.year, "grad.year.10", "10 years after hs grad"),
         grad.year = fct_relevel(grad.year, "5 years after hs grad", "10 years after hs grad")) %>%
  filter(grad.year != "10 years after hs grad") %>%
  filter(new.ps.grad.InstitutionSector != "Multiple") %>%
  mutate(higher.prop = ifelse(new.ps.grad.InstitutionSector == "2-year", "Higher prop - SW",
                              ifelse(new.ps.grad.InstitutionSector == "4-year", "Higher prop - MN, not SW",
                                     ifelse(new.ps.grad.InstitutionSector %in% c("No college", "Out-of-state"), "Higher prop - no MN emp record", "No higher prop"))),
         higher.prop = fct_relevel(higher.prop, "Higher prop - SW", "Higher prop - MN, not SW", "Higher prop - no MN emp record", "No higher prop"),
         data_id = seq(n()))




# Create ps type chart ------------------------------------------------------------
names(data.sector)

plot.sector <- ggplot(data = data.sector, aes(new.ps.grad.InstitutionSector, prop, fill = higher.prop)) +
  geom_bar_interactive(stat = "identity", position = position_stack(), aes(data_id = prop, tooltip = paste("Years after graduating hs: ", grad.year, "\nType of college graduated from: ", new.ps.grad.InstitutionSector, "\nNumber of individuals: ", comma(n), "\nPercent of individuals: ", percent(prop, accuracy = .1), sep = ""))) +
  geom_label(aes(label = percent(prop, accuracy = 1)), show.legend = FALSE, size = 4, color = "black") +
  geom_text(aes(y = prop / 2, label = paste("N = ", comma(n), sep = "")), show.legend = FALSE, size = 3) +
  labs(x="", y = "", title = "Proportion of individuals with meaningful employment in Southwest\nby the institution sector from which they graduated", subtitle = "A significantly larger proportion of individuals graduating from a 2-year college have\nmeaningful workforce participation in Southwest\n") +
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, .3)) +
  scale_fill_manual(values = c("#d73027", "#fc8d59", "#91bfdb", "grey75"),
                    guide = guide_legend(ncol = 2)) +
  theme_bar+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 15, hjust = 1))

plot.sector
# Cowplot -----------------------------------------------------------------

plot.row.1.2 <- plot_grid(ps.loc.plot + theme(legend.position = "none",
                                              plot.title = element_blank(),
                                              plot.subtitle = element_blank()), plot.highest.cred + theme(legend.position = "none",
                                                                                                          plot.title = element_blank(),
                                                                                                          plot.subtitle = element_blank()))

plot.row.3.4 <- plot_grid(NULL, plot.sector + theme(legend.position = "none",
                                                    plot.title = element_blank(),
                                                    plot.subtitle = element_blank()), NULL, rel_widths = c(.25,.5,.25),
                          ncol = 3)

legend <- get_legend(ps.loc.plot)

title <- ggdraw() +
  draw_label("Proportion of SW high school grads by college location, highest\ncredential earned, and type of college",
             fontface = "bold",
             x = 0,
             hjust = 0,
             fontfamily = "Avenir",
             size = 14) +
  draw_label("The categories with the highest proportions of individuals with meaningful workforce participation in\nSouthwest have the lowest numbers",
             x = 0,
             y = .22,
             hjust = 0,
             fontfamily = "Avenir",
             size = 10) +
  theme(plot.margin = margin(-35,0,0,0))

plot <- plot_grid(title, plot.row.1.2, plot.row.3.4,legend,
          ncol = 1,
          rel_heights = c(0.5, 1, 1, .25))

i.plot <- girafe(ggobj = plot,
                 options = list(opts_selection(type = "none")))

i.plot

withr::with_dir("Charts/WordPress", saveWidget(i.plot, file="fig-8-ps-loc-highest-cred-ps-type-n.html"))
