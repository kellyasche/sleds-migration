library(tidyverse)


# prep no mn emp data -----------------------------------------------------

original <- read_csv("Data/SLEDS/Masters/After analysis/Master-meaningful-emp-sw.csv") 

ps.grad.location <- read_csv("Data/SLEDS/Masters/Post secondary/Master-post-secondary-location.csv") %>%
  rename(ps.grad = 2,
         ps.state = 7,
         ps.county = 11,
         ps.edr = 14,
         ps.pr = 15)

master <- original %>%
  left_join(ps.grad.location[,c(1,2,7,11,14,15)], by = "PersonID") 

no.mn.emp <- master %>%
  select(grad.year.5) %>%
  filter(grad.year.5 == "No MN emp record, not attending ps") %>%
  summarize(n = n())


# Subtract not participating in labor force

lf.original <- read_csv("Data/Labor force/master-lf.csv") %>%
  mutate_at(c("age", "Name", "edr"), factor)

avg.no.lf <- lf.original %>%
  filter(age %in% c("20 to 24", "25 to 34")) %>%
  mutate(not.lf.rate = 1 - lf.rate) %>%
  summarize(avg.not.lf.rate = mean(not.lf.rate))

avg.no.lf$avg.not.lf.rate * no.mn.emp$n

no.mn.emp$n - (avg.no.lf$avg.not.lf.rate * no.mn.emp$n)


names(avg.no.lf)
levels(avg.no.lf$age)

grad.year.5.n <- master %>%
  select(grad.year.5) %>%
  filter(grad.year.5 != "After 2019") %>%
  filter(grad.year.5 != "Attending ps")

4000 / 18490
