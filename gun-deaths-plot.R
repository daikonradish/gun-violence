library("astsa")
library("dplyr")
library("ggplot2")

gunviolence <- read.csv('~/Documents/gun-violence/gun-violence-data.csv') %>%
  mutate(
    incident_date = as.Date(incident_date, "%B %d, %Y"),
    days_since = difftime(incident_date, as.Date('2014-01-01'), units="days") %>% as.numeric,
    months_since = floor((days_since / 365.0) * 12.0) %>% as.integer,
    quarters_since = floor((days_since / 365.0) * 4.0 %>% as.integer)
  ) %>%
  select(-operations)

gundeaths_monthly <- gunviolence %>% 
  mutate(
    days_since = difftime(incident_date, as.Date('2014-01-01'), units="days") %>% as.numeric,
    months_since = floor((days_since / 365.0) * 12.0) %>% as.integer) %>%
  group_by(months_since) %>%
  summarize(
    n_killed = sum(n_killed),
    n_incidents = n()) 

ggplot(gundeaths_monthly, aes(x=months_since, y=n_killed, label=annotation_text)) + 
  geom_line(color='red', alpha=0.8) +
  labs(y="Number of Individuals Killed", x="Months Since Jan 2014") +
  annotate("text", x=29.2, y=99, label="Pulse Nightclub Massacre", face="bold", color="darkblue") +
  annotate("text", x=45.2, y=88, label="Las Vegas Shooting", face="bold", color="darkblue")

sarima(gundeaths_monthly$n_killed, 1, 1, 0)
