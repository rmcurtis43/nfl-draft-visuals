library(tidyverse)
library(ggimage)
library(gghighlight)
library(gganimate)
library(zoo)
library(hrbrthemes)

# data load

conference_info <- readRDS("data/conference_info.RDS")

draft_info_all <- readRDS("data/draft_info_all.RDS")
###############################

draft_info_clean <- draft_info_all %>%
  mutate(Position_tidy = case_when(
    Position %in% c("T", "G", "C") ~ "OL",
    Position %in% c("DE", "DT", "Dt", "De", "E") ~ "DL",
    Position %in% c("RB", "B", "HB") ~ "RB",
    TRUE ~ as.character(Position)
  ))

ex_draft <- draft_info_clean %>%
  left_join(
    team_info %>%
      distinct(College=school, team_logo=logo_light),
    by="College"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name1) %>%
      distinct(College=alt_name1, team_logo_1=logo_light),
    by="College"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name3) %>%
      distinct(College=alt_name3, team_logo_3=logo_light),
    by="College"
  ) %>%
  left_join(
    conference_info %>%
      distinct(College=ncaa_name, conference, url_conference, team_logo_4=logos),
    by="College"
  ) %>%
  ungroup() %>%
  left_join(
    conference_info %>%
      distinct(College=reference_name,conference_1=conference, url_conference_1=url_conference, team_logo_5=logos),
    by="College"
  ) %>%
  mutate(team_logo = case_when(
    !is.na(team_logo) ~ team_logo,
    is.na(team_logo) & !is.na(team_logo_1) ~ team_logo_1,
    is.na(team_logo) &  is.na(team_logo_1) & !is.na(team_logo_3) ~ team_logo_3,
    is.na(team_logo) &  is.na(team_logo_1) & is.na(team_logo_3) & !is.na(team_logo_4) ~ team_logo_4,
    is.na(team_logo) &  is.na(team_logo_1) & is.na(team_logo_3) & is.na(team_logo_4) & !is.na(team_logo_5) ~ team_logo_5,
    TRUE ~ as.character("https://www.freepnglogos.com/uploads/ncaa-png-logo/ncaa-png-logo-0.png")
  )) %>%
  mutate(conference = case_when(
    !is.na(conference) ~ conference,
    is.na(conference) & !is.na(conference_1) ~ conference_1,
    TRUE ~ as.character(NA_character_)
  )) %>%
  mutate(url_conference = case_when(
    !is.na(url_conference) ~ url_conference,
    is.na(url_conference) & !is.na(url_conference_1) ~ url_conference_1,
    TRUE ~ as.character(NA_character_)
  )) %>%
  select(-c(team_logo_1, team_logo_3,team_logo_4, team_logo_5,conference_1, url_conference_1)) %>%
  mutate(team_logo = case_when(
    grepl("Culver", College) ~ paste0("https://d21gd0ap5v1ndt.cloudfront.net/web02/csc/images_web/headerLogo.png"),
    grepl("Louisiana-Lafayette	", College) ~ paste0("https://rebelnation1.s3.amazonaws.com/wp-content/uploads/2017/12/wsi-imageoptim-louisiana-lafayette-ragin-cajuns-logo.png"),
    TRUE ~ as.character(team_logo)
  )) 

combine_team_ranking_year <- ex_draft %>%
  filter(conference %in% c("Big 12", "Big Ten", "Pac-12", "ACC", "SEC")) %>%
  drop_na(Player) %>%
  group_by(Year, College, logo=team_logo) %>%
  summarise(
    conference = first(conference),
    url_conference = first(url_conference),
    count = n(),
    count_first_round = sum(ifelse(Round == 1, 1, 0)),
    average_draft_overall = mean(Player)
  ) %>%
  ungroup() %>%
  filter(!logo == "https://www.freepnglogos.com/uploads/ncaa-png-logo/ncaa-png-logo-0.png") %>%
  ungroup() %>%
  group_by(College) %>%
  arrange(College, Year) %>%
  tidyr::complete(Year = full_seq(Year, period = 1), fill = list(count = 0, count_first_round = 0)) %>%
  mutate(logo = zoo::na.locf0(logo, fromLast = TRUE)) %>%
  mutate(conference = zoo::na.locf0(conference, fromLast = TRUE)) %>%
  mutate(url_conference = zoo::na.locf0(url_conference, fromLast = TRUE)) %>%
  mutate(
    draft_num_cumsum = cumsum(count),
    draft_num_1st_cumsum = cumsum(count_first_round)
  ) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    mean_draft_num_cumsum = mean(draft_num_cumsum),
    mean_draft_num_1st_cumsum = mean(draft_num_1st_cumsum)
  )


overall_draft_year_plot <- combine_team_ranking_year %>%
  ggplot(aes(x=draft_num_cumsum, y=draft_num_1st_cumsum)) +  
  geom_image(aes(image=logo), size=.07) +
  theme_ipsum_rc() 


overall_draft_animate <- overall_draft_year_plot +
  labs(x="Total Players Drafted", y="Total Players Drafted in the 1st Round",
       title="NFL Draft Performance (1936-2021)",
       subtitle="Power 5 Conferences",
       caption="@RyanM_Curtis | data: Scraped from DraftHistory.com") +
  geom_text(aes(x = 450, y = 20, label = as.factor(Year)) , alpha = 0.2,  col = "gray", size = 20) +
  transition_time(Year) 



animate(overall_draft_animate)

anim_save("animations/overall_draft_year.gif")

