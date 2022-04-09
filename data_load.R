library(tidyverse)
library(tictoc)
library(cfbfastR)
library(cfbplotR)
library(combineR)
library(iVoNcaa)


###############################################################

### Team Info #########################

team_info <- cfbfastR::cfbd_team_info() %>%
  select(school, alt_name1, alt_name2, alt_name3, logos) %>%
  mutate(logo_light = map(logos, pluck, 1) %>% reduce(c)) %>%
  mutate(logo_dark = map(logos, pluck, last) %>% reduce(c)) %>%
  mutate(alt_name1 = case_when(
    school == 'Ole Miss' ~ 'Mississippi',
    school == 'Boston College' ~ 'Boston Col.',
    TRUE ~ as.character(alt_name1)
  )) %>%
  mutate(alt_name1 = ifelse(alt_name1 == school, NA, alt_name1)) %>%
  mutate(alt_name2 = ifelse(alt_name2 == school | alt_name2 == alt_name1, NA, alt_name2)) %>%
  mutate(alt_name3 = ifelse(alt_name3 == school | alt_name3 == alt_name2 | alt_name3 == alt_name1, NA, alt_name3))


saveRDS(team_info, "data/team_info.RDS")






###############################################################

### Conference Info #########################

conference_info <- get_ncaa() 

conference_info <- conference_info %>%
  mutate(ncaa_name = str_trim(ncaa_name)) %>%
  mutate(reference_name = str_trim(reference_name)) %>%
  mutate(ncaa_name = case_when(
    name == "Southern California Trojans" ~ paste0("USC"),
    ncaa_name == "Boston College" ~ "Boston Col.",
    ncaa_name == "Southern Miss." ~ "Southern Miss",
    ncaa_name == "Middle Tenn." ~ "Middle Tennessee State",
    ncaa_name == "Louisiana-Monroe" ~ "La-Monroe",
    TRUE ~ as.character(ncaa_name)
  ))

saveRDS(conference_info, "data/conference_info.RDS")
###############################################################

### Draft Info #########################

tic("draft_info_load")
draft_info_all <- data.frame()
for (i in 1936:2021) {
  draft_info_all <- draft_info_all %>%
    bind_rows(
      rvest::read_html(paste0("https://www.drafthistory.com/index.php/years/",i)) %>%
        rvest::html_nodes("table:nth-child(2)") %>%
        rvest::html_table() %>%
        dplyr::first() %>%
        janitor::clean_names() %>%
        slice(-1) %>%
        janitor::row_to_names(1) %>%
        mutate_at(vars(1:3), as.numeric) %>%
        tidyr::fill(Round) %>%
        mutate(Year = i) %>%
        select(Year, everything())
    ) %>%
    arrange(desc(Year), Player)
}
toc()
saveRDS(draft_info_all, "data/draft_info_all.RDS")