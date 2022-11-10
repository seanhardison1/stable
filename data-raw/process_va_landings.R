library(tidyverse)
library(tsibble)
library(lubridate)
va_landings <-
  readxl::read_excel(here::here("data/Hardison 2002-2018_all.xlsx"),
                     sheet = 1) %>% 
  dplyr::rename(year = 1,
                month = 2,
                spec_grp = 3,
                species = 4,
                location = 5,
                location_full = 6,
                usd = 7,
                lbs = 8) %>% 
  filter(!is.na(month)) %>%
  mutate(ymon = yearmonth(paste(year, month,"01",sep="-")),
         year = year(ymon),
         month = month(ymon)) %>% 
  mutate(species = ifelse(species == "BASS, STRIPED" & month %in% c(3,5), "striped bass spring",
                          ifelse(species == "BASS, STRIPED" & month %in% c(7,9,11), 
                                 "striped bass fall", species))) %>% 
  group_by(year, species) %>% 
  dplyr::summarise(total_landings = sum(lbs, na.rm = T),
                   total_value = sum(usd, na.rm = T)) %>% 
  mutate(species = ifelse(species == "BLUE CRAB",
                          "CRAB, BLUE",
                          ifelse(species == "WHITING,KING",
                                 "WHITING, KING",
                                 ifelse(species == "CATFISH, NK (UNCLASSIFIED)",
                                        "CATFISH, NK",
                                        ifelse(species == "BLUE CTAFISH",
                                               "CATFISH, BLUE", species))))) %>% 
  mutate(species = str_to_lower(species),
         species = case_when(species == "croaker, atlantic" ~ "Atlantic croaker",
                             species == "crab, blue" ~ "blue crab",
                             species == "crab, horseshoe" ~ "horseshoe crab",
                             # species == "bass, striped" ~ "striped bass",
                             species == "dogfish, smooth" ~ "smooth dogfish",
                             species == "drum, black" ~ "black drum",
                             species == "flounder, summer" ~ "summer flounder",
                             species == "perch, white" ~ "white perch",
                             species == "seatrout, grey" ~ "weakfish",
                             species == "shad, gizzard" ~ "gizzard shad",
                             species == "whiting, king" ~ "kingfishes",
                             species == "ray, cownose" ~ "cownose ray",
                             species == "catfish, blue" ~ "blue catfish",
                             species == "catfish, nk" ~ "channel catfish",
                             TRUE ~ species)) %>% 
  filter(!str_detect(species, "crab")) %>% 
  dplyr::mutate(region = "VA")

write.csv(va_landings, file = here::here("data/va_landings.csv"), row.names = F)
