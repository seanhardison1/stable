## code to prepare `va_demersal_fish` dataset goes here

va_demersal_fish <- read.csv(file = here::here('data/va_demersal_fish.csv')) %>% 
  dplyr::select(-metacomm) %>% 
  dplyr::rename(population = grp,
                biomass_est = est)
usethis::use_data(va_demersal_fish, overwrite = TRUE)
