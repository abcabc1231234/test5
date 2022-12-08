# Subset flipper lengths and body mass data, and remove missing data

remove_empty_flipperlength_bodymass <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm), !is.na(body_mass_g)) %>%
    select(species, flipper_length_mm, body_mass_g)
}