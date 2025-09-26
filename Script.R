# load packages
library(readxl)
library(dplyr)
library(tidyr)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(stringr)
library(countrycode)

# load dataset
setwd("C:/Users/yenul/Downloads/ACTL4305")
Freely_quote_data <- read_excel("Freely_quote_data.xlsx", sheet = "Quotes")
data <- Freely_quote_data

data <- data %>%
  mutate(quote_id = row_number()) %>%
  relocate(quote_id)

# check for missing data
print(sapply(data, function(x) sum(is.na(x))))

# reduce cardinality
# load cities
data(world.cities)

city_lookup <- world.cities %>%
  mutate(
    city_clean = str_to_lower(name),
    country_clean = str_to_lower(country.etc)
  ) %>%
  select(city_clean, country_clean) %>%
  distinct()

data_long <- data %>%
  separate_rows(destinations, sep = ";") %>%
  mutate(destinations = str_trim(destinations)) %>%
  mutate(destination_clean = str_to_lower(destinations) %>%
           str_trim() %>%
           str_replace("^usa$|^united states$|^america$|^all of north america$|^hawaii$", "united states") %>%
           str_replace(".*\\buk\\b.*|.*united kingdom.*|.*england.*|.*scotland.*|.*wales.*", "united kingdom") %>%
           str_replace("^all of europe$|^europe$", "europe_general") %>%
           str_replace("^all of the pacific$|^pacific$", "oceania_general") %>%
           str_replace("^all of africa$", "africa_general") %>%
           str_replace("^south america$|^all of south america$", "south_america_general") %>%
           str_replace("^all of asia$", "asia_general") %>%
           str_replace("^bali$", "indonesia")) %>%
  mutate(region = countrycode(destination_clean, "country.name", "region")) %>%
  left_join(city_lookup, by = c("destination_clean" = "city_clean")) %>%
  mutate(
    # If region is NA but we matched a city, use the city’s country
    region = if_else(is.na(region) & !is.na(country_clean),
                     countrycode(country_clean, "country.name", "region"),
                     region)) %>%
  mutate(region = case_when(
    str_detect(destination_clean, "europe_general") ~ "Europe & Central Asia",
    str_detect(destination_clean, "africa_general") ~ "Sub-Saharan Africa",
    str_detect(destination_clean, "oceania_general") ~ "East Asia & Pacific",
    str_detect(destination_clean, "asia_general") ~ "East Asia",
    str_detect(destination_clean, "south_america_general") ~ "Latin America",
    is.na(region) ~ "Other/Unknown",
    TRUE ~ region),
    
    has_cruise = str_detect(str_to_lower(destinations),"\\bcruis\\w*\\b"),
    is_worldwide = str_detect(str_to_lower(destinations),"\\bworld\\w*\\b"),

    region_simple = case_when(
      has_cruise ~ "cruise",
      is_worldwide ~ "worldwide",
      str_detect(region, "Europe") ~ "europe",
      destination_clean %in% c("australia", "new zealand", "fiji", "samoa", "tonga", 
                               "vanuatu", "papua new guinea", "solomon islands", 
                               "cook islands", "tahiti", "french polynesia") ~ "oceania",
      str_detect(region, "Asia") & str_detect(region, "East") ~ "east_asia",
      str_detect(region, "Asia") & str_detect(region, "South") ~ "south_asia",
      str_detect(region, "Middle East") ~ "middle_east",
      str_detect(region, "Africa") ~ "africa",
      str_detect(region, "America") & str_detect(region, "North") ~ "north_america",
      str_detect(region, "America") & str_detect(region, "Latin") ~ "south_america",
      region == "East Asia & Pacific" ~ "oceania",
      TRUE ~ "other")) %>%
  group_by(quote_id) %>%
  summarise(
    europe = as.numeric(any(region_simple == "europe")),
    east_asia = as.numeric(any(region_simple == "east_asia")), 
    south_asia = as.numeric(any(region_simple == "south_asia")),
    middle_east = as.numeric(any(region_simple == "middle_east")),
    africa = as.numeric(any(region_simple == "africa")),
    north_america = as.numeric(any(region_simple == "north_america")),
    south_america = as.numeric(any(region_simple == "south_america")),
    oceania = as.numeric(any(region_simple == "oceania")),
    other_region = as.numeric(any(region_simple == "other")),
    cruise = as.numeric(any(has_cruise)),
    .groups = "drop")

data <- data %>%
  left_join(data_long, by = "quote_id") %>%
  mutate(
    is_all_americas = str_detect(str_to_lower(destinations), "all of the americas"),
    north_america = if_else(is_all_americas, 1L, north_america),
    south_america = if_else(is_all_americas, 1L, south_america),
    other_region = if_else(is_all_americas, 0L, other_region)) %>%
  select(-is_all_americas)


















data(world.cities)
get_country <- function(place_name) {
  if (place_name %in% world.cities$country.etc) {
    return(place_name)
  }
  
  city_match <- world.cities %>% 
    filter(name == place_name) %>%
    slice(1)
  
  if (nrow(city_match) > 0) {
    return(city_match$country.etc)
  } else {
    return(NA)
  }
}

test_df <- data %>%
  separate_rows(destinations, sep = ";\\s*")  %>%
  group_by(destinations) %>%
  summarise(
    total_quotes = n(),
    conversions = sum(convert == "YES")) %>%
  mutate(conv_rate = conversions / total_quotes) %>%
  filter(!grepl("^All of", destinations),
         !destinations %in% c("Worldwide"),
         !grepl("cruise", destinations)) %>%
  rowwise() %>% 
  mutate(country = get_country(destinations)) %>%
  ungroup() %>%
  mutate(country = if_else(destinations == "United States of America", "USA", country),
         country = if_else(destinations == "United Kingdom", "UK", country),
         country = if_else(destinations == "Korea (south)", "South Korea", country))



country_conversions <- data %>%
  separate_rows(destinations, sep = ";\\s*")  %>%
  group_by(destinations) %>%
  filter(!grepl("^All of", destinations),
         !destinations %in% c("Worldwide"),
         !grepl("cruise", destinations)) %>%
  rowwise() %>% 
  mutate(country = get_country(destinations)) %>%
  ungroup() %>%
  mutate(country = if_else(destinations == "USA", "United States of America", country),
         country = if_else(destinations == "UK", "United Kingdom", country),
         country = if_else(destinations == "Korea (south)", "South Korea", country),
         country = if_else(destinations == "Republic of Ireland", "Ireland", country),
         country = if_else(destinations == "England", "United Kingdom", country),
         country = if_else(destinations == "Korea (south) (South Korea)", "South Korea", country),
         country = if_else(destinations == "Hong Kong", "Hong Kong", country)) %>%
  group_by(country) %>%
  summarise(
    total_quotes = n(),
    conversions = sum(convert == "YES")) %>%
  mutate(conv_rate = conversions / total_quotes)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Join your conversion data with world map
world_data <- world %>%
  left_join(country_conversions, by = c("name" = "country"))

# Plot
ggplot(data = world_data) +
  geom_sf(aes(fill = conv_rate)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Conversion Rate by Country",
    fill = "Conv Rate"
  )

region_conversions <- data %>%
  separate_rows(destinations, sep = ";\\s*")  %>%
  group_by(destinations) %>%
  filter(grepl("^All of", destinations),
         !destinations %in% c("Worldwide", "Domestic Cruise"),
         !grepl("cruise", destinations)) %>%
  mutate(continent = case_when(
    grepl("Africa", destinations, ignore.case = TRUE) ~ "Africa",
    grepl("Asia", destinations, ignore.case = TRUE) ~ "Asia",
    grepl("Europe", destinations, ignore.case = TRUE) ~ "Europe",
    grepl("UK", destinations, ignore.case = TRUE) ~ "Europe",
    grepl("North America", destinations, ignore.case = TRUE) ~ "North America",
    grepl("South America", destinations, ignore.case = TRUE) ~ "South America",
    grepl("Central", destinations, ignore.case = TRUE) ~ "South America",
    grepl("Pacific", destinations, ignore.case = TRUE) ~ "Oceania",
    grepl("Middle East", destinations, ignore.case = TRUE) ~ "Middle East")) %>% # excludes "All of the Americas"
  group_by(continent) %>%
  summarise(
    total_quotes = n(),
    conversions = sum(convert == "YES")) %>%
  mutate(conv_rate = conversions / total_quotes)

continental_data <- world %>%
  left_join(region_conversions, by = "continent")

ggplot(continental_data) +
  geom_sf(aes(fill = conv_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Global Conversion Rates by Regional Policies*",
    fill = "Conversion Rate",
    subtitle = "*Excludes 'All of the Americas'"
  )
