### ============================================================================
### 1. load packages
### ============================================================================
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
library(purrr)
library(stringr)
library(lubridate)


### ============================================================================
### 2. load dataset
### ============================================================================
setwd("C:/Users/yenul/Downloads/ACTL4305")
Freely_quote_data <- read_excel("Freely_quote_data.xlsx", sheet = "Quotes")
data <- Freely_quote_data

### ============================================================================
### 3. DATA CLEANING
### ============================================================================
# check for missing data
print(sapply(data, function(x) sum(is.na(x))))

### ============================================================================
### 3.1 CLEAN DATES
### ============================================================================
# --- Helper Function, normalize empty data/markers to NA ----------------------------
na_tokens <- c("", "NA", "N/A", "n/a", "-", "--", "null", "NULL")
norm_na <- function(x) {
  x_chr <- str_trim(as.character(x))
  x_chr[x_chr %in% na_tokens] <- NA_character_
  x_chr
}

# Character normalizer 
norm_chr <- function(x) {
  out <- trimws(as.character(x))
  out[out %in% na_tokens] <- NA_character_
  out
}
is_plausible_excel <- function(n) is.finite(n) & n > 10 & n < 100000  

# --- Helper Function, takes in different date values and turn them into a proper R Date -------------
to_date_robust <- function(x) {
  x_chr <- norm_na(x)
  num    <- suppressWarnings(as.numeric(x_chr))
  is_num <- !is.na(num)
  out    <- rep(as.Date(NA), length(x_chr))
  if (any(is_num)) {
    plausible <- num > 10 & num < 100000
    out[is_num & plausible] <- as.Date(num[is_num & plausible], origin = "1899-12-30")
  }
  to_parse <- which(!is_num & !is.na(x_chr))
  if (length(to_parse)) {
    orders <- c("Ymd","dmY","mdY","dmy","mdy","ymd","d-b-Y","d-b-y","b-d-Y","b d Y","d b Y",
                "Ymd HMS","Ymd HM","Ymd H","dmy HMS","mdy HMS","ymd HMS")
    parsed <- suppressWarnings(parse_date_time(x_chr[to_parse], orders = orders, tz = "UTC"))
    out[to_parse] <- as.Date(parsed)
  }
  out
}

# --- Parser for date times, handles both Excel number dates and text date/time formats -----
to_time_robust <- function(x, tz = "Australia/Sydney") {
  x_chr  <- norm_na(x)
  num    <- suppressWarnings(as.numeric(x_chr))
  is_num <- !is.na(num)
  out    <- rep(as.POSIXct(NA, tz = tz), length(x_chr))
  if (any(is_num)) {
    plausible <- num > 10 & num < 100000
    out[is_num & plausible] <- as.POSIXct(num[is_num & plausible] * 86400,
                                          origin = "1899-12-30", tz = tz)
  }
  to_parse <- which(!is_num & !is.na(x_chr))
  if (length(to_parse)) {
    orders <- c("Ydm HMS","Ydm HM","Ydm H","Ydm","Ymd HMS","Ymd HM","Ymd H","Ymd")
    out[to_parse] <- suppressWarnings(parse_date_time(x_chr[to_parse], orders = orders, tz = tz))
  }
  out
}



### ---------------------- Logical rules for dates ---------------------- ###
tz_use <- "Australia/Sydney"

# date coercer function
as_date_robust <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% na_tokens] <- NA
  out <- rep(as.Date(NA), length(x_chr))
  num <- suppressWarnings(as.numeric(x_chr))
  is_num <- !is.na(num)
  if (any(is_num)) {
    serial <- num[is_num]
    plaus  <- serial > 10 & serial < 100000
    out[is_num][plaus] <- as.Date(serial[plaus], origin = "1899-12-30")
  }
  need <- is.na(out) & !is.na(x_chr)
  if (any(need)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_chr[need],
      orders = c("Ymd","ymd","dmy","mdy","Y-m-d","d-m-Y","m-d-Y"),
      tz = "UTC"
    ))
    out[need] <- as.Date(parsed)
  }
  out
}

as_datetime_robust <- function(x, tz = tz_use) {
  if (inherits(x, "POSIXct")) return(x)
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% na_tokens] <- NA
  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01", tz = tz)
  num <- suppressWarnings(as.numeric(x_chr))
  is_num <- !is.na(num)
  if (any(is_num)) {
    serial <- num[is_num]
    plaus  <- serial > 10 & serial < 100000
    out[is_num][plaus] <- as.POSIXct(serial[plaus] * 86400, origin = "1899-12-30", tz = tz)
  }
  need <- is.na(out) & !is.na(x_chr)
  if (any(need)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_chr[need],
      orders = c(
        "Ymd HMS","Ymd HM","Ymd H","ymd HMS","ymd HM","ymd H",
        "dmy HMS","dmy HM","dmy H","mdy HMS","mdy HM","mdy H",
        "Y-m-d H:M:S","Y-m-d H:M","Y-m-d H","d-m-Y H:M:S","m-d-Y H:M:S",
        "Ymd","ymd","dmy","mdy","Y-m-d","d-m-Y","m-d-Y"
      ),
      tz = tz
    ))
    out[need] <- as.POSIXct(parsed, tz = tz)
  }
  out
}

# dd<->mm swap helpers (only when both ≤ 12 and not equal)
ddmm_swap_possible <- function(d) !is.na(d) & day(d) <= 12 & month(d) <= 12 & day(d) != month(d)
ddmm_swap <- function(d) make_date(year(d), month = day(d), day = month(d))



# ---------------- Swapping and Capping Date Logic ----------------
# 1) Parse quote_create_time, then split & drop 
data <- data %>%
  mutate(quote_id = row_number()) %>%
  relocate(quote_id) %>%
  mutate(quote_create_time = case_when(
    suppressWarnings(!is.na(as.numeric(quote_create_time))) ~
      as_datetime(as.numeric(quote_create_time) * 86400, origin = "1899-12-30"),
    TRUE ~ as_datetime(quote_create_time, format = "%Y-%d-%m %H:%M:%S")),
    quote_date = as.Date(quote_create_time),
    quote_time = format(quote_create_time, "%H:%M:%S")) %>%
  select(-quote_create_time)

# 2) FIX TRIP START/END DATES 
data <- data %>%
  mutate(
    .ts_chr = norm_chr(trip_start_date),
    .te_chr = norm_chr(trip_end_date),
    .ts_num = suppressWarnings(as.numeric(.ts_chr)),
    .te_num = suppressWarnings(as.numeric(.te_chr)),
    trip_start_date = dplyr::case_when(
      !is.na(.ts_num) & is_plausible_excel(.ts_num) ~
        as_date(.ts_num, origin = "1899-12-30"),
      !is.na(suppressWarnings(lubridate::dmy(.ts_chr))) ~
        suppressWarnings(lubridate::dmy(.ts_chr)),
      TRUE ~ suppressWarnings(lubridate::mdy(.ts_chr))
    ),
    trip_end_date = dplyr::case_when(
      !is.na(.te_num) & is_plausible_excel(.te_num) ~
        as_date(.te_num, origin = "1899-12-30"),
      !is.na(suppressWarnings(lubridate::dmy(.te_chr))) ~
        suppressWarnings(lubridate::dmy(.te_chr)),
      TRUE ~ suppressWarnings(lubridate::mdy(.te_chr))
    )
  ) %>%
  select(-.ts_chr, -.te_chr, -.ts_num, -.te_num)

# 3) mm↔dd swap helper 
swap_ddmm <- function(x) {
  make_date(
    year  = year(x),
    day   = month(x),   
    month = day(x)      
  )
}

# 4) Rule 1 swap: if start < quote_date, swap START (only when day <= 12)
data <- data %>%
  mutate(
    trip_start_date_raw = ymd(trip_start_date),
    trip_start_date_swapped = if_else(trip_start_date_raw < quote_date, 1L, 0L),
    trip_start_date = if_else(
      trip_start_date_raw < quote_date,
      swap_ddmm(trip_start_date_raw),
      trip_start_date_raw
    )
  ) %>%
  select(-trip_start_date_raw)

# 5) Rule 2 swaps
data <- data %>%
  mutate(
    trip_start_date_raw = ymd(trip_start_date, quiet = TRUE),
    trip_end_date_raw   = ymd(trip_end_date,   quiet = TRUE),
    
    trip_end_date_swapped = 0L,
    
    # Case 1: if end <= start and end is dd-ambiguous (day <= 12), swap END
    trip_end_date = case_when(
      trip_end_date_raw <= trip_start_date_raw & day(trip_end_date_raw) <= 12 ~
        swap_ddmm(trip_end_date_raw),
      TRUE ~ trip_end_date_raw
    ),
    trip_end_date_swapped = if_else(
      trip_end_date_raw <= trip_start_date_raw & day(trip_end_date_raw) <= 12, 1L, 0L
    ),
    
    # Case 2: still end <= start, swap START (only if day <= 12 and not already swapped)
    trip_start_date = case_when(
      trip_end_date_raw <= trip_start_date_raw &
        day(trip_start_date_raw) <= 12 &
        trip_start_date_swapped == 0L ~ swap_ddmm(trip_start_date_raw),
      TRUE ~ trip_start_date_raw
    ),
    trip_start_date_swapped = if_else(
      trip_end_date_raw <= trip_start_date_raw &
        day(trip_start_date_raw) > 12 &
        trip_start_date_swapped == 0L, 1L, trip_start_date_swapped
    ),
    
    # Case 3: if start == end, swap END if possible AND it makes end > start
    trip_end_date = if_else(
      trip_end_date == trip_start_date &
        day(trip_end_date) <= 12 &
        swap_ddmm(trip_end_date) > trip_start_date,
      swap_ddmm(trip_end_date),
      trip_end_date
    )
  ) %>%
  select(-trip_start_date_raw, -trip_end_date_raw, -trip_end_date_swapped, -trip_start_date_swapped)


# --- Check violations ---
viol_start_after_end <- with(data,
                             !is.na(trip_start_date) & !is.na(trip_end_date) & trip_start_date > trip_end_date
)
viol_quote_after_start <- with(data,
                               !is.na(quote_date) & !is.na(trip_start_date) & quote_date > trip_start_date
)
cat("Rows where departure (start) > return (end):", sum(viol_start_after_end, na.rm = TRUE), "\n")
cat("Rows where quote_date > departure (start):", sum(viol_quote_after_start, na.rm = TRUE), "\n")



# Now apply the original capping rules (Rule 1 then Rule 2), using the potentially swapped dates
## ==================== Rule 1: quote date ≤ departure date ====================
## ==================== Rule 1: quote date ≤ departure date (cap START, not quote) ====================
# Ensure quote_date exists (Date from quote_create_time)
#if (!"quote_date" %in% names(data)) {
#data$quote_date <- as.Date(data$quote_create_time, tz = tz_use)
#}

# Flag violations: quote_date after start date
#data$quote_after_departure <- ifelse(
#is.na(data$quote_date) | is.na(data$trip_start_date),
#NA,
#data$quote_date > data$trip_start_date
#)

#viol_q <- data$quote_after_departure %in% TRUE

# Cap START up to the quote_date where violated (leave quote_* unchanged)
#if (any(viol_q, na.rm = TRUE)) {
# optional audit flag
#if (!"trip_start_capped_to_quote" %in% names(data)) {
#data$trip_start_capped_to_quote <- FALSE
#}
#data$trip_start_capped_to_quote[viol_q] <- TRUE

# perform the cap
#data$trip_start_date[viol_q] <- data$quote_date[viol_q]
#}

# Recompute indicator after capping (should now be FALSE where it was capped)
#data$quote_after_departure <- ifelse(
#is.na(data$quote_date) | is.na(data$trip_start_date),
#NA,
#data$quote_date > data$trip_start_date
#)


## ========== Rule 2: departure (start) ≤ return (end); flag then cap ==========
#data$trip_start_date_after_trip_end_date <- ifelse(
#is.na(data$trip_start_date) | is.na(data$trip_end_date),
#NA,
#data$trip_start_date > data$trip_end_date
#3)

#viol_se_cap <- !is.na(data$trip_start_date) & !is.na(data$trip_end_date) &
#(data$trip_start_date > data$trip_end_date)

#if (any(viol_se_cap, na.rm = TRUE)) {
#data$trip_start_date[viol_se_cap] <- data$trip_end_date[viol_se_cap]
#}



# ===== Rule 3: each boost_i window must lie within [trip_start, trip_end] =====
clamp_boost_window <- function(df, i, trip_start = "trip_start_date", trip_end = "trip_end_date",
                               keep_raw = TRUE) {
  s_col <- paste0("boost_", i, "_start_date")
  e_col <- paste0("boost_", i, "_end_date")
  if (!all(c(s_col, e_col) %in% names(df))) return(df)
  
  # Coerce
  s <- as_date_robust(df[[s_col]])
  e <- as_date_robust(df[[e_col]])
  t0 <- df[[trip_start]]
  t1 <- df[[trip_end]]
  
  # Keep originals once
  if (keep_raw) {
    sr <- paste0(s_col, "_raw"); if (!sr %in% names(df)) df[[sr]] <- s
    er <- paste0(e_col, "_raw"); if (!er %in% names(df)) df[[er]] <- e
  }
  
  # Flags
  df[[paste0(s_col, "_before_trip_start")]] <- ifelse(!is.na(s) & !is.na(t0), s < t0, NA)
  df[[paste0(e_col, "_after_trip_end")]]    <- ifelse(!is.na(e) & !is.na(t1), e > t1, NA)
  df[[paste0("boost_", i, "_start_after_end")]] <- ifelse(!is.na(s) & !is.na(e), s > e, NA)
  
  # Clamp into [t0, t1] (NA safe via ifelse)
  s <- ifelse(!is.na(s) & !is.na(t0) & s < t0, t0, s)
  e <- ifelse(!is.na(e) & !is.na(t1) & e > t1, t1, e)
  
  # Ensure start ≤ end post clamp
  need_fix <- !is.na(s) & !is.na(e) & s > e
  s[need_fix] <- e[need_fix]
  
  df[[s_col]] <- s
  df[[e_col]] <- e
  df
}

for (i in 1:8) data <- clamp_boost_window(data, i)

# coerce any numeric “dates” back to Date
fix_date_class <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x))       return(as.Date(x, origin = "1970-01-01"))  # restore Date
  as_date_robust(x)
}

date_cols_all <- grep("(_date$|_date_raw$)", names(data), value = TRUE, ignore.case = TRUE)

data <- data %>%
  dplyr::mutate(across(all_of(date_cols_all), fix_date_class))

# One Day Trip Indicator -> TRUE if start == end, else FALSE (NAs become FALSE)
data <- data %>%
  dplyr::mutate(one_day_trip = dplyr::coalesce(trip_start_date == trip_end_date, FALSE))





# ==================== Troubleshoot/diagnostics; how many were capped per rule ====================
# --- Count swap attempts vs successes ---

# Attempt conditions (from earlier logic)
attempt_start <- ddmm_swap_possible(data$trip_start_date_raw)
attempt_end   <- ddmm_swap_possible(data$trip_end_date_raw)

# Success flags (we stored these earlier)
success_start <- data$trip_start_date_swapped_ddmm
success_end   <- data$trip_end_date_swapped_ddmm

# Summarise counts
swap_summary <- tibble(
  swap_type = c("Start Date", "End Date"),
  attempted = c(sum(attempt_start, na.rm = TRUE),
                sum(attempt_end,   na.rm = TRUE)),
  successful = c(sum(success_start, na.rm = TRUE),
                 sum(success_end,   na.rm = TRUE))
) %>%
  mutate(success_rate = if_else(attempted > 0, successful / attempted, NA_real_))

print(swap_summary)



# Rule 1: quote date > trip_start_date -> capped to trip_start_date
#n_rule1 <- sum(viol_q, na.rm = TRUE)

# Rule 2: trip_start_date > trip_end_date -> start set to end
#n_rule2 <- sum(viol_se, na.rm = TRUE)

# Rule 3: for each boost_i, count start capped, end capped, and start> end fixes
r3_list <- lapply(1:8, function(i) {
  s_col <- paste0("boost_", i, "_start_date")
  e_col <- paste0("boost_", i, "_end_date")
  sr    <- paste0(s_col, "_raw")
  er    <- paste0(e_col, "_raw")
  flag_s_before <- paste0(s_col, "_before_trip_start")
  flag_e_after  <- paste0(e_col, "_after_trip_end")
  flag_s_gt_e   <- paste0("boost_", i, "_start_after_end")
  
  # Skip if this boost pair doesn't exist
  if (!all(c(s_col, e_col, sr, er, flag_s_before, flag_e_after, flag_s_gt_e) %in% names(data))) {
    return(NULL)
  }
  
  # Start capped: changed AND original was before trip start
  n_start_capped <- sum(
    !is.na(data[[sr]]) & !is.na(data[[s_col]]) &
      data[[sr]] != data[[s_col]] &
      (data[[flag_s_before]] %in% TRUE),
    na.rm = TRUE
  )
  
  # End capped: changed AND original was after trip end
  n_end_capped <- sum(
    !is.na(data[[er]]) & !is.na(data[[e_col]]) &
      data[[er]] != data[[e_col]] &
      (data[[flag_e_after]] %in% TRUE),
    na.rm = TRUE
  )
  
  # Start > End fixes
  n_start_after_end_fixed <- sum(data[[flag_s_gt_e]] %in% TRUE, na.rm = TRUE)
  
  tibble(
    boost = i,
    start_capped = n_start_capped,
    end_capped = n_end_capped,
    start_after_end_fixed = n_start_after_end_fixed
  )
})

r3 <- bind_rows(r3_list)

# High-level summary
summary_tbl <- bind_rows(
  #tibble(rule = "Rule 1: quote date capped to trip_start", count = n_rule1),
  #tibble(rule = "Rule 2: trip_start capped to trip_end", count = n_rule2),
  tibble(rule = "Rule 3: boost windows clamped (total start)", count = sum(r3$start_capped, na.rm = TRUE)),
  tibble(rule = "Rule 3: boost windows clamped (total end)",   count = sum(r3$end_capped,   na.rm = TRUE))
  # tibble(rule = "Rule 3: start > end fixed",                   count = sum(r3$start_after_end_fixed, na.rm = TRUE))
)

print(summary_tbl)

if (nrow(r3)) {
  message("Per-boost breakdown:")
  print(r3)
}




# --- Audit: case (1) and case (3) ---
audit_equal <- data %>%
  mutate(
    all_three_equal = !is.na(quote_date) & !is.na(trip_start_date) & !is.na(trip_end_date) &
      quote_date == trip_start_date & quote_date == trip_end_date,
    start_eq_end    = !is.na(trip_start_date) & !is.na(trip_end_date) &
      trip_start_date == trip_end_date,
    start_eq_end_only = start_eq_end & !all_three_equal
  )



# Counts per Year–Month from quote_date
monthly_counts <- data %>%
  filter(!is.na(quote_date)) %>%
  mutate(quote_month = floor_date(quote_date, unit = "month")) %>%
  count(quote_month, name = "n") %>%
  arrange(quote_month)

monthly_counts


# Overall count where start == end (both non-missing)
same_day_idx <- with(data,
                     !is.na(trip_start_date) & !is.na(trip_end_date) & trip_start_date == trip_end_date
)
cat("Rows with trip_start_date == trip_end_date:", sum(same_day_idx), "\n")

# keep only needed columns
data <- data %>%
  select(quote_id:quote_time)


### ============================================================================
### 3.2 CLEAN DESTINATIONS
### ============================================================================
# reduce cardinality for destinations
# load cities
data(world.cities)

city_lookup <- world.cities %>%
  mutate(
    city_clean = str_to_lower(name),
    country_clean = str_to_lower(country.etc)
  ) %>%
  select(city_clean, country_clean) %>%
  distinct()

df_long <- data %>%
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
    is_antarctica = str_detect(str_to_lower(destinations),"\\bantarctica\\w*\\b"),
    is_australia = str_detect(str_to_lower(destinations),"\\baustralia\\w*\\b"),
    
    region_simple = case_when(
      has_cruise ~ "cruise",
      is_worldwide ~ "worldwide",
      is_antarctica ~ "antarctica",
      is_australia ~ "australia",
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
    worldwide = as.numeric(any(is_worldwide)),
    antarctica = as.numeric(any(is_antarctica)),
    domestic = as.numeric(any(is_australia)),
    .groups = "drop")

data <- data %>%
  left_join(df_long, by = "quote_id") %>%
  mutate(
    is_all_americas = str_detect(str_to_lower(destinations), "all of the americas"),
    north_america = if_else(is_all_americas, 1L, north_america),
    south_america = if_else(is_all_americas, 1L, south_america),
    other_region = if_else(is_all_americas, 0L, other_region)) %>%
  select(-is_all_americas)

### ============================================================================
### 3.3 CLEAN TRAVELLER AGES
### ============================================================================
# create dummies for traveller demographics
data <- data %>%
  mutate(ages_list = map(strsplit(traveller_ages, ";"), as.numeric),
    
    # define categories with counts
    children_count = map_dbl(ages_list, ~ sum(.x < 18)),
    young_adult_count = map_dbl(ages_list, ~ sum(.x >= 18 & .x <= 40)),
    middle_age_count = map_dbl(ages_list, ~ sum(.x >= 41 & .x <= 65)),
    elderly_count = map_dbl(ages_list, ~ sum(.x >= 66)),
    adult_count = map_dbl(ages_list, ~ sum(.x >= 18)),
    number_travellers = map_dbl(ages_list, length),
    
    # Create dummy variables
    solo = as.numeric(number_travellers == 1),
    has_children = as.numeric(children_count > 0),
    family = as.numeric(children_count > 0 & adult_count > 0),
    couple = as.numeric(number_travellers == 2 & adult_count == 2),
    any_young_adult = as.numeric(young_adult_count > 0),
    any_middle_age = as.numeric(middle_age_count > 0),
    any_elderly = as.numeric(elderly_count > 0)) %>%
  select(-ages_list, -ends_with("_count"))

### ============================================================================
### 4. EDA
### ============================================================================
# heatmap of conversion rates by region
df_long <- data %>%
  pivot_longer(
    cols = c(europe, east_asia, south_asia, middle_east, africa, 
             north_america, south_america, oceania, other_region, antarctica, domestic),
    names_to = "region",
    values_to = "in_region") %>%
  filter(in_region == 1) %>%
  mutate(convert = ifelse(convert == "YES", 1, 0)) %>%
  group_by(region) %>%
  summarise(
    total_quotes = n(),
    conversions = sum(convert, na.rm = TRUE),
    conversion_rate = conversions / total_quotes) %>%
  arrange(desc(conversion_rate))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(region_sub = countrycode(sourcevar = name,
                                  origin = "country.name",
                                  destination = "region23"),
         region_un = if_else(region_sub %in% c("Eastern Asia", "South-Eastern Asia", "Central Asia"), "East Asia", region_un),
         region_un = if_else(subregion == "Southern Asia", "South Asia", region_un),
         region_un = if_else(subregion == "Western Asia", "Middle East", region_un),
         region_un = if_else(subregion == "Northern America" , "North America" , region_un),
         region_un = if_else(subregion %in% c("South America", "Central America", "Caribbean"), "South America" , region_un),
         region_un = if_else(name == "Australia", "Australia", region_un),
         continent = region_un)

region_map <- tibble(region = c("europe", "east_asia", "south_asia", "middle_east", 
             "africa", "north_america", "south_america", "oceania", "antarctica", "domestic"),
             continent = c("Europe", "East Asia", "South Asia", "Middle East", "Africa", 
                           "North America", "South America", "Oceania", "Antarctica", "Australia"))

region_rates <- region_map %>%
  left_join(df_long, by = "region")

world_rates <- world %>%
  left_join(region_rates, by = "continent")

ggplot(world_rates) +
  geom_sf(aes(fill = conversion_rate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(title = "Conversion Rate by Region", fill = "Rate") +
  theme_minimal()