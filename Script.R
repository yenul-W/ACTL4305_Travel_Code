# load packages
library(readxl)
library(dplyr)
library(lubridate)

# load dataset
setwd("C:/Users/yenul/Downloads/ACTL4305")
Freely_quote_data <- read_excel("Freely_quote_data.xlsx", sheet = "Quotes")
data <- Freely_quote_data

data <- data %>%
  mutate(quote_id = row_number()) %>%
  relocate(quote_id)

# Correct a date pair
process_date_pair <- function(df, start_col, end_col) {
  # Check if both columns exist in the data
  if (!all(c(start_col, end_col) %in% names(df))) {
    return(NULL)
  }
  
  if (all(is.na(df[[start_col]])) && all(is.na(df[[end_col]]))) {
    return(NULL)
  }
  
  start_final <- paste0(start_col, "_final")
  end_final <- paste0(end_col, "_final")
  
  date_data <- df %>%
    select(quote_id, all_of(c(start_col, end_col)))
  
  # serial dates
  serial_dates <- date_data %>%
    filter(
      (!is.na(!!sym(start_col)) & grepl("^\\d{5}$", as.character(!!sym(start_col))) & as.numeric(!!sym(start_col)) > 25568) |
        (!is.na(!!sym(end_col)) & grepl("^\\d{5}$", as.character(!!sym(end_col))) & as.numeric(!!sym(end_col)) > 25568)
    ) %>%
    mutate(
      !!paste0(start_col, "1") := if_else(
        !is.na(!!sym(start_col)) & grepl("^\\d{5}$", as.character(!!sym(start_col))) & as.numeric(!!sym(start_col)) > 25568,
        as.Date(as.numeric(!!sym(start_col)), origin = "1899-12-30"),
        as.Date(!!sym(start_col), format = "%d/%m/%Y")
      ),
      !!paste0(end_col, "1") := if_else(
        !is.na(!!sym(end_col)) & grepl("^\\d{5}$", as.character(!!sym(end_col))) & as.numeric(!!sym(end_col)) > 25568,
        as.Date(as.numeric(!!sym(end_col)), origin = "1899-12-30"),
        as.Date(!!sym(end_col), format = "%d/%m/%Y")
      )
    )
  
  # non-serial dates
  non_serial_dates <- date_data %>%
    filter(
      !((!is.na(!!sym(start_col)) & grepl("^\\d{5}$", as.character(!!sym(start_col))) & as.numeric(!!sym(start_col)) > 25568) |
          (!is.na(!!sym(end_col)) & grepl("^\\d{5}$", as.character(!!sym(end_col))) & as.numeric(!!sym(end_col)) > 25568))
    ) %>%
    mutate(
      !!paste0(start_col, "1") := as.Date(!!sym(start_col), format = "%d/%m/%Y"),
      !!paste0(end_col, "1") := as.Date(!!sym(end_col), format = "%d/%m/%Y")
    )
  
  all_dates <- bind_rows(serial_dates, non_serial_dates)
  
  # illogical dates (start > end)
  all_dates_corrected <- all_dates %>%
    rowwise() %>%
    mutate(
      start_temp = !!sym(paste0(start_col, "1")),
      end_temp = !!sym(paste0(end_col, "1")),
      
      !!start_final := if (!is.na(start_temp) && !is.na(end_temp) && start_temp > end_temp & day(start_temp) <= 12) {
        make_date(year(start_temp), day(start_temp), month(start_temp))
      } else {
        start_temp
      },
      
      !!end_final := if (!is.na(start_temp) && !is.na(end_temp) && start_temp > end_temp & day(end_temp) <= 12) {
        make_date(year(end_temp), day(end_temp), month(end_temp))
      } else {
        end_temp
      }
    ) %>%
    ungroup() %>%
    select(quote_id, all_of(c(start_final, end_final)))
  
  return(all_dates_corrected)
}

all_corrected_dates <- data %>% select(quote_id)

date_pairs <- list(
  c("trip_start_date", "trip_end_date"),
  c("boost_1_start_date", "boost_1_end_date"),
  c("boost_2_start_date", "boost_2_end_date"),
  c("boost_3_start_date", "boost_3_end_date"),
  c("boost_4_start_date", "boost_4_end_date"),
  c("boost_5_start_date", "boost_5_end_date"),
  c("boost_6_start_date", "boost_6_end_date"),
  c("boost_7_start_date", "boost_7_end_date"),
  c("boost_8_start_date", "boost_8_end_date"))

# apply function to all start/end date pairs
for (pair in date_pairs) {
  start_col <- pair[1]
  end_col <- pair[2]
  
  corrected_pair <- process_date_pair(data, start_col, end_col)
  
  if (!is.null(corrected_pair)) {
    all_corrected_dates <- left_join(all_corrected_dates, corrected_pair, by = "quote_id")
  } else {
    message(paste("Fail for", start_col, "and", end_col))
  }
}

# remove unnecessary columns
columns_to_remove <- character()
for (pair in date_pairs) {
  start_col <- pair[1]
  end_col <- pair[2]
  start_final <- paste0(start_col, "_final")
  end_final <- paste0(end_col, "_final")
  
  if (start_final %in% names(all_corrected_dates) && end_final %in% names(all_corrected_dates)) {
    columns_to_remove <- c(columns_to_remove, start_col, end_col)
  }
}

# left join to original df
data <- data %>%
  left_join(all_corrected_dates, by = "quote_id")

if (length(columns_to_remove) > 0) {
  data <- data %>%
    select(-all_of(columns_to_remove))
}

data <- data %>%
  mutate(trip_length = difftime(trip_end_date_final, trip_start_date_final, units = "days"))














