
require(tidyverse)

# Pre-function setup ------------------------------------------------------

# Define column names
col_names <- c(c("station", "year", "month", "variable"),
               map2(.x = replicate(n = 31,
                                   expr = c("value", "mflag", "qflag", "sflag"),
                                   simplify = FALSE),
                    .y = 1:31,
                    .f = ~ paste0(.x, .y)) %>%
                 reduce(.f = c))

# Define column types using names. Year/month/value are integer; others = char
col_types <- map(.x = col_names,
                 .f = ~ ifelse(test = grepl(pattern = "year|month|value",
                                            x = .x),
                               yes = "i", no = "c"))

# Define column widths
col_widths <- c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31))


# parse function ----------------------------------------------------------

parse_GHCN_data_new <- function(dly_file) {
  # This function converts GHCN 'daily' data files to an R dataframe by reshaping
  # the data from 128 columns into 10. It retains all variables (temp, rain,
  # precip, etc.) as well as all measurement, source and quality flags.

  raw <- read_fwf(file = dly_file,
                  col_positions = fwf_widths(widths = col_widths,
                                             col_names = col_names),
                  col_types = col_types, na = character(), trim_ws = TRUE)

  wide_data <- pivot_longer(data = raw,
                            cols = !c("station", "year", "month", "variable"),
                            names_to = c(".value", "day"),
                            names_pattern = "([a-z]{5})([0-9]{1,2})") %>%
    mutate(date = as.Date(paste(year, month, day, sep = "-"),
                          format = "%Y-%m-%d")) %>%
    filter(!is.na(date)) %>%
    mutate(across(.cols = everything(), .fns = ~ na_if(., -9999))) %>%
    arrange(date, variable)

  return(wide_data)
}