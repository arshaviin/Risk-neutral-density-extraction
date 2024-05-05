library(readr)
library(dplyr)
library(lubridate)
library(RND)
library(tidyr)
library(readr)
library(dplyr)


# Load data
data <- read_csv("m122020.csv")

# Get unique dates and convert them to Date objects
unique_dates <- unique(data$`[QUOTE_DATE]`)
# Initialize an empty data frame to collect filtered data
filtered_data <- data.frame()

# Loop through each unique date
for (date in as.list(unique_dates)) {
  # Filter data for the current unique date
  date_filtered_data <- data %>% 
    filter(`[QUOTE_DATE]` == as.character(date)) %>% 
    mutate(`[DTE]` = as.numeric(`[DTE]`)) # Ensure DTE is numeric for sorting
  
  # Get the 4th minimum DTE if it exists
  if (n_distinct(date_filtered_data$`[DTE]`) >= 8) {
    fourth_min_dte <- sort(unique(date_filtered_data$`[DTE]`))[8]
    # Select only the rows with the 4th smallest DTE
    date_filtered_data <- date_filtered_data %>% 
      filter(`[DTE]` == fourth_min_dte)
    
    # Bind the rows for the 4th smallest DTE to the filtered_data dataframe
    filtered_data <- bind_rows(filtered_data, date_filtered_data)
  } else {
    # Handle the case where there are less than 4 unique DTE values for the date
    message("Not enough DTE values for date ", date)
  }
}

# Now bind the rows that have the 4th smallest DTE for their respective QUOTE_DATE back with the main data
final_data <- filtered_data

data<-final_data

extract_shimko_density <- function(data, r, y, te, s0, lower, upper) {
  market_calls <- data$`[C_LAST]`
  call_strikes <- data$`[STRIKE]`
  df <- data.frame(market_calls, call_strikes)
  df <- df[order(df$call_strikes, df$market_calls), ]
  
  shimko_results <- RND::extract.shimko.density(
    market.calls = df$market_calls,
    call.strikes = df$call_strikes,
    r = r, y = y, te = te, s0 = s0, lower = lower, upper = upper
  )
  
  return(data.frame(Strike = df$call_strikes, Density = shimko_results$shimko.density))
}

# Initialize lists for results
all_densities <- list()

# Process each unique date
for (date in unique_dates) {
  tryCatch({
    specific_option_chain <- filter(data,`[QUOTE_DATE]`== date)
    if (nrow(specific_option_chain) > 0) {
      r <- 0.001
      y <- 0.00
      te <- specific_option_chain$`[DTE]`[1] / 365
      s0 <- specific_option_chain$`[UNDERLYING_LAST]`[1]
      lower <- -10000
      upper <- 10000
      
      shimko_density_results <- extract_shimko_density(
        specific_option_chain, r, y, te, s0, lower, upper
      )
      #print(shimko_density_results)
      all_densities[[as.character(date)]] <- shimko_density_results
    }
  }, error = function(e) {
    message("Error with date ", date, ": ", e$message)
  })
}

# Save densities with corresponding strikes for each day
#for (date in names(all_densities)) {
# write_csv(all_densities[[date]], paste0("Density_Strikes_", date, ".csv"))
#}
# Bind all the density dataframes together, adding a date column for pivoting
combined_df <- bind_rows(all_densities, .id = "Date")
# Pivot the data to wide format
wide_df <- pivot_wider(
  combined_df,
  names_from = Date,
  values_from = Density,
  names_prefix = ""
)

write_csv(wide_df, "farDensities_Strikes_Monthly12.csv")

