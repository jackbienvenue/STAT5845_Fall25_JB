setwd('/Users/jackbienvenuejr/Desktop/Desktop - jack-bienvenue/Fall2025Classes/STAT5845/STAT5845_Fall25_JB/project')

# Load necessary library
library(dplyr)

# Path to your data folder
data_path <- "./data/"

# 1. Read city_info.csv
city_info <- read.csv(file.path(data_path, "city_info.csv"), stringsAsFactors = FALSE)

# 2. Filter stations with Stn.edDate = "12/31/23"
active_sites <- city_info %>%
  filter(Stn.edDate == "2023-12-31")

# 3. Extract IDs
site_ids <- active_sites$ID

# Vector of site IDs of interest


# Base URL for NOAA GHCN daily access
base_url <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"

# Directory where files will be saved
out_dir <- "ghcn_daily_data"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Loop through IDs and download CSVs
for (id in site_ids) {
  file_url <- paste0(base_url, id, ".csv")
  dest_file <- file.path(out_dir, paste0(id, ".csv"))
  
  message("Downloading: ", id)
  tryCatch(
    {
      download.file(file_url, destfile = dest_file, mode = "wb", quiet = TRUE)
    },
    error = function(e) {
      warning("Failed to download ", id, ": ", e$message)
    }
  )
}
