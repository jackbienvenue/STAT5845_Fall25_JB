#install.packages(c("terra", "sf", "stars", "ggplot2", "rnaturalearth", "rnaturalearthdata"))

library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

f <- "../data/hrrr.20150714_conus_hrrr.t03z.wrfsubhf12.grib2"

# Read as a multi-layer SpatRaster (each GRIB field is a layer)
r <- rast(f)

# Find 2 m temperature layer
nm <- names(r)
idx_t2m <- grep("TMP|2.?m.*(temp|TMP)|2.?metre.*temp", nm, ignore.case = TRUE)[1]
stopifnot(!is.na(idx_t2m))

t2m <- r[[idx_t2m]]

# Reproject to geographic lon/lat (HRRR native is Lambert Conformal)
t2m_ll <- project(t2m, "EPSG:4326")
names(t2m_ll) <- "t2m"     # already in °C 

# To data frame for ggplot
df <- as.data.frame(t2m_ll, xy = TRUE, na.rm = TRUE)

# State borders
states <- ne_states(country = "United States of America", returnclass = "sf")

p <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = t2m)) +
  scale_fill_viridis_c(name = "2 m Temp (°C)") +
  geom_sf(data = states, fill = NA, color = "grey30", linewidth = 0.3) +
  coord_sf(xlim = range(df$x), ylim = range(df$y), expand = FALSE) +
  labs(title = "HRRR 2 m Temperature",
       subtitle = basename(f)) +
  theme_minimal(base_size = 12)

# Build output filename from the basename of input file
out_file <- file.path(
  "../figures",
  paste0(tools::file_path_sans_ext(basename(f)), ".png")
)

# Save the plot
ggsave(out_file, plot = p, width = 10, height = 4, dpi = 300)

# --- Connecticut subset ---
ct <- subset(states, name == "Connecticut")
ct_ll <- st_transform(ct, 4326)
ct_v  <- vect(ct_ll)

t2m_ct <- mask(crop(t2m_ll, ct_v), ct_v)
df_ct  <- as.data.frame(t2m_ct, xy = TRUE, na.rm = TRUE)
bb     <- st_bbox(ct_ll)

p_ct <- ggplot() +
  geom_raster(data = df_ct, aes(x = x, y = y, fill = t2m)) +
  scale_fill_viridis_c(name = "2 m Temp (°C)") +
  geom_sf(data = ct_ll, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = 0) +
  labs(title = "HRRR 2 m Temperature — Connecticut",
       subtitle = basename(f)) +
  theme_minimal(base_size = 12)

# Build output filename from the basename of input file
out_file <- file.path(
  "../figures",
  paste0(tools::file_path_sans_ext(basename(f)), "_CT_only.png")
)

# Save the plot
ggsave(out_file, plot = p_ct, width = 10, height = 4, dpi = 300)

# ----- second dataset (ACTUAL TEMPERATURE) -----
f2 <- "../data/hrrr.20150714_conus_hrrr.t15z.wrfsfcf00.grib2"  # change as needed
r2 <- rast(f2)
nm2 <- names(r2)
idx_t2m2 <- grep("TMP|2.?m.*(temp|TMP)|2.?metre.*temp", nm2, ignore.case = TRUE)[1]
stopifnot(!is.na(idx_t2m2))

t2m2      <- r2[[idx_t2m2]]
t2m2_ll   <- project(t2m2, "EPSG:4326")
names(t2m2_ll) <- "t2m"

# Crop/mask to CT and align to the first CT grid
t2m_ct2 <- mask(crop(t2m2_ll, ct_v), ct_v)
t2m_ct2 <- resample(t2m_ct2, t2m_ct, method = "bilinear")

# Difference (dataset2 - dataset1), units remain °C
diff_ct <- t2m_ct2 - t2m_ct
names(diff_ct) <- "diff"

df_diff <- as.data.frame(diff_ct, xy = TRUE, na.rm = TRUE)
rng <- max(abs(df_diff$diff), na.rm = TRUE)
bb  <- st_bbox(ct_ll)

p_diff <- ggplot() +
  geom_raster(data = df_diff, aes(x = x, y = y, fill = diff)) +
  scale_fill_gradient2(name = "ΔT (°C)", midpoint = 0, limits = c(-rng, rng)) +
  geom_sf(data = ct_ll, fill = NA, color = "black", linewidth = 0.6) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = 0) +
  labs(title = "2 m Temperature Difference — Connecticut") +
  theme_minimal(base_size = 12)

# Build output filename from the basename of input file
out_file <- file.path(
  "../figures/forecast_error.png")


# Save the plot
ggsave(out_file, plot = p_diff, width = 10, height = 4, dpi = 300)

