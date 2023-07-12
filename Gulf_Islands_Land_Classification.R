##############################################
#####Supervised Land Cover Classification#####
##############################################
my_polygons <- "C:\\Users\\wendiz3.stu\\OneDrive - UBC\\Documents\\GEM 520 Remote Sensing & R\\Labs\\Lab 6_Spectral Signature and Image Classification\\classification_polygons_WZ.shp"

library(tidyverse)
library(terra)
library(sf)
library(RStoolbox)

# read the Gulf Islands Landsat imagery
ls_image <- rast("C:\\Users\\wendiz3.stu\\OneDrive - UBC\\Documents\\GEM 520 Remote Sensing & R\\Labs\\Lab 6_Spectral Signature and Image Classification\\Data\\LT05_L2SP_20060723_GulfIslands_BSTACK.tif")

ls_image

terra::plotRGB(ls_image, r = 3, g = 2, b = 1, stretch = "lin")

# load the training area polygons
class_poly <- st_read(my_polygons)

# make sure that the geometry is valid
class_poly <- st_make_valid(class_poly)

# tranform lc_class to factor
class_poly <- class_poly %>%
  mutate(lc_class = factor(lc_class, 
                           levels = c("Broadleaf Forest", "Coniferous Forest", "Exposed soil and rocks", "High density developed", "Low density developed", "Non-forest vegetation", "Water")))

# plot
terra::plotRGB(ls_image, r = 3, g = 2, b = 1, stretch = "lin")
plot(class_poly[, "lc_class"], add = TRUE)

# write a summery of the number of polygon per land cover class
poly_summary <- class_poly %>%
  st_drop_geometry() %>%
  group_by(lc_class) %>%
  summarize(n_poly = n())

poly_summary

# Assign a unique ID to each polygon 
class_poly <- tibble::rowid_to_column(class_poly, var = "ID")

set.seed(1234)

# Sample 70% of the polygons in each land cover class
poly_train <- class_poly %>%
  group_by(lc_class) %>%
  sample_frac(0.7) %>%
  mutate(set = "training") %>% st_cast(to = 'POLYGON')

# Use the ID field to select the polygons for validation
poly_val <- class_poly %>%
  filter(!ID %in% poly_train$ID) %>%
  mutate(set = "validation") %>% st_cast(to = 'POLYGON')

poly_set <- rbind(poly_train, 
                  poly_val)

# Plot poly_set
plot(poly_set[, "set"])

# Extract the values of the Landsat image pixels in the polygons
poly_set_vals <- terra::extract(ls_image, vect(poly_set))

# We need to perform an inner_join to retrieve lc_class
poly_set_vals <- inner_join(poly_set, poly_set_vals) %>%
  st_drop_geometry()

poly_set_vals # each row of corresponds to one pixel of the Landsat image

# check the number of pixel per class 
poly_stats <- poly_set_vals %>%
  group_by(set, lc_class) %>%
  summarize(n_px = n())

poly_stats

# Turn the data into a tidy format
poly_set_vals_long <- pivot_longer(poly_set_vals,
                                   blue:swir2, 
                                   names_to = "band", 
                                   values_to = "reflectance")

poly_set_vals_long

# Calculate summary statistics of the classification
spectral_sign <- poly_set_vals_long %>%
  group_by(lc_class, band) %>%
  summarize(r_mean = mean(reflectance, na.rm = TRUE), 
            r_q05 = quantile(reflectance, 0.05, na.rm = TRUE), 
            r_q95 = quantile(reflectance, 0.95, na.rm = TRUE))

spectral_sign

# visualize the spectural signature of classified classes
# Wavelength corresponding to each band
bands_wavelength <- read_csv("C:\\Users\\wendiz3.stu\\OneDrive - UBC\\Documents\\GEM 520 Remote Sensing & R\\Labs\\Lab 6_Spectral Signature and Image Classification\\Data\\bands_wavelength.csv")

bands_wavelength

# Join wavelength
spectral_sign <- inner_join(spectral_sign, bands_wavelength)

# Graph
ggplot(spectral_sign, aes(x = wavelength, y = r_mean, group = 1)) +
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = r_q05, ymax = r_q95), alpha = 0.2) + 
  facet_wrap(vars(lc_class)) + 
  theme_bw() + 
  labs(x = "Wavelength (nm)", 
       y = "Reflectance")

# accuracy assessment
set.seed(1234)

poly_train <- poly_train %>% rename(class = lc_class)
poly_val <- poly_val %>% rename(class = lc_class)

mlc_model <- superClass(img = raster::stack(ls_image), 
                        trainData = as(poly_train, "Spatial"), 
                        valData = as(poly_val, "Spatial"), 
                        responseCol = "class", 
                        model = "mlc", 
                        nSamples = 500)

classified_map <- mlc_model$map

# Write the classified map as a tif file
raster::writeRaster(classified_map, 
                    filename = "outputs/classified_map.tif", 
                    overwrite = TRUE)

# Plot with colors
raster::plot(classified_map,
             col = c('#A6D96A','#33A02C','#DE3B13','#D63CF1','#00D2D2','#F1A026','#2B83BA'),
             main = "Gulf Islands Land Cover Classification Map")

# Validation df
val_preds <- mlc_model$validation$validationSamples

head(val_preds)

# write the confusion matrix
conf_matrix <- table(st_drop_geometry(val_preds[, c("prediction", "reference")]))

knitr::kable(conf_matrix)

# Calculate accuracy metrics here

col_sum <- sum(colSums(conf_matrix))
col_sum

row_sum <- sum(rowSums(conf_matrix))
row_sum

diagnal_sum <- sum(diag(conf_matrix))
diagnal_sum

OA <- diagnal_sum/col_sum * 100
OA

#predicted classes in the rows
#reference classes in the columns

UA <- diag(conf_matrix)/rowSums(conf_matrix)
UA

PA <- diag(conf_matrix)/colSums(conf_matrix)
PA

#write a neat confusion metric

confusion_metrix <- data.frame(OA, UA, PA)

