####

library(tidyr)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(reshape2) 
library(gridExtra)
library(patchwork)
library(ggpubr)
###
setwd("C:/Users/dafna/Desktop/JGR/Data")

sf_data<-read.csv('sf_data_22_23.csv', sep = ','   )   
tower_data <-read.csv('tower_data_22_23.csv', sep = ','   )  

sf_and_tower <- inner_join(sf_data, tower_data, by = "Time")

sf_and_tower$Time <- ymd_hms(sf_and_tower$Time)  
##################################################

# Create new columns for year, month, week, day, and hour
sf_and_tower$Year <- year(sf_and_tower$Time)
sf_and_tower$Month <- month(sf_and_tower$Time)
sf_and_tower$Week <- week(sf_and_tower$Time)
sf_and_tower$date <- format(sf_and_tower$Time, "%Y-%m-%d") 
sf_and_tower$doy <- yday(sf_and_tower$Time)
sf_and_tower$hour <- format(sf_and_tower$Time, "%H:%M:%S") 


sf_and_tower <- sf_and_tower %>%
  mutate(Season = case_when(
    (doy >= 60 & doy <= 91) ~ "Spring",            # March 1st (DOY 60) to April 1st (DOY 91)
    (doy >= 121 & doy <= 171) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (doy >= 172 & doy <= 253) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (doy >= 254 & doy <= 304) ~ "Post-monsoon",   # September 11th (DOY 254) to October 31st (DOY 304)
    TRUE ~ "Winter"                                # All other days are Winter
  ))




sf_and_tower_daytime<- sf_and_tower %>%
  filter(hour >= "06:00:00" & hour <= "18:00:00")

sf_and_tower_noontime<- sf_and_tower %>%
  filter(hour >= "11:00:00" & hour <= "14:00:00")

sf_and_tower_mo<- sf_and_tower[sf_and_tower$Month >= 5 & sf_and_tower$Month <= 10, ]
sf_and_tower_daytime_mo<- sf_and_tower_mo %>%
  filter(hour >= "06:00:00" & hour <= "18:00:00")

#sf_flux_mo<- sf_flux[sf_flux$Month >= 5 & sf_flux$Month <= 10, ]
sf_and_tower_noontime_mo<- sf_and_tower_mo %>%
  filter(hour >= "11:00:00" & hour <= "14:00:00")



####ENVIRONMENTAL CONDITIONS FIGURE@@ MONTHLY ##RAIN###
###add the muly year avg for the first figure###

tower_data_AM <- read.csv('AMF_US-MtB_BASE_HH_4-5.csv', sep = ',',skip=2,
                          na.strings = c("-9999", "-9999.000"))

tower_data_AM $TIMESTAMP_START <- ymd_hm(tower_data_AM $TIMESTAMP_START)

tower_data_AM$top_soil_swc_N <- rowMeans(tower_data_AM[, c("SWC_1_1_1", "SWC_1_2_1", "SWC_1_3_1")], na.rm = TRUE)
tower_data_AM$top_soil_swc_E <- rowMeans(tower_data_AM[, c("SWC_2_1_1", "SWC_2_2_1", "SWC_2_3_1")], na.rm = TRUE)
tower_data_AM$top_soil_swc_S <- rowMeans(tower_data_AM[, c("SWC_3_1_1", "SWC_3_2_1", "SWC_3_3_1")], na.rm = TRUE)

tower_data_AM <- tower_data_AM %>%
  mutate(mean_swc_average = rowMeans(dplyr::select(., top_soil_swc_N, top_soil_swc_E, top_soil_swc_S), na.rm = TRUE))


#tower_data_AM<- tower_data_AM%>%
# mutate(mean_swc_average = rowMeans(select(., top_soil_swc_N, top_soil_swc_E, top_soil_swc_S ), na.rm = TRUE))

tower_data_AM$Year <- year(tower_data_AM$TIMESTAMP_START)
tower_data_AM$Month <- month(tower_data_AM$TIMESTAMP_START)
tower_data_AM$Week <- week(tower_data_AM$TIMESTAMP_START)
tower_data_AM$hour <- format(tower_data_AM$TIMESTAMP_START, "%H:%M:%S")
tower_data_AM$doy <- yday(tower_data_AM$TIMESTAMP_START)


tower_data_vpd_temp_AM <- tower_data_AM %>%
  dplyr::select(TIMESTAMP_START, Year, Month, Week, doy, hour, TA_1_2_1, TA_1_3_1, TA_1_4_1, VPD_PI_1_2_1, VPD_PI_1_3_1, VPD_PI_1_4_1, mean_swc_average, P_1_1_1, P_2_1_1)

#tower_data_vpd_temp_AM <- tower_data_AM %>%
# select( TIMESTAMP_START, Year, Month,Week, doy,hour,TA_1_2_1,TA_1_3_1,TA_1_4_1,VPD_PI_1_2_1, VPD_PI_1_3_1, VPD_PI_1_4_1,mean_swc_average, P_1_1_1, P_2_1_1)
tower_data_vpd_temp_AM <- na.omit(tower_data_vpd_temp_AM)

tower_2014_2021 <- tower_data_vpd_temp_AM %>% filter(Year != 2013)

####muly year avg ##RAIN###

monthly_totals <- tower_2014_2021 %>%
  group_by(Year, Month) %>%
  summarise(
    total_rain_1 = sum(P_1_1_1, na.rm = TRUE),  # Total rainfall (P_1_1_1) for the month
    total_rain_2 = sum(P_2_1_1, na.rm = TRUE)   # Total rainfall (P_2_1_1) for the month
  ) %>%
  ungroup()

# Step 2: Calculate multi-year statistics (mean, SD, SE) for each month
monthly_8_year_ <- monthly_totals %>%
  group_by(Month) %>%
  summarise(
    mean_rain_1 = mean(total_rain_1, na.rm = TRUE),      # Mean monthly rainfall (P_1_1_1)
    sd_rain_1 = sd(total_rain_1, na.rm = TRUE),         # SD for monthly rainfall (P_1_1_1)
    se_rain_1 = sd_rain_1 / sqrt(n()),                  # SE for monthly rainfall (P_1_1_1)
    mean_rain_2 = mean(total_rain_2, na.rm = TRUE),      # Mean monthly rainfall (P_2_1_1)
    sd_rain_2 = sd(total_rain_2, na.rm = TRUE),         # SD for monthly rainfall (P_2_1_1)
    se_rain_2 = sd_rain_2 / sqrt(n())                   # SE for monthly rainfall (P_2_1_1)
  )

rain_summary <- monthly_8_year_ %>%
  summarize(
    total_rainfall = sum(mean_rain_2, na.rm = TRUE),
    summer_rainfall = sum(mean_rain_2[Month %in% 6:9], na.rm = TRUE),
    winter_rainfall = sum(mean_rain_2[Month %in% c(10, 11, 12, 1, 2, 3, 4)], na.rm = TRUE)
  )


monthly_8_year_with_year <- monthly_8_year_ %>%
  mutate(
    Year = "Multi Year Avg"  
  )  %>%
  rename(
    sum_rain_1 = mean_rain_1,  # Rename mean columns to match `sum_rain_allyear`
    sum_rain_2 = mean_rain_2
  )



Monthly_rainfall= sf_and_tower %>%
  group_by(Month,Year) %>%
  summarise(
    sum_rain_1= sum (P_1_1_1, na.rm=TRUE),
    sum_rain_2= sum (P_2_1_1, na.rm=TRUE),
    N= n())

Monthly_rainfall <- na.omit(Monthly_rainfall)


sum_rain_allyear_prepared <- Monthly_rainfall %>%
  mutate(
    Year = as.character(Year),  # Convert Year to character to match `monthly_8_year`
    sd_rain_1 = NA_real_,       # Placeholder for standard deviation
    se_rain_1 = NA_real_,       # Placeholder for standard error
    sd_rain_2 = NA_real_,
    se_rain_2 = NA_real_
  )

combined_rain_data_2 <- bind_rows(sum_rain_allyear_prepared, monthly_8_year_with_year)

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

combined_rain_data_2$Year <- factor(combined_rain_data_2$Year, levels = c("2022", "2023", "Multi Year Avg"))



Monthly_Rain_with_multy_year_avg_se <- ggplot() +
  # Bar plot for yearly data (2022 and 2023 only)
  geom_col(
    data = combined_rain_data_2 %>% filter(Year != "Multi Year Avg"),
    aes(x = Month, y = sum_rain_1, fill = as.factor(Year)),
    position = "dodge", width = 0.8
  ) +
  
  # Line for multi-year average
  geom_line(
    data = combined_rain_data_2 %>% filter(Year == "Multi Year Avg"),
    aes(x = Month, y = sum_rain_1, color = "Multi-Year Average"),
    size = 1
  ) +
  
  # Points on the line for multi-year average
  geom_point(
    data = combined_rain_data_2 %>% filter(Year == "Multi Year Avg"),
    aes(x = Month, y = sum_rain_1, color = "Multi-Year Average"),
    size = 1
  ) +
  
  # Error bars (SD) on the points
  geom_errorbar(
    data = combined_rain_data_2 %>% filter(Year == "Multi Year Avg"),
    aes(
      x = Month,
      ymin = sum_rain_1 - se_rain_1,
      ymax = sum_rain_1 + se_rain_1,
      color = "Multi-Year Average"
    ),
    width = 0.2,  # Width of the error bars
    size = 1      # Thickness of the error bars
  ) +
  
  # Labels and scales
  labs(x = "Month", y = "Precipitation (mm)", title = "") +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  scale_fill_manual(values = c("2022" = "dodgerblue", "2023" = "darksalmon")) +  # For yearly data
  scale_color_manual(values = c("Multi-Year Average" = "gray36")) +  # For line and points
  
  # Theme and appearance
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = c(0.8, 0.8),  # Position legend inside the plot
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

Monthly_Rain_with_multy_year_avg_se
Monthly_Rain_with_multy_year_avg




tower_2014_2021_noontime<- tower_2014_2021 %>%
  filter(hour >= "11:00:00" & hour <= "14:00:00")

monthly_noontime_by_year <- tower_2014_2021_noontime %>%
  group_by(Year, Month) %>%
  summarise(
    mean_t = mean(TA_1_4_1, na.rm = TRUE),
    mean_vpd = mean(VPD_PI_1_4_1, na.rm = TRUE) / 10,  # Adjusted for VPD scaling
    mean_swc = mean(mean_swc_average, na.rm = TRUE),
    .groups = "drop"
  )

multy_years_Monthly_noontime_ <- monthly_noontime_by_year %>%
  group_by(Month) %>%
  summarise(
    mean_t_ = mean(mean_t, na.rm = TRUE),
    mean_vpd_ = mean(mean_vpd, na.rm = TRUE),
    mean_swc_ = mean(mean_swc, na.rm = TRUE),
    N = n_distinct(Year),  # Count distinct years
    se_t = sd(mean_t, na.rm = TRUE) / sqrt(N),
    se_vpd = sd(mean_vpd, na.rm = TRUE) / sqrt(N),
    se_swc = sd(mean_swc, na.rm = TRUE) / sqrt(N),
    sd_t = sd(mean_t, na.rm = TRUE) ,
    sd_vpd = sd(mean_vpd, na.rm = TRUE) ,
    sd_swc = sd(mean_swc, na.rm = TRUE),
    .groups = "drop"
  )

#multy_years_Monthly_noontime_

multy_years_Monthly_noontime_ <- multy_years_Monthly_noontime_ %>%
  mutate(
    Year = "Multi Year Avg" )  
multy_years_Monthly_noontime_ <- multy_years_Monthly_noontime_ %>%
  rename(
    mean_t = mean_t_,       # Remove trailing underscore
    mean_vpd = mean_vpd_,   # Remove trailing underscore
    mean_swc = mean_swc_    # Remove trailing underscore
  )



Monthly_noontime_= sf_and_tower %>%
  group_by(Month,Year) %>%
  summarise(
    mean_t= mean (TA_1_4_1, na.rm=TRUE),
    mean_vpd= mean ((VPD_1_4_1)/10, na.rm=TRUE),
    mean_swc= mean (mean_swc_average, na.rm=TRUE),
    N= n())

Monthly_noontime_ <- na.omit(Monthly_noontime_)

Monthly_noontime_with_sd <- Monthly_noontime_ %>%
  mutate(
    sd_t = NA_real_,    # Add SD columns with NA
    sd_vpd = NA_real_,
    sd_swc = NA_real_
  )

Monthly_noontime_with_sd <- Monthly_noontime_ %>%
  mutate(
    sd_t = NA_real_,    # Add SD columns with NA
    sd_vpd = NA_real_,
    sd_swc = NA_real_
  )

# Ensure 'Year' is of type character in both datasets
Monthly_noontime_with_sd <- Monthly_noontime_with_sd %>%
  mutate(Year = as.character(Year))

multy_years_Monthly_noontime_ <- multy_years_Monthly_noontime_ %>%
  mutate(Year = as.character(Year))
combined_noontime_data <- bind_rows(Monthly_noontime_with_sd, multy_years_Monthly_noontime_)


Monthly_SWC_with_multy_year_avg <- ggplot(combined_noontime_data, aes(x = Month, y = mean_swc, color = as.factor(Year))) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = mean_swc - sd_swc, ymax = mean_swc + sd_swc, fill = as.factor(Year)), alpha = 0.2, color = NA) +  # No perimeter lines
  labs(x = "Month", y = "Volumetric soil moisture (%)", title = "") +
  scale_color_manual(values = c("Multi-Year Average" = "gray66" , alpha = 0.5, "2022" = "dodgerblue", "2023" = "darksalmon")) +  # Lighter gray for the line
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral", "Multi-Year Average" = "lightgray")) +  # Lighter gray for ribbon
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "none"
  )

print(Monthly_SWC_with_multy_year_avg)

ggsave(
  filename = "Monthly_SWC_with_multy_year_avg.tiff",
  plot = Monthly_SWC_with_multy_year_avg,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 5,         # Set width in inches
  height = 4,         # Set height in inches
  units = "in"        # Units for width/height
)



Monthly_vpd_with_multy_year_avg <- ggplot(combined_noontime_data, aes(x = Month, y = mean_vpd, color = as.factor(Year), group = Year)) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin = mean_vpd - sd_vpd, ymax = mean_vpd + sd_vpd, fill = as.factor(Year)), alpha = 0.2, color = NA) +
  labs(x = "Month", y = "VPD (kPa)", title = "") +
  scale_color_manual(values = c("Multi-Year Average"= "gray66", "2022"= "dodgerblue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "none",
    panel.border = element_blank()  # Removes the perimeter lines
  )


Monthly_vpd_with_multy_year_avg
ggsave(
  filename = "Monthly_vpd_with_multy_year_avg.tiff",
  plot = Monthly_vpd_with_multy_year_avg,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 5.5,         # Set width in inches
  height = 4,         # Set height in inches
  units = "in"        # Units for width/height
)


Monthly_temp_with_multy_year_avg <- ggplot(combined_noontime_data, aes(x = Month, y = mean_t , color = as.factor(Year), group = Year)) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin = mean_t  - sd_t , ymax = mean_t  + sd_t , fill = as.factor(Year)), alpha = 0.2, color = NA) +
  labs(x = "Month", y = "Temperature (\u00B0C)", title = "") +
  scale_color_manual(values = c("Multi-Year Average"= "gray66", "2022"= "dodgerblue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "none"
  )

Monthly_temp_with_multy_year_avg
ggsave(
  filename = "Monthly_temp_with_multy_year_avg.tiff",
  plot = Monthly_temp_with_multy_year_avg,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 5.5,         # Set width in inches
  height = 4,         # Set height in inches
  units = "in"        # Units for width/height
)



combined_plot_montky_with_multi_avg_sd_rain <- ( Monthly_Rain_with_multy_year_avg_se| Monthly_SWC_with_multy_year_avg) /
  (Monthly_vpd_with_multy_year_avg | Monthly_temp_with_multy_year_avg)



combined_plot_montky_with_multi_avg_sd_rain

ggsave(
  filename = "combined_plot_montLy_with_multi_avg_sd_rain.tiff",
  plot = combined_plot_montky_with_multi_avg_sd_rain,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 14,         # Set width in inches
  height = 10,         # Set height in inches
  units = "in"        # Units for width/height
)


