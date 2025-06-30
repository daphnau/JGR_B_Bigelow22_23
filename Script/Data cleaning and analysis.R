####### Good Luck#######
setwd("C:/Users/dafna/Desktop/For Github/this is it/Data")
dir()

library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(patchwork)
library(ggpubr)
library(segmented)

##sap flow ### 2022###
source("C:/Users/dafna/Desktop/For Github/this is it/Data/source/round.POSIXct.R")

custom_headers_ <- c("time", "Average_SWP",  "Average_PP","Average_DF") 
sf_data_2022 <- read.csv('MB_SapFlow_SpeciesAverage_2022.csv', header = FALSE, col.names = custom_headers_, sep = ',')

#####changing column 1 to date and time####
doy<- floor(sf_data_2022$time)
min_fraction<-sf_data_2022$time-doy
min_minutes<-round((min_fraction*24)*60, digits = 0)
hrs <- as.integer(floor(min_minutes/60))
mins <- min_minutes %% 60
time_stamp<- paste0(hrs,":",mins, ":00")
date_stamp<- as.Date(x= (doy-1),origin = "2022-01-01")
sf_data_2022$date_time<-ymd_hms(paste0(date_stamp," ", time_stamp))
sf_data_2022$date_time_round<- round.POSIXct(sf_data_2022$date_time,"30 min")

##sap flow### 2023
sf_data_2023 <- read.csv('MB_SapFlow_SpeciesAverage_2023.csv', header = FALSE, col.names = custom_headers_, sep = ',')
#####changing column 1 to date and time####
doy<- floor(sf_data_2023$time)
min_fraction<-sf_data_2023$time-doy
min_minutes<-round((min_fraction*24)*60, digits = 0)
hrs <- as.integer(floor(min_minutes/60))
mins <- min_minutes %% 60
time_stamp<- paste0(hrs,":",mins, ":00")
date_stamp<- as.Date(x= (doy-1),origin = "2023-01-01")
sf_data_2023$date_time<-ymd_hms(paste0(date_stamp," ", time_stamp))
sf_data_2023$date_time_round<- round.POSIXct(sf_data_2023$date_time,"30 min")

###marging 2022 2023###
sf_data <- bind_rows(sf_data_2022, sf_data_2023)

column_index <- which(names(sf_data) == "date_time_round")

names(sf_data)[column_index] <- "Time"

###AVARAGE SAPFLOW FOT THREE SP.###
sf_data<- sf_data%>%
  mutate(mean_sap_flow = rowMeans(dplyr::select(., Average_SWP, Average_PP, Average_DF ), na.rm = TRUE))

##########################################################################################

####TOWER DATA####

tower_data_2022 <- read.csv('GapfilledPartitionedFluxes_US-Mtb_HH_202201010000_202301010000.csv', sep = ',',
                            na.strings = c("-9999", "-9999.000"))
tower_data_2023 <- read.csv('GapfilledPartitionedFluxes_US-Mtb_HH_202301010000_202401010000.csv', sep = ',',
                            na.strings = c("-9999", "-9999.000"))
tower_data_2022$TIMESTAMP_START <- ymd_hm(tower_data_2022$TIMESTAMP_START)
tower_data_2023$TIMESTAMP_START <- ymd_hm(tower_data_2023$TIMESTAMP_START)      

tower_data_2022$TA_1_3_1[!is.finite(tower_data_2022$TA_1_3_1) | tower_data_2022$TA_1_3_1 == -9999.000] <- NA
tower_data_2023[tower_data_2023 == -9999.000] <- NA

tower_data<- bind_rows(tower_data_2022, tower_data_2023)
column_index <- which(names(tower_data) == "TIMESTAMP_START")

names(tower_data)[column_index] <- "Time"
#############################################################################################

tower_data$top_soil_swc_N <- rowMeans(tower_data[, c("SWC_1_1_1", "SWC_1_2_1", "SWC_1_3_1")], na.rm = TRUE)
tower_data$top_soil_swc_E <- rowMeans(tower_data[, c("SWC_2_1_1", "SWC_2_2_1", "SWC_2_3_1")], na.rm = TRUE)
tower_data$top_soil_swc_S <- rowMeans(tower_data[, c("SWC_3_1_1", "SWC_3_2_1", "SWC_3_3_1")], na.rm = TRUE)

tower_data<- tower_data%>%
  mutate(mean_swc_average = rowMeans(dplyr::select(., top_soil_swc_N, top_soil_swc_E, top_soil_swc_S ), na.rm = TRUE))


latent_heat_of_vaporization_J_per_kg <- 2.45 * 10^6  # Approximately 2.45 MJ/kg

# Calculate ET based on LE
tower_data$ET <- tower_data$LE / latent_heat_of_vaporization_J_per_kg



tower_data_selected<- tower_data %>% dplyr::select(Time, TA_1_3_1, TA_1_4_1,RH_1_3_1, RH_1_4_1,VPD_1_3_1,
                                                   VPD_1_4_1,PPFD_IN,P_1_1_1,P_2_1_1,mean_swc_average,
                                                   GPP,ET,USTAR)


####Thermal###
thermal_cleaning <- read.csv('thermal_data.csv', sep = ','   )    

thermal_cleaning$Year <- lubridate::year(thermal_cleaning$datetime)
thermal_cleaning$Month <- lubridate::month(thermal_cleaning$datetime)
thermal_cleaning$Week <-lubridate:: week(thermal_cleaning$datetime)
thermal_cleaning$doy <- lubridate::yday(thermal_cleaning$datetime)
thermal_cleaning$Hour <- format(thermal_cleaning$thermal_cleaning, "%H:%M:%S")

#Filter rain###
high_rain_days <- tower_data[tower_data$P_2_1_1 > 2, ]

high_rain_days_mo <- high_rain_days %>%
  filter(month(Time) >= 5 & month(Time) <= 10)

# Extract just the date (if your column has both date and time)
high_rain_days_mo$Date_only <- as.Date(high_rain_days_mo$Time)

# Keep only the first occurrence for each unique date
unique_high_rain_days <- high_rain_days_mo[!duplicated(high_rain_days_mo$Date_only), ]

thermal_cleaning <- thermal_cleaning %>%
  mutate(Date_only = as.Date(datetime ))

filtered_rain_data <- thermal_cleaning %>%
  filter(!Date_only %in% unique_high_rain_days$Date_only)

###filtering 10-14####

filtered_rain_data_10_14 <- filtered_rain_data %>%
  filter(hour(datetime) %in% 10:14 )
####filter wind speed####

thermal_data_with_wind_data <- filtered_rain_data %>%
  group_by(Month) %>%
  mutate(
    mean_ustar = mean(USTAR, na.rm = TRUE),
    sd_ustar = sd(USTAR, na.rm = TRUE),
    exceeds_1sd = ifelse(USTAR > mean_ustar + 1 * sd_ustar, TRUE, FALSE),
    exceeds_2sd = ifelse(USTAR > mean_ustar + 2 * sd_ustar, TRUE, FALSE)  # Flag rows exceeding 1 SD# Flag rows exceeding 1 SD
  ) %>%
  ungroup()


thermal_without_rain_and_high_wind_1sd <- thermal_data_with_wind_data %>%
  filter(exceeds_1sd == FALSE)

#########
###multi-year flux tower data###
###multi year analysis####

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

##############################################################################3

###level 2 - tables combintaions and data analysis for paper figures###

###combin##
sf_and_tower <- inner_join(sf_data, tower_data, by = "Time")
sf_and_tower$Time <- ymd_hms(sf_and_tower$Time)

# Create new columns for year, month, week, day, and hour
sf_and_tower$Year <- year(sf_and_tower$Time)
sf_and_tower$Month <- month(sf_and_tower$Time)
sf_and_tower$Week <- week(sf_and_tower$Time)
sf_and_tower$date <- format(sf_and_tower$Time, "%Y-%m-%d") 
sf_and_tower$doy <- yday(sf_and_tower$Time)
sf_and_tower$hour <- format(sf_and_tower$Time, "%H:%M:%S") 


sf_and_tower_daytime<- sf_and_tower %>%
  filter(hour >= "06:00:00" & hour <= "18:00:00")

sf_and_tower_noontime<- sf_and_tower %>%
  filter(hour >= "11:00:00" & hour <= "14:00:00")

###FILTERING GROWING SEASON###
sf_and_tower_mo<- sf_and_tower[sf_and_tower$Month >= 5 & sf_and_tower$Month <= 10, ]
sf_and_tower_daytime_mo<- sf_and_tower_mo %>%
  filter(hour >= "06:00:00" & hour <= "18:00:00")

sf_and_tower_noontime_mo<- sf_and_tower_mo %>%
  filter(hour >= "11:00:00" & hour <= "14:00:00")


##########weekly noontime means###################

WEEKLY_noontime_MO= sf_and_tower_noontime_mo %>%
  group_by(Week,Year) %>%
  summarise(mean_t= mean(TA_1_4_1, na.rm=TRUE),
            mean_vpd= (mean (VPD_1_4_1, na.rm=TRUE))/10,
            mean_swc= mean(mean_swc_average, na.rm=TRUE),
            mean_gpp= mean(GPP,na.rm=TRUE),
            mean_ET= mean(ET, na.rm=TRUE),
            #mean_WUE=mean(WUE, na.rm=TRUE),
            mean_NEE= mean(NEE, na.rm=TRUE),
            mean_ppfd=mean(PPFD_IN, na.rm=TRUE),
            mean_SWP= mean(Average_SWP, na.rm= TRUE),
            mean_PP= mean(Average_PP, na.rm= TRUE),
            mean_DF= mean(Average_DF, na.rm= TRUE),
            mean_3sap= mean(mean_sap_flow, na.rm= TRUE),
            N= n(),
            se_t = sd(TA_1_4_1, na.rm = TRUE) / sqrt(N),  
            se_vpd = sd(VPD_1_4_1, na.rm = TRUE) / sqrt(N),
            se_GPP = sd(GPP, na.rm = TRUE) / sqrt(N),
            se_swc=sd(mean_swc_average, na.rm = TRUE) / sqrt(N),
            se_ET=sd(ET, na.rm = TRUE) / sqrt(N),
            se_SWP= sd(Average_SWP, na.rm= TRUE) / sqrt(N),
            se_PP= sd(Average_PP, na.rm= TRUE) / sqrt(N),
            se_DF= sd(Average_DF, na.rm= TRUE) / sqrt(N),
            se_3sap= sd(mean_sap_flow, na.rm= TRUE) / sqrt(N),
            se_ppfd= sd(PPFD_IN, na.rm= TRUE) / sqrt(N))

#####
################################################################
####
###fig 2###
# Filter for 2022 only
data_2022 <- WEEKLY_noontime_MO %>% filter(Year == 2022)

# Convert to long format for the three species and their SEs
species_long <- data_2022 %>%
  pivot_longer(cols = c(mean_SWP, mean_PP, mean_DF),
               names_to = "Species",
               values_to = "Flow") %>%
  pivot_longer(cols = c(se_SWP, se_PP, se_DF),
               names_to = "SE_Species",
               values_to = "SE") %>%
  mutate(Species = factor(Species, levels = c("mean_SWP", "mean_PP", "mean_DF")),
         SE_Species = recode(SE_Species, 
                             se_SWP = "mean_SWP", 
                             se_PP = "mean_PP", 
                             se_DF = "mean_DF")) 

# Plot
fig_2a=ggplot(species_long, aes(x = Week, y = Flow, color = Species)) +
  geom_line(size = 1) +
  
  # Add error ribbon (SE) for each species (without legend)
  geom_ribbon(data = species_long, 
              aes(x = Week, ymin = Flow - SE*1.96, ymax = Flow + SE*1.96, fill = Species),
              alpha = 0.3, inherit.aes = FALSE, show.legend = FALSE) +
  
  # Add soil moisture ribbon (blue, no legend)
  geom_ribbon(data = data_2022, 
              aes(x = Week, ymin = 0, ymax = mean_swc), 
              fill = "dodgerblue", alpha = 0.25, inherit.aes = FALSE, show.legend = FALSE) +
  
  labs(
    x = "",
    y = expression(Sap~flow~velocity~(cm~hr^{-1}))
  ) +
  
  scale_color_manual(values = c(
    "mean_SWP" = "forestgreen", 
    "mean_PP" = "darkorange", 
    "mean_DF" = "purple"
  ),
  labels = c(
    "mean_SWP" = expression(italic("Pinus strobiformis")), 
    "mean_PP" = expression(italic("Pinus ponderosa")), 
    "mean_DF" = expression(italic("Pseudotsuga menziesii"))
  )) +
  
  scale_fill_manual(values = c(
    "mean_SWP" = "forestgreen", 
    "mean_PP" = "darkorange", 
    "mean_DF" = "purple"
  )) +
  
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
  ) +
  
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, color= "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 14, face = "italic"),
    legend.position = "none",
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks.x = element_line(size = 0.8)
  )
fig_2a
#####
# Filter for 2023 only
data_2023 <- WEEKLY_noontime_MO %>% filter(Year == 2023)

# Convert to long format for the three species and their SEs
species_long_23 <- data_2023 %>%
  pivot_longer(cols = c(mean_SWP, mean_PP, mean_DF),
               names_to = "Species",
               values_to = "Flow") %>%
  pivot_longer(cols = c(se_SWP, se_PP, se_DF),
               names_to = "SE_Species",
               values_to = "SE") %>%
  mutate(Species = factor(Species, levels = c("mean_SWP", "mean_PP", "mean_DF")),
         SE_Species = recode(SE_Species, 
                             se_SWP = "mean_SWP", 
                             se_PP = "mean_PP", 
                             se_DF = "mean_DF"))

# Plot
fig_2b= ggplot(species_long_23, aes(x = Week, y = Flow, color = Species)) +
  geom_line(size = 1) +
  
  # Add error ribbon (SE) for each species (without legend)
  geom_ribbon(data = species_long_23, 
              aes(x = Week, ymin = Flow - SE*1.96, ymax = Flow + SE*1.96, fill = Species),
              alpha = 0.3, inherit.aes = FALSE, show.legend = FALSE) +
  
  # Add soil moisture ribbon (blue, no legend)
  geom_ribbon(data = data_2023, 
              aes(x = Week, ymin = 0, ymax = mean_swc), 
              fill = "dodgerblue", alpha = 0.25, inherit.aes = FALSE, show.legend = FALSE) +
  
  labs(
    x = "",
    y = expression(Sap~flow~velocity~(cm~hr^{-1}))
  ) +
  
  scale_color_manual(values = c(
    "mean_SWP" = "forestgreen", 
    "mean_PP" = "darkorange", 
    "mean_DF" = "purple"
  ),
  labels = c(
    "mean_SWP" = expression(italic("Pinus strobiformis")), 
    "mean_PP" = expression(italic("Pinus ponderosa")), 
    "mean_DF" = expression(italic("Pseudotsuga menziesii"))
  )) +
  
  scale_fill_manual(values = c(
    "mean_SWP" = "forestgreen", 
    "mean_PP" = "darkorange", 
    "mean_DF" = "purple"
  )) +
  
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
  ) +
  scale_y_continuous(
    limits = c(0, 17)  # Set the y-axis range from 0 to 15
  ) +
  
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, color= "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 14, face = "italic"),
    legend.position = "none",
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks.x = element_line(size = 0.8)
  )
fig_2b
#####
####boxplot for 2022 2023####

#subset_stats <- cbind(data, date, years, weeks, doy)
sf_and_tower_noontime_mo_22=subset(sf_and_tower_noontime_mo, Year==2022)
sf_and_tower_noontime_mo_23=subset(sf_and_tower_noontime_mo, Year==2023)

sf_and_tower_noontime_mo_22 <- sf_and_tower_noontime_mo_22 %>%
  mutate(Season = case_when(
    (Week >= 18 & Week <= 25) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (Week >= 26 & Week <= 40) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (Week >= 41 & Week <= 44) ~ "Post-monsoon",
    TRUE ~ "Other"  # Assigns "Other" to any Week value not covered above
  ))

sf_and_tower_noontime_mo_23 <- sf_and_tower_noontime_mo_23 %>%
  mutate(Season = case_when(
    (Week >= 18 & Week <= 28) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (Week >= 29 & Week <= 40) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (Week >= 41 & Week <= 44) ~ "Post-monsoon"
  ))

sf_and_tower_noontime_mo_22_23 <- rbind(sf_and_tower_noontime_mo_22, sf_and_tower_noontime_mo_23)


sf_and_tower_noontime_mo_22_23$Season <- factor(sf_and_tower_noontime_mo_22_23$Season,
                                                levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))

# Make sure Year is treated as a factor
sf_and_tower_noontime_mo_22_23$Year <- as.factor(sf_and_tower_noontime_mo_22_23$Year)


# Create the boxplot
fig_2c= ggplot(sf_and_tower_noontime_mo_22_23, aes(x = Season, y = mean_sap_flow, fill = Year)) +
  geom_boxplot() +
  scale_fill_manual(values = c("2022" = "dodgerblue", "2023" = "darksalmon")) +
  labs(title = "",
       x = "Season",
       y = expression(Sap~flow~velocity~(cm~hr^{-1})),
       fill = "Year") +
  scale_y_continuous(
    limits = c(0, 10)  # Set the y-axis range from 0 to 15
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, color= "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 14, face = "italic"),
    legend.position = "right"
    #  axis.ticks.length = unit(-0.2, "cm"),
    # axis.ticks.x = element_line(size = 0.8)
  )

fig_2c
New_fig2_aprl <- fig_2a | fig_2b | fig_2c

New_fig2_aprl

ggsave(
  filename = "New_fig2_aprl.tiff",
  plot = New_fig2_aprl,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 12,         # Set width in inches
  height = 5,         # Set height in inches
  units = "in"        # Units for width/height
)

####Fig 3####
### diurnal for eeach sp.### 
# Create new columns for year, month, week, day, and hour
sf_and_tower_daytime_mo$Year <- year(sf_and_tower_daytime_mo$Time)
sf_and_tower_daytime_mo$Month <- month(sf_and_tower_daytime_mo$Time)
sf_and_tower_daytime_mo$Week <- week(sf_and_tower_daytime_mo$Time)
sf_and_tower_daytime_mo$date <- format(sf_and_tower_daytime_mo$Time, "%Y-%m-%d") 
sf_and_tower_daytime_mo$doy <- yday(sf_and_tower_daytime_mo$Time)
sf_and_tower_daytime_mo$hour <- format(sf_and_tower_daytime_mo$Time, "%H:%M:%S") 

#subset_stats <- cbind(data, date, years, weeks, doy)
sf_and_tower_daytime_mo_22=subset(sf_and_tower_daytime_mo, Year==2022)
sf_and_tower_daytime_mo_23=subset(sf_and_tower_daytime_mo, Year==2023)



sf_and_tower_daytime_mo_22 <- sf_and_tower_daytime_mo_22 %>%
  mutate(Season = case_when(
    (Week >= 18 & Week <= 25) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (Week >= 26 & Week <= 40) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (Week >= 41 & Week <= 44) ~ "Post-monsoon",
    TRUE ~ "Other"  # Assigns "Other" to any Week value not covered above
  ))

sf_and_tower_daytime_mo_23 <- sf_and_tower_daytime_mo_23 %>%
  mutate(Season = case_when(
    (Week >= 18 & Week <= 28) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (Week >= 29 & Week <= 40) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (Week >= 41 & Week <= 44) ~ "Post-monsoon"
  ))

sf_and_tower_daytime_mo_22_23 <- rbind(sf_and_tower_daytime_mo_22, sf_and_tower_daytime_mo_23)

#
sapflow_diurnal_mo= sf_and_tower_daytime_mo_22_23  %>%
  group_by(hour,Season,Year) %>%
  summarise(mean_SWP= mean(Average_SWP, na.rm= TRUE),
            mean_PP= mean(Average_PP, na.rm= TRUE),
            mean_DF= mean(Average_DF, na.rm= TRUE),
            mean_3sap= mean(mean_sap_flow, na.rm= TRUE),
            N= n(),
            se_SWP= sd(Average_SWP, na.rm= TRUE) / sqrt(N),
            se_PP= sd(Average_PP, na.rm= TRUE) / sqrt(N),
            se_DF= sd(Average_DF, na.rm= TRUE) / sqrt(N),
            se_3sap= sd(mean_sap_flow, na.rm= TRUE) / sqrt(N))

#####

sapflow_diurnal_mo$Season <- factor(sapflow_diurnal_mo$Season, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))

diurnal_swp<- ggplot(sapflow_diurnal_mo, aes(x = hour, y = mean_SWP, color = as.factor(Year),group = interaction(Year, Season))) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin = mean_SWP - 1.96*se_SWP, ymax =  mean_SWP + 1.96*se_SWP, fill = as.factor(Year)), alpha = 0.5) +
  #labs(x = "Time of Day", y = "Sapflow (cm/hour)", title = "Southwestern white pine ") +
  labs(
    x = "",
    y = expression(Vs~(cm~hr^{-1})),
    title = "Pinus strobiformis"
  )+
  scale_color_manual(values = c("2022"= "dodgerblue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  facet_wrap(~Season, scales = "free_x") +  # Add facet_wrap here
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),  # Rotate x-axis text
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    strip.text = element_blank(),  
    plot.title = element_text(size = 14, face = "italic"),
    axis.text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_discrete(
    breaks = c("00:00:00", "03:00:00", "06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00"),
    labels = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00")
  )

diurnal_swp


diurnal_pp<- ggplot(sapflow_diurnal_mo, aes(x = hour, y = mean_PP, color = as.factor(Year),group = interaction(Year, Season))) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin = mean_PP - 1.96*se_PP, ymax =  mean_PP + 1.96*se_PP, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "",  y = expression(Vs(cm~hr^{-1})), title = "Pinus ponderosa") +  scale_color_manual(values = c("2022"= "dodgerblue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  facet_wrap(~Season, scales = "free_x") +  # Add facet_wrap here
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),  # Rotate x-axis text
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    strip.text = element_blank(),  
    plot.title = element_text(size = 14, face = "italic"),
    axis.text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_discrete(
    breaks = c("00:00:00", "03:00:00", "06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00"),
    labels = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00")
  )

diurnal_pp

diurnal_df<- ggplot(sapflow_diurnal_mo, aes(x = hour, y = mean_DF, color = as.factor(Year),group = interaction(Year, Season))) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin = mean_DF - 1.96*se_DF, ymax =  mean_DF + 1.96*se_DF, fill = as.factor(Year)), alpha = 0.5) +
  #labs(x = "Time of Day", y = "Sapflow (cm/hour)", title = "Douglas fir ") +
  labs(x = "",  y = expression(Vs~(cm~hr^{-1})), title = "Pseudotsuga menziesii") +  scale_color_manual(values = c("2022"= "dodgerblue", "2023"= "darksalmon")) +
  scale_color_manual(values = c("2022"= "dodgerblue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  facet_wrap(~Season, scales = "free_x") +  # Add facet_wrap here
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, angle = 60, hjust = 1),  # Rotate x-axis text
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_blank(),  
    plot.title = element_text(size = 14, face = "italic"),
    axis.text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_discrete(
    breaks = c("00:00:00", "03:00:00", "06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00"),
    labels = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00")
  )

diurnal_df
sf_diurnal_plot <- diurnal_swp / diurnal_pp / diurnal_df

ggsave(
  filename = "sf_diurnal_plot_1.96SE.tiff",
  plot = sf_diurnal_plot,
  device = "tiff",
  dpi = 300,    
  width = 8,    
  height = 8,   
  units = "in"   
)

##########FIGURE 4 CORRLTIONS####
dir()
r2_table<-read.csv('corr_table.csv', sep = ','   )  
swc_data <- r2_table %>% filter(relation == "swc")
vpd_data <- r2_table %>% filter(relation == "vpd")
custom_colors <- c("2022" = "skyblue2", "2023" = "darksalmon")
swc_data$Season <- factor(swc_data$Season, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))
vpd_data$Season <- factor(vpd_data$Season, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))
swc_data$species <- factor(swc_data$species, levels = c("Pinus strobiformis", "Pinus ponderosa", "Pseudotsuga menziesii", "Three Sp. Mean"))
vpd_data$species <- factor(vpd_data$species, levels = c("Pinus strobiformis", "Pinus ponderosa", "Pseudotsuga menziesii", "Three Sp. Mean"))

# Plot for relation = "swc"
swc_plot <- ggplot(swc_data, aes(x = species, y = r2, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(r2, 2), y = r2 + 0.05),  # Adjust label position dynamically
            position = position_dodge(width = 0.9),  # Correct position of dodged bars
            vjust = -0.5, size = 4) +  # Adjust vjust for correct label placement
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~ Season) +
  labs(
    title = "",
    x = "Species",
    y = "R² (Sap Flow ~ SWC)",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_blank(),       # Remove x-axis text (species names)
    axis.title.x = element_blank(),      # Remove x-axis title
    axis.ticks.x = element_blank(),      # Remove x-axis ticks
    axis.title.y = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
swc_plot

# Plot for relation = "vpd"
vpd_plot <- ggplot(vpd_data, aes(x = species, y = r2, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(r2, 2), y = r2 + 0.05),  # Adjust label position dynamically
            position = position_dodge(width = 0.9),  # Correct position of dodged bars
            vjust = -0.5, size = 4) +  # Adjust vjust for correct label placement
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~ Season) +
  labs(
    title = "",
    x = "Species",
    y = "R² (Sap Flow ~ VPD)",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_blank(),        # Remove facet titles
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

vpd_plot

r2_PLOT <- swc_plot /
  vpd_plot

r2_PLOT

ggsave(
  filename = "r2_PLOT.tiff",
  plot = r2_PLOT,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 12,         # Set width in inches
  height = 10,         # Set height in inches
  units = "in"        # Units for width/height
)


####Figure 5####
library(segmented)

###Daily means of sap flow, swc,vpd

daily_mean_MO_22_23= sf_and_tower_daytime_mo %>%
  group_by(doy,Year) %>%
  summarise(mean_vpd= (mean (VPD_1_4_1, na.rm=TRUE))/10,
            mean_swc= mean(mean_swc_average, na.rm=TRUE),
            mean_gpp= mean(GPP,na.rm=TRUE),
            mean_ET= mean(ET, na.rm=TRUE),
            mean_NEE= mean(NEE, na.rm=TRUE),
            mean_ppfd=mean(PPFD_IN, na.rm=TRUE),
            mean_SWP= mean(Average_SWP, na.rm= TRUE),
            mean_PP= mean(Average_PP, na.rm= TRUE),
            mean_DF= mean(Average_DF, na.rm= TRUE),
            mean_3sap= mean(mean_sap_flow, na.rm= TRUE),
            N= n(),
            se_t = sd(TA_1_4_1, na.rm = TRUE) / sqrt(N),  
            se_vpd = sd(VPD_1_4_1, na.rm = TRUE) / sqrt(N),
            se_GPP = sd(GPP, na.rm = TRUE) / sqrt(N),
            se_swc=sd(mean_swc_average, na.rm = TRUE) / sqrt(N),
            se_ET=sd(ET, na.rm = TRUE) / sqrt(N),
            se_SWP= sd(Average_SWP, na.rm= TRUE) / sqrt(N),
            se_PP= sd(Average_PP, na.rm= TRUE) / sqrt(N),
            se_DF= sd(Average_DF, na.rm= TRUE) / sqrt(N),
            se_3sap= sd(mean_sap_flow, na.rm= TRUE) / sqrt(N),
            se_ppfd= sd(PPFD_IN, na.rm= TRUE) / sqrt(N))


lin_mod <- lm(mean_3sap ~ mean_swc, data = daily_mean_MO_22_23)

seg_mod <- segmented(lin_mod, seg.Z = ~mean_swc, psi = 10)
summary(seg_mod)

breakpoint <- seg_mod$psi[1, "Est."]
print(breakpoint)
daily_mean_MO_22_23$seg_fit <- predict(seg_mod)


# Create predicted values from segmented model
daily_mean_MO_22_23$seg_fit <- predict(seg_mod)

# Plot with piecewise trend
FIG_SWC_VPD_SEG <- ggplot(daily_mean_MO_22_23, aes(x = mean_swc, y = mean_3sap)) +
  geom_point(color="gray66", alpha= 0.8) +
  geom_line(aes(y = seg_fit), color = "firebrick", size = 1.2) +  # Add segmented trend line
  labs(x = "Volumetric soil moisture (%)", y = expression(Sap~flow~velocity~(cm~hr^{-1}))) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)
  ) +
  ylim(0, 8) +
  scale_x_continuous(limits= c(5,20), breaks = seq(5,20,5))

FIG_SWC_VPD_SEG

ggsave(
  filename = "FIG_SWC_VPD_SEG.tiff",
  plot =  FIG_SWC_VPD_SEG,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 8,         # Set width in inches
  height = 4,         # Set height in inches
  units = "in"        # Units for width/height
)


##############Fig 5 B c###
####################
############################

data_below_6_5 <- subset(daily_mean_MO_22_23, mean_swc < 6.8)
data_above_6_5 <- subset(daily_mean_MO_22_23, mean_swc > 6.8)

# Plot sapflow vs VPD for SWC < 6.5
glm_model <- glm(mean_3sap ~ mean_vpd, data = data_below_6_5, family = gaussian())
summary(glm_model)

plot_below_6_5 <- ggplot(data_below_6_5, aes(x = mean_vpd, y = mean_3sap)) +
  geom_point(color = "darkorange4", alpha=0.3) +
  geom_smooth(method = "glm", color = "darkorange4") +  # Add a smoothing line
  labs(x = "VPD (kPa)", y = expression(Sap~flow~velocity~(cm~hr^{-1})), title = "(SWC < 6.8 %)") +
  theme_minimal() +
  theme(
    # legend.title = element_text(size = 12),
    #legend.text = element_text(size = 10),
    strip.text = element_blank(),        # Remove facet titles
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)
  )+
  ylim(0, 6) +  # Set y-axis limits
  xlim(0, 2.5)    # Set x-axis limits

plot_below_6_5
# Plot sapflow vs VPD for SWC > 6.5
glm_model <- glm(mean_3sap ~ mean_vpd, data = data_above_6_5, family = gaussian())
summary(glm_model)

plot_above_6_5 <- ggplot(data_above_6_5, aes(x = mean_vpd, y = mean_3sap)) +
  geom_point(color = "blue2", alpha=0.3) +
  geom_smooth(method = "glm", color = "blue2") +  # Add a smoothing line
  labs(x = "VPD (kPa)", y = "Sap Flow", title = " (SWC > 6.8 %)") +
  theme_minimal() +
  theme(
    # legend.title = element_text(size = 12),
    #legend.text = element_text(size = 10),
    strip.text = element_blank(),        # Remove facet titles
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)
  )+
  ylim(0, 6) +  # Set y-axis limits
  xlim(0, 2.5) 

plot_above_6_5


Fig_swc_vpd_new<- (plot_below_6_5  |plot_above_6_5) 
Fig_swc_vpd_new
ggsave(
  filename = " Fig_swc_vpd_new.tiff",
  plot =  Fig_swc_vpd_new,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 8,         # Set width in inches
  height = 4,         # Set height in inches
  units = "in"        # Units for width/height
)


###fig 6 remote sensing sap flow####
data<-read.csv('Weekly_means_sapflow_tower_thermal.csv', sep = ','   )  

# Format dates and add to dataframe
years <- format(data$Year, format = "%Y")
weeks <- format(data$Week, format = "%w")
date <- as.Date(paste(years,weeks,1,sep=""),"%Y%U%u")
doy <- yday(date)

# add to main data frame
subset_stats <- cbind(data, date, years, weeks, doy)
subset_stats_2022=subset(subset_stats, Year.x==2022)
subset_stats_2023=subset(subset_stats, Year.x==2023)

###adding Season###


subset_stats_2022 <- subset_stats_2022 %>%
  mutate(Season = case_when(
    (Week.x >= 18 & Week.x <= 25) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (Week.x >= 26 & Week.x<= 40) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (Week.x>= 41 & Week.x<= 44) ~ "Post-monsoon",
    TRUE ~ "Other"  # Assigns "Other" to any Week value not covered above
  ))

subset_stats_2023 <- subset_stats_2023 %>%
  mutate(Season = case_when(
    (Week.x >= 18 & Week.x <= 28) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (Week.x >= 29 & Week.x <= 40) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (Week.x >= 41 & Week.x <= 44) ~ "Post-monsoon"
  ))



#### Plotting Sap Flow v Delta T
#### Figure DeltaT Sap Flow 2022 timeseries 

Fig_22_c <- ggplot(subset_stats_2022, aes(x=Week.x)) + 
  geom_line(aes(y=scale(tdiff_daf_8m)),linewidth=1.5,linetype="solid") +
  geom_line(aes(y=scale(mean_3sap)),linewidth=1.5,linetype="twodash") +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov") # Custom labels
  ) +
  # scale_color_manual(values = c("Pre-Monsoon" = "#E69536", 
  #                              "Monsoon" = "#5B8DB8", 
  #                             "Post-Monsoon" = "#66A76F"))+
  
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 18)  # Increase facet wrap text size
  ) +
  ylab("Z-Score") + # add a y-axis label
  xlab("Month in 2022") + # add a x-axis label
  annotate("text", x = 22, y = 2, label = "ΔT (Solid)", hjust = 0, size = 5)+
  annotate("text", x = 22, y = 1.5, label = "Sap Flow (Dashed)", hjust = 0, size = 5) 

Fig_22_c

Fig_23_c <- ggplot(subset_stats_2023, aes(x=Week.x)) + 
  geom_line(aes(y=scale(tdiff_daf_8m)),linewidth=1.5,linetype="solid") +
  geom_line(aes(y=scale(mean_3sap)),linewidth=1.5,linetype="dashed") +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov") # Custom labels
  ) +
  # scale_color_manual(values = c("Pre-Monsoon" = "#E69536", 
  #                              "Monsoon" = "#5B8DB8", 
  #                             "Post-Monsoon" = "#66A76F"))+
  
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 18)  # Increase facet wrap text size
  ) +
  ylab("Z-Score")  # add a y-axis label
# xlab("Month in 2023")  # add a x-axis label
#annotate("text", x = 22, y = 2, label = "DeltaT (Solid)", hjust = 0, size = 5) +
#annotate("text", x = 22, y = 1.5, label = "Sap Flow (Dashed)", hjust = 0, size = 5) 

Fig_23_c


SF_DELTAT_<- (Fig_22_c |Fig_23_c) 
SF_DELTAT_
ggsave(
  filename = "SF_DELTAT_.tiff",
  plot = SF_DELTAT_,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 10,         # Set width in inches
  height = 4,         # Set height in inches
  units = "in"        # Units for width/height
)

###################
#########################################
###### TABLE 1#######



####TABLE 1 R 2022 ###

response_variables <- c("mean_3sap", "mean_gpp", "mean_ET", "mean_t", "mean_vpd", "mean_swc")
proxy_variables <- c("tdiff_daf_8m" )

correlation_results <- data.frame(
  season = character(),
  response_variable = character(),
  proxy_variable = character(),
  correlation = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each season, response variable, and proxy variable
for (season in unique(subset_stats_2022$Season)) {
  for (response in response_variables) {
    for (proxy in proxy_variables) {
      
      # Subset data for the specific season
      season_data <- subset_stats_2022 %>% filter(Season == season)
      
      # Remove rows with NA values for the current response and proxy pair
      season_data <- na.omit(season_data[, c(response, proxy)])
      
      if (nrow(season_data) > 1) {  # Ensure there are enough data points to calculate correlation
        # Calculate correlation (r)
        correlation_value <- cor(season_data[[response]], season_data[[proxy]])
        
        # Append the results to the data frame
        correlation_results <- rbind(correlation_results, data.frame(
          season = season,
          response_variable = response,
          proxy_variable = proxy,
          correlation = correlation_value
        ))
      }
    }
  }
}

# View the resulting table
print(correlation_results)




#####
####TABLE 1 R 2023 ###

response_variables <- c("mean_3sap", "mean_gpp", "mean_ET", "mean_t", "mean_vpd", "mean_swc")
proxy_variables <- c("tdiff_daf_8m" )

correlation_results <- data.frame(
  season = character(),
  response_variable = character(),
  proxy_variable = character(),
  correlation = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each season, response variable, and proxy variable
for (season in unique(subset_stats_2023$Season)) {
  for (response in response_variables) {
    for (proxy in proxy_variables) {
      
      # Subset data for the specific season
      season_data <- subset_stats_2023 %>% filter(Season == season)
      
      # Remove rows with NA values for the current response and proxy pair
      season_data <- na.omit(season_data[, c(response, proxy)])
      
      if (nrow(season_data) > 1) {  # Ensure there are enough data points to calculate correlation
        # Calculate correlation (r)
        correlation_value <- cor(season_data[[response]], season_data[[proxy]])
        
        # Append the results to the data frame
        correlation_results <- rbind(correlation_results, data.frame(
          season = season,
          response_variable = response,
          proxy_variable = proxy,
          correlation = correlation_value
        ))
      }
    }
  }
}

# View the resulting table
print(correlation_results)


###########################
###################
###fig S1 ET AND GPP####

Weekly_GPP <- ggplot(WEEKLY_noontime_MO, aes(x = Week, y = mean_gpp, color = as.factor(Year))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_gpp - 1.96*se_GPP, ymax = mean_gpp + 1.96*se_GPP, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "Month",y = "GPP         ", title = "") +  # Removed the y-axis title from `labs()`
  scale_color_manual(values = c("2022" = "dodgerblue", "2023" = "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) +
  # scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov"), # Custom labels
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, color= "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 14, face = "italic"),
    legend.position = "none",
    axis.ticks.length = unit(-0.2, "cm"), # Ensures ticks are visible
    axis.ticks.x = element_line(size = 0.8) # Thickens x-axis ticks
  )

Weekly_GPP


Weekly_ET <- ggplot(WEEKLY_noontime_MO, aes(x = Week, y = (mean_ET)*3600*24, color = as.factor(Year))) +
  geom_line() +
  geom_ribbon(aes(ymin = (mean_ET)*3600*24 - (1.96*se_ET)*3600*24, ymax = (mean_ET)*3600*24 + (1.96*se_ET)*3600*24, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "Month",y = "ET         ", title = "") +  # Removed the y-axis title from `labs()`
  scale_color_manual(values = c("2022" = "dodgerblue", "2023" = "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) +
  # scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov"), # Custom labels
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, color= "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 14, face = "italic"),
    legend.position = "none",
    axis.ticks.length = unit(-0.2, "cm"), # Ensures ticks are visible
    axis.ticks.x = element_line(size = 0.8) # Thickens x-axis ticks
  )

Weekly_ET



Weekly_3SAP <- ggplot(WEEKLY_noontime_MO, aes(x = Week, y = mean_3sap, color = as.factor(Year))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_3sap - 1.96*se_3sap, ymax = mean_3sap + 1.96*se_3sap, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "Month",y = "Sap Flow (cm/hour)", title = "") +  # Removed the y-axis title from `labs()`
  scale_color_manual(values = c("2022" = "dodgerblue", "2023" = "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) +
  # scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov"), # Custom labels
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, color= "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 14, face = "italic"),
    legend.position = "none",
    axis.ticks.length = unit(-0.2, "cm"), # Ensures ticks are visible
    axis.ticks.x = element_line(size = 0.8) # Thickens x-axis ticks
  )

Weekly_3SAP

combined_plot_S1 <- Weekly_ET / Weekly_GPP / Weekly_3SAP

combined_plot_S1

ggsave(
  filename = "combined_plot_S1_new_SE.tiff",
  plot = combined_plot_S1,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 10,         # Set width in inches
  height = 9,         # Set height in inches
  units = "in"        # Units for width/height
)

########################################3
###fig 1 2024-2021###########

###muly year avg ##RAIN###

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

#####good job!!####
