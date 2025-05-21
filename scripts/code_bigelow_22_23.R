###work###
setwd("C:/Users/Daphnau/Box/Post-doc/Bigelow/ORIGINAL DATA/for paper")
dir()
##home###
setwd("C:/Users/dafna/Box/Post-doc/Bigelow/ORIGINAL DATA/for paper")

#####
setwd("C:/Users/dafna/Desktop/JGR/Data")

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

##thermal##
thermal_rain_and_wind_filter_ad<-read.csv('thermal_data.csv', sep = ','   )   
thermal_ad_22_23 <- thermal_rain_and_wind_filter_ad %>%
  select(Median_All, roi_temp_mean, tdiff_median_MOS_16m, tdiff_roi_DAF_16m, datetime) %>%
  mutate(Time = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

###sapflow###
sapflow_data<-read.csv('sf_data_22_23.csv', sep = ','   )   
###tower###
####TOWER DATA####
tower_data<-read.csv('tower_data_22_23.csv', sep = ','   )   

###combin##
sf_and_tower <- inner_join(sapflow_data, tower_data, by = "Time")
sf_and_tower$Time <- ymd_hms(sf_and_tower$Time)

sf_and_tower_thremal <- inner_join(sf_and_tower, thermal_ad_22_23, by = "Time")

sf_and_tower_thremal$Time <- ymd_hms(sf_and_tower_thremal$Time) 

#sf_and_tower_thremal_hyper<- inner_join(sf_and_tower_thremal, NDVI_PRI, by = "Time")


###sapflow####
##################################################

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

sf_and_tower_mo<- sf_and_tower[sf_and_tower$Month >= 5 & sf_and_tower$Month <= 10, ]
sf_and_tower_daytime_mo<- sf_and_tower_mo %>%
  filter(hour >= "06:00:00" & hour <= "18:00:00")

#sf_flux_mo<- sf_flux[sf_flux$Month >= 5 & sf_flux$Month <= 10, ]
sf_and_tower_noontime_mo<- sf_and_tower_mo %>%
  filter(hour >= "11:00:00" & hour <= "14:00:00")


##########sap flow###################

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

##########corrlations####################

daily_noontime_mo= sf_and_tower_daytime_mo_22_23 %>%
  group_by(doy,Season,Year) %>%
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
daily_noontime_mo$Season <- factor(daily_noontime_mo$Season, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))

##swc###

# Calculate p-values for correlations
correlation_results <- daily_noontime_mo %>%
  group_by(Season, Year) %>%
  summarize(
    cor = cor(mean_swc, mean_3sap, use = "complete.obs"),
    p_value = cor.test(mean_swc, mean_3sap)$p.value
  ) %>%
  ungroup()

# Filter significant correlations for drawing regression lines
significant_data <- daily_noontime_mo %>%
  inner_join(correlation_results %>% filter(p_value < 0.05), by = c("Season", "Year"))

# Plot
swc_sapflow <- ggplot(daily_noontime_mo, aes(x = mean_swc, y = mean_3sap)) +
  # Points for all data
  geom_point(aes(color = as.factor(Year)), size = 5, alpha = 0.6) +
  
  # Regression lines for significant correlations
  geom_smooth(
    data = significant_data,
    aes(color = as.factor(Year)),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    size = 1.5
  ) +
  
  # Add correlation statistics
  stat_cor(
    aes(color = as.factor(Year)),
    size = 6,
    label.sep = ", ",
    p.accuracy = 0.001, # Rounds p-values for better readability
    r.accuracy = 0.01,  # Rounds R values for better readability
    label.x.npc = "left", # Align labels to the left side
    label.y.npc = 0.9  ,   # Align labels to the upper area
    fontface = "bold"   
  ) +
  
  scale_y_continuous(limits = c(0, 10)) +
  # Custom color scheme
  scale_color_manual(values = c("2023" = "darksalmon", "2022" = "dodgerblue")) +
  
  # Labels
  labs(x = "Volumetric soil moisture (%)", y = "Sap Flow (cm/hr)") +
  
  # Themes
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    axis.ticks = element_line(linewidth = 0.8),  # Updated argument
    axis.text = element_text(size = 18)
  )+
  
  
  # Facets by season
  facet_wrap(~Season, scales = "fixed")

swc_sapflow

####vpd####

# Calculate correlation and filter for significant results
correlation_results_vpd <- daily_noontime_mo %>%
  group_by(Season, Year) %>%
  summarize(
    cor = cor(mean_vpd, mean_3sap, use = "complete.obs"),
    p_value = cor.test(mean_vpd, mean_3sap)$p.value
  ) %>%
  ungroup()

# Filter significant correlations for drawing regression lines
significant_data_vpd <- daily_noontime_mo %>%
  inner_join(correlation_results_vpd %>% filter(p_value < 0.05), by = c("Season", "Year"))

# Plot
VPD_sapflow <- ggplot(daily_noontime_mo, aes(x = mean_vpd, y = mean_3sap)) +
  # Points for all data
  geom_point(aes(color = as.factor(Year)), size = 5, alpha = 0.6) +
  
  # Regression lines for significant correlations
  geom_smooth(
    data = significant_data_vpd,
    aes(color = as.factor(Year)),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    size = 1.5
  ) +
  
  # Add correlation statistics
  stat_cor(
    aes(color = as.factor(Year)),
    size = 6,
    label.sep = ", ",
    p.accuracy = 0.001, # Rounds p-values for better readability
    r.accuracy = 0.01,  # Rounds R values for better readability
    label.x.npc = "left", # Align labels to the left side
    label.y.npc = 0.9,    # Align labels to the upper area
    fontface = "bold"     # Bold the correlation text
  ) +
  
  # Set y-axis limits to create more space
  scale_y_continuous(limits = c(0, 10)) +
  
  # Custom color scheme
  scale_color_manual(values = c("2023" = "darksalmon", "2022" = "dodgerblue")) +
  
  # Labels
  labs(x = "VPD (kPa)", y = "Sap Flow (cm/hr)") +
  
  # Themes
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.text = element_blank(),  # Remove facet titles
    axis.ticks = element_line(size = 0.8),
    axis.text = element_text(size = 18)
  ) +
  
  # Facets by season
  facet_wrap(~Season, scales = "fixed")

VPD_sapflow

corrlations <- swc_sapflow / VPD_sapflow 

corrlations


ggsave(
  filename = "corrlations.tiff",
  plot = corrlations,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 10,         # Set width in inches
  height = 8,         # Set height in inches
  units = "in"        # Units for width/height
)


####Figure 5####
library(segmented)
lin_mod <- lm(mean_3sap ~ mean_swc, data = daily_noontime_mo)

seg_mod <- segmented(lin_mod, seg.Z = ~mean_swc, psi = 10)
summary(seg_mod)

breakpoint <- seg_mod$psi[1, "Est."]
print(breakpoint)
daily_noontime_mo$seg_fit <- predict(seg_mod)


# Create predicted values from segmented model
daily_noontime_mo$seg_fit <- predict(seg_mod)

# Plot with piecewise trend
FIG_SWC_VPD_SEG <- ggplot(daily_noontime_mo, aes(x = mean_swc, y = mean_3sap)) +
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
  ylim(0, 6) +
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

data_below_6_5 <- subset(daily_noontime_mo, mean_swc < 6.8)
data_above_6_5 <- subset(daily_noontime_mo, mean_swc > 6.8)

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


Fig_swc_vpd_new

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
###########figure  6 remote sensing#####

data<-read.csv('weekly_11-14_remote_sensing.csv', sep = ','   )   

# Format dates and add to dataframe
years <- format(data$Year, format = "%Y")
weeks <- format(data$Week, format = "%w")
date <- as.Date(paste(years,weeks,1,sep=""),"%Y%U%u")
doy <- yday(date)

# add to main data frame
subset_stats <- cbind(data, date, years, weeks, doy)
subset_stats_2022=subset(subset_stats, Year==2022)
subset_stats_2023=subset(subset_stats, Year==2023)




#### Plotting Sap Flow v Delta T
#### Figure DeltaT Sap Flow 2022 timeseries 
Fig_22_c <- ggplot(subset_stats_2022, aes(x=Week)) + 
  geom_line(aes(y=scale(tdiff_daf_8m),color=Season),linewidth=2,linetype="solid") +
  geom_line(aes(y=scale(mean_3sap),color=Season),linewidth=2,linetype="dashed") +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov") # Custom labels
  ) +
  scale_color_manual(values = c("Pre-Monsoon" = "#E69536", 
                                "Monsoon" = "#5B8DB8", 
                                "Post-Monsoon" = "#66A76F"))+
  
  
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

Fig_23_c <- ggplot(subset_stats_2023, aes(x=Week)) + 
  geom_line(aes(y=scale(tdiff_daf_8m),color=Season),linewidth=2,linetype="solid") +
  geom_line(aes(y=scale(mean_3sap),color=Season),linewidth=2,linetype="dashed") +
  scale_x_continuous(
    breaks = c(18, 23, 27, 31, 36, 40, 44),  # Major ticks for weeks and months
    labels = c("May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov") # Custom labels
  ) +
  scale_color_manual(values = c("Pre-Monsoon" = "#E69536", 
                                "Monsoon" = "#5B8DB8", 
                                "Post-Monsoon" = "#66A76F"))+
  
  
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



##############################################
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

combined_plot_S1 <- Weekly_ET | Weekly_GPP | Weekly_3SAP

combined_plot_S1

ggsave(
  filename = "combined_plot_S1_new_SE.tiff",
  plot = combined_plot_S1,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 12,         # Set width in inches
  height = 5.5,         # Set height in inches
  units = "in"        # Units for width/height
)

###fig (S2) r2###
dir()
r2_table<-read.csv('r2table.csv', sep = ','   )  
swc_data <- r2_table %>% filter(relation == "swc")
vpd_data <- r2_table %>% filter(relation == "vpd")
custom_colors <- c("2022" = "skyblue2", "2023" = "darksalmon")
swc_data$Season <- factor(swc_data$Season, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))
vpd_data$Season <- factor(vpd_data$Season, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))
swc_data$species <- factor(swc_data$species, levels = c("Pinus strobiformis", "Pinus ponderosa", "Pseudotsuga menziesii", "Three Sp. Mean"))
vpd_data$species <- factor(vpd_data$species, levels = c("Pinus strobiformis", "Pinus ponderosa", "Pseudotsuga menziesii", "Three Sp. Mean"))
# Plot for relation = "swc"
swc_plot <- ggplot(swc_data, aes(x = species, y = r2, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
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
vpd_plot <- ggplot(vpd_data, aes(x = species, y = r2, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
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
# Print the plots
print(swc_plot)
print(vpd_plot)

r2_PLOT <- swc_plot /
  vpd_plot

r2_PLOT

ggsave(
  filename = "r2_PLOT.tiff",
  plot = r2_PLOT,
  device = "tiff",
  dpi = 300,          # High resolution (dots per inch)
  width = 10,         # Set width in inches
  height = 8,         # Set height in inches
  units = "in"        # Units for width/height
)

