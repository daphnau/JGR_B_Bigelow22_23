###multi-year flux tower data###



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
