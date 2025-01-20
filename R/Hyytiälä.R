plot_hyytiälä <- function(raw.directory = "/home/joanna/Asiakirjat/Hyytiälä/",
                          download = F,
                          clean_data = F,
                          gapfill = F,
                          pdf.print = F) {
  if (download) {
    http.origin = "https://smear-backend.rahtiapp.fi/search/timeseries/csv?tablevariable=HYY_"
    for (variable in c(paste0("META.", c("RH672", "RH1250", "PAR", "CO2168", "T336", "T168", "Precip", "tsoil_5", "tsoil_10", "wsoil_B1", "Glob", "Pamb336")), "EDDY233.GPP")) {
      variable = paste0("META.", "Pamb0")
      from = "&from="
      year1 = seq(2017, 2022, by = 2)
      to = "-01-01T00%3A00%3A00.000&to="
      year2 = seq(2018, 2022, by = 2)
      end = "-12-31T23%3A59%3A59.999&quality=ANY&aggregation=NONE&interval=1"

      files = as.list(paste0(http.origin, variable, from, year1, to, year2, end))
      imported_file = as.list(paste0(raw.directory, variable, from, year1, "to", year2))

      mapply(download.file, files, imported_file)
    }
  }

  ###
  # Clean data
  ###
  if (clean_data) {
    # Read data
    RH672 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "RH672")), read.csv))
    RH1250 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "RH1250")), read.csv))
    PAR <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "PAR")), read.csv))
    CO2168 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "CO2168")), read.csv))
    T336 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "T336")), read.csv))
    T168 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "T168")), read.csv))
    Precip <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "Precip")), read.csv))
    tsoil_5 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "tsoil_5")), read.csv))
    tsoil_10 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "tsoil_10")), read.csv))
    wsoil_B1 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "wsoil_B1")), read.csv))
    Glob <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "Glob")), read.csv))
    Pamb336 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "Pamb336")), read.csv))
    Pamb0 <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "Pamb0")), read.csv))
    GPP <- dplyr::bind_rows(lapply(paste0(raw.directory, list.files(raw.directory, "GPP")), read.csv))


    GPP$YMD <- as.Date(paste(GPP$Year, GPP$Month, GPP$Day, sep = "-"), "%Y-%m-%e")
    GPP$HH <- NA
    GPP$HH[GPP$Minute < 30] <- 0
    GPP$HH[GPP$Minute >= 30] <- 30
    GPP$YMDHHH <- as.POSIXct(paste(GPP$Year, GPP$Month, GPP$Day, GPP$Hour, GPP$HH, sep = "-"), format = "%Y-%m-%e-%H-%M")
    all_data <- as.data.frame(cbind(RH672$Year, RH672$Month, RH672$Day, RH672$Hour, RH672$Minute, RH672$Second,
                                    RH672$HYY_META.RH672, RH1250$HYY_META.RH1250, PAR$HYY_META.PAR,
                                    CO2168$HYY_META.CO2168, T336$HYY_META.T336, T168$HYY_META.T168,
                                    Glob$HYY_META.Glob, Pamb336$HYY_META.Pamb336, Pamb0$HYY_META.Pamb0,
                                    Precip$HYY_META.Precip, tsoil_5$HYY_META.tsoil_5, tsoil_10$HYY_META.tsoil_10,
                                    wsoil_B1$HYY_META.wsoil_B1, GPP$HYY_EDDY233.GPP))
    names(all_data) <- c("Year", "Month", "Day", "Hour", "Minute", "Second",
                         "RH672", "RH1250", "PAR", "CO2168", "T336", "T168",
                         "Glob", "Pamb336", "Pamb0", "Precip",
                         "tsoil_5", "tsoil_10", "wsoil_B1", "GPP")

    # Make into daily values!
    all_data$YMD = as.Date(paste(all_data$Year, all_data$Month, all_data$Day, sep = "-"), "%Y-%m-%e")
    all_data$HH <- NA
    all_data$HH[all_data$Minute < 30] <- 0
    all_data$HH[all_data$Minute >= 30] <- 30
    all_data$YMDHHH = as.POSIXct(paste(all_data$Year, all_data$Month, all_data$Day, all_data$Hour, all_data$HH, sep = "-"), format = "%Y-%m-%e-%H-%M")
    all.daily = aggregate(cbind(RH672, RH1250, CO2168, T336, T168, tsoil_5, tsoil_10, wsoil_B1) ~ YMD, data = all_data, mean, na.rm = T, na.action = NULL)
    # µmol m⁻² s⁻¹
    all.daily$GPP = aggregate(HYY_EDDY233.GPP ~ YMD, data = GPP, mean, na.rm = T, na.action = NULL)$HYY_EDDY233.GPP
    all.daily.sum = aggregate(cbind(PAR, Precip) ~ YMD, data = all_data, sum, na.rm = T, na.action = NULL)
    # Here the data looks strange for the first two years, so made into NA values
    all.daily.sum$Precip[1:(365*2)] <- NA

    half.an.hour = aggregate(cbind(RH672, RH1250, CO2168, T336, T168, tsoil_5, tsoil_10, wsoil_B1, PAR, Glob, Pamb336, Pamb0) ~ YMDHHH, data = all_data, mean, na.rm = T, na.action = NULL)
    half.an.hour$GPP = aggregate(HYY_EDDY233.GPP ~ YMDHHH, data = GPP, mean, na.rm = T, na.action = NULL)$HYY_EDDY233.GPP
    half.an.hour.sum$Precip[1:(365*2*48)] <- NA

    # PAR data sorting
    PAR$Missing <-is.nan(PAR$HYY_META.PAR)
    PAR$YMD <- as.Date(paste(PAR$Year, PAR$Month, PAR$Day, sep = "-"))
    PAR$HH <- NA
    PAR$HH[PAR$Minute < 30] <- 0
    PAR$HH[PAR$Minute >= 30] <- 30
    PAR$YMDHHH <- as.POSIXct(paste(PAR$Year, PAR$Month, PAR$Day, PAR$Hour, PAR$HH, sep = "-"), format = "%Y-%m-%e-%H-%M")
    Daily_Missing <- aggregate(Missing~YMD, PAR, sum)
    HHourly_Missing <- aggregate(Missing~YMDHHH, PAR, sum)
    Daily_Missing$Percentage <- Daily_Missing$Missing/(24*60*60)
    HHourly_Missing$Percentage <- HHourly_Missing$Missing/(24*60*60)
    second_multiplier <- function(x) {
      multi <- (24 - 1) * (x) + 1
      return(multi)
    }
    # Unit correction, µmol m⁻² s⁻¹ to sum mmol m⁻² day-1
    all.daily.sum$PAR <- 0.000001 * 24 * second_multiplier(Daily_Missing$Percentage) * all.daily.sum$PAR
    all.daily.sum$PAR[Daily_Missing$Missing >= 0.75] <- NA

    all.gapfill <- merge(all.daily, all.daily.sum, all = T)
    all.hh.gapfill <- merge(half.an.hour, half.an.hour.sum, all = T)

    # Gap fill with the other level if possible
    all.gapfill$RH672[is.na(all.gapfill$RH672)] <- all.gapfill$RH1250[is.na(all.gapfill$RH672)] - mean(all.gapfill$RH1250, na.rm = T) + mean(all.gapfill$RH672, na.rm = T)
    all.gapfill$RH1250[is.na(all.gapfill$RH1250)] <- all.gapfill$RH672[is.na(all.gapfill$RH1250)] - mean(all.gapfill$RH672, na.rm = T) + mean(all.gapfill$RH1250, na.rm = T)
    all.hh.gapfill$RH672[is.na(all.hh.gapfill$RH672)] <- all.hh.gapfill$RH1250[is.na(all.hh.gapfill$RH672)] - mean(all.hh.gapfill$RH1250, na.rm = T) + mean(all.hh.gapfill$RH672, na.rm = T)
    all.hh.gapfill$RH1250[is.na(all.hh.gapfill$RH1250)] <- all.hh.gapfill$RH672[is.na(all.hh.gapfill$RH1250)] - mean(all.hh.gapfill$RH672, na.rm = T) + mean(all.hh.gapfill$RH1250, na.rm = T)
    all.hh.gapfill$Pamb336[is.na(all.hh.gapfill$Pamb336)] <- all.hh.gapfill$Pamb0[is.na(all.hh.gapfill$Pamb336)] - mean(all.hh.gapfill$Pamb0, na.rm = T) + mean(all.hh.gapfill$Pamb336, na.rm = T)
    all.hh.gapfill$Glob[is.na(all.hh.gapfill$Glob)] <- all.hh.gapfill$Glob67[is.na(all.hh.gapfill$Glob)] - mean(all.hh.gapfill$Glob67, na.rm = T) + mean(all.hh.gapfill$Glob, na.rm = T)
    all.hh.gapfill$Glob[is.na(all.hh.gapfill$Glob)] <- all.hh.gapfill$Globmast[is.na(all.hh.gapfill$Glob)] - mean(all.hh.gapfill$Globmast, na.rm = T) + mean(all.hh.gapfill$Glob, na.rm = T)
    all.hh.gapfill$Glob[is.na(all.hh.gapfill$Glob)] <- bigleaf::Rg.to.PPFD(all.hh.gapfill$PAR[is.na(all.hh.gapfill$Glob)]) - mean(bigleaf::Rg.to.PPFD(all.hh.gapfill$PAR), na.rm = T) + mean(all.hh.gapfill$Glob)
    all.hh.gapfill$PAR[is.na(all.hh.gapfill$PAR)] <- bigleaf::PPFD.to.Rg(all.hh.gapfill$Glob[is.na(all.hh.gapfill$PAR)]) - mean(bigleaf::PPFD.to.Rg(all.hh.gapfill$Glob), na.rm = T) + mean(all.hh.gapfill$PAR)


    # TODO: this was acidently deleted, so need to make sure that it still make sense, then make this into function

    # If the gap is smaller than 4 days apply linear interpolation - Pauliina
    all.gapfill[,-1] <- zoo::na.approx(all.gapfill[,-1], na.rm = F, maxgap = 6)
    all.hh.gapfill[,-1] <- zoo::na.approx(all.hh.gapfill[,-1], na.rm = F, maxgap = 6)

    save(all.gapfill, file = paste0(raw.directory, "Hyde_weather.RData"))
    save(all.hh.gapfill, file = paste0(raw.directory, "Hyde_weather_HH.RData"))
  }

  all.gapfill <- Tandem::loadRData(paste0(raw.directory, "Hyde_weather.RData"))
  all.hh.gapfill <- Tandem::loadRData(paste0(raw.directory, "Hyde_weather_HH.RData"))

  if (sum(is.na(all.gapfill[,c("RH672", "RH1250", "T336", "T168", "PAR", "Precip")])) == 0) {
    gapfill <- F
    warning("No need to gapfill, so skipped")
  }

  if (gapfill) {
    Precip_FMI <- read.csv(paste0(raw.directory, "csv-1f6dea7a-6001-49cc-9e13-90cbccdd3d82.csv"), dec = ".")
    names(Precip_FMI)[6] <- "Precip"
    Precip_FMI$Precip <- as.numeric(Precip_FMI$Precip)
    Precip_FMI$YMD <- as.Date(paste(Precip_FMI$Year, Precip_FMI$m, Precip_FMI$d, sep = "-"))
    Precip_FMI_daily <- aggregate(Precip ~ YMD, data = Precip_FMI, sum)
    Precip_FMI$Hour <- as.numeric(substring(Precip_FMI$Time, 1, 2))
    Precip_FMI$Minute <- 0

    Precip_FMI_HH <- Precip_FMI[,c("Year", "m", "d", "Hour")]
    Precip_FMI_HH <- Precip_FMI_HH[rep(1:nrow(Precip_FMI_HH),each=2),]
    Precip_FMI_HH$Minute <- rep(c(0, 30), length.out = nrow(Precip_FMI_HH))
    rownames(Precip_FMI_HH) <- 1:nrow(Precip_FMI_HH)
    Precip_FMI_HH <- merge(Precip_FMI_HH, Precip_FMI, all.x = T)

    all.gapfill$Precip[is.na(all.gapfill$Precip)] <- Precip_FMI_daily$Precip[is.na(all.gapfill$Precip)]
    all.hh.gapfill$Precip[is.na(all.hh.gapfill$Precip)] <- Precip_FMI_HH$Precip[is.na(all.hh.gapfill$Precip)]
    all.hh.gapfill$Precip <- zoo::na.approx(all.hh.gapfill$Precip, maxgap = 6, na.rm = F)

    # Saves to the raw data directory
    save(all.gapfill, file = paste0(raw.directory, "Hyde_weather.RData"))
    save(all.hh.gapfill, file = paste0(raw.directory, "Hyde_weather_HH.RData"))

    ###
    # Form needed for SPP
    ###

    spp_weather_input <- data.frame(year = lubridate::year(all.hh.gapfill$YMDHHH),
                                    month = lubridate::month(all.hh.gapfill$YMDHHH),
                                    day = lubridate::day(all.hh.gapfill$YMDHHH),
                                    hour = lubridate::hour(all.hh.gapfill$YMDHHH),
                                    minute =  lubridate::minute(all.hh.gapfill$YMDHHH),
                                    CO2 = rep(mean(all.hh.gapfill$CO2168, na.rm = T), length.out = nrow(all.hh.gapfill)), # ppm
                                    TotGlobal = all.hh.gapfill$Globmast, #
                                    TotPAR = all.hh.gapfill$PAR, # umol m-2 s-1
                                    TAir = all.hh.gapfill$T168, # C
                                    Precip = all.hh.gapfill$Precip, # mm
                                    Press = 0.1*all.hh.gapfill$Pamb336, #kPa
                                    VPD = bigleaf::rH.to.VPD(0.01*all.hh.gapfill$RH1250, all.hh.gapfill$T168), #kPa
                                    RH = all.hh.gapfill$RH1250) #%


    for (years in 2017:2022) {
      write.table(spp_weather_input[spp_weather_input$year == years,],
                  file = paste0("/home/joanna/Asiakirjat/SPP_Tandem/HyytialaData/HyytialaWeather", years, ".txt"),
                  sep = " ", col.names = F, row.names = F, na = "-9999", eol = "\r\n")
    }


  }

  ###
  # Run Preles
  ###
  Hyde_weather <- Tandem::loadRData(paste0(raw.directory, "Hyde_weather.RData"))
  Hyde_weather$CO2168[is.na(Hyde_weather$CO2168)] <- mean(Hyde_weather$CO2168, na.rm = T)
  PAR_average <- rep(aggregate(PAR ~ substring(YMD, 6, 11), data = Hyde_weather, mean, na.rm = T, na.action = NULL)$PAR, length.out = nrow(Hyde_weather))
  Hyde_weather$PAR[which(is.na(Hyde_weather$PAR))] <- PAR_average[which(is.na(Hyde_weather$PAR))]

  runs <- Rprebasso::PRELES(PAR = Hyde_weather$PAR,
                            TAir = Hyde_weather$T336,
                            Precip = Hyde_weather$Precip,
                            CO2 = Hyde_weather$CO2168,
                            VPD = bigleaf::rH.to.VPD(0.01*Hyde_weather$RH672, Hyde_weather$T336),
                            fAPAR = rep(0.7, length.out = length(Hyde_weather$CO2168)))
  preles_out <- data.frame(matrix(unlist(runs), ncol= 3))
  names(preles_out) <- c("GPP", "ET", "SWC")
  preles_out$Date <- seq(as.Date("2017-01-01"), as.Date("2022-12-31"), by = "day")

  LAI.files.all <- list.files(path = "/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/", pattern = "_D_R0_Output.out")[c(10, 14, 18, 26, 30, 34)]
  LAI.files.pine <- list.files(path = "/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/", pattern = "_SC1_D_R0_Output.out")[c(3, 4, 5, 7, 8, 9)]
  LAI.SPP.all <- lapply(paste0("/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/", LAI.files.all), read.delim, sep = "\t", dec = ",")
  LAI.SPP.pine <- lapply(paste0("/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/", LAI.files.pine), read.delim, sep = "\t", dec = ",")
  SPP.all.df <- do.call(rbind, LAI.SPP.all)

  real_fAPAR <- unlist(lapply(LAI.SPP.pine, function(x) {
    vector1 = x$Canopy_light_interception/x$PPFD
    vector1[vector1 > 0.9] <- mean(vector1, na.rm = T)
    vector1[vector1 < 0] <- mean(vector1, na.rm = T)
    vector1[is.na(vector1)] <- mean(vector1, na.rm = T)
    return(vector1)
    }))
  par(mfrow = c(1, 1))
  plot(real_fAPAR, ylab = "fAPAR", xlab = "Days Since 01.01.2017", main = "Graph to show the fAPAR from SPP")

  runs_2 <- Rprebasso::PRELES(PAR = Hyde_weather$PAR,
                            TAir = Hyde_weather$T336,
                            Precip = Hyde_weather$Precip,
                            CO2 = Hyde_weather$CO2168,
                            VPD = bigleaf::rH.to.VPD(0.01*Hyde_weather$RH672, Hyde_weather$T336),
                            fAPAR = real_fAPAR)
  preles_out$GPP_real_fAPAR <- runs_2[[1]]
  preles_out$ET_real_fAPAR <- runs_2[[2]]
  preles_out$SWC_real_fAPAR <- runs_2[[3]]

  preles_out$GPP_cumsum <- preles_out$ET_cumsum <- preles_out$SWC_cumsum <- NA
  for (y in 2017:2022) {
    preles_out$GPP_cumsum[lubridate::year(preles_out$Date) == y] <- cumsum(preles_out$GPP[lubridate::year(preles_out$Date) == y])
    preles_out$ET_cumsum[lubridate::year(preles_out$Date) == y] <- cumsum(preles_out$ET[lubridate::year(preles_out$Date) == y])
    preles_out$SWC_cumsum[lubridate::year(preles_out$Date) == y] <- cumsum(preles_out$SWC[lubridate::year(preles_out$Date) == y])
    preles_out$GPP_real_fAPAR_cumsum[lubridate::year(preles_out$Date) == y] <- cumsum(preles_out$GPP_real_fAPAR[lubridate::year(preles_out$Date) == y])
    preles_out$ET_real_fAPAR_cumsum[lubridate::year(preles_out$Date) == y] <- cumsum(preles_out$ET_real_fAPAR[lubridate::year(preles_out$Date) == y])
    preles_out$SWC_real_fAPAR_cumsum[lubridate::year(preles_out$Date) == y] <- cumsum(preles_out$SWC_real_fAPAR[lubridate::year(preles_out$Date) == y])
  }

  ###
  # Plot
  ###
  if (pdf.print) {
    save(file = paste0(raw.directory, "preles_out_hyde.pdf"))
  }

  #####
  # To see the difference between stand density
  #####

  original.stand = read.delim("/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/Hyytiala_Stand_1304_2018_SC1_D_R0_Output.out", sep = "\t", dec = ",")
  thinned.stand = read.delim("/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/Hyytiala_Stand_522_2018_SC1_D_R0_Output.out", sep = "\t", dec = ",")
  LAI.2018.changed.stand = read.delim("/home/joanna/Asiakirjat/SPP_Tandem/HyytialaResults/Hyytiala_2018_SC1_D_R0_Output.out", sep = "\t", dec = ",")

  # LAI of 5 is 75/74
  # LAI of 5.5 is 78/77
  # LAI of 4 is 69/68

  # TODO: redo with the 2018 values as the year looks more normal
  par(mfrow = c(1, 1))
  plot(original.stand$Canopy_light_interception/original.stand$PPFD, type = "l", col = "black",
       xlab = "Days of the Year", ylab = "fAPAR, %", main = "Difference in Stand 2018", ylim = c(0.1, 0.7))
  lines(thinned.stand$Canopy_light_interception/thinned.stand$PPFD, col = "blue")
  lines((522/1304)*original.stand$Canopy_light_interception/original.stand$PPFD, col = "green")
  lines(LAI.2018.changed.stand$Canopy_light_interception/original.stand$PPFD, col = "pink")
  legend(100, 0.175, c(1304, 522, "1304 multiplied by 522/1304"), col = c("black", "blue", "green"), title = "Stand Density", bty = "n", lty = 1)

  mean(LAI.2018.changed.stand$Canopy_light_interception/original.stand$Canopy_light_interception, na.rm = T)
  median(LAI.2018.changed.stand$Canopy_light_interception/original.stand$Canopy_light_interception, na.rm = T)

  #####
  # Constraint LAI
  #####

  par(mfrow = c(2, 2))
  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "GPP, fAPAR assumed 0.7", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(min(preles_out$GPP), max(preles_out$GPP)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$GPP[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])
  legend(0, max(preles_out$GPP), 2017:2022, Tandem::colours_cb_2(7)[c(1:5, 7)], bty = "n")

  plot(NULL, ylim = c(-6, 6), xlim = c(0, 365), ylab = "Difference in GPP compared to measured", xlab = "Days of the Year")
  for (y in 2017:2022) points(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$GPP[lubridate::year(preles_out$Date) == y] - Hyde_weather$GPP[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, xlim = c(1, 365), ylim = c(min(preles_out$ET), max(preles_out$ET)),
       main = "ET, fAPAR assumed 0.7", ylab = "ET", xlab = "Days of the Year", type = "p", pch = 16)
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$ET[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, xlim = c(1, 365), ylim = c(min(preles_out$SWC), max(preles_out$SWC)),
       main = "SWC, fAPAR assumed 0.7", ylab = "SWC", xlab = "Days of the Year", type = "p", pch = 16)
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$SWC[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  par(mfrow = c(3, 1))
  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "GPP, fAPAR assumed 0.7", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(0, max(preles_out$GPP_cumsum)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$GPP_cumsum[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])
  legend(0, 850, 2017:2022, Tandem::colours_cb_2(7)[c(1:5, 7)], bty = "n")

  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "ET, fAPAR assumed 0.7", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(0, max(preles_out$ET_cumsum)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$ET_cumsum[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "SWC, fAPAR assumed 0.7", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(0, max(preles_out$SWC_cumsum)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$SWC_cumsum[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  par(mfrow = c(3, 2))
  for (y in 2017:2022) {
    plot(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
         preles_out$GPP[lubridate::year(preles_out$Date) == y],
         col = Tandem::colours_cb_2(7)[c(1:5, 7)][y-2016],
         main = paste("GPP, fAPAR assumed 0.7,", y), ylab = "GPP", xlab = "Days of the Year",
         ylim = c(0, max(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y], preles_out$GPP[lubridate::year(preles_out$Date) == y])),
         type = "p", pch = 16, xlim = c(0, 365))
    lines(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y])
    legend(0, max(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y]), y, Tandem::colours_cb_2(7)[c(1:5, 7)][y-2016], bty = "n")
  }

  #####
  # SPP LAI
  #####

  par(mfrow = c(2, 2))
  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "GPP, fAPAR SPP", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(min(preles_out$GPP_real_fAPAR), max(preles_out$GPP_real_fAPAR)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$GPP_real_fAPAR[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])
  legend(0, max(preles_out$GPP_real_fAPAR), 2017:2022, Tandem::colours_cb_2(7)[c(1:5, 7)], bty = "n")

  plot(NULL, ylim = c(-6, 6), xlim = c(0, 365), ylab = "Difference in GPP compared to measured", xlab = "Days of the Year")
  for (y in 2017:2022) points(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                              preles_out$GPP_real_fAPAR[lubridate::year(preles_out$Date) == y] - Hyde_weather$GPP[lubridate::year(preles_out$Date) == y],
                              col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, xlim = c(1, 365), ylim = c(min(preles_out$ET_real_fAPAR), max(preles_out$ET_real_fAPAR)),
       main = "ET, fAPAR SPP", ylab = "ET", xlab = "Days of the Year", type = "p", pch = 16)
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$ET_real_fAPAR[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, xlim = c(1, 365), ylim = c(min(preles_out$SWC_real_fAPAR), max(preles_out$SWC_real_fAPAR)),
       main = "SWC, fAPAR SPP", ylab = "SWC", xlab = "Days of the Year", type = "p", pch = 16)
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$SWC_real_fAPAR[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  par(mfrow = c(3, 2))
  for (y in 2017:2022) {
    plot(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
         preles_out$GPP_real_fAPAR[lubridate::year(preles_out$Date) == y],
         col = Tandem::colours_cb_2(7)[c(1:5, 7)][y-2016],
         main = paste("GPP, fAPAR SPP,", y), ylab = "GPP", xlab = "Days of the Year",
         ylim = c(0, max(Hyde_weather$GPP_real_fAPAR[lubridate::year(preles_out$Date) == y], Hyde_weather$GPP[lubridate::year(preles_out$Date) == y])),
         type = "p", pch = 16, xlim = c(0, 365))
    lines(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y])
    legend(0, max(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y]), y, Tandem::colours_cb_2(7)[c(1:5, 7)][y-2016], bty = "n")
  }

  par(mfrow = c(3, 1))
  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "GPP, fAPAR GPP", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(0, max(preles_out$GPP_real_fAPAR_cumsum)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$GPP_real_fAPAR_cumsum[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])
  legend(0, 850, 2017:2022, Tandem::colours_cb_2(7)[c(1:5, 7)], bty = "n")

  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "ET, fAPAR GPP", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(0, max(preles_out$ET_real_fAPAR_cumsum)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$ET_real_fAPAR_cumsum[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "SWC, fAPAR GPP", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = c(0, max(preles_out$SWC_real_fAPAR_cumsum)), xlim = c(0, 365))
  for (y in 2017:2022) lines(lubridate::yday(preles_out$Date[lubridate::year(preles_out$Date) == y]),
                             preles_out$SWC_real_fAPAR_cumsum[lubridate::year(preles_out$Date) == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  ####
  # SPP results
  ####

  par(mfrow = c(2, 2))
  plot(NULL, col = Tandem::colours_cb_2(7)[as.factor(lubridate::year(preles_out$Date))],
       main = "GPP, SPP all levels", ylab = "GPP", xlab = "Days of the Year", type = "p", pch = 16,
       ylim = 12*c(min(SPP.all.df$Photosynthesis), max(SPP.all.df$Photosynthesis)), xlim = c(0, 365))
  for (y in 2017:2022) lines(SPP.all.df$DoY[SPP.all.df$Year == y],
                             12*SPP.all.df$Photosynthesis[SPP.all.df$Year == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])
  legend(0, max(12*SPP.all.df$Photosynthesis), 2017:2022, Tandem::colours_cb_2(7)[c(1:5, 7)], bty = "n")

  plot(NULL, ylim = c(-6, 6), xlim = c(0, 365), ylab = "Difference in GPP compared to measured", xlab = "Days of the Year")
  for (y in 2017:2022) points(SPP.all.df$DoY[SPP.all.df$Year == y],
                              12*SPP.all.df$Photosynthesis[SPP.all.df$Year == y] - Hyde_weather$GPP[lubridate::year(preles_out$Date) == y],
                              col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, xlim = c(1, 365), ylim = c(min(SPP.all.df$Transpiration), max(SPP.all.df$Transpiration)),
       main = "ET, SPP all levels", ylab = "ET", xlab = "Days of the Year", type = "p", pch = 16)
  for (y in 2017:2022) lines(SPP.all.df$DoY[SPP.all.df$Year == y],
                             SPP.all.df$Transpiration[SPP.all.df$Year == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  plot(NULL, xlim = c(1, 365), ylim = c(min(SPP.all.df$Modelled_SWC), max(SPP.all.df$Modelled_SWC)),
       main = "SWC, SPP all levels", ylab = "SWC", xlab = "Days of the Year", type = "p", pch = 16)
  for (y in 2017:2022) lines(SPP.all.df$DoY[SPP.all.df$Year == y],
                             SPP.all.df$Modelled_SWC[SPP.all.df$Year == y],
                             col = Tandem::colours_cb_2(7)[c(1:5, 7)][if (y < 2022) y-2016 else y-2015])

  par(mfrow = c(3, 2))
  for (y in 2017:2022) {
    plot(SPP.all.df$DoY[SPP.all.df$Year == y],
         12*SPP.all.df$Photosynthesis[SPP.all.df$Year == y],
         col = Tandem::colours_cb_2(7)[c(1:5, 7)][y-2016],
         main = paste("GPP, SPP all levels", y), ylab = "GPP", xlab = "Days of the Year",
         ylim = c(0, max(Hyde_weather$GPP_real_fAPAR[lubridate::year(preles_out$Date) == y], Hyde_weather$GPP[lubridate::year(preles_out$Date) == y])),
         type = "p", pch = 16, xlim = c(0, 365))
    lines(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y])
    legend(0, max(Hyde_weather$GPP[lubridate::year(preles_out$Date) == y]), y, Tandem::colours_cb_2(7)[c(1:5, 7)][y-2016], bty = "n")
  }

  if (pdf.print) {
    dev.off()
  }

}
