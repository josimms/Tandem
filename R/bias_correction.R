####
# Make the data averaged over the history for the rest of the dataset
####

bias_correction <- function(dataset_cmip, source_id, ssp_scenario, plot = T, PAR.plot = F) {
  ###
  # Load data
  ###
  bias_correction <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  load(paste0(bias_correction, "preles_norunda_weather.RData"))
  load(paste0(bias_correction, "preles_wust_weather.RData"))
  load(paste0(bias_correction, "kun.weather.preles.RData"))
  load(paste0(bias_correction, "stkat.weather.preles.RData"))

  ###
  # Site data formatting
  ###
  mean.temp.kun.weather <- kun.weather.preles # PAR correct!
  mean.temp.kun.weather$DoY <- c(1:365, 1:366, 1:365, 1:365, 1:365, 1:366)
  mean.kun.weather <- aggregate(cbind(TAir, VPD, PAR, CO2, Precip, fAPAR)~DoY, mean.temp.kun.weather, mean)
  mean.kun.weather <- mean.kun.weather[-60,] # 29th Feb removed

  mean.temp.nor.weather <- preles_norunda_weather[366:2557,] # PAR correct!
  mean.temp.nor.weather$DoY <- c(1:365, 1:366, 1:365, 1:365, 1:365, 1:366)
  mean.nor.weather <- aggregate(cbind(TAir, VPD, PAR, CO2, Precip, fAPAR)~DoY, mean.temp.nor.weather, mean)
  mean.nor.weather <- mean.nor.weather[-60,] # 29th Feb removed

  ###
  # CMIP Data formatting
  ###
  ### DATE CONDITIONS! No leap years!
  dataset_cmip$date <- as.Date(dataset_cmip$date)
  dataset_cmip <- dataset_cmip[!find_leap(dataset_cmip$date),] # Leap year days removed!
  dataset_cmip_temp <- dataset_cmip[substring(dataset_cmip$date, 1, 4) <= 2020,] # Years lower than 2020 for the historical corrections
  dataset_cmip_temp$DoY <- rep(1:365, length.out = nrow(dataset_cmip_temp))
  ndays = 365
  mean.dataset_cmip <- aggregate(cbind(TAir, VPD, PAR, Precip)~DoY, dataset_cmip_temp, mean) # Baseline created

  ### Difference between // mean(a) - mean(b) = mean(a-b)
  kun_corrections.year <- mean.dataset_cmip[,c("TAir", "VPD", "PAR", "Precip")] - mean.kun.weather[,c("TAir", "VPD", "PAR", "Precip")]
  kun_corrections.year$DoY = 1:ndays
  nor_corrections.year <- mean.dataset_cmip[,c("TAir", "VPD", "PAR", "Precip")] - mean.nor.weather[,c("TAir", "VPD", "PAR", "Precip")]
  nor_corrections.year$DoY = 1:ndays

  ###
  # PAR corrections
  ###
  kun_corrections.year$DoY = nor_corrections.year$DoY = 1:ndays
  kun_corrections.year_pattern <- aggregate(PAR ~ plyr::round_any(DoY, 5, f = ceiling), data = kun_corrections.year[,c("DoY", "PAR")], max)
  nor_corrections.year_pattern <- aggregate(PAR ~ plyr::round_any(DoY, 5, f = ceiling), data = nor_corrections.year[,c("DoY", "PAR")], max)
  names(kun_corrections.year_pattern)[1] <- names(nor_corrections.year_pattern)[1] <- "DoY"

  fun <- function(corrections.year_pattern, par, optimisation = T) {
    PAR <- sum(abs(abs(corrections.year_pattern$PAR) - par[3]*dnorm(corrections.year_pattern$DoY, par[1], par[2])))
  }

  kun_o = optim(par = c(183, 15, 30), fun, corrections.year_pattern = kun_corrections.year_pattern)
  kun_params = kun_o$par
  nor_o = optim(par = c(183, 15, 30), fun, corrections.year_pattern = nor_corrections.year_pattern)
  nor_params = nor_o$par
  if (PAR.plot == T) {
    par(mfrow = c(1, 2))
    plot(kun_corrections.year$PAR)
    points(kun_corrections.year_pattern$DoY, kun_corrections.year_pattern$PAR, pch = 16, col = "blue")
    lines(1:ndays, kun_params[3]*dnorm(1:ndays, kun_params[1], kun_params[2]), col = "blue")

    plot(nor_corrections.year$PAR) # TODO: why are these the same?
    points(nor_corrections.year_pattern$DoY, nor_corrections.year_pattern$PAR, pch = 16, col = "blue")
    lines(1:ndays, nor_params[3]*dnorm(1:ndays, nor_params[1], nor_params[2]), col = "blue")
  }

  # Add to corrections
  kun_corrections.year$PAR.smooth <- kun_params[3]*dnorm(1:ndays, kun_params[1], kun_params[2])
  nor_corrections.year$PAR.smooth <- nor_params[3]*dnorm(1:ndays, nor_params[1], nor_params[2])

  # Make long enough
  kun_corrections <- NULL
  nor_corrections <- NULL
  for (rep in lubridate::year(dataset_cmip$date[1]):lubridate::year(dataset_cmip$date[length(dataset_cmip$date)])) {
    nor_corrections <- rbind(nor_corrections, nor_corrections.year)
    kun_corrections <- rbind(kun_corrections, kun_corrections.year)
  }
  nor_corrections <- nor_corrections[1:length(dataset_cmip$date),]
  kun_corrections <- kun_corrections[1:length(dataset_cmip$date),]

  ###
  # Difference applied over future
  ###
  kun_dataset_out = dataset_cmip[,c("TAir", "VPD", "PAR", "Precip")] - kun_corrections[,c("TAir", "VPD", "PAR.smooth", "Precip")]
  nor_dataset_out = dataset_cmip[,c("TAir", "VPD", "PAR", "Precip")] - nor_corrections[,c("TAir", "VPD", "PAR.smooth", "Precip")]

  ###
  # Zero values
  ###
  kun_dataset_out$VPD[kun_dataset_out$VPD < 0] <- 0
  nor_dataset_out$VPD[nor_dataset_out$VPD < 0] <- 0

  kun_dataset_out$Precip[kun_dataset_out$Precip < 0] <- 0
  nor_dataset_out$Precip[nor_dataset_out$Precip < 0] <- 0

  kun_dataset_out$PAR[kun_dataset_out$PAR < 0] <- 0
  nor_dataset_out$PAR[nor_dataset_out$PAR < 0] <- 0
  # TODO: PAR values are still a cloud in the middle of the season, mend this!

  ###
  # fAPAR: Assuming growth is not wildly affected the fAPAR is assumed to be the same as now
  ###
  kun_fAPAR_leap <- aggregate(kun.weather.preles$fAPAR ~ substring(kun.weather.preles$Date, 5, 8), data = kun.weather.preles, mean)[,2]
  kun_fAPAR <- kun_fAPAR_leap[-60]
  norunda_fAPAR_leap <- aggregate(fAPAR ~ substring(Date, 5, 8), data = preles_norunda_weather[366:2557,], mean)[,2]
  norunda_fAPAR <- norunda_fAPAR_leap[-60]

  ###
  # Add the extras to the dataset
  ###
  kun_dataset_out$fAPAR <- rep(kun_fAPAR, length.out = nrow(kun_dataset_out))
  nor_dataset_out$fAPAR <- rep(norunda_fAPAR, length.out = nrow(nor_dataset_out))
  kun_dataset_out$date <- dataset_cmip$date
  nor_dataset_out$date <- dataset_cmip$date
  kun_dataset_out <- kun_dataset_out[substring(kun_dataset_out$date, 1, 4) < 2100,]
  nor_dataset_out <- nor_dataset_out[substring(nor_dataset_out$date, 1, 4) < 2100,]

  ###
  # Save file!
  ###
  write.csv(kun_dataset_out, file = paste(bias_correction, source_id, ssp_scenario, "kun.csv", sep = "_"), row.names = F)
  write.csv(nor_dataset_out, file = paste(bias_correction, source_id, ssp_scenario, "nor.csv", sep = "_"), row.names = F)
  ### Return dataset
  datasets_out <- list("kun" = kun_dataset_out,
                       "nor" = nor_dataset_out)

  if (plot == T) { # TODO: make these plots into another function!
    par(mfrow = c(2, 2))
    boxplot(preles_norunda_weather[366:2557,]$TAir,
            nor_dataset_out$TAir[substring(nor_dataset_out$date, 1, 4) <= 2020],
            kun.weather.preles$TAir,
            kun_dataset_out$TAir[substring(kun_dataset_out$date, 1, 4) <= 2020],
            main = "Air Temperature 'C",
            xaxt = "n",
            col = c("red", "orange", "yellow", "green"))
    text(x = seq(1.25, 4.25, by =1),
         y = -23,
         labels = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
         srt = 45,
         adj = 1,
         xpd = T)

    boxplot(preles_norunda_weather[366:2557,]$VPD,
            nor_dataset_out$VPD[substring(nor_dataset_out$date, 1, 4) <= 2020],
            kun.weather.preles$VPD,
            kun_dataset_out$VPD[substring(kun_dataset_out$date, 1, 4) <= 2020],
            main = "VPD, kPa",
            #names = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
            col = c("red", "orange", "yellow", "green"))
    text(x = seq(1.25, 4.25, by =1),
         y = -0.25,
         labels = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
         srt = 45,
         adj = 1,
         xpd = T)

    boxplot(preles_norunda_weather[366:2557,]$Precip,
            nor_dataset_out$Precip[substring(nor_dataset_out$date, 1, 4) <= 2020],
            kun.weather.preles$Precip,
            kun_dataset_out$Precip[substring(kun_dataset_out$date, 1, 4) <= 2020],
            main = "Precip, mm",
            xaxt = "n",
            names = c("Baseline Norunda", "Corrected Nor", "Baseline Kungsbo", "Corrected Kun"),
            col = c("red", "orange", "yellow", "green"))
    text(x = seq(1.25, 4.25, by =1), # TODO: not in the plot
         y = -10,
         labels = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
         srt = 45,
         adj = 1,
         xpd = T)

    plot(preles_norunda_weather[366:2557,]$PAR, col = "red", main = "PAR, sum mmol m-2, day-1")
    points(nor_dataset_out$PAR[substring(nor_dataset_out$date, 1, 4) <= 2020], col = "orange")
    points(kun.weather.preles$Precip, col = "yellow")
    points(kun_dataset_out$PAR[substring(kun_dataset_out$date, 1, 4) <= 2020], col = "green")

    # TODO: make the indicies work and change the rest of the plots
    boxplot(preles_wust_weather[1:730,]$TAir,
            nor_dataset_out$TAir[substring(nor_dataset_out$date, 1, 4) <= 2099],
            stkat.weather.preles$TAir,
            kun_dataset_out$TAir[substring(kun_dataset_out$date, 1, 4) <= 2020],
            main = "Air Temperature 'C",
            xaxt = "n",
            col = c("red", "orange", "yellow", "green"))
    text(x = seq(1.25, 4.25, by =1),
         y = -23,
         labels = c("Baseline\nWust", "Corrected\nNor", "Baseline\nWust", "Corrected\nKun"),
         srt = 45,
         adj = 1,
         xpd = T)

    boxplot(preles_norunda_weather[366:2557,]$VPD,
            nor_dataset_out$VPD[substring(nor_dataset_out$date, 1, 4) <= 2020],
            kun.weather.preles$VPD,
            kun_dataset_out$VPD[substring(kun_dataset_out$date, 1, 4) <= 2020],
            main = "VPD, kPa",
            #names = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
            col = c("red", "orange", "yellow", "green"))
    text(x = seq(1.25, 4.25, by =1),
         y = -0.25,
         labels = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
         srt = 45,
         adj = 1,
         xpd = T)

    boxplot(preles_norunda_weather[366:2557,]$Precip,
            nor_dataset_out$Precip[substring(nor_dataset_out$date, 1, 4) <= 2020],
            kun.weather.preles$Precip,
            kun_dataset_out$Precip[substring(kun_dataset_out$date, 1, 4) <= 2020],
            main = "Precip, mm",
            xaxt = "n",
            names = c("Baseline Norunda", "Corrected Nor", "Baseline Kungsbo", "Corrected Kun"),
            col = c("red", "orange", "yellow", "green"))
    text(x = seq(1.25, 4.25, by =1), # TODO: not in the plot
         y = -10,
         labels = c("Baseline\nNorunda", "Corrected\nNor", "Baseline\nKungsbo", "Corrected\nKun"),
         srt = 45,
         adj = 1,
         xpd = T)

    plot(preles_norunda_weather[366:2557,]$PAR, col = "red", main = "PAR, sum mmol m-2, day-1")
    points(nor_dataset_out$PAR[substring(nor_dataset_out$date, 1, 4) <= 2020], col = "orange")
    points(kun.weather.preles$Precip, col = "yellow")
    points(kun_dataset_out$PAR[substring(kun_dataset_out$date, 1, 4) <= 2020], col = "green")
  }

  print(source_id)

  return(datasets_out)
}
