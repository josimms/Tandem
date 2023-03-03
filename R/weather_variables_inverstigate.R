###
# Weather Variables investigate
###

weather_variables_inverstigate <- function(Corrected_All_245, Corrected_All_585, site, pdf_print = T) {
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  if (pdf_print) {
    pdf(paste0(bias_correction_direct, paste(site, "preles_variables_both_scenarios", sep = "_"), ".pdf"))
  }

  ### Import data
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  calibration_direct <- "/home/joanna/Asiakirjat/CMIP6/Callibration/"

  if (site == "kun") {
    load(paste0(bias_correction_direct, "kun.weather.preles.RData"))
    load(paste0(bias_correction_direct, "stkat.weather.preles.RData"))

    stkat.weather.preles$Date <- as.Date(stkat.weather.preles$Date)
    stkat.weather.preles <- stkat.weather.preles[!find_leap(stkat.weather.preles$Date),]
    stkat.weather.preles$DoY <- rep(1:365, length.out = nrow(stkat.weather.preles))

    weather_data <- Corrected_All_585[1,]

    stop("Need to add the code here! And download the parameters from OneDrive!") # TODO

  } else {
    weather_reference <- loadRData(paste0(bias_correction_direct, "preles_wust_weather.RData"))
    weather_reference$YMD <- as.Date(weather_reference$YMD)
    weather_reference <- preles_wust_weather[!find_leap(weather_reference$YMD),]
    weather_reference$DoY <- rep(1:365, length.out = nrow(weather_reference))
    if ("TotPAR" %in% names(weather_reference)) {names(weather_reference) = gsub("TotPAR", "PAR", names(weather_reference_mean))}
    weather_reference_mean <- aggregate(.~DoY, weather_reference, mean)

    pPREL_site <- pPREL
    pPREL_site[5] <- loadRData(paste0(calibration_direct, "wust_beta.RData"))
    pPREL_site[c(6:11, 14:18)] <- BayesianTools::MAP(loadRData(paste0(calibration_direct, "2022-10-05 10_08_15 out_bayesian_icos_1_long_wust.RData")))$parametersMAP[1:11]

    preles_average_wust <- PRELES(TAir = weather_reference$TAir, PAR = weather_reference$PAR, Precip = weather_reference$Precip,
                                  VPD = weather_reference$VPD, CO2 = weather_reference$CO2, fAPAR = weather_reference$fAPAR, p = pPREL_site)

    weather_data_245 <- Corrected_All_245[c("nor"),]
    weather_data_585 <- Corrected_All_585[c("nor"),]
  }

  par(mfrow = c(4, 2))
  for (j in 1:length(models)) {
    for (out in c("GPP", "ET", "SW")) {
      weather_reference_max = max(preles_average_wust[[out]])
      weather_reference_sum = sum(preles_average_wust[[out]])
      for (variable in c("TAir", "PAR", "Precip", "VPD")) {
        ###
        # Edit weather data and make simulations
        ###
        weather_data_variable_245 <- weather_data_245
        weather_data_variable_245 <- lapply(weather_data_245, function(x) {
          x[,c(variable)] <- rep(weather_reference_mean[,c(variable)], length.out = length(x[,c(variable)]))
          return(x)
        })
        weather_data_variable_585 <- weather_data_585
        weather_data_variable_585 <- lapply(weather_data_585, function(x) {
          x[,c(variable)] <- rep(weather_reference_mean[,c(variable)], length.out = length(x[,c(variable)]))
          return(x)
        })

        preles_out_variable_245 <- lapply(weather_data_variable_245, function(x) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = rep(380, length(x$Precip)), fAPAR = x$fAPAR, p = pPREL_site)})
        preles_out_variable_585 <- lapply(weather_data_variable_585, function(x) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = rep(380, length(x$Precip)), fAPAR = x$fAPAR, p = pPREL_site)})

        preles_out_variable_aggregated_max_245 <- lapply(preles_out_variable_245, function(x) {
          data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
          aggregate(data ~ Year, data, max)
        })
        preles_out_variable_aggregated_sum_245 <- lapply(preles_out_variable_245, function(x) {
          data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
          aggregate(data ~ Year, data, sum)
        })
        preles_out_variable_aggregated_max_585 <- lapply(preles_out_variable_585, function(x) {
          data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
          aggregate(data ~ Year, data, max)
        })
        preles_out_variable_aggregated_sum_585 <- lapply(preles_out_variable_585, function(x) {
          data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
          aggregate(data ~ Year, data, sum)
        })


        ###
        # Plot
        ###
        axis_limits = c(min(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {min(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {min(x[,c("data")])})), weather_reference_max) - 0.1*min(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {min(x[,c("data")])})),
                                                                                                                                                       unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {min(x[,c("data")])})), weather_reference_max),
                        max(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {max(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {max(x[,c("data")])})), weather_reference_max) + 0.1*max(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {max(x[,c("data")])})),
                                                                                                                                                       unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {max(x[,c("data")])})), weather_reference_max))
        print(axis_limits)

        plot(NULL, main = paste0(variable, ": ", models_245[j]), xlab = "Years", ylab = paste(out, "(max)"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, preles_out_variable_aggregated_max_245[[i]][,c("data")], col = "tan", pch = 16)}
        for (i in 1:length(models_585)) {points(2015:2099, preles_out_variable_aggregated_max_585[[i]][,c("data")], col = "grey", pch = 3)}
        lines(2015:2099, preles_out_variable_aggregated_max_245[[j]][,c("data")], col = cols[j], type = "l", lwd = 2)
        if (j < 8) {
          lines(2015:2099, preles_out_variable_aggregated_max_585[[j]][,c("data")], col = cols[j], type = "l", lwd = 3, lty = 2)
        }
        lines(2015:2099, rep(weather_reference_max, length.out = 2099-2014), col = "red")

        axis_limits = c(min(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {min(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {min(x[,c("data")])})), weather_reference_sum)-0.1*min(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {min(x[,c("data")])})),
                                                                                                                                                     unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {min(x[,c("data")])})), weather_reference_sum),
                        max(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {max(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {max(x[,c("data")])})), weather_reference_sum)+0.1*max(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {max(x[,c("data")])})),
                                                                                                                                                     unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {max(x[,c("data")])})), weather_reference_sum))
        plot(NULL, main = paste0(variable, ": ", models_245[j]), xlab = "Years", ylab = paste(out, "(sum)"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, preles_out_variable_aggregated_sum_245[[i]][,c("data")], col = "tan", pch = 16)}
        for (i in 1:length(models_585)) {points(2015:2099, preles_out_variable_aggregated_sum_585[[i]][,c("data")], col = "grey", pch = 3)}
        lines(2015:2099, preles_out_variable_aggregated_sum_245[[j]][,c("data")], col = cols[j], type = "l", lwd = 2)
        if (j < 8) {
          lines(2015:2099, preles_out_variable_aggregated_sum_585[[j]][,c("data")], col = cols[j], type = "l", lwd = 3, lty = 2)
        }
        lines(2015:2099, rep(weather_reference_sum, length.out = 2099-2014), col = "red")
      }
    }
  }

  if (pdf_print) {
    dev.off()
  }
}
