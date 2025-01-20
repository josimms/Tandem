###
# Weather Variables investigate
###

weather_variables_inverstigate <- function(Corrected_All_245, Corrected_All_585, preles_site_normal, site, comparison, pPREL_site, no_co2 = F, pdf_print = T) {
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  if (pdf_print) {
    if (no_co2) {
      pdf(paste0(bias_correction_direct, paste(site, comparison, "preles_variables_both_scenarios_no_co2", sep = "_"), ".pdf"))
    } else {
      pdf(paste0(bias_correction_direct, paste(site, comparison, "preles_variables_both_scenarios", sep = "_"), ".pdf"))
    }
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
    if (comparison == "South") {
      weather_reference <- loadRData(paste0(bias_correction_direct, "preles_wust_weather.RData"))
      weather_reference$YMD <- as.Date(weather_reference$YMD)
      weather_reference <- weather_reference[!find_leap(weather_reference$YMD),]
      weather_reference$DoY <- rep(1:365, length.out = nrow(weather_reference))
      if ("TotPAR" %in% names(weather_reference)) {names(weather_reference) = gsub("TotPAR", "PAR", names(weather_reference))}
      weather_reference_mean <- aggregate(.~DoY, weather_reference, mean)

      preles_reference <- data.frame(GPP = preles_site_normal[[1]],
                                     ET = preles_site_normal[[2]],
                                     SW = preles_site_normal[[3]])
      preles_reference_max <- apply(preles_reference, 2, max)
      preles_reference_sum <- apply(preles_reference, 2, sum)
    } else if (comparison == "Middle") {
      weather_reference <- loadRData(paste0(bias_correction_direct, "preles_htl_weather.RData"))
      weather_reference$Date <- as.Date(weather_reference$Date, format = "%Y%m%d")
      weather_reference <- weather_reference[!find_leap(weather_reference$Date),]
      weather_reference$DoY <- rep(1:365, length.out = nrow(weather_reference))
      weather_reference_mean <- aggregate(.~DoY, weather_reference, mean)

      preles_reference <- data.frame(GPP = preles_site_normal[[1]],
                                     ET = preles_site_normal[[2]],
                                     SW = preles_site_normal[[3]])
      preles_reference_max <- apply(preles_reference, 2, max)
      preles_reference_sum <- apply(preles_reference, 2, sum)
    }

    weather_data_245 <- Corrected_All_245[c("nor"),]
    weather_data_585 <- Corrected_All_585[c("nor"),]
  }

  cols_245 <- colours_cb_245(3)[1]
  cols_585 <- colours_cb_585(3)[3]

  par(mfrow = c(4, 2))
  for (j in 1:length(models_245)) {
    for (out in c("GPP", "ET", "SW")) {
      for (variable in c("TAir", "PAR", "Precip", "VPD")) {
        ###
        # Edit weather data and make simulations
        ###
        weather_data_variable_245 <- weather_data_245
        weather_data_variable_245 <- lapply(weather_data_variable_245, function(x) {
          x[,c(variable)] <- rep(weather_reference_mean[,c(variable)], length.out = length(x[,c(variable)]))
          return(x)
        })
        weather_data_variable_585 <- weather_data_585
        weather_data_variable_585 <- lapply(weather_data_variable_585, function(x) {
          x[,c(variable)] <- rep(weather_reference_mean[,c(variable)], length.out = length(x[,c(variable)]))
          return(x)
        })

        if (no_co2) {
          weather_data_variable_245 <- lapply(weather_data_variable_245, function(x) {
            x[,c("co2")] <- rep(410, length.out = length(x[,c("co2")]))
            return(x)
          })
          weather_data_variable_585 <- lapply(weather_data_variable_585, function(x) {
            x[,c("co2")] <- rep(410, length.out = length(x[,c("co2")]))
            return(x)
          })
        }

        preles_out_variable_245 <- lapply(weather_data_variable_245, function(x, pPREL_site) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = x$co2, fAPAR = x$fAPAR, p = pPREL_site)}, pPREL_site)
        preles_out_variable_585 <- lapply(weather_data_variable_585, function(x, pPREL_site) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = x$co2, fAPAR = x$fAPAR, p = pPREL_site)}, pPREL_site)

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
        if (out == "SW") {
          a = 0.01
        } else {
          a = 0.1
        }

        axis_limits = c(min(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {min(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {min(x[,c("data")])})),
                            preles_reference_max[out]) - a*min(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {min(x[,c("data")])})),
                                                               unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {min(x[,c("data")])})),
                                                               preles_reference_max[out]),
                        max(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {max(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {max(x[,c("data")])})),
                            preles_reference_max[out]) + a*max(unlist(lapply(preles_out_variable_aggregated_max_245, function(x) {max(x[,c("data")])})),
                                                               unlist(lapply(preles_out_variable_aggregated_max_585, function(x) {max(x[,c("data")])})),
                                                               preles_reference_max[out]))

        plot(NULL, main = paste0(variable, ": ", models_245[j]), xlab = "Years", ylab = paste(out, "(max)"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, preles_out_variable_aggregated_max_245[[i]][,c("data")], col = colours_pale(6)[2], pch = 16)}
        for (i in 1:length(models_585)) {points(2015:2099, preles_out_variable_aggregated_max_585[[i]][,c("data")], col = colours_pale(6)[6], pch = 3)}
        lines(2015:2099, preles_out_variable_aggregated_max_245[[j]][,c("data")], col = cols_245, type = "l", lwd = 2)
        if (j < 8) {
          lines(2015:2099, preles_out_variable_aggregated_max_585[[j]][,c("data")], col = cols_585, type = "l", lwd = 2, lty = 1)
        }
        lines(2015:2099, rep(preles_reference_max[out], length.out = 2099-2014), col = "black")

        axis_limits = c(min(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {min(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {min(x[,c("data")])})),
                            preles_reference_sum[out])-a*min(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {min(x[,c("data")])})),
                                                             unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {min(x[,c("data")])})),
                                                             preles_reference_sum[out]),
                        max(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {max(x[,c("data")])})),
                            unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {max(x[,c("data")])})),
                            preles_reference_sum[out])+a*max(unlist(lapply(preles_out_variable_aggregated_sum_245, function(x) {max(x[,c("data")])})),
                                                             unlist(lapply(preles_out_variable_aggregated_sum_585, function(x) {max(x[,c("data")])})),
                                                             preles_reference_sum[out]))

        plot(NULL, main = paste0(variable, ": ", models_245[j]), xlab = "Years", ylab = paste(out, "(sum)"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, preles_out_variable_aggregated_sum_245[[i]][,c("data")], col = colours_pale(6)[2], pch = 16)}
        for (i in 1:length(models_585)) {points(2015:2099, preles_out_variable_aggregated_sum_585[[i]][,c("data")], col = colours_pale(6)[6], pch = 3)}
        lines(2015:2099, preles_out_variable_aggregated_sum_245[[j]][,c("data")], col = cols_245, type = "l", lwd = 2)
        if (j < 8) {
          lines(2015:2099, preles_out_variable_aggregated_sum_585[[j]][,c("data")], col = cols_585, type = "l", lwd = 2, lty = 1)
        }
        lines(2015:2099, rep(preles_reference_sum[out], length.out = 2099-2014), col = "black")
      }
    }
  }

  if (pdf_print) {
    dev.off()
  }
}
