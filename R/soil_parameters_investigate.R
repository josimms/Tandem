soil_parameters_investigate <- function(Corrected_All_245, Corrected_All_585, models_245, models_585, site, no_co2 = F, pdf_print = T) {
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  if (pdf_print) {
    if (no_co2) {
      pdf(paste0(bias_correction_direct, paste(site, "preles_soil_parameters_both_scenarios_no_co2", sep = "_"), ".pdf"))
    } else {
      pdf(paste0(bias_correction_direct, paste(site, "preles_soil_parameters_both_scenarios", sep = "_"), ".pdf"))
    }
  }

  ### Import data
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  # calibration_direct <- "/home/joanna/Asiakirjat/CMIP6/Callibration/"

  if (site == "kun") {
    load(paste0(bias_correction_direct, "kun.weather.preles.RData"))
    load(paste0(bias_correction_direct, "stkat.weather.preles.RData"))

    stkat.weather.preles$Date <- as.Date(stkat.weather.preles$Date)
    stkat.weather.preles <- stkat.weather.preles[!find_leap(stkat.weather.preles$Date),]
    stkat.weather.preles$DoY <- rep(1:365, length.out = nrow(stkat.weather.preles))

    weather_data <- Corrected_All_585[1,]

    stop("Need to add the code here! And download the parameters from OneDrive!") # TODO

  } else {
    weather_data_245 <- Corrected_All_245[c("nor"),]
    weather_data_585 <- Corrected_All_585[c("nor"),]

    pPREL_north <- pPREL_south <- pPREL
    pPREL_north[5] <- loadRData(paste0(calibration_direct, "nor_beta.RData"))
    pPREL_south[5] <- loadRData(paste0(calibration_direct, "wust_beta.RData"))
    pPREL_north[c(6:11, 14:18)] <- BayesianTools::MAP(loadRData(paste0(calibration_direct, "2022-09-14 17_39_49 out_bayesian_icos_1_long_nor.RData")))$parametersMAP[1:11]
    pPREL_south[c(6:11, 14:18)] <- BayesianTools::MAP(loadRData(paste0(calibration_direct, "2022-10-05 10_08_15 out_bayesian_icos_1_long_wust.RData")))$parametersMAP[1:11]
  }

  cols_245 <- colours_cb_245(3)[1]
  cols_585 <- colours_cb_585(3)[3]

  bounds <- data.frame(min = c(400, 0.3, 0.05, 1), max = c(500, 0.5, 0.3, 5)) # TODO: find the bounds!

  params = c("soildepth", "ThetaFC", "ThetaPWP", "tauDrainage")
  par(mfrow = c(3,3))
  for (j in 1:length(models_245)) {
    for (out in c("GPP", "ET", "SW")) {
      par(mfrow = c(2, 3))
      counter = 1
      for (k in 1:4) {
        values = seq(bounds[k,1], bounds[k, 2], length.out = 2)
        for (w in values) {
          pPREL_replace <- pPREL_north
          pPREL_replace[k] <- w

            if (no_co2) {
              weather_data_245 <- lapply(weather_data_245, function(x) {
                x[,c("co2")] <- rep(410, length.out = length(x[,c("co2")]))
                return(x)
              })
              weather_data_585 <- lapply(weather_data_585, function(x) {
                x[,c("co2")] <- rep(410, length.out = length(x[,c("co2")]))
                return(x)
              })
            }

          preles_out_parameter_245_original <- lapply(weather_data_245, function(x, pPREL_north) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = x$co2, fAPAR = x$fAPAR, p = pPREL_north)}, pPREL_north)
          preles_out_parameter_585_original <- lapply(weather_data_585, function(x, pPREL_north) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = x$co2, fAPAR = x$fAPAR, p = pPREL_north)}, pPREL_north)

          preles_out_parameter_245 <- lapply(weather_data_245, function(x, pPREL_replace) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = x$co2, fAPAR = x$fAPAR, p = pPREL_replace)}, pPREL_replace)
          preles_out_parameter_585 <- lapply(weather_data_585, function(x, pPREL_replace) {PRELES(PAR = x$PAR, TAir = x$TAir, VPD = x$VPD, Precip = x$Precip, CO2 = x$co2, fAPAR = x$fAPAR, p = pPREL_replace)}, pPREL_replace)

          preles_out_parameter_aggregated_max_245 <- lapply(preles_out_parameter_245, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, max)
          })
          preles_out_parameter_aggregated_sum_245 <- lapply(preles_out_parameter_245, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, sum)
          })
          preles_out_parameter_aggregated_max_585 <- lapply(preles_out_parameter_585, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, max)
          })
          preles_out_parameter_aggregated_sum_585 <- lapply(preles_out_parameter_585, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, sum)
          })

          preles_out_parameter_aggregated_max_245_original <- lapply(preles_out_parameter_245_original, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, max)
          })
          preles_out_parameter_aggregated_sum_245_original <- lapply(preles_out_parameter_245_original, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, sum)
          })
          preles_out_parameter_aggregated_max_585_original <- lapply(preles_out_parameter_585_original, function(x) {
            data = data.frame(data = x[[out]], Year = rep(2015:2099, each = 365))
            aggregate(data ~ Year, data, max)
          })
          preles_out_parameter_aggregated_sum_585_original <- lapply(preles_out_parameter_585_original, function(x) {
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

          ### MAX

          axis_limits_max = c(min(unlist(lapply(preles_out_parameter_aggregated_max_245, function(x) {min(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_max_585, function(x) {min(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_max_245_original, function(x) {min(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_max_585_original, function(x) {min(x[,c("data")])}))) -
                                a*min(unlist(lapply(preles_out_parameter_aggregated_max_245, function(x) {min(x[,c("data")])})),
                                      unlist(lapply(preles_out_parameter_aggregated_max_585, function(x) {min(x[,c("data")])})),
                                      unlist(lapply(preles_out_parameter_aggregated_max_245_original, function(x) {min(x[,c("data")])})),
                                      unlist(lapply(preles_out_parameter_aggregated_max_585_original, function(x) {min(x[,c("data")])}))),
                              max(unlist(lapply(preles_out_parameter_aggregated_max_245, function(x) {max(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_max_585, function(x) {max(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_max_245_original, function(x) {max(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_max_585_original, function(x) {max(x[,c("data")])}))) +
                                a*max(unlist(lapply(preles_out_parameter_aggregated_max_245, function(x) {max(x[,c("data")])})),
                                      unlist(lapply(preles_out_parameter_aggregated_max_585, function(x) {max(x[,c("data")])})),
                                      unlist(lapply(preles_out_parameter_aggregated_max_245_original, function(x) {max(x[,c("data")])})),
                                      unlist(lapply(preles_out_parameter_aggregated_max_585_original, function(x) {max(x[,c("data")])}))))

          plot(NULL, main = paste0(params[counter],  " = ", w, " ", models_245[j]), xlab = "Years", ylab = paste(out, "(max)"),
               ylim = axis_limits_max, xlim = c(2015, 2099))
          for (i in 1:length(models_245)) {points(2015:2099, preles_out_parameter_aggregated_max_245[[i]][,c("data")], col = "tan", pch = 16)}
          for (i in 1:length(models_585)) {points(2015:2099, preles_out_parameter_aggregated_max_585[[i]][,c("data")], col = "grey", pch = 3)}
          lines(2015:2099, preles_out_parameter_aggregated_max_245_original[[j]][,c("data")], col = colours_cb_2(4)[4], type = "l", lwd = 2)
          if (j < 8) {
            lines(2015:2099, preles_out_parameter_aggregated_max_585_original[[j]][,c("data")], col = colours_cb_2(4)[1], type = "l", lwd = 2, lty = 1)
          }
          lines(2015:2099, preles_out_parameter_aggregated_max_245[[j]][,c("data")], col = cols_245, type = "l", lwd = 2)
          if (j < 8) {
            lines(2015:2099, preles_out_parameter_aggregated_max_585[[j]][,c("data")], col = cols_585, type = "l", lwd = 2, lty = 1)
          }

          ### SUM

          axis_limits = c(min(unlist(lapply(preles_out_parameter_aggregated_sum_245, function(x) {min(x[,c("data")])})),
                              unlist(lapply(preles_out_parameter_aggregated_sum_585, function(x) {min(x[,c("data")])})),
                              unlist(lapply(preles_out_parameter_aggregated_sum_245_original, function(x) {min(x[,c("data")])})),
                              unlist(lapply(preles_out_parameter_aggregated_sum_585_original, function(x) {min(x[,c("data")])}))) -
                            a*min(unlist(lapply(preles_out_parameter_aggregated_sum_245, function(x) {min(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_sum_585, function(x) {min(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_sum_245_original, function(x) {min(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_sum_585_original, function(x) {min(x[,c("data")])}))),
                          max(unlist(lapply(preles_out_parameter_aggregated_sum_245, function(x) {max(x[,c("data")])})),
                              unlist(lapply(preles_out_parameter_aggregated_sum_585, function(x) {max(x[,c("data")])})),
                              unlist(lapply(preles_out_parameter_aggregated_sum_245_original, function(x) {max(x[,c("data")])})),
                              unlist(lapply(preles_out_parameter_aggregated_sum_585_original, function(x) {max(x[,c("data")])}))) +
                            a*max(unlist(lapply(preles_out_parameter_aggregated_sum_245, function(x) {max(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_sum_585, function(x) {max(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_sum_245_original, function(x) {max(x[,c("data")])})),
                                  unlist(lapply(preles_out_parameter_aggregated_sum_585_original, function(x) {max(x[,c("data")])}))))

          plot(NULL, main = paste0(params[counter],  " = ", w, " ", models_245[j]), xlab = "Years", ylab = paste(out, "(sum)"),
               ylim = axis_limits, xlim = c(2015, 2099))
          for (i in 1:length(models_245)) {points(2015:2099, preles_out_parameter_aggregated_sum_245[[i]][,c("data")], col = "tan", pch = 16)}
          for (i in 1:length(models_585)) {points(2015:2099, preles_out_parameter_aggregated_sum_585[[i]][,c("data")], col = "grey", pch = 3)}
          lines(2015:2099, preles_out_parameter_aggregated_sum_245_original[[j]][,c("data")], col = colours_cb_2(4)[4], type = "l", lwd = 2)
          if (j < 8) {
            lines(2015:2099, preles_out_parameter_aggregated_sum_585_original[[j]][,c("data")], col = colours_cb_2(4)[1], type = "l", lwd = 2, lty = 1)
          }
          lines(2015:2099, preles_out_parameter_aggregated_sum_245[[j]][,c("data")], col = cols_245, type = "l", lwd = 2)
          if (j < 8) {
            lines(2015:2099, preles_out_parameter_aggregated_sum_585[[j]][,c("data")], col = cols_585, type = "l", lwd = 2, lty = 1)
          }


          ### YEAR
          axis_limits = c(0, axis_limits_max[2])

          # Counter up 1, so the names of the graphs are correct
          plot(NULL, main = paste0(params[counter],  " = ", w, " ", models_245[j]), xlab = "Days", ylab = paste(out, "(daily mean across years)"),
               ylim = axis_limits, xlim = c(1, 365))
          for (i in 1:length(models_245)) {points(1:365, preles_out_parameter_245[[i]][[out]][365*84+(1:365)], col = "tan", pch = 16)}
          for (i in 1:length(models_585)) {points(1:365, preles_out_parameter_585[[i]][[out]][365*84+(1:365)], col = "grey", pch = 3)}
          points(1:365, preles_out_parameter_245_original[[j]][[out]][365*84+(1:365)], col = colours_cb_2(4)[4], pch = 16)
          if (j < 8) {
            points(1:365, preles_out_parameter_585_original[[j]][[out]][365*84+(1:365)], col = colours_cb_2(4)[1], pch = 16)
          }
          points(1:365, preles_out_parameter_245[[j]][[out]][365*84+(1:365)], col = cols_245, pch = 16)
          if (j < 8) {
            points(1:365, preles_out_parameter_585[[j]][[out]][365*84+(1:365)], col = cols_585, pch = 16)
          }
        }
        counter = counter + 1
      }
    }
  }

  if (pdf_print) {
    dev.off()
  }
}
