southern_future_north_comparison <- function(models, site, preles_out, pdf_print = T) {
  ###
  # Files
  ###
  # Inport Data
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"

  load(paste0(bias_correction_direct, "kun.weather.preles.RData"))
  load(paste0(bias_correction_direct, "stkat.weather.preles.RData"))
  load(paste0(bias_correction_direct, "preles_norunda_weather.RData"))
  load(paste0(bias_correction_direct, "preles_wust_weather.RData"))

  # Making dates
  preles_wust_weather$YMD <- as.Date(preles_wust_weather$YMD)
  preles_wust_weather <- preles_wust_weather[!find_leap(preles_wust_weather$YMD),]
  preles_wust_weather$DoY <- rep(1:365, length.out = nrow(preles_wust_weather))
  wust_ref <- aggregate(GPP~DoY, data = preles_wust_weather, mean)[,2]
  preles_norunda_weather$Date <- as.Date(preles_norunda_weather$Date, format = "%Y%m%d")
  preles_norunda_weather <- preles_norunda_weather[!find_leap(preles_norunda_weather$Date),]
  preles_norunda_weather$DoY <- rep(1:365, length.out = nrow(preles_norunda_weather))
  nor_ref <- aggregate(GPP~DoY, data = preles_norunda_weather, mean)[,2]
  kun.weather.preles$Date <- as.Date(kun.weather.preles$Date)
  kun.weather.preles <- kun.weather.preles[!find_leap(kun.weather.preles$Date),]
  kun.weather.preles$DoY <- rep(1:365, length.out = nrow(kun.weather.preles))
  kun_ref <- aggregate(GPP~DoY, data = kun.weather.preles, mean)[,2]
  stkat.weather.preles$Date <- as.Date(stkat.weather.preles$Date)
  stkat.weather.preles <- stkat.weather.preles[!find_leap(stkat.weather.preles$Date),]
  stkat.weather.preles$DoY <- rep(1:365, length.out = nrow(stkat.weather.preles))
  stkat_ref <- aggregate(GPP~DoY, data = stkat.weather.preles, mean)[,2]

  # Start PDF
  if (pdf_print) {
    pdf(paste0(bias_correction_direct, paste(site, "preles", "comparison", sep = "_"), ".pdf"))
  }

  par(mfrow = c(3, 3))
  for (time in c("future", "history")) { # TODO: ET data and SW data
    ###
    # Plot
    ###
    for (j in 1:length(models)) {
      for (variable in c("GPP", "ET", "SW")) {
        preles_out_df <- as.data.frame(matrix(unlist(lapply(preles_out, function(x) {x[[variable]]})), ncol = length(models)))
        preles_out_df$Date = seq(as.Date("2015-01-01"), as.Date("2099-12-31"), by = "day")[!find_leap(seq(as.Date("2015-01-01"), as.Date("2099-12-31"), by = "day"))]
        axis_limits = c(min(apply(preles_out_df[,-9], 2, min)), max(apply(preles_out_df[,-9], 2, max)))
        xlabs = ""
        if (time == "future") {
          x_axis_lims = c(as.Date("2098-01-01"), as.Date("2099-12-31"))
          axis_dates = seq(as.Date("2098-01-01"), as.Date("2099-12-31"), by = "day")
          axis_dates = axis_dates[!find_leap(axis_dates)]
          preles_out_df_time <- preles_out_df[preles_out_df$Date %in% axis_dates,]
          if (site == "kun") {
            reference = rep(stkat_ref, length.out = length(axis_dates))
          } else {
            reference = rep(wust_ref, length.out = length(axis_dates))
          }
        } else {
          x_axis_lims = c(as.Date("2015-01-01"), as.Date("2020-12-31"))
          axis_dates = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = "day")
          axis_dates = axis_dates[!find_leap(axis_dates)]
          preles_out_df_time <- preles_out_df[preles_out_df$Date %in% axis_dates,]
          if (site == "kun") {
            reference = rep(kun_ref, length.out = length(axis_dates))
          } else {
            reference = rep(nor_ref, length.out = length(axis_dates))
          }
        }

        plot(NULL, main = paste(time, variable, ":", models[j]), xlab = xlabs, ylab = variable,
             ylim = axis_limits, xlim = x_axis_lims)
        for (i in 1:length(models)) {lines(axis_dates, preles_out_df_time[,i], col = "grey", type = "l", lty = 1)}
        if (variable == "GPP") {
          lines(axis_dates, reference, col = "black", lty = 2)
        }
        lines(axis_dates, preles_out_df_time[,j], col = cols[j], type = "l")

        if (variable == "GPP") {
          rmse_variable_model <- Metrics::rmse(reference, preles_out_df_time[,j])
          mae_variable_model <- Metrics::mae(reference, preles_out_df_time[,j])
          title(sub = paste("RMSE =", round(rmse_variable_model, 2), "MAE =", round(mae_variable_model, 2), "\n Scenario and data compared"))
        }

        plot(reference, preles_out_df_time[,j], col = cols[j], main = "", xlab = "Data", ylab = models[j])
        for (i in 1:length(models)) {points(reference, preles_out_df_time[,i], col = "grey", pch = 16)}
        points(reference, preles_out_df_time[,j], col = cols[j])
        abline(0, 1, lty = 2)
        abline(lm(reference ~ preles_out_df_time[,j]), col = cols[j])

        plot(axis_dates, reference - preles_out_df_time[,j], col = cols[j], main = "Residuals", xlab = "Day", ylab = "Residuals")
        for (i in 1:length(models)) {points(axis_dates, reference - preles_out_df_time[,i], col = "grey", pch = 16)}
        points(axis_dates, reference - preles_out_df_time[,j], col = cols[j])
        abline(0, 0, lty = 2)
      }
    }
  }

  if (pdf_print) {dev.off()}
}

