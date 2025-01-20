southern_future_north_comparison <- function(models, preles_north_normal, preles_south_normal, site, scenario, preles_out, pdf_print = T) {
  ###
  # Files
  ###
  # Inport Data
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"

  # Start PDF
  if (pdf_print) {
    pdf(paste0(bias_correction_direct, paste(site, "preles", "comparison", scenario, sep = "_"), ".pdf"))
  }

  for (time in c("future", "history")) {
    ###
    # Plot
    ###
    #plot(NULL, xlim = c(0, 1), ylim = c(0, 1))
    #legend("topright", inset(0.5, -0.5), c("Other Climate Scenarios", "Reference Climate Scenario", "Preles output with historical data", "Data to Scenario Linear Model"),
    #       pch = 16, col = c(colours_pale(6)[6], cols, "black", "green"), bty = "n", xpd=TRUE)

    for (j in 1:length(models)) {
      par(mfrow = c(3, 3))
      for (variable in c("GPP", "ET", "SW")) {
        preles_out_df <- as.data.frame(matrix(unlist(lapply(preles_out, function(x) {x[[variable]]})), ncol = length(models)))
        preles_out_df$Date = seq(as.Date("2015-01-01"), as.Date("2099-12-31"), by = "day")[!find_leap(seq(as.Date("2015-01-01"), as.Date("2099-12-31"), by = "day"))]
        xlabs = ""
        if (time == "future") {
          x_axis_lims = c(as.Date("2098-01-01"), as.Date("2099-12-31"))
          axis_dates = seq(as.Date("2098-01-01"), as.Date("2099-12-31"), by = "day")
          axis_dates = axis_dates[!find_leap(axis_dates)]
          preles_out_df_time <- preles_out_df[preles_out_df$Date %in% axis_dates,]
          cols <- if (scenario == 245) {colours_pale(6)[6]} else {colours_pale(6)[2]}
          reference = rep(preles_south_normal[[variable]], length.out = length(axis_dates))
        } else {
          x_axis_lims = c(as.Date("2015-01-01"), as.Date("2020-12-31"))
          axis_dates = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = "day")
          axis_dates = axis_dates[!find_leap(axis_dates)]
          preles_out_df_time <- preles_out_df[preles_out_df$Date %in% axis_dates,]
          cols <- if (scenario == 245) {colours_cb_245(3)[1]} else {colours_cb_585(3)[3]}
          reference = rep(preles_north_normal[[variable]], length.out = length(axis_dates))
        }
        col_back <- "grey"
        cols_1 <- "black"
        axis_limits = c(min(apply(preles_out_df[,-ncol(preles_out_df)], 2, min), min(reference), na.rm = T),
                        max(apply(preles_out_df[,-ncol(preles_out_df)], 2, max), max(reference), na.rm = T))
        axis_limits_2 = c(min(apply(preles_out_df_time[,-ncol(preles_out_df_time)], 2, min), na.rm = T),
                          max(apply(preles_out_df_time[,-ncol(preles_out_df_time)], 2, max), na.rm = T))

        plot(axis_dates, preles_out_df_time[,j], main = paste(time, variable, ":", models[j]), xlab = xlabs, ylab = variable,
             ylim = axis_limits, xlim = x_axis_lims, col = NULL)
        for (i in 1:length(models)) {lines(axis_dates, preles_out_df_time[,i], col = col_back, type = "l", lty = 1)}
        lines(axis_dates, preles_out_df_time[,j], col = cols, type = "l", lwd  = 2)
        lines(axis_dates, reference, col = "black", lty = 2, lwd = 2)

        rmse_variable_model <- Metrics::rmse(reference, preles_out_df_time[,j])
        mae_variable_model <- Metrics::mae(reference, preles_out_df_time[,j])
        title(sub = paste("RMSE =", round(rmse_variable_model, 2), "MAE =", round(mae_variable_model, 2), "\n Scenario and data compared"))

        plot(reference, preles_out_df_time[,j], col = cols, main = "", xlab = "Reference", ylab = models[j], ylim = axis_limits_2)
        for (i in 1:length(models)) {points(reference, preles_out_df_time[,i], col = col_back, pch = 3)}
        points(reference, preles_out_df_time[,j], col = cols)
        abline(0, 1, lty = 2)
        abline(lm(reference ~ preles_out_df_time[,j]), col = cols_1, lwd = 2.5)

        plot(axis_dates, reference - preles_out_df_time[,j], col = cols, main = "Residuals", xlab = "Day", ylab = "Residuals")
        for (i in 1:length(models)) {points(axis_dates, reference - preles_out_df_time[,i], col = col_back, pch = 3)}
        points(axis_dates, reference - preles_out_df_time[,j], col = cols)
        abline(0, 0, lty = 2)
      }
    }
  }

  if (pdf_print) {dev.off()}
}

