plot_all_scenarios <- function(Dataset_In_All_245, Corrected_All_245,
                               Dataset_In_All_585, Corrected_All_585,
                               preles_out_245, preles_out_585,
                               site_weather,
                               models_245, models_585, co2 = T,
                               All_data_plot = F,
                               site, pdf_print = T) {
  ### DIRECTORY
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  if (site == "kun") {
    Corrected_All_245_site <- Corrected_All_245[c("kun"),]
    Corrected_All_585_site <- Corrected_All_585[c("kun"),]
  } else {
    Corrected_All_245_site <- Corrected_All_245[c("nor"),]
    Corrected_All_585_site <- Corrected_All_585[c("nor"),]
  }

  ### PDF STARTS
  if (pdf_print) {
    if (co2) {
      pdf(paste0(bias_correction_direct, paste(site, "Variables_and_Preles", sep = "_"), ".pdf"))
    } else {
      pdf(paste0(bias_correction_direct, paste(site, "Variables_and_Preles_no_co2_effect", sep = "_"), ".pdf"))
    }
  }
  cols_245 <- colours_cb_245(3)[1]
  cols_585 <- colours_cb_245(3)[3]

  if (All_data_plot) {
    for (variable in c("TAir", "PAR", "VPD", "Precip")) {
      # Time series
      par(mfrow = c(3, 1))
      plot(Dataset_In_All_245[[1]][,c(variable)], xlab = "Day since 2015.01.01", ylab = variable, main = "Original", type = "l")
      for (i in 2:length(Dataset_In_All_245)) {lines(Dataset_In_All_245[[i]][,c(variable)], col = cols_245)}
      plot(Corrected_All_245_site[[1]][,c(variable)], type = "l", main = paste("Bias Correction:", site), xlab = "Day since 2015.01.01", ylab = "TAir 'C")
      for (i in 2:length(Dataset_In_All_245)) {lines(Corrected_All_245_site[[i]][,c(variable)], col = cols_245)}
      plot(Corrected_All_585_site[[1]][,c(variable)], type = "l", main = paste("Bias Correction:", site), xlab = "Day since 2015.01.01", ylab = "TAir 'C")
      for (i in 2:length(Dataset_In_All_585)) {lines(Corrected_All_585_site[[i]][,c(variable)], col = cols_585)}
    }
  }

  for (j in 1:length(models_245)) {
    par(mfrow = c(2, 2))
    for (operation in c("mean", "sum")) {
      for (variable in c("TAir", "PAR", "VPD", "Precip")) {
        variable_df_245 <- data.frame(matrix(unlist(lapply(Corrected_All_245_site, function(x) {x[,c(variable)]})), ncol = length(models_245)))
        variable_df_585 <- data.frame(matrix(unlist(lapply(Corrected_All_585_site, function(x) {x[,c(variable)]})), ncol = length(models_585)))
        names(variable_df_245) <- models_245
        names(variable_df_585) <- models_585
        variable_df_245$year <- variable_df_585$year <- rep(2015:2099, each = 365)
        variable_df_245 <- variable_df_245[,c("year", names(variable_df_245)[-c(ncol(variable_df_245))])]
        variable_df_585 <- variable_df_585[,c("year", names(variable_df_585)[-c(ncol(variable_df_585))])]

        if (operation == "sum") {
          if (variable == "TAir") {
            variable_df_245[,-c(1)] <- variable_df_245[,-c(1)] - 5
            variable_df_585[,-c(1)] <- variable_df_585[,-c(1)] - 5
            variable_df_245[,-c(1)][variable_df_245[,-c(1)] < 0] <- 0
            variable_df_585[,-c(1)][variable_df_585[,-c(1)] < 0] <- 0
          }

          variable_operation_245 <- aggregate(.~year, variable_df_245, sum)
          variable_operation_585 <- aggregate(.~year, variable_df_585, sum)

          list_parameters <- lapply(site_weather[,c("PAR", "Precip", "CO2", "TAir", "RH", "VPD", "GPP")], sum, na.rm = T)
          list_parameters$TAir <- list_parameters$TAir -5

          axis_limits = c(min(c(apply(variable_operation_245[,-1], 2, min), apply(variable_operation_585[,-1], 2, min))),
                          max(c(apply(variable_operation_245[,-1], 2, max), apply(variable_operation_585[,-1], 2, max))))
        }
        if (operation == "mean") {
          variable_operation_245 <- aggregate(.~year, variable_df_245, mean)
          variable_min_245 <- aggregate(.~year, variable_df_245, min)
          variable_max_245 <- aggregate(.~year, variable_df_245, max)
          variable_operation_585 <- aggregate(.~year, variable_df_585, mean)
          variable_min_585 <- aggregate(.~year, variable_df_585, min)
          variable_max_585 <- aggregate(.~year, variable_df_585, max)

          list_parameters <- lapply(site_weather[,c("PAR", "Precip", "CO2", "TAir", "RH", "VPD", "GPP")], function(x) mean(x, na.rm = T))
          list_parameters_min <- lapply(site_weather[,c("PAR", "Precip", "CO2", "TAir", "RH", "VPD", "GPP")], min, na.rm = T)
          list_parameters_max <- lapply(site_weather[,c("PAR", "Precip", "CO2", "TAir", "RH", "VPD", "GPP")], max, na.rm = T)

          axis_limits = c(min(c(apply(variable_min_245[,-1], 2, min), apply(variable_min_585[,-1], 2, min))),
                          max(c(apply(variable_max_245[,-1], 2, max), apply(variable_max_585[,-1], 2, max))))
        }

        ###
        # Met Plot
        ###
        plot(NULL, main = paste("Yearly", operation, ":", models_245[j]), xlab = "", ylab = variable,
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, variable_operation_245[,i+1], col = colours_pale(6)[6], pch = 3)}
        if (operation == "mean") {for (i in 1:length(models_245)) {points(2015:2099, variable_min_245[,i+1], col = colours_pale(6)[6], pch = 3)}}
        if (operation == "mean") {for (i in 1:length(models_245)) {points(2015:2099, variable_max_245[,i+1], col = colours_pale(6)[6], pch = 3)}}
        if (j < 8) {
          for (i in 1:length(models_585)) {points(2015:2099, variable_operation_585[,i+1], col = colours_pale(6)[2], pch = 3)}
          if (operation == "mean") {for (i in 1:length(models_585)) {points(2015:2099, variable_min_585[,i+1], col = colours_pale(6)[2], pch = 3)}}
          if (operation == "mean") {for (i in 1:length(models_585)) {points(2015:2099, variable_max_585[,i+1], col = colours_pale(6)[2], pch = 3)}}
        }
        lines(2015:2099, variable_operation_245[,j+1], col = cols_245, lwd = 2)
        lines(2015:2099, variable_min_245[,j+1], col = cols_245, lwd = 2)
        lines(2015:2099, variable_max_245[,j+1], col = cols_245, lwd = 2)
        if (j < 8) {
          lines(2015:2099, variable_operation_585[,j+1], col = cols_585, lty = 1, lwd = 2)
          lines(2015:2099, variable_min_585[,j+1], col = cols_585, lty = 1, lwd = 3)
          lines(2015:2099, variable_max_585[,j+1], col = cols_585, lty = 1, lwd = 3)
        }
        abline(a = list_parameters_max[[c(variable)]], b = 0, lwd = 2)
        abline(a = list_parameters[[c(variable)]], b = 0, lwd = 2)
        abline(a = list_parameters_min[[c(variable)]], b = 0, lwd = 2)

        ### STATISTICS
        baseline_variable_model_245 <- apply(variable_df_245[,-c(1, j+1)], 1, mean)
        rmse_variable_model_245 <- Metrics::rmse(baseline_variable_model_245, variable_df_245[,j+1])
        mae_variable_model_245 <- Metrics::mae(baseline_variable_model_245, variable_df_245[,j+1])
        if (j < 8) { # TODO: there are some extra NAs I don't know why!
          baseline_variable_model_585 <- apply(variable_df_585[,-c(1, j+1)], 1, mean)
          rmse_variable_model_585 <- Metrics::rmse(baseline_variable_model_585, variable_df_585[,j+1])
          mae_variable_model_585 <- Metrics::mae(baseline_variable_model_585, variable_df_585[,j+1])
        }
        rmse_variable_model_585 = NA
        mae_variable_model_585 = NA
        title(sub = paste("RMSE =", round(rmse_variable_model_245, 2), "MAE =", round(mae_variable_model_245, 2),
                          "RMSE =", round(rmse_variable_model_585, 2), "MAE =", round(mae_variable_model_585, 2),
                          "\nagainst mean daily baseline w/o said data"))


        if (operation == "sum" & variable == "Precip") {
          drought_index_245 <- which(variable_df_245[,j] <= 0.1)
          drought_list_245 <- split(drought_index_245, cumsum(c(1, diff(drought_index_245) != 1)))
          # Length of longest time without rain
          longest_drought_245 = max(unlist(lapply(drought_list_245, length)))
          # Average time without water
          average_drought_245 = mean(unlist(lapply(drought_list_245, length)))
          if (j < 8) {
            drought_index_585 <- which(variable_df_585[,j] <= 0.1)
            drought_list_585 <- split(drought_index_585, cumsum(c(1, diff(drought_index_585) != 1)))
            # Length of longest time without rain
            longest_drought_585 = max(unlist(lapply(drought_list_585, length)))
            # Average time without water
            average_drought_585 = mean(unlist(lapply(drought_list_585, length)))
          } else {
            longest_drought_585 = NA
            average_drought_585 = NA
          }

          title(sub = paste(longest_drought_245, "/", longest_drought_585, "days = longest w/ preicp < 0.1\n",
                            round(average_drought_245), "/", round(average_drought_585), "days precip <0.1 on average each year"),
                line = -2)
        }

      }

      for (variable in c("GPP", "ET", "SW")) {
        preles_out_df_245 <- as.data.frame(matrix(unlist(lapply(preles_out_245, function(x) {x[[variable]]})), ncol = length(models_245)))
        preles_out_df_585 <- as.data.frame(matrix(unlist(lapply(preles_out_585, function(x) {x[[variable]]})), ncol = length(models_585)))
        names(preles_out_df_245) <- models_245
        names(preles_out_df_585) <- models_585
        preles_out_df_245$Year <- preles_out_df_585$Year <- rep(2015:2099, each = 365)

        if (operation == "sum") {
          preles_out_df_yearly_245 <- aggregate(.~Year, preles_out_df_245, sum)
          preles_out_df_yearly_585 <- aggregate(.~Year, preles_out_df_585, sum)
        }
        if (operation == "mean") {
          preles_out_df_yearly_245 <- aggregate(.~Year, preles_out_df_245, mean)
          preles_out_df_yearly_585 <- aggregate(.~Year, preles_out_df_585, mean)
        }

        axis_limits = c(min(c(apply(preles_out_df_yearly_245[,-1], 2, min), apply(preles_out_df_yearly_585[,-1], 2, min))),
                        max(c(apply(preles_out_df_yearly_245[,-1], 2, max), apply(preles_out_df_yearly_585[,-1], 2, max))))

        ### PLOT
        plot(NULL, main = paste0(variable, ": ", models_245[j]), xlab = "", ylab = paste(variable, "(", operation, ")"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, preles_out_df_yearly_245[,i+1], col = colours_pale(6)[6], pch = 3)}
        if (j < 8) {
          for (i in 1:length(models_585)) {points(2015:2099, preles_out_df_yearly_585[,i+1], col = colours_pale(6)[2], pch = 3)}
        }
        lines(2015:2099, preles_out_df_yearly_245[,j+1], col = cols_245, type = "l", lwd = 2)
        if (j < 8) {
          lines(2015:2099, preles_out_df_yearly_585[,j+1], col = cols_585, type = "l", lty = 1, lwd = 2)
        }
        abline(a = list_parameters[[c(variable)]], b = 0, lwd = 2)

        ### STATISTICS
        baseline_variable_model_245 <- apply(preles_out_df_yearly_245[,-c(1, j+1)], 1, mean)
        rmse_variable_model_245 <- Metrics::rmse(baseline_variable_model_245, preles_out_df_yearly_245[,j+1])
        mae_variable_model_245 <- Metrics::mae(baseline_variable_model_245, preles_out_df_yearly_245[,j+1])
        baseline_variable_model_585 <- apply(preles_out_df_yearly_585[,-c(1, j+1)], 1, mean)
        if (j < 8) {
          rmse_variable_model_585 <- Metrics::rmse(baseline_variable_model_585, preles_out_df_yearly_585[,j+1])
          mae_variable_model_585 <- Metrics::mae(baseline_variable_model_585, preles_out_df_yearly_585[,j+1])
        } else {
          rmse_variable_model_585 = NA
          mae_variable_model_585 = NA
        }
        title(sub = paste("RMSE =", round(rmse_variable_model_245, 2), "MAE =", round(mae_variable_model_245, 2),
                          "RMSE =", round(rmse_variable_model_585, 2), "MAE =", round(mae_variable_model_585, 2),
                          "\nagainst mean daily baseline w/o said data"))
      }
    }
  }

  ### END OF PDF
  if (pdf_print) {dev.off()}
}
