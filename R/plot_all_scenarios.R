plot_all_scenarios <- function(Dataset_In_All_245, Corrected_All_245,
                               Dataset_In_All_585, Corrected_All_585,
                               preles_out_245, preles_out_585,
                               models_245, models_585,
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
  if (pdf_print) {pdf(paste0(bias_correction_direct, paste(site, "Variables_and_Preles", sep = "_"), ".pdf"))}
  cols = muted(max(length(models_245), length(models_585)))

  for (variable in c("TAir", "PAR", "VPD", "Precip")) {
    # Time series
    par(mfrow = c(3, 1))
    plot(Dataset_In_All_245[[1]][,c(variable)], xlab = "Day since 2015.01.01", ylab = variable, main = "Original", type = "l")
    for (i in 2:length(Dataset_In_All_245)) {lines(Dataset_In_All_245[[i]][,c(variable)], col = cols[i-1])}
    plot(Corrected_All_245_site[[1]][,c(variable)], type = "l", main = paste("Bias Correction:", site), xlab = "Day since 2015.01.01", ylab = "TAir 'C")
    for (i in 2:length(Dataset_In_All_245)) {lines(Corrected_All_245_site[[i]][,c(variable)], col = cols[i-1])}
    plot(Corrected_All_585_site[[1]][,c(variable)], type = "l", main = paste("Bias Correction:", site), xlab = "Day since 2015.01.01", ylab = "TAir 'C")
    for (i in 2:length(Dataset_In_All_585)) {lines(Corrected_All_585_site[[i]][,c(variable)], col = cols[i-1])}
  }

  par(mfrow = c(2, 2))
  for (j in 1:length(models_245)) {
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
          variable_operation_245 <- aggregate(.~year, variable_df_245, sum)
          variable_operation_585 <- aggregate(.~year, variable_df_585, sum)
          preles_out_df_yearly_245 <- aggregate(.~Year, preles_out_df_245, sum)
          preles_out_df_yearly_585 <- aggregate(.~Year, preles_out_df_585, sum)

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
          preles_out_df_yearly_245 <- aggregate(.~Year, preles_out_df_245, mean)
          preles_out_df_yearly_585 <- aggregate(.~Year, preles_out_df_585, mean)

          axis_limits = c(min(c(apply(variable_min_245[,-1], 2, min), apply(variable_min_585[,-1], 2, min))),
                          max(c(apply(variable_max_245[,-1], 2, max), apply(variable_max_585[,-1], 2, max))))
        }

        ###
        # Met Plot
        ###
        plot(NULL, main = paste("Yearly means:", models_245[j]), xlab = "", ylab = variable,
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 1:length(models_245)) {points(2015:2099, variable_operation_245[,i+1], col = "tan")}
        if (operation == "mean") {for (i in 1:length(models_245)) {points(2015:2099, variable_min_245[,i+1], col = "tan")}}
        if (operation == "mean") {for (i in 1:length(models_245)) {points(2015:2099, variable_max_245[,i+1], col = "tan")}}
        if (j < 8) {
          for (i in 1:length(models_585)) {points(2015:2099, variable_operation_585[,i+1], col = "grey", pch = 3)}
          if (operation == "mean") {for (i in 1:length(models_585)) {points(2015:2099, variable_min_585[,i+1], col = "grey", pch = 3)}}
          if (operation == "mean") {for (i in 1:length(models_585)) {points(2015:2099, variable_max_585[,i+1], col = "grey", pch = 3)}}
        }
        lines(2015:2099, variable_means_245[,j+1], col = cols[j], lwd = 2)
        lines(2015:2099, variable_min_245[,j+1], col = cols[j], lwd = 2)
        lines(2015:2099, variable_max_245[,j+1], col = cols[j], lwd = 2)
        if (j < 8) {
          lines(2015:2099, variable_means_585[,j+1], col = cols[j], lty = 2, lwd = 3)
          lines(2015:2099, variable_min_585[,j+1], col = cols[j], lty = 2, lwd = 3)
          lines(2015:2099, variable_max_585[,j+1], col = cols[j], lty = 2, lwd = 3)
        }

        # TODO: sort out statistics!
        ### STATISTICS
        baseline_variable_model_245 <- apply(variable_df_245[,-c(1, j)], 1, mean)
        rmse_variable_model <- Metrics::rmse(baseline_variable_model_245, variable_df_245[,j])
        mae_variable_model <- Metrics::mae(baseline_variable_model_245, variable_df_245[,j])
        title(sub = paste("RMSE =", round(rmse_variable_model, 2), "MAE =", round(mae_variable_model, 2), "\nagainst mean daily baseline w/o said data"))
        #legend("bottom", legend = c("Range", "Mean"), lty = c(1, 3), bty = "n", cex = 0.7)

        ### STATISTICS
        baseline_variable_model_585 <- apply(variable_df_585[,-c(1, j)], 1, mean)
        rmse_variable_model <- Metrics::rmse(baseline_variable_model, variable_df_585[,j])
        mae_variable_model <- Metrics::mae(baseline_variable_model, variable_df_585[,j])
        title(sub = paste("RMSE =", round(rmse_variable_model, 2), "MAE =", round(mae_variable_model, 2), "\nagainst mean daily baseline w/o said data"))

        if (operation == "sum" & variable == "Precip") {
          drought_index <- which(variable_df_245[,j] <= 0.1)
          drought_list <- split(drought_index, cumsum(c(1, diff(drought_index) != 1)))

          # Length of longest time without rain
          longest_drought = max(unlist(lapply(drought_list, length)))
          # Average time without water
          average_drought = mean(unlist(lapply(drought_list, length)))

          title(sub = paste(longest_drought, "days = longest w/ preicp < 0.1\n", round(average_drought), "days precip <0.1 on average each year"))

          drought_index <- which(variable_df_585[,j] <= 0.1)
          drought_list <- split(drought_index, cumsum(c(1, diff(drought_index) != 1)))

          # Length of longest time without rain
          longest_drought = max(unlist(lapply(drought_list, length)))
          # Average time without water
          average_drought = mean(unlist(lapply(drought_list, length)))

          title(sub = paste(longest_drought, "days = longest w/ preicp < 0.1\n", round(average_drought), "days precip <0.1 on average each year"))
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
        for (i in 1:length(models_245)) {points(2015:2099, preles_out_df_yearly_245[,i+1], col = "tan", pch = 16)}
        if (j < 8) {
          for (i in 1:length(models_585)) {points(2015:2099, preles_out_df_yearly_585[,i+1], col = "grey", pch = 3)}
        }
        lines(2015:2099, preles_out_df_yearly_245[,j+1], col = cols[j], type = "l", lwd = 2)
        if (j < 8) {
          lines(2015:2099, preles_out_df_yearly_585[,j+1], col = cols[j], type = "l", lty = 2, lwd = 3) # TOOD: colour?
        }

        ### STATISTICS
        baseline_variable_model <- apply(preles_out_df_yearly[,-c(1, j)], 1, mean) # TODO: c(1, j) should this be indexed differently
        rmse_variable_model <- Metrics::rmse(baseline_variable_model, preles_out_df_yearly[,j])
        mae_variable_model <- Metrics::mae(baseline_variable_model, preles_out_df_yearly[,j])
        title(sub = paste("RMSE =", round(rmse_variable_model, 2), "MAE =", round(mae_variable_model, 2), "\nagainst mean daily baseline w/o said data"))
      }
    }
  }

  ### END OF PDF
  if (pdf_print) {dev.off()}
}
