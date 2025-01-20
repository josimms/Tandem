####
# Preles Scenario plotter, with grey for other scenarios and then coloured for the rest of the scenarios
####

preles_scenario_plotter_yearly <- function(bias_correction_direct, preles_out_list_245, preles_out_list_585, models_245, models_585, site, scenario, pdf_print = T) {
  if (pdf_print) {
    pdf(paste0(bias_correction_direct, paste(site, "preles_basic_output", sep = "_"), ".pdf"))
  }
  par(mfrow = c(4, 2))

  for (operation in c("mean", "sum")) {
    for (variable in c("GPP", "ET", "SW")) {
      preles_out_df_245 <- as.data.frame(matrix(unlist(lapply(preles_out_list_245, function(x) {x[[variable]]})), ncol = length(models_245)))
      preles_out_df_585 <- as.data.frame(matrix(unlist(lapply(preles_out_list_585, function(x) {x[[variable]]})), ncol = length(models_585)))
      names(preles_out_df_245) <- models_245
      names(preles_out_df_585) <- models_585
      preles_out_df_245$Year <- rep(2015:2099, each = 365)
      preles_out_df_585$Year <- rep(2015:2099, each = 365)

      if (operation == "sum") {
        preles_out_df_yearly_245 <- aggregate(.~Year, preles_out_df_245, sum)
        preles_out_df_yearly_585 <- aggregate(.~Year, preles_out_df_585, sum)
      }
      if (operation == "mean") {
        preles_out_df_yearly_245 <- aggregate(.~Year, preles_out_df_245, mean)
        preles_out_df_yearly_585 <- aggregate(.~Year, preles_out_df_585, mean)
      }

      cols_245 = colours_cb_245(3)[1]
      cols_585 = colours_cb_585(3)[3]

      axis_limits = c(min(apply(preles_out_df_yearly_245[,-1], 2, min), apply(preles_out_df_yearly_585[,-1], 2, min)),
                      max(apply(preles_out_df_yearly_245[,-1], 2, max), apply(preles_out_df_yearly_585[,-1], 2, max)))
      for (j in 2:ncol(preles_out_df_yearly_245)) {
        ### PLOT
        plot(NULL, main = paste0(variable, ": ", models_245[j-1]), xlab = "", ylab = paste(variable, "(", operation, ")"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 2:ncol(preles_out_df_yearly_245)) {points(2015:2099, preles_out_df_yearly_245[,i], col = colours_pale(6)[6], pch = 16)}
        if (j < 9) {
          for (i in 2:ncol(preles_out_df_yearly_585)) {points(2015:2099, preles_out_df_yearly_585[,i], col = colours_pale(6)[2], pch = 3)}
          lines(2015:2099, preles_out_df_yearly_585[,j], col = cols_585, type = "l", lwd = 3)
        }
        lines(2015:2099, preles_out_df_yearly_245[,j], col = cols_245, type = "l", lwd = 3)


        ### STATISTICS
        baseline_variable_model_245 <- apply(preles_out_df_yearly_245[,-c(1, j)], 1, mean)
        rmse_variable_model_245 <- Metrics::rmse(baseline_variable_model_245, preles_out_df_yearly_245[,j])
        mae_variable_model_245 <- Metrics::mae(baseline_variable_model_245, preles_out_df_yearly_245[,j])
        if (j < 9) {
          baseline_variable_model_585 <- apply(preles_out_df_yearly_585[,-c(1, j)], 1, mean)
          rmse_variable_model_585 <- Metrics::rmse(baseline_variable_model_585, preles_out_df_yearly_585[,j])
          mae_variable_model_585 <- Metrics::mae(baseline_variable_model_585, preles_out_df_yearly_585[,j])
        }
        title(sub = paste("RMSE =", round(rmse_variable_model_245, 2), "MAE =", round(mae_variable_model_245, 2),
                          "RMSE =", round(rmse_variable_model_585, 2), "MAE =", round(mae_variable_model_585, 2),
                          "\nagainst mean daily baseline w/o said data"))
      }
    }
  }

  if (pdf_print) {dev.off()}
}
