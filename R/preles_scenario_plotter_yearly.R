####
# Preles Scenario plotter, with grey for other scenarios and then coloured for the rest of the scenarios
####

preles_scenario_plotter_yearly <- function(preles_out_list, models, site, scenario, pdf_print = T) {
  bias_correction_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"
  if (pdf_print) {
    pdf(paste0(bias_correction_direct, paste(site, "preles", scenario, sep = "_"), ".pdf"))
    par(mfrow = c(2, 2))
  }

  for (operation in c("mean", "sum")) {
    for (variable in c("GPP", "ET", "SW")) {
      preles_out_df <- as.data.frame(matrix(unlist(lapply(preles_out_list, function(x) {x[[variable]]})), ncol = length(models)))
      names(preles_out_df) <- models
      preles_out_df$Year <- rep(2015:2099, each = 365)

      if (operation == "sum") {
        preles_out_df_yearly <- aggregate(.~Year, preles_out_df, sum)
      }
      if (operation == "mean") {
        preles_out_df_yearly <- aggregate(.~Year, preles_out_df, mean)
      }

      cols = muted(length(models)*2) # TODO: more colours

      axis_limits = c(min(apply(preles_out_df_yearly[,-1], 2, min)), max(apply(preles_out_df_yearly[,-1], 2, max)))
      for (j in 2:ncol(preles_out_df_yearly)) {
        ### PLOT
        plot(NULL, main = paste0(variable, ": ", models[j-1]), xlab = "", ylab = paste(variable, "(", operation, ")"),
             ylim = axis_limits, xlim = c(2015, 2099))
        for (i in 2:ncol(preles_out_df_yearly)) {lines(2015:2099, preles_out_df_yearly[,i], col = "grey", type = "l", lty = 1)}
        lines(2015:2099, preles_out_df_yearly[,j], col = cols[j-1], type = "l")

        ### STATISTICS
        baseline_variable_model <- apply(preles_out_df_yearly[,-c(1, j)], 1, mean)
        rmse_variable_model <- Metrics::rmse(baseline_variable_model, preles_out_df_yearly[,j])
        mae_variable_model <- Metrics::mae(baseline_variable_model, preles_out_df_yearly[,j])
        title(sub = paste("RMSE =", round(rmse_variable_model, 2), "MAE =", round(mae_variable_model, 2), "\nagainst mean daily baseline w/o said data"))
      }
    }
  }

  if (pdf_print) {dev.off()}
}
