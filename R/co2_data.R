co2_data <- function(ssp, downloaded = T) {
  temperory_direct <- "/home/joanna/Asiakirjat/CMIP6/Temperory_CMIP6/"
  answer_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"

  preles_norunda_weather <- loadRData(paste0(bias_correction_direct, "preles_norunda_weather.RData"))
  if ("TotPAR" %in% names(preles_norunda_weather)) names(preles_norunda_weather) <- gsub("Tot", "", names(preles_norunda_weather))
  if ("DoY" %in% names(preles_norunda_weather)) names(preles_norunda_weather) <- gsub("DoY", "Date", names(preles_norunda_weather))

  if (downloaded) {
    if (ssp == 245) {
      file_names = paste0(temperory_direct, list.files(path = temperory_direct, pattern = "co2_Amon_CESM2-WACCM_ssp245"))
      source_id = lapply(lapply(file_names, strsplit, "_"), function(x) {x[[1]][[4]]})[[1]]
    } else if (ssp == 585) {
      file_names = paste0(temperory_direct, list.files(path = temperory_direct, pattern = "co2_Amon_CESM2-WACCM_ssp585"))
      source_id = lapply(lapply(file_names, strsplit, "_"), function(x) {x[[1]][[4]]})[[1]]
    } else {
      stop("Scenario not planned for!")
    }

    spp_temp <- list()
    for (i in 1:length(file_names)) {
      temp = ncdf4::nc_open(file_names[i])

      # set site coordinates
      lon <- ncdf4::ncvar_get(temp, "lon")
      lon_index <- which.min(abs(lon-17.483333))
      lat <- ncdf4::ncvar_get(temp, "lat", verbose = F)
      lat_index <- which.min(abs(lat-60.083333))

      # extract data
      # TODO: should I do 10 Pa
      spp_temp[[i]] = ncdf4::ncvar_get(temp, varid = "co2", start = c(lon_index, lat_index, 2, 1), count = c(1, 1, 1, -1))
    }
    spp <- 1000000*unlist(spp_temp)

    ### Bias corrections and saving
    preles_norunda_weather_mean <- aggregate(CO2 ~ substring(Date, 1, 7), data = preles_norunda_weather, mean)
    bias_corrected_spp = spp - mean(spp[1:(12*5)] - preles_norunda_weather_mean$CO2[13:(6*12)])
    if (ssp == 245) {
      save(bias_corrected_spp, file = paste0(answer_direct, source_id, "_", ssp, "_", "co2", ".RData"))
    } else if (ssp == 585) {
      save(bias_corrected_spp, file = paste0(answer_direct, source_id, "_", ssp, "_", "co2", ".RData"))
    } else {
      stop("Scenario not planned for: Not saved. Shouldn't be possible to print this...")
    }
  } else {
    bias_corrected_spp <- loadRData(paste0(answer_direct, "CESM2-WACCM", "_", ssp, "_", "co2", ".RData"))
  }

  return(bias_corrected_spp)
}

