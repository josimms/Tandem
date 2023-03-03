####
# Function to get CMIP6 data
####
get_CMIP <- function(urls_ssp, source_id, ssp_scenario, download = FALSE) {
  ####
  # Getting the values for the source stated
  ####
  source_indexes = which(urls_ssp$source_id %in% source_id)
  urls_ssp_source <- urls_ssp[source_indexes,]
  temparyfile_direct <- "/home/joanna/Asiakirjat/CMIP6/Temperory_CMIP6/"
  answer_direct <- "/home/joanna/Asiakirjat/CMIP6/Bias_Correction_Data/"

  ## test!
  if (sum(urls_ssp_source$source_id != source_id) != 0) {error("Taking data from the wrong source_id!")}

  ####
  # Downloading files
  ####
  options(timeout = max(3000, getOption("timeout")))
  if (download == TRUE) {
    cmip_location = urls_ssp_source$file_url
    files = paste0(temparyfile_direct,
                   paste(ssp_scenario,
                         urls_ssp_source$source_id,
                         urls_ssp_source$variable_id,
                         urls_ssp_source$member_id,
                         urls_ssp_source$datetime_start,
                         urls_ssp_source$datetime_end, sep = "_"),
                   ".nc")
    files_missing <- files[!file.exists(files)]
    mapply(download.file, cmip_location, files_missing)
    print("Download Complete! :)")
  }

  parameters <- names(table(urls_ssp_source$variable_id))
  # For all of the variables
  for (variable in parameters) {
    ####
    # Opening the files and changing the format
    ####
    spp_files <- list.files(path = temparyfile_direct,
                            pattern = paste(ssp_scenario,
                                            urls_ssp_source$source_id,
                                            variable,
                                            urls_ssp_source$member_id,
                                            sep = "_"))
    spp_nc <- nc_open(paste0(temparyfile_direct, spp_files))
    spp_temp <- list()
    for (i in 1:length(spp_files)) {
      # Import the file
      temp <- nc_open(paste0(temparyfile_direct, spp_files[i]))

      # Set site coordinates
      lon <- ncvar_get(temp, "lon")
      lon_index <- which.min(abs(lon-17.483333))
      lat <- ncvar_get(temp, "lat", verbose = F)
      lat_index <- which.min(abs(lat-60.083333))

      spp_temp[[i]] = ncvar_get(temp, varid = variable, start = c(lon_index, lat_index, 1), count = c(1, 1, -1))

    }
    spp <- unlist(spp_temp)

    ####
    # Units should be consistent from CMIP, so transformations are hard coded in
    ####
    # TODO: test the values by seeing if they are in a certain range?
    if (variable == "pr") {
      Precip <- spp * 86400 # mm day-1
      if (min(Precip) < 0) {warning("Precip could be reading wrong data as negative values")}
    } else if (variable == "tas") {
      TAir <- spp - 273.15 # 'C
    } else if (variable == "rsds") {
      PAR <- bigleaf::Rg.to.PPFD(spp) * 0.000001 * 86400 # umol m-2 s-1 to sum mmol m-2 day-1
      if (min(PAR) < 0) {warning("Precip could be reading wrong data as negative values")}
    } else if (variable == "hurs") {
      RH <- spp # %
      if (min(RH) < 0) {warning("Precip could be reading wrong data as negative values")}
    }
    print(paste(variable, "done! :D"))
  }
  VPD = bigleaf::rH.to.VPD(RH*0.01, TAir)
  date <- seq(as.Date(urls_ssp_source$datetime_start[1]), as.Date(tail(urls_ssp_source$datetime_end, n = 1)), by = "day")
  if (length(date) != length(TAir)) {
    date <- date[!find_leap(date)]
    if (source_id == "CESM2-WACCM") {
      TAir <- TAir[-length(TAir)]
      PAR <- PAR[-length(PAR)]
      Precip <- Precip[-length(Precip)]
      RH <- RH[-length(RH)]
      VPD <- VPD[-length(VPD)]
    }
  }
  dataset_cmip = data.frame(date, Precip, TAir, PAR, RH, VPD)
  save(dataset_cmip, file = paste0(answer_direct, source_id, "_", ssp_scenario, ".RData")) # No bias corrections!

  ## Beep when finished as such a long function
  beepr::beep(3)
  return(dataset_cmip)
}
