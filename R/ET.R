kun.results <- "/home/joanna/Asiakirjat/SPP_Tandem/KunResults/"
bre.results <- "/home/joanna/Asiakirjat/SPP_Tandem/BreResults/"
stkat.results <- "/home/joanna/Asiakirjat/SPP_Tandem/StKatResults/"
ant.results <- "/home/joanna/Asiakirjat/SPP_Tandem/AntResults/"

kun.data <- "/home/joanna/Asiakirjat/SPP_Tandem/KunData/"
bre.data <- "/home/joanna/Asiakirjat/SPP_Tandem/BreData/"
stkat.data <- "/home/joanna/Asiakirjat/SPP_Tandem/StKatData/"
ant.data <- "/home/joanna/Asiakirjat/SPP_Tandem/AntData/"

files.kun = list.files(kun.results, "D_R1_Normal")
files.bre = list.files(bre.results, "D_R1_Normal")
files.stkat = list.files(stkat.results, "D_R1_Normal")
files.ant = list.files(ant.results, "D_R1_Normal")

results_spp_kun <- lapply(paste0(kun.results, files.kun), read.delim, sep = "\t")
results_spp_bre <- lapply(paste0(bre.results, files.bre), read.delim, sep = "\t")
results_spp_stkat <- lapply(paste0(stkat.results, files.stkat), read.delim, sep = "\t")
results_spp_ant <- lapply(paste0(ant.results, files.ant), read.delim, sep = "\t")

# Stand densities to normalise the data
Ant_stand_density = c(632, 573, 529, 511, 456, 456)
Bre_stand_density = c(606, 589, 589, 523, 155, 155)
Kun_stand_density = c(591, 554, 555, 539, 515, 515)
StKat_stand_density = c(720, 683, 683, 619, 618, 618)

Ant_leaf_mass = c(0.103236, 0.129159, 0.179815, 0.238538, 0.330812, 0.410604)
Bre_leaf_mass = c(0.256253, 0.323976, 0.323976, 0.518571, 0.690504, 0.743932)
Kun_leaf_mass = c(0.175606, 0.219715, 0.219715, 0.415263, 0.524181, 0.697161)
StKat_leaf_mass = c(0.229093, 0.304867, 0.304867, 0.517376, 0.747485, 0.978175)


par(mfrow = c(2, 2))
plot(NULL, xlim = c(1, 366), ylim = c(0, 0.0003), main = "Kungsbo", xlab = "Day of Year", ylab = "Photosynthesis, µmol m-2 s-1")
for (i in 4:length(results_spp_ant)) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_kun[[i]]$Photosynthesis/Kun_stand_density[i])/Kun_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
  legend(0, .0003, 2018:2020, col = colours_cb_245(3), lty = 1, bty = "n")
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.0003), main = "Bredinge", xlab = "Day of Year", ylab = "Photosynthesis, µmol m-2 s-1")
for (i in 4:length(results_spp_ant)) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_bre[[i]]$Photosynthesis/Bre_stand_density[i])/Bre_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.0003), main = "St Katherinen", xlab = "Day of Year", ylab = "Photosynthesis, µmol m-2 s-1")
for (i in 4:length(results_spp_ant)) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_stkat[[i]]$Photosynthesis/StKat_stand_density[i])/StKat_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.0003), main = "Antagnac", xlab = "Day of Year", ylab = "Photosynthesis, µmol m-2 s-1")
for (i in 4:length(results_spp_ant)) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_ant[[i]]$Photosynthesis/Ant_stand_density[i])/Ant_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

par(mfrow = c(2, 2))
plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "Kungsbo", xlab = "Day of Year", ylab = "Stomatal conductance, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_kun[[i]]$Stomatal_cond/Kun_stand_density[i])/Kun_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
  legend(0, .05, 2018:2020, col = colours_cb_245(3), lty = 1, bty = "n")
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "Bredinge", xlab = "Day of Year", ylab = "Stomatal conductance, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_bre[[i]]$Stomatal_cond/Bre_stand_density[i])/Bre_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "St Katherin", xlab = "Day of Year", ylab = "Stomatal conductance, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_stkat[[i]]$Stomatal_cond/StKat_stand_density[i])/StKat_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "Antagnac", xlab = "Day of Year", ylab = "Stomatal conductance, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_ant[[i]]$Stomatal_cond/Ant_stand_density[i])/Ant_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

par(mfrow = c(2, 2))
plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "Kungsbo", xlab = "Day of Year", ylab = "Transpiration, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_kun[[i]]$Transpiration/Kun_stand_density[i])/Kun_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
  legend(0, .05, 2018:2020, col = colours_cb_245(3), lty = 1, bty = "n")
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "Bredinge", xlab = "Day of Year", ylab = "Transpiration, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_bre[[i]]$Transpiration/Bre_stand_density[i])/Bre_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "St Katherinen", xlab = "Day of Year", ylab = "Transpiration, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_stkat[[i]]$Transpiration/StKat_stand_density[i])/StKat_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 0.05), main = "Antagnac", xlab = "Day of Year", ylab = "Transpiration, µmol m-2 s-1")
for (i in length(results_spp_ant):4) {
  lines(results_spp_kun[[i]]$DoY, (results_spp_ant[[i]]$Transpiration/Ant_stand_density[i])/Ant_leaf_mass[i], col = colours_cb_245(3)[i-3], lwd  = 2)
}


#####
## Nounrda, SPP vs PRELES
#####

# TODO: write a bayesian analysis programme here
nor.results <- "/home/joanna/Asiakirjat/SPP_Tandem/NorundaResults/"
files.nor = list.files(nor.results, "SC1_D_R1_20220630")
results_spp_nor <- lapply(paste0(nor.results, files.nor), read.delim, sep = "\t")

library(Rprebasso)
library(BayesianTools)

spp_preles_bayseian <- function(parameters) {
  stkat.results <- "/home/joanna/Asiakirjat/SPP_Tandem/StKatResults/"

  results_spp_stkat <- lapply(paste0(stkat.results,
                                     list.files(path = stkat.results,
                                                pattern = "*SWELow")[2]),
                              read.delim,
                              sep = "\t", dec = ",",
                              na.strings = -9999, row.names = NULL)

  load(paste0(stkat.results, "stkat.weather.preles.RData"))

  pPREL_StKat <- pPREL
  pPREL_StKat[c(6:11, 14:18)] <- parameters[1:11]
  pPREL_StKat[5] <- max(12*results_spp_stkat[[1]]$Photosynthesis[results_spp_stkat[[1]]$Canopy_light_interception > 0]/
                          results_spp_stkat[[1]]$Canopy_light_interception[results_spp_stkat[[1]]$Canopy_light_interception > 0], na.rm = T)

  stkat.weather.preles_2020 <- stkat.weather.preles[substring(stkat.weather.preles$Date, 1, 4) == 2020,]
  out = PRELES(PAR = stkat.weather.preles_2020$PAR,
               TAir = stkat.weather.preles_2020$TAir,
               VPD = stkat.weather.preles_2020$VPD,
               Precip = stkat.weather.preles_2020$Precip,
               CO2 = stkat.weather.preles_2020$CO2,
               fAPAR = results_spp_stkat[[1]]$Canopy_light_interception/results_spp_stkat[[1]]$PPFD,
               p = pPREL_StKat)

  diff = c(out[[1]], out[[3]]) - c(12*results_spp_stkat[[1]]$Photosynthesis,
                                             1000*results_spp_stkat[[1]]$Measured_SWC)

  sdX <- c(parameters[12]*out[[1]]+parameters[13],
           parameters[14]*out[[2]]+parameters[15],
           parameters[16]*out[[3]]+parameters[17])

  sdX[is.nan(sdX)] <- 1e-6
  sdX <- abs(sdX)

  likelihood <- sum(dnorm(diff, mean = 0, sd = sdX, log = T))

  return(likelihood)
}

pPREL_bounds <- read.delim(paste0(calibration_direct, "pPREL_bounds.csv"), sep = ",", header = T, fileEncoding = "UTF-8")
prior_all_sites <- createUniformPrior(lower = c(pPREL_bounds[c(6:11, 14:18),2], 1e-16, 1, 1e-16, 1, 1e-16, 1),
                                      upper = c(pPREL_bounds[c(6:11, 14:18),3], 3, 3, 3, 3, 3, 3),
                                      best = NULL)

BayesianSetup <- createBayesianSetup(likelihood = spp_preles_bayseian,
                                         prior = prior_all_sites,
                                         names = c(pPREL_bounds[c(6:11, 14:18),1], paste0("parameter", 12:17)),
                                         parallel = F)

settings = list(iterations = 3e6, thin = 100, nrChains = 1, message = T)

SWELow_StKat <- runMCMC(bayesianSetup = BayesianSetup, sampler = "DREAMzs", settings = settings)
save(SWELow_StKat, file = paste0(calibration_direct, gsub(":", "_", Sys.time()), " SWELow_StKat.RData"))


stkat.results <- "/home/joanna/Asiakirjat/SPP_Tandem/StKatResults/"
results_spp_stkat <- lapply(paste0(stkat.results,
                                   list.files(path = stkat.results, pattern = "*SWELow")[2]),
                            read.delim,
                            sep = "\t", dec = ",",
                            na.strings = -9999, row.names = NULL)
load(paste0(stkat.results, "stkat.weather.preles.RData"))
load(paste0(calibration_direct, "2023-03-24 20_52_51 SWELow_StKat.RData"))

pPREL_StKat <- pPREL
pPREL_StKat[c(6:11, 14:18)] <- MAP(SWELow_StKat)$parametersMAP[1:11]
pPREL_StKat[5] <- max(12*results_spp_stkat[[1]]$Photosynthesis[results_spp_stkat[[1]]$Canopy_light_interception > 0]/
                        results_spp_stkat[[1]]$Canopy_light_interception[results_spp_stkat[[1]]$Canopy_light_interception > 0], na.rm = T)

stkat.weather.preles_2020 <- stkat.weather.preles[substring(stkat.weather.preles$Date, 1, 4) == 2020,]
StKat_SWELow = PRELES(PAR = stkat.weather.preles_2020$PAR,
                     TAir = stkat.weather.preles_2020$TAir,
                     VPD = stkat.weather.preles_2020$VPD,
                     Precip = stkat.weather.preles_2020$Precip,
                     CO2 = stkat.weather.preles_2020$CO2,
                     fAPAR = results_spp_stkat[[1]]$Canopy_light_interception/results_spp_stkat[[1]]$PPFD,
                     p = pPREL_StKat)

par(mfrow = c(2, 1))
plot(12*results_spp_stkat[[1]]$Photosynthesis, type = "l", main = "Photosynthesis", ylab = "gC m-2 day-1", xlab = "Days of the Year, 2020")
lines(StKat_SWELow[[1]], col = "blue")

plot(StKat_SWELow[[3]], type = "l", main = "Soil Water", ylab = "mm day-1", xlab = "Days of the Year, 2020")
lines(1000*results_spp_stkat[[1]]$Measured_SWC, col = "blue")

plot(StKat_SWELow[[1]], 12*results_spp_stkat[[1]]$Photosynthesis, main = "Difference Between", ylab = "SPP", xlab = "PRELES", col = "blue", pch = 16)
ds <- data.frame(SPP = StKat_SWELow[[1]],  PRELES = 12*results_spp_stkat[[1]]$Photosynthesis)
abline(lm(SPP ~ PRELES, data = ds), lwd = 2)
abline(0, 1, lty = 2, lwd = 2)

par(mfrow = c(1,1))
plot(NULL, xlim = c(0, 1), ylim = c(0, 1))
legend(0.5, 0.5, c("SPP", "PRELES"), col = c("black", "blue"), bty = "n", lty = 1, lwd = 2)

#####
## Weather data Tandem
######
kun.results <- "/home/joanna/Asiakirjat/SPP_Tandem/KunResults/"
bre.results <- "/home/joanna/Asiakirjat/SPP_Tandem/BreResults/"
stkat.results <- "/home/joanna/Asiakirjat/SPP_Tandem/StKatResults/"
ant.results <- "/home/joanna/Asiakirjat/SPP_Tandem/AntResults/"

load(paste0(kun.results, "kun.weather.preles.RData"))
load(paste0(bre.results, "bre.weather.preles.RData"))
load(paste0(stkat.results, "stkat.weather.preles.RData"))
load(paste0(ant.results, "ant.weather.preles.RData"))

SW <- readxl::read_excel("/home/joanna/Lataukset/Climate Tgradient sum 2014-2020_corr RH.xlsx", 1, col_names = T)
names(SW) <- SW[1,]
SW <- SW[-1,]

par(mfrow = c(2, 2))
plot(as.Date(ant.weather.preles$Date), ant.weather.preles$TAir, main = "Air Temperature", type = "l",
     ylab = "Air Temperature, Degrees C", xlab = "Date", col = colours_cb(7)[1], lwd = 2, ylim = c(-20, 30))
lines(as.Date(stkat.weather.preles$Date), stkat.weather.preles$TAir, col = colours_cb(7)[2], lwd = 2)
lines(as.Date(bre.weather.preles$Date), bre.weather.preles$TAir, col = colours_cb(7)[4], lwd = 2)
lines(as.Date(kun.weather.preles$Date), kun.weather.preles$TAir, col = colours_cb(7)[7], lwd = 2)

plot(as.Date(ant.weather.preles$Date), ant.weather.preles$PAR, main = "Daily Sum of PAR", type = "l",
     ylab = "PAR, Daily Sum, sum mmmol day-1", xlab = "Date", col = colours_cb(7)[1], lwd = 2, ylim = c(0, 60))
lines(as.Date(stkat.weather.preles$Date), stkat.weather.preles$PAR, col = colours_cb(7)[2], lwd = 2)
lines(as.Date(bre.weather.preles$Date), bre.weather.preles$PAR, col = colours_cb(7)[4], lwd = 2)
lines(as.Date(kun.weather.preles$Date), kun.weather.preles$PAR, col = colours_cb(7)[7], lwd = 2)

plot(as.Date(ant.weather.preles$Date), ant.weather.preles$VPD, main = "Vapour Pressure Deficit", type = "l",
     ylab = "VPD, kPa", xlab = "Date", col = colours_cb(7)[1], lwd = 2, ylim = c(0, 3))
lines(as.Date(stkat.weather.preles$Date), stkat.weather.preles$VPD, col = colours_cb(7)[2], lwd = 2)
lines(as.Date(kun.weather.preles$Date), kun.weather.preles$VPD, col = colours_cb(7)[7], lwd = 2)
lines(as.Date(bre.weather.preles$Date), bre.weather.preles$VPD, col = colours_cb(7)[4], lwd = 2)

plot(as.Date(ant.weather.preles$Date), ant.weather.preles$Precip, main = "Daily Sum of Precipitation", type = "l",
     ylab = "Precipitation, sum mm", xlab = "Date", col = colours_cb(7)[1], lwd = 2, ylim = c(0, 160))
lines(as.Date(stkat.weather.preles$Date), stkat.weather.preles$Precip, col = colours_cb(7)[2], lwd = 2)
lines(as.Date(bre.weather.preles$Date), bre.weather.preles$Precip, col = colours_cb(7)[4], lwd = 2)
lines(as.Date(kun.weather.preles$Date), kun.weather.preles$Precip, col = colours_cb(7)[7], lwd = 2)
legend(as.Date("2015-01-01"), 150, c("Antngac", "St Katherin", "Bredinge", "Kungsbo"), col = colours_cb(7)[c(1, 2, 4, 7)], lty = 1, bty = "n", lwd = 2)


####
# Soil Water
####

SW <- read.csv(paste0("/home/joanna/Asiakirjat/CMIP6/Callibration/FLX_SE-Nor_FLUXNET2015_FULLSET_HH_2014-2020_beta-3.csv"))
SW_later <- SW[substring(SW$TIMESTAMP_START, 1, 4) > 2014,]
SW_later_yearly <- aggregate(SWC_F_MDS_2 ~ substring(TIMESTAMP_START, 1, 8),  data = SW_later, mean)[,2]

results_spp_kun <- lapply(paste0(kun.data, list.files(kun.data, "KunWeather")[2:7]), read.delim, sep = " ")
results_spp_bre <- lapply(paste0(bre.data, list.files(bre.data, "BreWeather")[2:7]), read.delim, sep = " ")
results_spp_stkat <- lapply(paste0(stkat.data, list.files(stkat.data, "StKatWeather")[2:7]), read.delim, sep = " ")
results_spp_ant <- lapply(paste0(ant.data, list.files(ant.data, "AntWeather")[2:7]), read.delim, sep = " ")

results_spp_kun <- lapply(results_spp_kun, function(x) {
  names(x) = paste0("x", 1:ncol(x))
  return(x)
  })
results_spp_bre <- lapply(results_spp_bre, function(x) {
  names(x) = paste0("x", 1:ncol(x))
  return(x)
})
results_spp_stkat <- lapply(results_spp_stkat, function(x) {
  names(x) = paste0("x", 1:ncol(x))
  return(x)
})
results_spp_ant <- lapply(results_spp_ant, function(x) {
  names(x) = paste0("x", 1:ncol(x))
  return(x)
})


weather_spp_ant <- do.call(rbind, results_spp_ant)
weather_spp_ant$Date <- as.Date(paste(weather_spp_ant$x1, weather_spp_ant$x2, weather_spp_ant$x3, sep = "-", na.strings = -9999))
weather_spp_kun <- do.call(rbind, results_spp_kun)
weather_spp_kun$Date <- as.Date(paste(weather_spp_kun$x1, weather_spp_kun$x2, weather_spp_kun$x3, sep = "-", na.strings = -9999))
weather_spp_stkat <- do.call(rbind, results_spp_stkat)
weather_spp_stkat$Date <- as.Date(paste(weather_spp_stkat$x1, weather_spp_stkat$x2, weather_spp_stkat$x3, sep = "-", na.strings = -9999))
weather_spp_bre <- do.call(rbind, results_spp_bre)
weather_spp_bre$Date <- as.Date(paste(weather_spp_bre$x1, weather_spp_bre$x2, weather_spp_bre$x3, sep = "-", na.strings = -9999))

ant.weather.preles$SW <- aggregate(x14 ~ Date, weather_spp_ant, mean)[,2]
kun.weather.preles$SW <- aggregate(x14 ~ Date, weather_spp_kun, mean)[,2]
stkat.weather.preles$SW <- aggregate(x14 ~ Date, weather_spp_stkat, mean)[,2]
bre.weather.preles$SW <- aggregate(x14 ~ Date, weather_spp_bre, mean)[,2]

bre.weather.preles$SW[(bre.weather.preles$SW < quantile(bre.weather.preles$SW, .05)) | (bre.weather.preles$SW > quantile(bre.weather.preles$SW, .90))] <- NA
kun.weather.preles$SW[kun.weather.preles$SW < quantile(kun.weather.preles$SW, .28, na.rm = T)] <- NA

kun.weather.preles$SW[is.na(kun.weather.preles$SW)] <- 0.01*SW_later_yearly[is.na(kun.weather.preles$SW)] + (mean(kun.weather.preles$SW, na.rm = T) - 0.01*mean(SW_later_yearly, na.rm = T))

plot(as.Date(ant.weather.preles$Date), ant.weather.preles$SW, main = "Soil Water Content", type = "l",
     ylab = "Soil Water Content, decimal %", xlab = "Date", col = colours_cb(7)[1], lwd = 2, ylim = c(0, 0.4))
lines(as.Date(stkat.weather.preles$Date), stkat.weather.preles$SW, col = colours_cb(7)[2], lwd = 2)
lines(as.Date(bre.weather.preles$Date), bre.weather.preles$SW, col = colours_cb(7)[4], lwd = 2)
lines(as.Date(kun.weather.preles$Date), kun.weather.preles$SW, col = colours_cb(7)[7], lwd = 2)


####
# Weather data swapped
####

kun.results <- "/home/joanna/Asiakirjat/SPP_Tandem/KunResults/"
bre.results <- "/home/joanna/Asiakirjat/SPP_Tandem/BreResults/"
stkat.results <- "/home/joanna/Asiakirjat/SPP_Tandem/StKatResults/"
ant.results <- "/home/joanna/Asiakirjat/SPP_Tandem/AntResults/"

files.kun = list.files(kun.results, "_WeatherSwap")[c(2, 5, 8)]
files.bre = list.files(bre.results, "_WeatherSwap")[c(2, 5, 8)]
files.stkat = list.files(stkat.results, "_WeatherSwap")[c(2, 5, 8)]
files.ant = list.files(ant.results, "_WeatherSwap")[c(2, 5, 8)]

weather_swap_kun <- lapply(paste0(kun.results, files.kun), read.delim, sep = "\t")
weather_swap_bre <- lapply(paste0(bre.results, files.bre), read.delim, sep = "\t")
weather_swap_stkat <- lapply(paste0(stkat.results, files.stkat), read.delim, sep = "\t")
weather_swap_ant <- lapply(paste0(ant.results, files.ant), read.delim, sep = "\t")

par(mfrow = c(2, 2))
plot(NULL, xlim = c(1, 366), ylim = c(0, 1.1), main = "Kungsbo", xlab = "Day of Year", ylab = "Photosynthesis, g C m-2 day-1")
for (i in 1:length(weather_swap_kun)) {
  lines(weather_swap_kun[[i]]$DoY, 12*weather_swap_kun[[i]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][i], lwd  = 2)
  lines(weather_swap_kun[[i]]$DoY, 12*results_spp_kun[[5]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][4], lwd  = 2)
}
legend(0, 1, c("Kungsbo", "Bredinge", "St Katherinen", "Antagnac"), col = colours_cb_245(3), lty = 1, bty = "n")

plot(NULL, xlim = c(1, 366), ylim = c(0, 1.1), main = "Bredinge", xlab = "Day of Year", ylab = "Photosynthesis, g C m-2 day-1")
for (i in 1:length(weather_swap_kun)) {
  if (i == 1) {j = i} else {j = i + 1}
  lines(weather_swap_bre[[i]]$DoY, 12*weather_swap_bre[[i]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][j], lwd  = 2)
  lines(weather_swap_kun[[i]]$DoY, 12*results_spp_bre[[5]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][2], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 1.1), main = "St Katherinen", xlab = "Day of Year", ylab = "Photosynthesis, g C m-2 day-1")
for (i in 1:length(weather_swap_kun)) {
  if (i <= 2) {j = i} else {j = i + 1}
  lines(weather_swap_stkat[[i]]$DoY, 12*weather_swap_stkat[[i]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][j], lwd  = 2)
  lines(weather_swap_kun[[i]]$DoY, 12*results_spp_stkat[[5]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][3], lwd  = 2)
}

plot(NULL, xlim = c(1, 366), ylim = c(0, 1.1), main = "Antagnac", xlab = "Day of Year", ylab = "Photosynthesis, g C m-2 day-1")
for (i in 1:length(weather_swap_kun)) {
  j = i + 1
  lines(weather_swap_kun[[i]]$DoY, 12*results_spp_ant[[5]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][1], lwd  = 2)
  lines(weather_swap_ant[[i]]$DoY, 12*weather_swap_ant[[i]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][i+1], lwd  = 2)
}

####
# Weather changes, parameter changes
####

photo_spp_ant <- lapply(paste0(ant.results, list.files(path = ant.results, pattern = "_D_")[c(20, 26:28)]),
                        read.delim, sep = "\t", dec = ".", na.strings = -9999, row.names = NULL)

par(mfrow = c(1, 1))
plot(NULL, xlim = c(1, 366), ylim = c(0, 0.5), main = "Parameters Changed", xlab = "Day of Year, 2019", ylab = "Photosynthesis, gC m-2 day-1")
for (i in length(photo_spp_ant):1) {
  lines(photo_spp_ant[[i]]$DoY,
        12*photo_spp_ant[[i]]$Photosynthesis,
        col = colours_cb(7)[c(1, 2, 4, 7)][i], lwd  = 2)
}
legend(0, 0.5, c("Antagnac", "Bredinge", "Kungsbo", "St Katherinen"), col = colours_cb(7)[c(1, 2, 4, 7)], lty = 1, bty = "n", lwd = 2)

plot(NULL, xlim = c(1, 366), ylim = c(0, 7), main = "Parameters Changed", xlab = "Day of Year, 2019", ylab = "Transpiration, µmol m-2 s-1")
for (i in length(photo_spp_ant):1) {
  lines(photo_spp_ant[[i]]$DoY,
        photo_spp_ant[[i]]$Transpiration,
        col = colours_cb(7)[c(1, 2, 4, 7)][i], lwd  = 2)
}
legend(0, 6.5, c("Antagnac", "Bredinge", "Kungsbo", "St Katherinen"), col = colours_cb(7)[c(1, 2, 4, 7)], lty = 1, bty = "n", lwd = 2)



####
# Different genotypes
####

origins_spp_stkat <- lapply(paste0(stkat.results, list.files(path = stkat.results, pattern = "_D_")[c(8, 9, 12, 13)]),
                            read.delim, sep = "\t", dec = ",", na.strings = -9999, row.names = NULL)

par(mfrow = c(1, 1))
plot(NULL, xlim = c(1, 366), ylim = c(0, 1.6), main = "St Katherinen different clones", xlab = "Day of Year", ylab = "Photosynthesis, g C m-2 day-1")
for (i in length(origins_spp_stkat):1) {
  lines(origins_spp_stkat[[i]]$DoY, 12*origins_spp_stkat[[i]]$Photosynthesis, col = colours_cb(7)[c(1, 2, 4, 7)][i], lwd  = 2)
}
legend(0, 1.6, c("BLRHigh", "BLRLow", "SWEHigh", "SWELow"), col = colours_cb(7)[c(1, 2, 4, 7)], lty = 1, bty = "n", lwd = 2)




