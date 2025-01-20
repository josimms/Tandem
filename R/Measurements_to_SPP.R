medlyn_2001_eq18 <- function(Tk, parameter_max) {
  R = 8.314
  out = parameter_max$Kopt * (parameter_max$Hd * exp(parameter_max$Ha * (Tk - parameter_max$Topt) / (Tk*R*parameter_max$Topt))) / (parameter_max$Hd - parameter_max$Ha * (1 - exp(parameter_max$Hd*(Tk - parameter_max$Topt)/(Tk*R*parameter_max$Topt))))
  return(out)
}

Collatz_1992 <- function(TC, parameter_max) {
  parameter_max <- lapply(parameter_max, as.numeric)
  out = parameter_max$k25 * parameter_max$Q10^((TC - parameter_max$Topt)/10)/((1+exp(parameter_max$s2*(parameter_max$Tmin-TC)))*(1+exp(parameter_max$s1*(TC-parameter_max$Tmax))))
  return(out)
}

Measurements_to_SPP <- function(Lasse_parameters) {

  ### FUNCTION
  sse_photosynthesis <- function(SPP, Lasse) {
    TC = 1:41
    Tk = 1:41 + 273.15
    return(sum(abs(medlyn_2001_eq18(Tk, Lasse) - Collatz_1992(TC, SPP))))
  }

  ### INITIAL CONDITIONS
  # Real data to be transofred
  Lasse <- list(Vcmax = list(Topt = Lasse_parameters[1],
                             Ha = Lasse_parameters[2],
                             Kopt = Lasse_parameters[3],
                             Hd = Lasse_parameters[4],
                             k25 = Lasse_parameters[5]/3),
                Jmax = list(Topt = Lasse_parameters[6],
                            Ha = Lasse_parameters[7],
                            Kopt = Lasse_parameters[8],
                            Hd = Lasse_parameters[9],
                            k25 = Lasse_parameters[10]/3))

  # Output
  SPP <- list(Vcmax = list(Q10 = NA,
                           Tmin = NA,
                           Tmax = NA,
                           s2 = NA,
                           s1 = NA,
                           Topt = NA,
                           k25 = NA),
              Jmax = list(Q10 = NA,
                          Tmin = NA,
                          Tmax = NA,
                          s2 = NA,
                          s1 = NA,
                          Topt = NA,
                          k25 = NA))

  # Initial conditions
  SPP_0 = list(Vcmax = list(Q10 = 2.04,
                            Tmin = 0.0000181,
                            Tmax = 37.24,
                            s2 = 0.255,
                            s1 = 0.285,
                            Topt = 25,
                            k25 = Lasse$Vcmax$k25),
               Jmax = list(Q10 = 1.69,
                           Tmin = -2.478,
                           Tmax = 35.38,
                           s2 = 0.210043,
                           s1 = 0.2954,
                           Topt = 25,
                           k25 = Lasse$Jmax$k25))

  ### Optimisation function
  for (parameter in c("Jmax", "Vcmax")) {
    out = optim(par = SPP_0[[parameter]], sse_photosynthesis, Lasse = Lasse[[parameter]], method = "CG")

    SPP[[parameter]]$Q10 <- as.numeric(out$par[1])
    SPP[[parameter]]$Tmin <- as.numeric(out$par[2])
    SPP[[parameter]]$Tmax <- as.numeric(out$par[3])
    SPP[[parameter]]$s2 <- as.numeric(out$par[4])
    SPP[[parameter]]$s1 <- as.numeric(out$par[5])
    SPP[[parameter]]$Topt <- as.numeric(out$par[6])
    SPP[[parameter]]$k25 <- Lasse[[parameter]]$k25
  }

  return(SPP)
}

# k25 divided by 3 for the lead area
data_fitted <- c(Kun = list(BELHigh = c(302.817, 32437.525, 119.73, 200000, 110.6308375), # Jmax
                            BELLow = c(303.175, 43178.749, 122.89, 200000, 109.5070388), # Jmax
                            SWEHigh = c(302.202, 48940.257, 137.359, 200000, 125.7526343), # Jmax
                            SWELow = c(302.766, 48547.806, 116.282, 200000, 104.1589822)), # Jmax
                 Bre = list(BELHigh = c(),
                            BELLow = c(),
                            SWEHigh = c(),
                            SWELow = c()),
                 StKat = list(BELHigh = c(304.193, 68229.812, 84.808/3, 200000, 67.46984312, # Vcmax
                                          300.774, 37453.807, 175.047/3, 200000, 169.4663374), # Jmax
                              BELLow = c(305.905, 53666.439, 97.959/3, 200000, 73.83178072, # Vcmax
                                         302.775, 47522.312, 199.862/3, 200000, 179.3067973), # Jmax
                              SWEHigh = c(304.49, 50401.944, 92.692/3, 200000, 76.35376239, # Vcmax
                                          300.495, 31214.664, 166.476/3, 200000, 162.7632576), # Jmax
                              SWELow = c(305.4, 52901.493, 102.626/3, 200000, 79.84290298, # Vcmax
                                         303.411, 25339.729, 215.111/3, 200000, 199.176284))) # Jmax

origin <- data_fitted$StKat.BELLow
Lasse <- list(Vcmax = list(Topt = origin[1],
                           Ha = origin[2],
                           Kopt = origin[3],
                           Hd = origin[4],
                           k25 = origin[5]),
              Jmax = list(Topt = origin[6],
                          Ha = origin[7],
                          Kopt = origin[8],
                          Hd = origin[9],
                          k25 = origin[10]))

Colataz_params <- Measurements_to_SPP(data_fitted$StKat.BELLow)

par(mfrow = c(2, 1))
for (parameter in c("Jmax", "Vcmax")) {
  plot(1:41, medlyn_2001_eq18(1:41 + 273.15, Lasse[[c(parameter)]]), type = "l", xlab = "Degrees C", ylab = parameter, main = parameter)
  lines(1:41, Collatz_1992(1:41, Colataz_params[[c(parameter)]]), type = "l", col = "blue")
}

#Lasse_parameters = c(305.1833333, 53233.33333, 39.58888889, 200000, 30.82222222*3, # Vcmax
#                     302.9166667, 45466.66667, 63.36666667, 200000, 56.27777778*3) # Jmax
#Lasse <- list(Vcmax = list(Topt = Lasse_parameters[1],
#                           Ha = Lasse_parameters[2],
#                           Kopt = Lasse_parameters[3],
#                           Hd = Lasse_parameters[4],
#                           k25 = Lasse_parameters[5]),
#              Jmax = list(Topt = Lasse_parameters[6],
#                          Ha = Lasse_parameters[7],
#                          Kopt = Lasse_parameters[8],
#                          Hd = Lasse_parameters[9],
#                          k25 = Lasse_parameters[10]))
#test_cali <- Measurements_to_SPP(Lasse_parameters)

#par(mfrow = c(2, 1))
#for (parameter in c("Jmax", "Vcmax")) {
#  plot(1:41, medlyn_2001_eq18(1:41 + 273.15, Lasse[[parameter]]), type = "l", xlab = "Degrees C", ylab = parameter, main = parameter)
#  lines(1:41, Collatz_1992(1:41, test_cali[[parameter]]), type = "l", col = "blue")
#}



