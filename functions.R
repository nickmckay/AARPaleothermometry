# AAR Paleothermometry forward model — functions.R
# Written Jan 2023 for NSF 2317409. Defines the core racemization pipeline:
# Arrhenius rate constant -> incremental D/L accumulation in power-law space ->
# sediment temperature damping -> full per-sample integration.
# Source powerLaw.R for sensitivity analysis; these functions are called from there.

# Power-law exponent for linearizing D/L: D/L = Rx^(1/x). Default 3 used here;
# powerLaw.R also tests x=4. Bada (1985) recommends 3 for isoleucine.
x <- 3

racemize <- function(age,sumT,winT,x,dT,startingDL = 0,scenario = NA,...){
  #age needs to go from old to young
  as <- sort(age,decreasing = TRUE,index.return = TRUE)
  age <- age[as$ix]
  sumT <- sumT[as$ix]
  winT <- winT[as$ix]
  
  
  
  Rx <- startingDL
  for(i in 2:length(age)){
    #summer half
    drSum <- dRacPL(kt = arrhenius(Temp = sumT[i] + 273),delta.yr = dT/2,...)
    
    #winter half
    drWin <- dRacPL(kt = arrhenius(Temp = winT[i] + 273),delta.yr = dT/2,...)
    
    Rx <- Rx + drSum + drWin 
  }
  # Rx is accumulated in (D/L)^x space; back-transform here to return actual D/L.
  rac <- data.frame(age = max(age),
                        DL = Rx^(1/x),
                        scenario = scenario)
  
  return(rac)
}

# As sediment is buried, the seasonal air temperature signal attenuates — summer
# and winter sediment temps converge toward the mean annual. This is physically
# motivated by thermal diffusion: the annual cycle damps with depth. We model it
# as a linear interpolation from full seasonal amplitude (fresh, weight1=startDamp)
# to attenuated (buried, weight1=endDamp) over dampTime ka.
dampSedTemps <- function(sumT, 
                         winT, 
                         age, 
                         startDamp = 1,
                         endDamp = 0.5,
                         dampTime = 8){
  #age needs to go from old to young
  as <- sort(age,decreasing = TRUE,index.return = TRUE)
  age <- age[as$ix]
  sumT <- sumT[as$ix]
  winT <- winT[as$ix]
  
  filterStart <- max(age,na.rm = TRUE)

  ffrac <- (filterStart - age)/dampTime
  # ffrac goes from 0 (oldest, fully buried) to 1 (youngest, at surface).
  # tf is the weight on the same-season temperature; (1-tf) is the cross-season
  # weight, so a deeply buried summer sediment layer gets partly 'winter' temp.
  tf <- endDamp*ffrac + startDamp*(1-ffrac)
  tf[tf < endDamp] <- endDamp
  
  weight1 <- tf
  weight0 <- 1-tf

  sumTsed <- sumT * weight1 + winT * weight0
  winTsed <- winT * weight1 + sumT * weight0
  
  out <- data.frame(age = age,
                    sumT = sumT,
                    winT = winT,
                    sumTsed = sumTsed,
                    winTsed = winTsed)
  
  return(out)
}

#racemize one sample
racemizeSample <- function(sampleAge,
                           ageVec,
                           sumT,
                           winT,
                           x,
                           dT,
                           startingDL = 0,
                           scenario = NA,
                           startDamp = 1, 
                           endDamp = 0.5,
                           dampTime = 10,
                           ...){

#figure out sample temperature history
  # Select only the temperature history up to this sample's deposition age;
  # older parts of the record are irrelevant because this grain wasn't there yet.
  ageIndex <- which(ageVec <= sampleAge)
  
  sAge <- ageVec[ageIndex]
  sSumT <- sumT[ageIndex]
  sWinT <- winT[ageIndex]
  
#now damp each sample
  
  dampened <- dampSedTemps(sumT = sSumT,
                           winT = sWinT,
                           age = sAge,
                           startDamp = startDamp,
                           endDamp = endDamp,
                           dampTime = dampTime)
  
#now racemize that sample
  thisDL <- racemize(age = dampened$age,
                     sumT = dampened$sumTsed,
                     winT = dampened$winTsed,
                     x = x,
                     dT = dT,
                     startingDL = startingDL,
                     scenario = scenario,
                     ...)
  return(thisDL)
  
}


plotInputs <- function(dampened){
  
  toPlot <- pivot_longer(dampened,
                         starts_with("sumT") | starts_with("winT") | starts_with("annT"),
                         values_to = "Temperature",names_to = "scenario")
  
  inputs <- ggplot(toPlot) +
    geom_line(aes(x = age, y = Temperature, color = scenario,linetype = scenario)) +
    theme_bw() + 
    scale_color_manual(values = c("black","black","red","red","blue","blue")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) 
  
  return(inputs)
}




# --- Test/demo block ---
# Exercises racemizeSample() over a 14 ka synthetic core with 3 segments:
#   early (0-4.7 ka):  sumT=15, winT=4  (warm)
#   middle (4.7-9.3 ka): sumT=15, winT=-20 (cold winters — glacial-like?)
#   late (9.3-14 ka):  sumT=20, winT=4  (hot summers)
# Note: endDamp has a typo (endDamep) on line 143 so endDamp falls back to
# whatever value it holds from a previous run — fix before using this demo.
dT <- 10 #timestep
Rx <- 0 #starting racemization
age <- (seq(00,14000,by = dT)/1000)

sth <- 15
stl <- 10

sumT <- c(rep(sth,times = length(age)/3),
          rep(sth,times = length(age)/3),
          rep(sth + 5,times = length(age)/3))
winT <- c(rep(4,times = length(age)/3),
          rep(-20,times = length(age)/3),
          rep(4,times = length(age)/3))


sampleAge <- (seq(00,14000,by = 100)/1000)

#plot inputs for one sample
startDamp <- 1
endDamp <- .5
dampTime <- 10
dampened <- dampSedTemps(sumT = sumT,
                         winT = winT, 
                         age = age, 
                         startDamp = startDamp,
                         endDamp = endDamp,
                         dampTime = dampTime)

inputs <- plotInputs(dampened)


#racemize all samples
out <- map_dfr(.x = sampleAge,
               .f = racemizeSample,
               ageVec = age,
               sumT = sumT,
               winT = winT,
               x = x,
               dT = dT,
               startingDL = 0,
               scenario = "test",
               startDamp = 1, 
               endDamp = 0.5,
               dampTime = dampTime)

# rac <- racemize(age = dampened$age,
#                 sumT = dampened$sumT,
#                 winT = dampened$winT,
#                 x = x,
#                 dT = dT,
#                 scenario = "test")

dl <- ggplot(out) + 
  geom_line(aes(x = age, y = DL, color = scenario)) +
  theme_bw() + 
  xlab("Age (BP)") +
  ylab("D/L")
  

dl

library(egg)
ggarrange(plots = list(inputs,dl),ncol = 1)
