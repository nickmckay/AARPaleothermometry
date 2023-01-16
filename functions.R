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
  rac <- data.frame(age = max(age),
                        DL = Rx^(1/x),
                        scenario = scenario)
  
  return(rac)
}

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
endDamep <- .5
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
