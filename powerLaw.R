# AAR Paleothermometry sensitivity analysis — powerLaw.R
# Written Jan 2023 for NSF 2317409 ("Testing amino acid paleothermometry in
# radiocarbon-dated lake sediment"). The goal is to understand how different
# Holocene temperature histories would produce distinguishable D/L signals,
# and whether sediment burial damping obscures that signal.
# Arrhenius parameters (Ae, Ea) use literature defaults; not yet calibrated to
# Leah Marshall's calibration samples from the AAGL.

#D/Lx = kTt + C	 (eq 1)
# Placeholder for the integrated form — the incremental version dRacPL is used instead.
racemizationPL <- function(kt,yr,C){

}

# Incremental racemization in power-law space: (D/L)^x increases linearly with kt*dt.
# Working in (D/L)^x rather than D/L is the Bada linearization — avoids the sigmoidal
# curvature that makes D/L accumulation rate-dependent on the current value.
dRacPL <- function(kt,delta.yr){
 deltaRacX <- kt * delta.yr
 return(deltaRacX)
}


#k = Ae–Ea/RT  or:  ln(k) = – (Ea/R) x T-1 + ln(A)	(eq 2)
#R is a constant (0.001987 kcal K-1 mol-1).
# Ea = 31.5 kcal/mol and Ae = exp(42.12) are standard literature values for
# isoleucine epimerization in foraminifera (Kaufman 2000 context). These will need
# to be fit to Leah's calibration samples once those are available.
arrhenius <- function(Ae = exp(42.12),Ea = 31.5 ,Temp = 278,R = 0.001987){
  lnk <- -(Ea/R) * Temp^-1 + log(Ae)
  kt <- exp(lnk)
  #kt <- Ae - Ea/(R*Temp)
  return(kt)
}

# x is the power-law exponent: D/L = Rx^(1/x). x=3 is a common choice; x=4 is also
# tested later. Higher x compresses the upper end of the D/L scale more.
x <- 3.0

# Sanity check: 200 ka at 4°C then 200 ka at 6°C. Just confirms the loop works
# and that a warmer period accelerates accumulation as expected.
Rx <- 0
Temp <- c(rep(4+273,200),rep(6+273,200))
for(i in 2:400){
  dr <- dRacPL(arrhenius(Temp = Temp[i]),1000)
  Rx[i] <- Rx[i-1] + dr

}

Rac <- Rx^(1/x)
age <- 0:399

plot(age,Rx,type = "l")



# seasonality -------------------------------------------------------------

# Key question: does seasonality matter, or does only mean annual temperature matter?
# Because Arrhenius is nonlinear (exponential in 1/T), a 1°C warmer summer adds more
# to kt than a 1°C cooler winter subtracts. So two sites with the same mean annual T
# but different seasonal amplitudes will accumulate different D/L — this is the core
# motivation for treating summer and winter half-years separately throughout the model.

library(tidyverse)

#simulate the effect of seasonality on a sample.
sumT <- 15 #summer half year
winT <- 4 # winter half year
Rx <- 0 #starting racemization
dT <- 1000 #timestep
age <- seq(0,400000,by = dT)/1000


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
  
}


toplot <- data.frame(age = age,
                     DL = Rx^(1/x),
                     seasonality = paste("sum =",sumT," & win = ",winT))


sumT <- 19 #summer half year
winT <- 0 # winter half year
for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
  
}

toplot <- toplot %>% 
  bind_rows(data.frame(age = age,
                     DL = Rx^(1/x),
                     seasonality = paste("sum =",sumT," & win = ",winT)))



sumT <- 11 #summer half year
winT <- 8 # winter half year
for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
  
}

toplot <- toplot %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       seasonality = paste("sum =",sumT," & win = ",winT)))

ggplot(toplot) + 
  geom_line(aes(x = age,y = DL,color = seasonality)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw()



# Changing temperature -----------------------------------------------------

# Three Holocene summer temperature trends over 10 ka. The central question is whether
# AAR D/L can distinguish a Holocene Thermal Maximum (warming → cooling) from a stable
# or monotonically warming scenario — relevant to the "Holocene temperature conundrum"
# (proxies show ~0.7°C late-Holocene cooling; models show warming).
# Winter T held constant at 4°C so changes are driven by summer only.

dT <- 10 #timestep
Rx <- 0 #starting racemization
age <- seq(0,10000,by = dT)/1000
sumT <- seq(15,10,length.out = length(age))
winT <- seq(4,4,length.out = length(age))


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2 <- data.frame(age = age,
           DL = Rx^(1/x),
           scenario = "cooling summer")

sumT <- seq(12.5,12.5,length.out = length(age))
for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}
  

tp2 <- tp2 %>% 
  bind_rows(data.frame(age = age,
           DL = Rx^(1/x),
           scenario = "stable summer"))

sumT <- seq(10,15,length.out = length(age))
for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2 <- tp2 %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "warming summer"))

ggplot(tp2) + 
  geom_line(aes(x = age,y = DL,color = scenario)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() + 
  ggtitle("Effect of summer temperature trend on racemization")


# Changing temperature with damping ---------------------------------------

# Repeats the three-scenario analysis but applies sediment burial damping:
# as sediment is buried deeper, the seasonal air temperature signal attenuates —
# summer and winter sediment temperatures converge toward the mean annual.
# weight1 (full-signal weight) goes from 1.0 (surface) to 0.5 (buried),
# weight0 (cross-season weight) mirrors it, so at full attenuation
# sumTsed ≈ winTsed ≈ mean(sumT, winT).
# The comparison with/without damping shows how much burial attenuates our ability
# to detect temperature trends in D/L — older samples are most affected.
# Age vector is reversed here (old to young) so the loop accumulates correctly.

dT <- 10 #timestep
Rx <- 0 #starting racemization
age <- rev(seq(0,10000,by = dT)/1000)
sumT <- seq(13,10,length.out = length(age))
winT <- seq(4,4,length.out = length(age))
weight1 <- seq(1,0.5,length.out = length(age))
weight0 <- seq(0,0.5,length.out = length(age))
plotColorsRac <- RColorBrewer::brewer.pal(n = 3,name = "Set1")[c(1,3,2)]
plotColorsTrend <- c("black",RColorBrewer::brewer.pal(n = 3,name = "Set1")[1:2])


cooling <- data.frame(age  = age,
                          temperature = sumT,
                          location = "air",
                          season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winT,
                        location = "air",
                        season = "winter")) %>% 
  bind_rows( data.frame(age  = age,
                        temperature = rowMeans(cbind(winT,sumT)),
                        location = "air",
                        season = "annual"))


#damping
sumTsed <- sumT * weight1 + winT * weight0
winTsed <- winT * weight1 + sumT * weight0

#ann
coolingDamp <- data.frame(age  = age,
                          temperature = sumTsed,
                          location = "sediment",
                          season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winTsed,
                        location = "sediment",
                        season = "winter"))

coolingTp <- bind_rows(cooling,coolingDamp)

coolingPlot <- ggplot(coolingTp) +
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  theme_bw() + 
  scale_color_manual(values = plotColorsTrend) +
  ggtitle(glue::glue("Summer cooling trend - Holocene Mean air Temp = {mean(c(winT,sumT))}"))



for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumTsed[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winTsed[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2damp <- data.frame(age = age,
                  DL = Rx^(1/x),
                  scenario = "cooling")


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2 <- data.frame(age = age,
                      DL = Rx^(1/x),
                      scenario = "cooling")

#scenario two


sumT <- seq(10,10,length.out = length(age))


stable <- data.frame(age  = age,
                      temperature = sumT,
                      location = "air",
                      season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winT,
                        location = "air",
                        season = "winter")) %>% 
  bind_rows( data.frame(age  = age,
                        temperature = rowMeans(cbind(winT,sumT)),
                        location = "air",
                        season = "annual"))

#damping
sumTsed <- sumT * weight1 + winT * weight0
winTsed <- winT * weight1 + sumT * weight0

#ann
stableDamp <- data.frame(age  = age,
                          temperature = sumTsed,
                          location = "sediment",
                          season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winTsed,
                        location = "sediment",
                        season = "winter"))

stableTp <- bind_rows(stable,stableDamp)

stablePlot <- ggplot(stableTp) +
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  theme_bw() + 
  scale_color_manual(values = plotColorsTrend) +
  ggtitle("No summer trend")




for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumTsed[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winTsed[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2damp <- tp2damp %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "stable"))


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}

tp2 <- tp2 %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "stable"))

sumT <- seq(7,10,length.out = length(age))



warming <- data.frame(age  = age,
                     temperature = sumT,
                     location = "air",
                     season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winT,
                        location = "air",
                        season = "winter")) %>% 
  bind_rows( data.frame(age  = age,
                        temperature = rowMeans(cbind(winT,sumT)),
                        location = "air",
                        season = "annual"))

#damping
sumTsed <- sumT * weight1 + winT * weight0
winTsed <- winT * weight1 + sumT * weight0

#ann
warmingDamp <- data.frame(age  = age,
                         temperature = sumTsed,
                         location = "sediment",
                         season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winTsed,
                        location = "sediment",
                        season = "winter"))

warmingTp <- bind_rows(warming,warmingDamp)

warmingPlot <- ggplot(warmingTp) +
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  theme_bw() + 
  scale_color_manual(values = plotColorsTrend) +
  ggtitle("Summer warming trend")




for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumTsed[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winTsed[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}

tp2damp <- tp2damp %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "warming"))


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}

tp2 <- tp2 %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "warming"))

coolingTp$trend <- "cooling"
stableTp$trend <- "stable"
warmingTp$trend <- "warming"

allTrends <- bind_rows(coolingTp, stableTp, warmingTp)
inputDataPlot <- ggplot(allTrends) + 
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  scale_color_manual(values = plotColorsTrend) +
  facet_grid(trend ~ .,switch = "both") +
  ylab("Temperature (deg C)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() +
  ggtitle(glue::glue("Simulated temperature trends. Holocene Mean air Temp = {mean(c(winT,sumT))} deg C"))


annTrends <- allTrends %>% 
  filter(season == "annual" & location == "air") %>% 
  ggplot() +
  geom_line(aes(x = age, y = temperature, color = trend)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("Temperature (deg C)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() +
  ggtitle("Annual air temperature trends")



noDampNL <- ggplot(tp2) + 
  geom_line(aes(x = rev(age),y = DL,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("D/L") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = c(0.8,0.3)) +
  ggtitle("Temperature trend and racemization - no signal attenuation")



DampNL <- ggplot(tp2damp) + 
  geom_line(aes(x = rev(age),y = DL,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("D/L") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = "none") +
  ggtitle("Temperature trend and racemization - with attenuation")

# Re-run plots with x=4 to show how choice of power-law exponent affects visual
# spread between scenarios — higher x stretches the D/L scale more at low values.
x <- 4

noDampL <- ggplot(tp2) + 
  geom_line(aes(x = rev(age),y = DL^x,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("(D/L)^x") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = "none") +
  
  ggtitle("Temperature trend and racemization (linearized) - no attenuation")



DampL <- ggplot(tp2damp) + 
  geom_line(aes(x = rev(age),y = DL^x,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("(D/L)^x") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = "none") +
  ggtitle("Temperature trend and racemization (linearized) - with attenuation")


library(egg)
racPlot <- ggarrange(plots = list(noDampNL,DampNL,noDampL,DampL), ncol = 2)

ggsave(racPlot,filename = "Holocene sensitivity racemization.pdf",scale = .75)
ggsave(inputDataPlot,filename = "Temperature scenarios.pdf",scale = .75)
ggsave(annTrends,filename = "Annual temperature scenarios.pdf",scale = .75)


# Wave temperature with damping ---------------------------------------

# Second scenario set: non-monotonic Holocene-like patterns over 14 ka.
# Three scenarios represent plausible paleoclimate histories at a lake site:
#   "cool mid-Holocene" (sth/stl/sth): warm early + late, cool middle
#   "warm mid-Holocene" (stl/sth/stl): cool early + late, warm middle (classic HTM)
#   "dry lake" scenario: extreme winter cooling (-40°C) in the middle segment,
#     representing a period when the lake was dry or ice-covered (glacial or arid interval)
#     so the sediment was exposed to cold continental air rather than water temperatures.
# The question is whether the resulting D/L patterns are distinguishable from each other.

dT <- 10 #timestep
Rx <- 0 #starting racemization
age <- rev(seq(00,14000,by = dT)/1000)

sth <- 15
stl <- 10

sumT <- c(rep(sth,times = length(age)/3),
          rep(stl,times = length(age)/3),
          rep(sth,times = length(age)/3))
winT <- seq(4,4,length.out = length(age))
weight1 <- seq(1,0.5,length.out = length(age))
weight0 <- seq(0,0.5,length.out = length(age))
plotColorsRac <- RColorBrewer::brewer.pal(n = 3,name = "Set1")[c(1,3,2)]
plotColorsTrend <- c("black",RColorBrewer::brewer.pal(n = 3,name = "Set1")[1:2])


cooling <- data.frame(age  = age,
                      temperature = sumT,
                      location = "air",
                      season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winT,
                        location = "air",
                        season = "winter")) %>% 
  bind_rows( data.frame(age  = age,
                        temperature = rowMeans(cbind(winT,sumT)),
                        location = "air",
                        season = "annual"))


#damping
sumTsed <- sumT * weight1 + winT * weight0
winTsed <- winT * weight1 + sumT * weight0

#ann
coolingDamp <- data.frame(age  = age,
                          temperature = sumTsed,
                          location = "sediment",
                          season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winTsed,
                        location = "sediment",
                        season = "winter"))

coolingTp <- bind_rows(cooling,coolingDamp)

coolingPlot <- ggplot(coolingTp) +
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  theme_bw() + 
  scale_color_manual(values = plotColorsTrend) +
  ggtitle(glue::glue("Summer cooling trend - Holocene Mean air Temp = {mean(c(winT,sumT))}"))



for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumTsed[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winTsed[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2damp <- data.frame(age = age,
                      DL = Rx^(1/x),
                      scenario = "cooling")


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2 <- data.frame(age = age,
                  DL = Rx^(1/x),
                  scenario = "cooling")

#scenario two

sumT <- c(rep(stl,times = length(age)/3),rep(sth,times = length(age)/3),rep(stl,times = length(age)/3))
winT <- seq(4,4,length.out = length(age))


stable <- data.frame(age  = age,
                     temperature = sumT,
                     location = "air",
                     season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winT,
                        location = "air",
                        season = "winter")) %>% 
  bind_rows( data.frame(age  = age,
                        temperature = rowMeans(cbind(winT,sumT)),
                        location = "air",
                        season = "annual"))

#damping
sumTsed <- sumT * weight1 + winT * weight0
winTsed <- winT * weight1 + sumT * weight0

#ann
stableDamp <- data.frame(age  = age,
                         temperature = sumTsed,
                         location = "sediment",
                         season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winTsed,
                        location = "sediment",
                        season = "winter"))

stableTp <- bind_rows(stable,stableDamp)

stablePlot <- ggplot(stableTp) +
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  theme_bw() + 
  scale_color_manual(values = plotColorsTrend) +
  ggtitle("No summer trend")




for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumTsed[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winTsed[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}


tp2damp <- tp2damp %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "stable"))


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}

tp2 <- tp2 %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "stable"))

winT <- c(rep(4,times = length(age)/3),
          rep(-40,times = length(age)/3),
          rep(4,times = length(age)/3))

sumT <- c(rep(stl,times = length(age)/3),
          rep(sth,times = length(age)/3),
          rep(sth+5,times = length(age)/3))

warming <- data.frame(age  = age,
                      temperature = sumT,
                      location = "air",
                      season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winT,
                        location = "air",
                        season = "winter")) %>% 
  bind_rows( data.frame(age  = age,
                        temperature = rowMeans(cbind(winT,sumT)),
                        location = "air",
                        season = "annual"))

#damping
sumTsed <- sumT * weight1 + winT * weight0
winTsed <- winT * weight1 + sumT * weight0

#ann
warmingDamp <- data.frame(age  = age,
                          temperature = sumTsed,
                          location = "sediment",
                          season = "summer") %>% 
  bind_rows( data.frame(age  = age,
                        temperature = winTsed,
                        location = "sediment",
                        season = "winter"))

warmingTp <- bind_rows(warming,warmingDamp)

warmingPlot <- ggplot(warmingTp) +
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  theme_bw() + 
  scale_color_manual(values = plotColorsTrend) +
  ggtitle("Summer warming trend")




for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumTsed[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winTsed[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}

tp2damp <- tp2damp %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "warming"))


for(i in 2:length(age)){
  #summer half
  drSum <- dRacPL(arrhenius(Temp = sumT[i] + 273),dT/2)
  
  #winter half
  drWin <- dRacPL(arrhenius(Temp = winT[i] + 273),dT/2)
  
  Rx[i] <- Rx[i-1] + drSum + drWin 
}

tp2 <- tp2 %>% 
  bind_rows(data.frame(age = age,
                       DL = Rx^(1/x),
                       scenario = "warming"))

coolingTp$trend <- "cool MH"
stableTp$trend <- "warm MH"
warmingTp$trend <- "dry lake"

allTrends <- bind_rows(coolingTp, stableTp, warmingTp)
inputDataPlot <- ggplot(allTrends) + 
  geom_line(aes(x = age, y = temperature, color = season, linetype = location)) +
  scale_color_manual(values = plotColorsTrend) +
  facet_grid(trend ~ .,switch = "both") +
  ylab("Temperature (deg C)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() +
  ggtitle(glue::glue("Simulated temperature trends. Holocene Mean air Temp = {mean(c(winT,sumT))} deg C"))


annTrends <- allTrends %>% 
  filter(season == "annual" & location == "air") %>% 
  ggplot() +
  geom_line(aes(x = age, y = temperature, color = trend)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("Temperature (deg C)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() +
  ggtitle("Annual air temperature trends")



noDampNL <- ggplot(tp2) + 
  geom_line(aes(x = rev(age),y = DL,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("D/L") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = c(0.8,0.3)) +
  ggtitle("Temperature trend and racemization - no signal attenuation")



DampNL <- ggplot(tp2damp) + 
  geom_line(aes(x = rev(age),y = DL,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("D/L") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = "none") +
  ggtitle("Temperature trend and racemization - with attenuation")

# Re-run with x=4 for the wave scenarios — same reason as above.
x <- 4

noDampL <- ggplot(tp2) + 
  geom_line(aes(x = rev(age),y = DL^x,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("(D/L)^x") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = "none") +
  
  ggtitle("Temperature trend and racemization (linearized) - no attenuation")



DampL <- ggplot(tp2damp) + 
  geom_line(aes(x = rev(age),y = DL^x,color = scenario)) +
  scale_color_manual(values = plotColorsRac) +
  ylab("(D/L)^x") +
  xlab("Sample age (ka)") + 
  scale_x_continuous(breaks = c(seq(0,10,by = 2))) +
  theme_bw() + 
  theme(legend.position = "none") +
  ggtitle("Temperature trend and racemization (linearized) - with attenuation")


library(egg)
racPlot <- ggarrange(plots = list(noDampNL,DampNL,noDampL,DampL), ncol = 2)

ggsave(racPlot,filename = "Holocene sensitivity racemization.pdf",scale = .75)
ggsave(inputDataPlot,filename = "Temperature scenarios.pdf",scale = .75)
ggsave(annTrends,filename = "Annual temperature scenarios.pdf",scale = .75)


