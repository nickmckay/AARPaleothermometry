#D/Lx = kTt + C	 (eq 1)
racemizationPL <- function(kt,yr,C){
  
}

dRacPL <- function(kt,delta.yr){
 deltaRacX <- kt * delta.yr
 return(deltaRacX)
}


#k = Ae–Ea/RT  or:  ln(k) = – (Ea/R) x T-1 + ln(A)	(eq 2)
#R is a constant (0.001987 kcal K-1 mol-1).
arrhenius <- function(Ae = exp(42.12),Ea = 31.5 ,Temp = 278,R = 0.001987){
  lnk <- -(Ea/R) * Temp^-1 + log(Ae)
  kt <- exp(lnk)
  #kt <- Ae - Ea/(R*Temp)
  return(kt)
}

x <- 3.0

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

sumT <- c(rep(sth+5,times = length(age)/3),
          rep(sth,times = length(age)/3),
          rep(stl,times = length(age)/3))

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


