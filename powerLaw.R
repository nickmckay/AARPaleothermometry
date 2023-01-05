#D/Lx = kTt + C	 (eq 1)
racemizationPL <- function(kt,yr,C){
  
}

dRacPL <- function(kt,delta.yr,x){
 deltaRacX <- kt * delta.yr
 deltaRac <- deltaRacX^(1/x)
 return(deltaRac)
}


#k = Ae–Ea/RT  or:  ln(k) = – (Ea/R) x T-1 + ln(A)	(eq 2)
#R is a constant (0.001987 kcal K-1 mol-1).
arrhenius <- function(Ae,Ea = ,Temp = 277,R = 0.001987){
  kt <- Ae–Ea/(R*Temp)
  return(kt)
}