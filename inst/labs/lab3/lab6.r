




  ebs.scene0

  ebs.scene1 <- rsim.scenario(ebs.balanced, ebs.unbalanced, years = 1990:2089)



  Bindex <- which(ebs.scene0$params$spname=="P.cod")
  Bbase  <- ebs.scene0$start_state$Biomass[Bindex] 

  test.biomass <- seq(0.1, 5.0 , 0.1) * Bbase
  qq <- rep(NA,length(test.biomass))
  ind <- 0
  for (b in test.biomass){
    ind <- ind + 1
    ebs.scene0$start_state$Biomass[Bindex] <- b * Bbase
    deriv <- rsim.deriv.q(ebs.scene0,1,0,0)
    qq[ind] <- deriv[deriv$Predator=="P.cod"&deriv$Prey=="Pollock","Q"] 
  }
  ebs.scene0$start_state$Biomass[Bindex] <- Bbase