
library(Rpath)

# Move to directory with files
  #setwd()
  
# From previous labs:  read in data, check parameters, balance Ecopath model
  ebs.base.file  <- "inst/labs/lab2/EBS_condensed_563_base.csv"
  ebs.diet.file  <- "inst/labs/lab2/EBS_condensed_563_diet.csv"
  ebs.unbalanced <- read.rpath.params(ebs.base.file, ebs.diet.file)
  check.rpath.params(ebs.unbalanced)
  ebs.balanced <- rpath(ebs.unbalanced, eco.name="E Bering")


# Create an Ecosim Scenario, setting up parameters and
# creating forcing time series (with default, equlibrium values)
  ebs.scene0 <-  rsim.scenario(ebs.balanced, ebs.unbalanced, years = 1990:2089)

  eco.area = 495000

# Run the ecosystem scenario for indicated years
  ebs.run0 <- rsim.run(ebs.scene,method="RK4",1990:2089)
  rsim.plot(ebs.run0,"Pollock")



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


  get.rsim.predprey <- function(scene,predator="all",prey="all"){  
     pd <- scene$params$spname[scene$params$PreyTo+1]
     py <- scene$params$spname[scene$params$PreyFrom+1]
     if (predator=="all") predator <- scene$params$spname
     if (prey=="all")     prey     <- scene$params$spname
     link <- which((pd %in% predator) & (py %in% prey))
     predname <- pd[link]
     preyname <- py[link]
     QQ <- scene$params$QQ[link]
     DD <- scene$params$DD[link]
     VV <- scene$params$VV[link]
     PredPredWeight <- scene$params$PredPredWeight[link]
     PreyPreyWeight <- scene$params$PreyPreyWeight[link]
     HandleSwitch <- scene$params$HandleSwitch[link]
     return(data.frame(link,preyname,predname,QQ,VV,DD,
            HandleSwitch,PredPredWeight,PreyPreyWeight))
  }  

  adjust.rsim.predprey <- function(scene,parameter,
                                   predator="all",prey="all",values){  
    pd <- scene$params$spname[scene$params$PreyTo+1]
    py <- scene$params$spname[scene$params$PreyFrom+1]
    if (predator=="all") predator <- scene$params$spname
    if (prey=="all")     prey     <- scene$params$spname
    link <- which((pd %in% predator) & (py %in% prey))

    if (parameter %in% c('QQ', 'VV', 'DD', 'HandleSwitch', 'PredPredWeight', 
                      'PreyPreyWeight')){
       scene$params[[parameter]][link] <- values
    }
    else {(warning(parameter," is not a valid predator/prey parameter."))}

    return(scene)
  } 

  scene <-  rsim.scenario(ebs.balanced, ebs.unbalanced, years = 1990:2089)

