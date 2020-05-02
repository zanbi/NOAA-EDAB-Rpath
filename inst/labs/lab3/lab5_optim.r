
library(Rpath)

  # f is a NAMED VECTOR of fishing rates.  Names must match species names.
  fishery_output <- function(f_rates, prices, costs, years, rsim.scene){

    species_list <- names(f_rates); 
    final_year   <- as.character(years[length(years)])

    for (p in 1:length(f_rates)){
      rsim.scene <- adjust.fishing(rsim.scene, "ForcedFRate", species_list[p], 
                                   sim.year = years, value=f_rates[p])
    }

    res <- rsim.run(rsim.scene, method="AB", years=years)
    landings <- res$annual_Catch[final_year,species_list]
    return( sum (495000000*(landings*prices) - (f_rates*costs)))
  
  }

# Ecopath Setup
  setwd("inst/labs/lab3")
  ebs.base.file  <- "EBS_condensed_563_base.csv"
  ebs.diet.file  <- "EBS_condensed_563_diet.csv"
  ebs.unbalanced <- read.rpath.params(ebs.base.file, ebs.diet.file)
  check.rpath.params(ebs.unbalanced)
  ebs.balanced <- rpath(ebs.unbalanced, eco.name="E Bering")

# Ecosim scenario setup (setting up unfished system)
  ebs.scene0   <- rsim.scenario(ebs.balanced, ebs.unbalanced, years=1990:2089)
  all.gear <- c("Trawl", "Cod.pots", "Longline", "Crab.pots", "State.fisheries") 
  ebs.unfished.scene <- adjust.fishing(ebs.scene0, "ForcedEffort", all.gear, 
                                       sim.year = 1990:2089, value=0)   

  ebs.f_rates <- c(Pollock=0.2, P.cod=0.2, Arrowtooth=0.0)
  ebs.prices  <- c(Pollock=0.1, P.cod=0.5, Arrowtooth=0.05)
  ebs.costs   <- c(Pollock=20000000,   P.cod=80000000, Arrowtooth=25000000) 

  fishery_output(ebs.f_rates, ebs.prices, ebs.costs, 1990:2089, ebs.unfished.scene)

    
  test <- optim(ebs.f_rates, fishery_output, prices=ebs.prices, costs=ebs.costs, 
                            years=1990:2049, rsim.scene=ebs.unfished.scene, 
                            control=list(fnscale = -1))

  test <- optim(ebs.f_rates, fishery_output, prices=ebs.prices, costs=ebs.costs, 
                            years=1990:2049, rsim.scene=ebs.unfished.scene, 
                            control=list(fnscale = -1), lower=0)





# 




