
setwd("E:/src/sean_rpath/EBS_ecosim")

# Stanzas model test
#  Ebase <- "models/EBS_ACLIM_72_BIO_base.csv"  # Base biomass, production, fishing, etc.
#  Ediet <- "models/EBS_ACLIM_72_BIO_diet.csv"  # Diet matrix
#  Eped  <- "models/EBS_ACLIM_72_BIO_ped_2.csv"   # Data pedigree = quality of input data
#  Estz  <- "models/EBS_ACLIM_72_BIO_stanzas.csv"   # Stanzas
#  Estg  <- "models/EBS_ACLIM_72_BIO_stanza_groups.csv" # Stanza groups
#  unbal <- rpath.stanzas(read.rpath.params(Ebase, Ediet, Eped, Estg, Estz)) # unbalanced

# Non-stanzas model test
  Ebase <- "models/sensetests/EBS_base_Aug2016_AGG.csv" 
  Ediet <- "models/sensetests/EBS_diet_Aug2016_AGG.csv" 
  Eped  <- "models/sensetests/EBS_ped_Aug2016_AGG.csv"   
  unbal <- read.rpath.params(Ebase, Ediet, Eped) 

# Setup Base Ecopath and Base Rsim scenario
  bal       <- rpath(unbal) # balanced
  all_years <- 1991:2090
  scene     <- rsim.scenario(bal, unbal, years = all_years) # Ecosim params

# testing routine to compare parameter sets by vector - supply the name of the vector
  par_compare <- function(this, p0,p1,p2,p3){
    original    <- p0[[this]];  sense.zero <- p1[[this]] 
    sense.path  <- p2[[this]];  sense.new  <- p3[[this]]
    dif.zero  <- sense.zero-original;  changed.zero <- ifelse(sense.zero==original,F,T)
    dif.sense <- sense.new-sense.path; changed.path <- ifelse(sense.new==sense.path,F,T)
    return(data.frame(original,sense.zero,dif.zero,changed.zero,sense.path,sense.new,dif.sense,changed.path))
  }

# saved version of base params
  par0 <- scene$params

# make a copy of the unbal rpath params to manipulate pedigree
# TODO: make some adjust functions for pedigree
  unbal0 <- unbal
  unbal0$pedigree$B[]    <- 0
  unbal0$pedigree$PB[]   <- 0
  unbal0$pedigree$QB[]   <- 0
  unbal0$pedigree$Diet[] <- 0

# par1 should have all variance as 0, therefore the output should match the input.
# seed shouldn't matter but worth testing
  set.seed(12345)
  par1 <- rsim.sense(scene, unbal0)
  # testing the pedigree=0 case in rsim.sense.path revealed a bug from before
  # (diet pedigrees of 0 broke the gamma function)
  # set.seed(12345)
  # par1a <- rsim.sense.path(scene,bal,unbal0)

# par2 uses the old method (renamed as rsim.sense.path but otherwise unchanged)  
  set.seed(12345)
  par2 <- rsim.sense.path(scene, bal, unbal)

# par3 uses the new method - with the same pedigree used in par2.  It should
# give the same results, with some possible numerical error due to methods
# (numerical error order of 10^-8 of original value). 
# Unlike rsim.sense.path, the default vulnerability variance is 0, so here the
# vul range (Vvary) used the range hardcoded in the rsim.sense.path method.
  set.seed(12345)
  par3 <- rsim.sense(scene, unbal, Vvary=c(-4.5,4.5))	
  
# Now examine all the variables
# Check means same results in old versus new methods 
# UNCHANGED means it's not recalculated from the base values
#
par_compare("B_BaseRef",par0,par1,par2,par3)       # Check
par_compare("PBopt",par0,par1,par2,par3)           # Check
par_compare("FtimeQBOpt",par0,par1,par2,par3)      # Check
par_compare("MzeroMort",par0,par1,par2,par3)       # Check
par_compare("UnassimRespFrac",par0,par1,par2,par3) # UNCHANGED
par_compare("ActiveRespFrac",par0,par1,par2,par3)  # Check - need to change method for negative respiration?
par_compare("NoIntegrate",par0,par1,par2,par3)     # Need to add Stanzas to NoIntegrate? (old and new versions)
par_compare("HandleSelf",par0,par1,par2,par3)      # UNCHANGED
par_compare("ScrambleSelf",par0,par1,par2,par3)    # UNCHANGED

par_compare("PreyFrom",par0,par1,par2,par3)        # UNCHANGED (recalculated in old method)
par_compare("PreyTo",par0,par1,par2,par3)          # UNCHANGED (recalculated in old method)
par_compare("QQ",par0,par1,par2,par3)              # Check - numerically different, added beta trap for 0 variance
par_compare("VV",par0,par1,par2,par3)              # Check - changed "outside - outside" vul from 0 to 2 
par_compare("DD",par0,par1,par2,par3)              # Bug in old version set these to 1001, not 1000 - changed to 1000.
                                                   # also sets first one to previous default (1000) instead of 0.
par_compare("PredPredWeight",par0,par1,par2,par3)  # Check - there were some cumulative addition errors (epsilon-sized)
par_compare("PreyPreyWeight",par0,par1,par2,par3)  # Check - there were some cumulative addition errors (epsilon-sized)

par_compare("FishFrom",par0,par1,par2,par3)        # UNCHANGED
par_compare("FishTo",par0,par1,par2,par3)          # UNCHANGED
par_compare("FishThrough",par0,par1,par2,par3)     # UNCHANGED
par_compare("FishQ",par0,par1,par2,par3)           # Check - TODO add variance here
par_compare("DetFrom",par0,par1,par2,par3)         # UNCHANGED
par_compare("DetTo",par0,par1,par2,par3)           # UNCHANGED
par_compare("DetFrac",par0,par1,par2,par3)         # UNCHANGED


source("rsim_sense_Nov2019.r")
# UNCHANGED FROM Andy's test loop EXCEPT set seed.
# Should be able to replace rsim.sense.path line as indicated
# and come up with identical output.
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 1000
parlist<-as.list(rep(NA,NUM_RUNS))
kept<-rep(NA,NUM_RUNS)

set.seed(12345)

for (i in 1:NUM_RUNS){
  EBSsense <- scene 
  # INSERT SENSE ROUTINE BELOW
  parlist[[i]]<- scene$params 		# Base ecosim params
  # LINE TO CHANGE
    parlist[[i]]<- rsim.sense.orig(scene, bal, unbal)	# Replace the base params with Ecosense params
  # New version should give same accept/rejects
    #parlist[[i]]<- rsim.sense(scene, unbal, Vvary=c(-4.5,4.5))	# Replace the base params with Ecosense params
  EBSsense$start_state$BB <- parlist[[i]]$B_BaseRef
  parlist[[i]]$BURN_YEARS <- 50			# Set Burn Years to 50
  EBSsense$params <- parlist[[i]]
  EBStest <- rsim.run(EBSsense, method="AB", years=all_years)
  failList <- which(is.na(EBStest$end_state$BB))
  {if (length(failList)>0)
  {cat(i,": fail in year ",EBStest$crash_year,": ",failList,"\n"); kept[i]<-F; flush.console()}
    else 
    {cat(i,": success!\n"); kept[i]<-T;  flush.console()}}
  parlist[[i]]$BURN_YEARS <- 1
}








