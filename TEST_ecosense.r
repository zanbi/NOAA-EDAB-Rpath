
setwd("C:/src/sean_rpath/EBS_ecosim")

Ebase <- "models/EBS_ACLIM_72_BIO_base.csv"  # Base biomass, production, fishing, etc.
Ediet <- "models/EBS_ACLIM_72_BIO_diet.csv"  # Diet matrix
Eped  <- "models/EBS_ACLIM_72_BIO_ped_2.csv"   # Data pedigree = quality of input data
Estz  <- "models/EBS_ACLIM_72_BIO_stanzas.csv"   # Stanzas
Estg  <- "models/EBS_ACLIM_72_BIO_stanza_groups.csv" # Stanza groups

# Setup Base Ecopath and Base Rsim scenario
unbal     <- rpath.stanzas(read.rpath.params(Ebase, Ediet, Eped, Estg, Estz)) # unbalanced
bal       <- rpath(unbal) # balanced
all_years <- 1991:2090
scene     <- rsim.scenario(bal, unbal, years = all_years) # Ecosim params

par0 <- scene$params
set.seed(12345)
par1 <- rsim.sense.path(scene, bal, unbal)	

set.seed(12345)
par2 <- rsim.sense.path(scene, bal, unbal)
set.seed(12345)
par3 <- rsim.sense(scene, bal, unbal)	

par_compare("B_BaseRef",par0,par2,par3)
par_compare("PBopt",par0,par2,par3)
par_compare("FtimeQBOpt",par0,par2,par3)

par_compare("MzeroMort",par0,par2,par3)
par_compare("UnassimRespFrac",par0,par2,par3) # UNCHANGED
par_compare("ActiveRespFrac",par0,par2,par3)
par_compare("NoIntegrate",par0,par2,par3)

par_compare <- function(this, pb, p0, p1){
 #sp   <- pb$spname 
 orig <- pb[[this]]
 base <- p0[[this]]; perturb <- p1[[this]]
 dif  <- perturb - base; changed <- ifelse(perturb==base,F,T)
 return(data.frame(orig,base,perturb,dif,changed))
}


sp_pars <- c("B_BaseRef","MzeroMort","UnassimRespFrac","ActiveRespFrac",
             "FtimeAdj","FtimeQBOpt","PBopt","NoIntegrate","HandleSelf",      
             "ScrambleSelf")


par_compare("B_BaseRef",par0,par1)
par_compare("MzeroMort",par0,par1)
par_compare("UnassimRespFrac",par0,par1) # UNCHANGED
par_compare("ActiveRespFrac",par0,par1)
par_compare("FtimeAdj",par0,par1)        # UNCHANGED
par_compare("FtimeQBOpt",par0,par1)
par_compare("PBopt",par0,par1)
par_compare("NoIntegrate",par0,par1)
par_compare("HandleSelf",par0,par1)      # UNCHANGED
par_compare("ScrambleSelf",par0,par1)    # UNCHANGED

par_compare("PreyFrom",par0,par1)    # UNCHANGED
par_compare("PreyTo",par0,par1)      # UNCHANGED
par_compare("QQ",par0,par1)
par_compare("DD",par0,par1)          # WEIRDLY CHANGED 1000 to 1001 also check first one
par_compare("VV",par0,par1)
par_compare("PredPredWeight",par0,par1)
par_compare("PreyPreyWeight",par0,par1)

par_compare("FishFrom",par0,par1)      # UNCHANGED
par_compare("FishThrough",par0,par1)   # UNCHANGED
par_compare("FishQ",par0,par1)

par_compare("DetFrom",par0,par1)       # UNCHANGED
par_compare("DetTo",par0,par1)         # UNCHANGED
par_compare("DetFrac",par0,par1)       # UNCHANGED

#> names(par0)
# [1] "NUM_GROUPS"       "NUM_LIVING"       "NUM_DEAD"         "NUM_GEARS"       
# [5] "NUM_BIO"          "spname"           "spnum"            "B_BaseRef"       
# [9] "MzeroMort"        "UnassimRespFrac"  "ActiveRespFrac"   "FtimeAdj"        
#[13] "FtimeQBOpt"       "PBopt"            "NoIntegrate"      "HandleSelf" 
#[17] "ScrambleSelf"     

# "PreyFrom"         "PreyTo"           "QQ"              
#[21] "DD"               "VV"               "HandleSwitch"     "PredPredWeight"  
#[25] "PreyPreyWeight"   "NumPredPreyLinks" "FishFrom"         "FishThrough"     
#[29] "FishQ"            "FishTo"           "NumFishingLinks"  "DetFrac"         
#[33] "DetFrom"          "DetTo"            "NumDetLinks"      "BURN_YEARS"      
#[37] "COUPLED"          "RK4_STEPS"    

