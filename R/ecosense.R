 ###############################################################################
# Ecosense Routines (Aydin et al. 2005) as
# implemented for the R version of ecosim (Rsim for Rpath) 
#
# Current version based on Whitehouse et al (submitted in 2019)
#
################################################################################

################################################################################
#' Ecosense function for rpath (rsim.sense.path)
#'
#' 5 November 2019
#'@family Rpath functions
#'
#' Whitehouse and Aydin (Submitted) Assessing the sensitivity of three Alaska marine
#' food webs to perturbations: an example of Ecosim simulations using Rpath
#'
#' This function generates random parameters around the Ecopath baseline,
#' not the scenario.
#'
#'@return Returns an Rsim.scenario object that can be supplied to the rsim.run function.
#'@useDynLib Rpath
#' @export
rsim.sense <- function(Rpath.scenario, Rpath, Rpath.params){
   
  orig.params  <- Rpath.scenario$params
  sense.params <- Rpath.scenario$params

  nliving <- orig.params$NUM_LIVING
  ndead   <- orig.params$NUM_DEAD
  ngears  <- orig.params$NUM_GEARS
  ngroups <- orig.params$NUM_GROUPS
 
  # Set-up pedigree vectors, including zeroes for gear groups.
  # THIS SHOULD BE THE ONLY USE OF THE Rpath.params object - do all extractions here
    BBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,2])),rep(0,ngears))
    PBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,3])),rep(0,ngears))
    QBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,4])),rep(0,ngears))
    DCVAR   <-   as.numeric(unlist(Rpath.params$pedigree[,5]))  
    #DCVAR  <- c(as.numeric(unlist(Rpath.params$pedigree[,5])),rep(0,Rpath$NUM_GEARS))
    TYPE    <- Rpath.params$model$Type 

  # KYA 12/30/19 when drawing from a scenario, the Outside is already added.
  # But we have to strip it off to align with pedigree using IND of 2..(ngroups+1) 
  # (generating an extra number would break the random seed setting alignment)
    IND <- 2:(ngroups+1)

  # Biomass (the most straightforward)
    ranBB <- orig.params$B_BaseRef[IND]  * (1 + BBVAR*runif(ngroups,-1.0,1.0))

  # PB, QB, Mzero, and the dependent respiration fractions
    ranPB <- orig.params$PBopt[IND]      * (1 + PBVAR*runif(ngroups,-1.0,1.0))                        
    ranQB <- orig.params$FtimeQBOpt[IND] * (1 + QBVAR*runif(ngroups,-1.0,1.0))

    # To keep Mzero scaled to EE, we back-calculate EE then use that
    # EE = 1 - Mzero/PB - Note that this introduces tiny numerical
    # differences (1e-16 order) compared to doing it from Rpath EE directly.
    ranM0 <- ranPB * (orig.params$MzeroMort[IND] / orig.params$PBopt[IND]) *
                                           (1 + PBVAR*runif(ngroups,-1.0,1.0))

    # Active respiration is proportion of CONSUMPTION that goes to "heat"
    # Passive respiration/ VonB adjustment is left out here
    # Switched ranQB>0 test to TYPE test as QB for prim prods isn't 0 in rsim
    # TODO Fix negative respiration rates?
    ranActive <- 1.0 - (ranPB/ranQB) - orig.params$UnassimRespFrac[IND]

  # Now copy these the output scenario, prepending the "Outside" values
  # and fixing values for different group types.
    sense.params$B_BaseRef      <- c(1.0, ranBB)
    sense.params$PBopt          <- c(1.0, ranPB)      
    sense.params$FtimeQBOpt     <- c(1.0, ifelse(TYPE==1, ranPB, ranQB))     
    sense.params$MzeroMort      <- c(0.0, ifelse(TYPE==3, 0, ranM0))
    sense.params$ActiveRespFrac <- c(0.0, ifelse(TYPE<1 , ranActive, 0))

  # No Integrate for A-B, fixing 2*steps_yr*steps_m as 24
    sense.params$NoIntegrate <- 
      ifelse(sense.params$MzeroMort*sense.params$B_BaseRef > 24, 0, sense.params$spnum)
  
  # TODO IMPORTANT - confirm that NoIntegrate gets switched appropriately for stanzas?

  # Version of new QB without "Outside" group 
    QBOpt <- sense.params$FtimeQBOpt[IND]

  #primary production links
  primTo   <- ifelse(Rpath$type > 0 & Rpath$type <= 1, 
                     1:length(ranPB),
                     0)
  primFrom <- rep(0, length(Rpath$PB))
  primQ    <- ranPB * ranBB 

  # Change production to consusmption for mixotrophs
  mixotrophs <- which(Rpath$type > 0 & Rpath$type < 1)
  primQ[mixotrophs] <- primQ[mixotrophs] / Rpath$GE[mixotrophs] * 
          Rpath$type[mixotrophs]

  #Predator/prey links
  preyfrom  <- row(Rpath$DC)
  preyto    <- col(Rpath$DC)	
  predpreyQ <- Rpath$DC[1:(nliving + ndead + 1), ] * 
    t(matrix(QBOpt[1:Rpath$NUM_LIVING] * ranBB[1:Rpath$NUM_LIVING],
            nliving, nliving + ndead + 1))
  
  #combined
  sense.params$PreyFrom <- c(primFrom[primTo > 0], preyfrom [predpreyQ > 0])
  # Changed import prey number to 0
  sense.params$PreyFrom[which(sense.params$PreyFrom == nrow(Rpath$DC))] <- 0
  sense.params$PreyTo   <- c(primTo  [primTo > 0], preyto   [predpreyQ > 0])
  
  ##### This is where we add uncertainty to diet  #####
  # Diet comp vector
  DCvector <- c(rep(0.0, sum(Rpath$type==1)), Rpath$DC[Rpath$DC>0])
  # Diet comp pedigree
  DCpedigree <- DCVAR[sense.params$PreyTo]
  ## Random diet comp
  EPSILON <- 1*10^-8
  betascale <- 1.0
  DCbeta <- betascale * DCpedigree * DCpedigree
  alpha <- DCvector/DCbeta
  DClinks <- rgamma(length(DCvector), shape=alpha, rate=DCbeta)
  DClinks2 <- ifelse(DClinks < EPSILON, 2 * EPSILON, DClinks)
  # DClinks2 prevents random diet comps from becoming too low, effectively
  # equal to zero. Zeros in DClinks will produce NaN's in sense.params$QQ, and
  # others, ultimately preventing ecosim.
  DCtot <- tapply(DClinks2, sense.params$PreyTo, "sum")    
  # Normalized diet comp
  DCnorm <- ifelse(Rpath$type[sense.params$PreyTo]==1, 1.0, DClinks2/DCtot[sense.params$PreyTo])
  # The "if" part of DCnorm is so the DC of phytoplankton (type==1) won't equal zero
  DCQB <- QBOpt[sense.params$PreyTo]
  DCBB <- ranBB[sense.params$PreyTo]  
  sense.params$QQ <- DCnorm * DCQB * DCBB             	
   
  numpredprey <- length(sense.params$QQ)

  # Sarah used the following formula to vary vulnerability in Gaichas et al. (2012)
  # That paper states that "vulnerability" (also known as X*predprey) has an
  # effective range from 1.01 to 91 in EwE.
  sense.params$VV	<-	1 + exp(9 * (runif(length(sense.params$QQ))-0.5))
  sense.params$DD	<-	1000 + exp(0 * (runif(length(sense.params$QQ))-0.5))

  # Scramble combined prey pools
  Btmp <- sense.params$B_BaseRef
  py   <- sense.params$PreyFrom + 1.0
  pd   <- sense.params$PreyTo + 1.0
  VV   <- sense.params$VV * sense.params$QQ / Btmp[py]
  AA   <- (2.0 * sense.params$QQ * VV) / (VV * Btmp[pd] * Btmp[py] - sense.params$QQ * Btmp[pd])
  sense.params$PredPredWeight <- AA * Btmp[pd] 
  sense.params$PreyPreyWeight <- AA * Btmp[py] 
  
  sense.params$PredTotWeight <- rep(0, length(sense.params$B_BaseRef))
  sense.params$PreyTotWeight <- rep(0, length(sense.params$B_BaseRef))
  
  for(links in 1:numpredprey){
    sense.params$PredTotWeight[py[links]] <- sense.params$PredTotWeight[py[links]] + sense.params$PredPredWeight[links]
    sense.params$PreyTotWeight[pd[links]] <- sense.params$PreyTotWeight[pd[links]] + sense.params$PreyPreyWeight[links]    
  }  

  sense.params$PredPredWeight <- sense.params$PredPredWeight/sense.params$PredTotWeight[py]
  sense.params$PreyPreyWeight <- sense.params$PreyPreyWeight/sense.params$PreyTotWeight[pd]

  sense.params$PreyFrom       <- c(0, sense.params$PreyFrom)
  sense.params$PreyTo         <- c(0, sense.params$PreyTo)
  sense.params$QQ             <- c(0, sense.params$QQ)
  sense.params$DD             <- c(0, sense.params$DD)
  sense.params$VV             <- c(0, sense.params$VV) 
  sense.params$PredPredWeight <- c(0, sense.params$PredPredWeight)
  sense.params$PreyPreyWeight <- c(0, sense.params$PreyPreyWeight)


  #catchlinks
  fishfrom    <- row(as.matrix(Rpath$Catch))
  fishthrough <- col(as.matrix(Rpath$Catch)) + (nliving + ndead)
  fishcatch   <- Rpath$Catch
  fishto      <- fishfrom * 0
  
  if(sum(fishcatch) > 0){
    sense.params$FishFrom    <- fishfrom   [fishcatch > 0]
    sense.params$FishThrough <- fishthrough[fishcatch > 0]
    sense.params$FishQ       <- fishcatch  [fishcatch > 0] / sense.params$B_BaseRef[sense.params$FishFrom + 1]  
    sense.params$FishTo      <- fishto     [fishcatch > 0]
  }

  #discard links
  for(d in 1:Rpath$NUM_DEAD){
    detfate <- Rpath$DetFate[(nliving + ndead + 1):Rpath$NUM_GROUPS, d]
    detmat  <- t(matrix(detfate, Rpath$NUM_GEAR, Rpath$NUM_GROUPS))
    
    fishfrom    <-  row(as.matrix(Rpath$Discards))                      
    fishthrough <-  col(as.matrix(Rpath$Discards)) + (nliving + ndead)
    fishto      <-  t(matrix(nliving + d, Rpath$NUM_GEAR, Rpath$NUM_GROUPS))
    fishcatch   <-  Rpath$Discards * detmat
    if(sum(fishcatch) > 0){
      sense.params$FishFrom    <- c(sense.params$FishFrom,    fishfrom   [fishcatch > 0])
      sense.params$FishThrough <- c(sense.params$FishThrough, fishthrough[fishcatch > 0])
      ffrom <- fishfrom[fishcatch > 0]
      sense.params$FishQ       <- c(sense.params$FishQ,  fishcatch[fishcatch > 0] / sense.params$B_BaseRef[ffrom + 1])  
      sense.params$FishTo      <- c(sense.params$FishTo, fishto   [fishcatch > 0])
    }
  } 

  sense.params$FishFrom        <- c(0, sense.params$FishFrom)
  sense.params$FishThrough     <- c(0, sense.params$FishThrough)
  sense.params$FishQ           <- c(0, sense.params$FishQ)  
  sense.params$FishTo          <- c(0, sense.params$FishTo)   

 
  class(sense.params) <- 'Rsim.params'
  return(sense.params)   
  
}


################################################################################
#' Ecosense function for rpath (rsim.sense.path)
#'
#' 5 November 2019
#'@family Rpath functions
#'
#' Whitehouse and Aydin (Submitted) Assessing the sensitivity of three Alaska marine
#' food webs to perturbations: an example of Ecosim simulations using Rpath
#'
#' This function generates random parameters around the Ecopath baseline,
#' not the scenario.
#'
#'@return Returns an Rsim.scenario object that can be supplied to the rsim.run function.
#'@useDynLib Rpath
#' @export
rsim.sense.path <- function(Rpath.scenario, Rpath, Rpath.params,
                            steps_yr = 12, steps_m = 1){
   ##NO##
  sense.params <- Rpath.scenario$params##NO##
##NO##
  nliving <- Rpath$NUM_LIVING##NO##
  ndead   <- Rpath$NUM_DEAD##NO##
  ##NO##
  # Set-up pedigree vectors, including zeroes for gear groups.##NO##
  BBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,2])),rep(0,Rpath$NUM_GEARS))##NO##
  PBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,3])),rep(0,Rpath$NUM_GEARS))##NO##
  QBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,4])),rep(0,Rpath$NUM_GEARS))##NO##
  DCVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,5])),rep(0,Rpath$NUM_GEARS))##NO##
  ##NO##
##NO##
  # Biomass##NO##
  ranBB <- Rpath$BB * (1 + BBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0))##NO##
  sense.params$B_BaseRef <- c(1.0, ranBB)##NO##
  # PB##NO##
  ranPB <- Rpath$PB * (1 + PBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0))##NO##
  # QB##NO##
  ranQB <- Rpath$QB * (1 + QBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0))##NO##
##NO##
  # Mzero##NO##
  sense.params$MzeroMort <- c(0.0, ranPB * (1.0 - Rpath$EE)  *##NO##
                          (1 + PBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0)))##NO##
    # The zero at the start of the M0 vector is for "outside"##NO##
    # same for the other vectors##NO##
##NO##
  #Active respiration is proportion of CONSUMPTION that goes to "heat"##NO##
  #Passive respiration/ VonB adjustment is left out here##NO##
  sense.params$ActiveRespFrac <-  c(0.0, ifelse(ranQB > 0, ##NO##
                                          1.0 - (ranPB / ranQB) - Rpath$GS, ##NO##
                                          0.0))##NO##
##NO##
  #####	rQBPrimProd is a added here because we need a non-zero QB for##NO##
  #####	primary production groups. QB's of zero produce NaN's in ecosim##NO##
  #####	and just bring the whole thing to a screaching hault. rQBPrimProd##NO##
  #####	sets the QB of PP groups equal to their PB.##NO##
  rQBPrimProd	<- ifelse(Rpath$type==1,ranPB,ranQB)##NO##
  sense.params$FtimeQBOpt <-   c(1.0, rQBPrimProd)##NO##
  #sense.params$FtimeQBOpt <-   c(1.0, ranQB)##NO##
  sense.params$PBopt      <-   c(1.0, ranPB)           ##NO##
##NO##
  #No Integrate##NO##
  sense.params$NoIntegrate <- ifelse(sense.params$MzeroMort * sense.params$B_BaseRef > ##NO##
                                 2 * steps_yr * steps_m, 0, sense.params$spnum)##NO##
  ##NO##
  #primary production links##NO##
  primTo   <- ifelse(Rpath$type > 0 & Rpath$type <= 1, ##NO##
                     1:length(ranPB),##NO##
                     0)##NO##
  primFrom <- rep(0, length(Rpath$PB))##NO##
  primQ    <- ranPB * ranBB ##NO##
##NO##
  # Change production to consusmption for mixotrophs##NO##
  mixotrophs <- which(Rpath$type > 0 & Rpath$type < 1)##NO##
  primQ[mixotrophs] <- primQ[mixotrophs] / Rpath$GE[mixotrophs] * ##NO##
          Rpath$type[mixotrophs]##NO##
##NO##
  #Predator/prey links##NO##
  preyfrom  <- row(Rpath$DC)##NO##
  preyto    <- col(Rpath$DC)	##NO##
  predpreyQ <- Rpath$DC[1:(nliving + ndead + 1), ] * ##NO##
    t(matrix(rQBPrimProd[1:Rpath$NUM_LIVING] * ranBB[1:Rpath$NUM_LIVING],##NO##
            nliving, nliving + ndead + 1))##NO##
  ##NO##
  #combined##NO##
  sense.params$PreyFrom <- c(primFrom[primTo > 0], preyfrom [predpreyQ > 0])##NO##
  # Changed import prey number to 0##NO##
  sense.params$PreyFrom[which(sense.params$PreyFrom == nrow(Rpath$DC))] <- 0##NO##
  sense.params$PreyTo   <- c(primTo  [primTo > 0], preyto   [predpreyQ > 0])##NO##
  ##NO##
  ##### This is where we add uncertainty to diet  #######NO##
  # Diet comp vector##NO##
  DCvector <- c(rep(0.0, sum(Rpath$type==1)), Rpath$DC[Rpath$DC>0])##NO##
  # Diet comp pedigree##NO##
  DCped <- as.numeric(unlist(Rpath.params$pedigree[,5]))##NO##
  DCpedigree <- DCped[sense.params$PreyTo]##NO##
  ## Random diet comp##NO##
  EPSILON <- 1*10^-8##NO##
  betascale <- 1.0##NO##
  DCbeta <- betascale * DCpedigree * DCpedigree##NO##
  alpha <- DCvector/DCbeta##NO##
  DClinks <- rgamma(length(DCvector), shape=alpha, rate=DCbeta)##NO##
  DClinks2 <- ifelse(DClinks < EPSILON, 2 * EPSILON, DClinks)##NO##
  # DClinks2 prevents random diet comps from becoming too low, effectively##NO##
  # equal to zero. Zeros in DClinks will produce NaN's in sense.params$QQ, and##NO##
  # others, ultimately preventing ecosim.##NO##
  DCtot <- tapply(DClinks2, sense.params$PreyTo, "sum")    ##NO##
  # Normalized diet comp##NO##
  DCnorm <- ifelse(Rpath$type[sense.params$PreyTo]==1, 1.0, DClinks2/DCtot[sense.params$PreyTo])##NO##
  # The "if" part of DCnorm is so the DC of phytoplankton (type==1) won't equal zero##NO##
  DCQB <- rQBPrimProd[sense.params$PreyTo]##NO##
  DCBB <- ranBB[sense.params$PreyTo]  ##NO##
  sense.params$QQ <- DCnorm * DCQB * DCBB             	##NO##
   ##NO##
  numpredprey <- length(sense.params$QQ)##NO##
##NO##
  # Sarah used the following formula to vary vulnerability in Gaichas et al. (2012)##NO##
  # That paper states that "vulnerability" (also known as X*predprey) has an##NO##
  # effective range from 1.01 to 91 in EwE.##NO##
  sense.params$VV	<-	1 + exp(9 * (runif(length(sense.params$QQ))-0.5))##NO##
  sense.params$DD	<-	1000 + exp(0 * (runif(length(sense.params$QQ))-0.5))##NO##
##NO##
  # Scramble combined prey pools##NO##
  Btmp <- sense.params$B_BaseRef##NO##
  py   <- sense.params$PreyFrom + 1.0##NO##
  pd   <- sense.params$PreyTo + 1.0##NO##
  VV   <- sense.params$VV * sense.params$QQ / Btmp[py]##NO##
  AA   <- (2.0 * sense.params$QQ * VV) / (VV * Btmp[pd] * Btmp[py] - sense.params$QQ * Btmp[pd])##NO##
  sense.params$PredPredWeight <- AA * Btmp[pd] ##NO##
  sense.params$PreyPreyWeight <- AA * Btmp[py] ##NO##
  ##NO##
  sense.params$PredTotWeight <- rep(0, length(sense.params$B_BaseRef))##NO##
  sense.params$PreyTotWeight <- rep(0, length(sense.params$B_BaseRef))##NO##
  ##NO##
  for(links in 1:numpredprey){##NO##
    sense.params$PredTotWeight[py[links]] <- sense.params$PredTotWeight[py[links]] + sense.params$PredPredWeight[links]##NO##
    sense.params$PreyTotWeight[pd[links]] <- sense.params$PreyTotWeight[pd[links]] + sense.params$PreyPreyWeight[links]    ##NO##
  }  ##NO##
##NO##
  sense.params$PredPredWeight <- sense.params$PredPredWeight/sense.params$PredTotWeight[py]##NO##
  sense.params$PreyPreyWeight <- sense.params$PreyPreyWeight/sense.params$PreyTotWeight[pd]##NO##
##NO##
  sense.params$PreyFrom       <- c(0, sense.params$PreyFrom)##NO##
  sense.params$PreyTo         <- c(0, sense.params$PreyTo)##NO##
  sense.params$QQ             <- c(0, sense.params$QQ)##NO##
  sense.params$DD             <- c(0, sense.params$DD)##NO##
  sense.params$VV             <- c(0, sense.params$VV) ##NO##
  sense.params$PredPredWeight <- c(0, sense.params$PredPredWeight)##NO##
  sense.params$PreyPreyWeight <- c(0, sense.params$PreyPreyWeight)##NO##
##NO##
##NO##
  #catchlinks##NO##
  fishfrom    <- row(as.matrix(Rpath$Catch))##NO##
  fishthrough <- col(as.matrix(Rpath$Catch)) + (nliving + ndead)##NO##
  fishcatch   <- Rpath$Catch##NO##
  fishto      <- fishfrom * 0##NO##
  ##NO##
  if(sum(fishcatch) > 0){##NO##
    sense.params$FishFrom    <- fishfrom   [fishcatch > 0]##NO##
    sense.params$FishThrough <- fishthrough[fishcatch > 0]##NO##
    sense.params$FishQ       <- fishcatch  [fishcatch > 0] / sense.params$B_BaseRef[sense.params$FishFrom + 1]  ##NO##
    sense.params$FishTo      <- fishto     [fishcatch > 0]##NO##
  }##NO##
##NO##
  #discard links##NO##
  for(d in 1:Rpath$NUM_DEAD){##NO##
    detfate <- Rpath$DetFate[(nliving + ndead + 1):Rpath$NUM_GROUPS, d]##NO##
    detmat  <- t(matrix(detfate, Rpath$NUM_GEAR, Rpath$NUM_GROUPS))##NO##
    ##NO##
    fishfrom    <-  row(as.matrix(Rpath$Discards))                      ##NO##
    fishthrough <-  col(as.matrix(Rpath$Discards)) + (nliving + ndead)##NO##
    fishto      <-  t(matrix(nliving + d, Rpath$NUM_GEAR, Rpath$NUM_GROUPS))##NO##
    fishcatch   <-  Rpath$Discards * detmat##NO##
    if(sum(fishcatch) > 0){##NO##
      sense.params$FishFrom    <- c(sense.params$FishFrom,    fishfrom   [fishcatch > 0])##NO##
      sense.params$FishThrough <- c(sense.params$FishThrough, fishthrough[fishcatch > 0])##NO##
      ffrom <- fishfrom[fishcatch > 0]##NO##
      sense.params$FishQ       <- c(sense.params$FishQ,  fishcatch[fishcatch > 0] / sense.params$B_BaseRef[ffrom + 1])  ##NO##
      sense.params$FishTo      <- c(sense.params$FishTo, fishto   [fishcatch > 0])##NO##
    }##NO##
  } ##NO##
##NO##
  sense.params$FishFrom        <- c(0, sense.params$FishFrom)##NO##
  sense.params$FishThrough     <- c(0, sense.params$FishThrough)##NO##
  sense.params$FishQ           <- c(0, sense.params$FishQ)  ##NO##
  sense.params$FishTo          <- c(0, sense.params$FishTo)   ##NO##
##NO##
 ##NO##
  class(sense.params) <- 'Rsim.params'##NO##
  return(sense.params)   ##NO##
  ##NO##
}##NO##

################################################################################ 
