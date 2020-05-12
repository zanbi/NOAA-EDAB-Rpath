################################################################################
#'@useDynLib Rpath
#'@export
read.fitting.biomass <- function(SCENE, filename){

# Base variables
  SIM   <- SCENE
  years <- as.numeric(row.names(SCENE$fishing$ForcedFRate))

  cdat  <-read.csv(filename)
  ccdat <-cdat[!is.na(cdat$Value)& cdat$Year %in% years ,]        
  #type <- as.character(rep("absolute",length(ccdat$YEAR)))
  ccdat$Year  <- as.character(ccdat$Year)
  ccdat$Group <- as.character(ccdat$Group) 
  obs  <- ifelse(as.numeric(ccdat$Scale)<0, as.numeric(ccdat$Value),
                 as.numeric(ccdat$Value) * as.numeric(ccdat$Scale))   
  sd   <- ifelse(as.numeric(ccdat$Scale)<0, as.numeric(ccdat$Stdev),
                 as.numeric(ccdat$Stdev) * as.numeric(ccdat$Scale))   
  wt   <- rep(1,length(obs))
  SIM$fitting$Biomass <- cbind(ccdat,obs,sd,wt)

return(SIM)
}

################################################################################
#'@export
read.fitting.catch <- function(SCENE, filename){
  SIM   <- SCENE
  years <- as.numeric(row.names(SCENE$fishing$ForcedFRate))
  # Columns needed
  #  Group	Year	Value	SD	Scale   
  cdat  <- read.csv(filename)
  ccdat <- cdat[!is.na(cdat$Value) & cdat$Year %in% years,] 
  ccdat$Year  <- as.character(ccdat$Year)
  ccdat$Group <- as.character(ccdat$Group) 
  obs  <- as.numeric(ccdat$Value) * as.numeric(ccdat$Scale)   
  sd   <- as.numeric(ccdat$Stdev) * as.numeric(ccdat$Scale)  
  wt   <- rep(1,length(obs))
  SIM$fitting$Catch <- cbind(ccdat,obs,sd,wt)
  #sdat  <- aggregate(as.numeric(ccdat$Value)*as.numeric(ccdat$Scale),list(ccdat$Year,ccdat$Group),"sum")
  #sd    <- 0.1*sdat$x
  #colnames(SIM$fitting$CATCH) <- c("year","species","obs","sd","wt")

  # Apply fit fishing to matrix
  #SIM$fishing$ForcedEffort[] <- 0
  #SIM$fishing$ForcedCatch[matrix(c(SIM$fitting$Catch$Year, SIM$fitting$Catch$Group),
  #                        length(SIM$fitting$Catch$Year),2)] <- SIM$fitting$Catch$obs
  return(SIM)
}

################################################################################
#'@export
apply.fit.to.catch <- function(SCENE){
  SIM <- SCENE
  SIM$fishing$ForcedEffort[] <- 0
  SIM$fishing$ForcedCatch[matrix(c(SIM$fitting$Catch$Year, SIM$fitting$Catch$Group),
                          length(SIM$fitting$Catch$Year),2)] <- SIM$fitting$Catch$obs
  return(SIM)
}

################################################################################
#'@export
rsim.plot.catch <- function(scene, run, species){
  qdat <- scene$fitting$Catch[scene$fitting$Catch$Group==species,]
  mn   <- qdat$obs
  up   <- mn + 1.96*qdat$sd
  dn   <- mn - 1.96*qdat$sd 
  tot <- 0 #sum(qdat$fit)
  plot(as.numeric(rownames(run$annual_Catch)),run$annual_Catch[,species],type="l",
       ylim=c(0,max(up,run$annual_Catch[,species])),xlab=tot,ylab="")
  mtext(side=2, line=2.2, paste(species,"catch"), font=2, cex=1.0)
  points(as.numeric(qdat$Year),mn)
  segments(as.numeric(qdat$Year),y0=up,y1=dn)
}

################################################################################
#'@export
rsim.plot.biomass <- function(scene, run, species){
  qdat <- scene$fitting$Biomass[scene$fitting$Biomass$Group==species,]
  survey_q <- 1
  mn   <- qdat$obs/survey_q
  up   <- mn + 1.96*qdat$sd/survey_q
  dn   <- mn - 1.96*qdat$sd/survey_q 
  tot  <- 0 #tot <- sum(qdat$fit)
  plot(as.numeric(rownames(run$annual_Biomass)),run$annual_Biomass[,species],type="l",
       ylim=c(0,max(up,run$annual_Biomass[,species])),xlab=tot,ylab="")
  mtext(side=2, line=2.2, paste(species,"biomass"), font=2, cex=1.0)
  points(as.numeric(qdat$Year),mn)
  segments(as.numeric(qdat$Year),y0=up,y1=dn)
}

#################################################################################
test<-function(){

#Group	Year	Value	SD	Scale

# DATA from CATCH time series (Angie provided)   


# APPLY FISHING TO FITTING
  SIM$fishing$EFFORT[]<-0
  colnames(SIM$fishing$CATCH)<-SIM$params$spname[1:(SIM$params$NUM_BIO+1)]
  #rownames(SIM$fishing$CATCH)<-c(years,end_year+1)
  SIM$fishing$CATCH[matrix(c(as.character(SIM$fitting$CATCH$year),as.character(SIM$fitting$CATCH$species)),
                          length(SIM$fitting$CATCH$year),2)] <- SIM$fitting$CATCH$obs 

# diet composition  
  dfiles <- c("data/HMC_GOA_pollockdiet.csv","data/HMC_GOA_coddiet.csv","data/HMC_GOA_atfdiet.csv","data/HMC_GOA_halibutdiet.csv")
  dcdat <- read_diet_alphas(SIM,dfiles)
  SIM$fitting$diets <- dcdat[dcdat$year %in% years,]
# total ration index
  qdat <- NULL
  for (f in dfiles){
    ddat <- read.csv(f)
    qdat <- rbind(qdat,data.frame(ddat$year,ddat$pred,ddat$cperwMean,ddat$cperwSD,rep(1,length(ddat[,1]))))
  }
  colnames(qdat)<-c("year","species","obs","sd","wt")
  SIM$fitting$ration<-qdat[qdat$year %in% years,]


}

