#####################################################################################
#'@useDynLib Rpath
#'@export
runRpathShiny <- function(lab) {
  # locate all the shiny app examples that exist
  validLabs <- list.files(system.file("labs", package = "Rpath"))
  validLabsMsg <-paste0("Valid Labs: '",paste(validLabs, collapse = "', '"),"'")

  # if an invalid example is given, throw an error
  if (missing(lab) || !nzchar(lab) ||
      !lab %in% validLabs) {stop(validLabsMsg, call. = F)}

  # find and launch the app
  appDir <- system.file("labs", lab, package = "Rpath")
  shiny::runApp(appDir, display.mode = "normal")
}

#####################################################################################
#'@export
rsim.deriv.q <- function(Rpath.scenario, year=0, month=0, tstep=0){
  scene <- copy(Rpath.scenario)
  rout <- deriv_vector(scene$params,  scene$start_state, 
                       scene$forcing, scene$fishing,
                       scene$stanzas, year, month, tstep)
  
  rtab <- data.frame(scene$params$spname[scene$params$PreyTo+1], 
                     scene$params$spname[scene$params$PreyFrom+1],
                     rout$Qlink)
  colnames(rtab)<-c("Predator","Prey","Q")
  return(rtab)
}

#####################################################################################
#'@export
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
  return(data.frame(link,predname,preyname,QQ,VV,DD,
                    HandleSwitch,PredPredWeight,PreyPreyWeight))
}  

#####################################################################################
#'@export
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
