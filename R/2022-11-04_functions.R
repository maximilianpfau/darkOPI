

# makeStimFunc ------------------------------------------------------------

genericMaiaParams <- list(image=NA,
                          level=NA,
                          size=0.43,
                          duration=200,
                          responseWindow=1500)


makeStimFunc <- function(params, x_coord, y_coord, currentLevel, color) {
  
  stim <- list(x=x_coord,
               y=y_coord,
               image=NA,
               level=currentLevel,
               size=params$size,
               color=color,
               duration=params$duration,
               responseWindow=params$responseWindow)
  
  class(stim) <- "opiStaticStimulus"
  
  return(stim)
}



# Start Five One -----------------------------------------------------------

fiveOne.start <- function(est=0, instRange=c(0,36), verbose=FALSE, ...) {
  
  return(list(name="fiveOne",
              startingEstimate=est,
              currentLevel=est,
              minStimulus=instRange[1],
              maxStimulus=instRange[2],
              lastSeen=NA,
              lastResponse=NA,
              stairResult=NA,
              finished="Not",                     # "Not", or one of "Max", "Min", "Rev"
              verbose=verbose,
              numberOfReversals=0,
              currSeenLimit=0,                    # number of times maxStimulus seen
              currNotSeenLimit=0,                 # number of times minStimulus not seen
              numPresentations=0,                 # number of presentations so far
              stimuli=NULL,                       # vector of stims shown
              responses=NULL,                     # vector of responses (1 seen, 0 not)
              responseTimes=NULL,                 # vector of response times
              opiResp=NULL,
              finalTimeStamp=NULL,
              opiParams=list(...)                 # the extra params
  ))
}






# Five one step funtion                     

fiveOne.step <- function(state) {
  
  # Create stimulus
  stim <- makeStimFunc(genericMaiaParams,
                       x_coord = state$opiParams$x_coord,
                       y_coord = state$opiParams$y_coord,
                       currentLevel= state$currentLevel,
                       color = state$opiParams$color)
  
  
  # Present stimulus
  opiResp <- maia.opiPresent(stim, NA)
  
  while (!is.null(opiResp$err))
    opiResp <- maia.opiPresent(stim, NA)
  
  # Save results
  state$stimuli          <- c(state$stimuli, state$currentLevel)
  state$responses        <- c(state$responses, opiResp$seen)
  state$responseTimes    <- c(state$responseTimes, opiResp$time)
  state$numPresentations <- state$numPresentations + 1
  state$opiResp <- c(state$opiResp, opiResp)
  
  if (opiResp$seen) 
    state$lastSeen <- state$currentLevel
  
  
  # Check for seeing min
  if (state$currentLevel == state$minStimulus && !opiResp$seen)
    state$currNotSeenLimit <- state$currNotSeenLimit + 1
  
  # Check for seeing max
  if (state$currentLevel == state$maxStimulus && opiResp$seen)
    state$currSeenLimit <- state$currSeenLimit + 1
  
  
  
  # Set next stimulus
  if (state$numberOfReversals == 0 && opiResp$seen)
    state$currentLevel <- min(state$maxStimulus, max(state$minStimulus, state$currentLevel + 5))
  
  
  if (!opiResp$seen) {
    
    state$currentLevel <- min(state$maxStimulus, max(state$minStimulus, state$currentLevel - 1))
    
    if (state$numberOfReversals == 0)
      state$numberOfReversals <- state$numberOfReversals+1
  }
  
  
  if (state$numberOfReversals == 1 && opiResp$seen) {
    state$finished <- "Rev"
    state$stairResult <- state$currentLevel
    state$finalTimeStamp <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  }
  
  
  
  
  # Check if finished
  if (state$currNotSeenLimit >= 2) {
    state$finished <- "Min"
    state$stairResult <- state$minStimulus
    state$finalTimeStamp <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    
  }
  
  if (state$currSeenLimit >= 2) {
    state$finished <- "Max"
    state$stairResult <- state$maxStimulus
    state$finalTimeStamp <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  } 
  
  return(state)
}




# All step function


fiveOne.allSteps <- function(stairResult, x_coord, y_coord, color) {
  
  state <- fiveOne.start(est=stairResult,
                         x_coord=x_coord,
                         y_coord=y_coord,
                         color=color)
  
  state$currentLevel <- min(36, max(0, state$currentLevel + 5))
  
  while( state$finished=="Not" ) {
    state <- fiveOne.step(state)
  }
  
  return(state)
}



# OPI related functions

# OPI Env
if (exists(".OpiEnv") && !exists("maia", where=.OpiEnv)) {
  assign("maia", new.env(25), envir=.OpiEnv)
  .OpiEnv$maia$endian <- "little"   # endianess of the maia OS
  
  .OpiEnv$maia$ZERO_DB_IN_ASB <- NA
  
  .OpiEnv$maia$MAX_DB <- 36 
  .OpiEnv$maia$MIN_DB <- 0  
  .OpiEnv$maia$MIN_X  <- -30
  .OpiEnv$maia$MAX_X  <- 30  
  .OpiEnv$maia$MIN_Y  <- -30
  .OpiEnv$maia$MAX_Y  <- 30  
  .OpiEnv$maia$MIN_RESP_WINDOW  <- 0    
  .OpiEnv$maia$MAX_RESP_WINDOW  <- 2680
  .OpiEnv$maia$MIN_DURATION  <- 1
  
  .OpiEnv$maia$SEEN     <- 1  
  .OpiEnv$maia$NOT_SEEN <- 0  
  
  # Utility functions for validating inputs
  .OpiEnv$maia$minCheck <- function(x, limit, txt) {
    if (x < limit) {
      opiClose()
      stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
    }
  }
  .OpiEnv$maia$maxCheck <- function(x, limit, txt) {
    if (x > limit) {
      opiClose()
      stop(paste("opiPresent: ", txt, "is too big (maximum ", limit, ")"))
    }
  }
}

# OPI Initialize
maia.opiInitialize <- function(ip = "192.168.1.2", port=5555) {
  cat("Looking for server... ")
  suppressWarnings(tryCatch(    
    v <- socketConnection(host = ip, port,
                          blocking = TRUE, open = "w+b",
                          timeout = 10)
    , error=function(e) { 
      stop(paste(" cannot find a server at", ip, "on port",port))
    }
  ))
  close(v)
  
  cat("found server at",ip,port,":)\n")
  
  socket <- tryCatch(
    socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
    error=function(e) stop(paste("Cannot connect to maia at",ip,"on port", port))
  )
  
  assign("socket", socket, envir = .OpiEnv$maia)
}  


# OPI open
maia.opiOpen <- function(socket) {
  
  
  writeLines("OPI-OPEN", socket)
  n <- readBin(socket, "int", size=4, endian=.OpiEnv$maia$endian)
  
  if (length(n) == 0) {    # maia was not happy with that OPEN, try until it is (AHT: Sep 2018)
    warning('maia did not like the OPEN command. Suggest closeAllConnections() and try again')
    return(list(err="Bad open", prl=NULL, onh=NULL, image=NULL))    
  } else {
    print(paste("opiInitialize read: ", n))
    prlx <- readBin(socket, "double", size=4, endian=.OpiEnv$maia$endian)
    prly <- readBin(socket, "double", size=4, endian=.OpiEnv$maia$endian)
    onhx <- readBin(socket, "double", size=4, endian=.OpiEnv$maia$endian)
    onhy <- readBin(socket, "double", size=4, endian=.OpiEnv$maia$endian)
    im <- readBin(socket, "raw", n=n-16, size=1, endian=.OpiEnv$maia$endian)
  } 
  return(list(err=NULL, prl=c(prlx, prly), onh=c(onhx, onhy), image=im))    
}


# OPI present

maia.opiPresent <- function(stim, nextStim=NULL) { UseMethod("maia.opiPresent") }
# setGeneric("opiPresent")

maia.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
  if (is.null(stim)) {
    warning("opiPresent: NULL stimulus")
    return(list(err="The NULL stimulus not supported", seen=NA, time=NA))
  }
  
  if(!is.null(stim$size)) warning("opiPresent: ignoring stimulus size")
  if(!is.null(stim$color)) warning("opiPresent: ignoring stimulus color")
  
  .OpiEnv$maia$minCheck(stim$x, .OpiEnv$maia$MIN_X, "Stimulus x")
  .OpiEnv$maia$maxCheck(stim$x, .OpiEnv$maia$MAX_X, "Stimulus x")
  .OpiEnv$maia$minCheck(stim$y, .OpiEnv$maia$MIN_Y, "Stimulus y")
  .OpiEnv$maia$maxCheck(stim$y, .OpiEnv$maia$MAX_Y, "Stimulus y")
  .OpiEnv$maia$minCheck(stim$duration, .OpiEnv$maia$MIN_DURATION, "Stimulus duration")
  .OpiEnv$maia$minCheck(stim$responseWindow, .OpiEnv$maia$MIN_RESP_WINDOW, "Stimulus responseWindow")
  .OpiEnv$maia$maxCheck(stim$responseWindow, .OpiEnv$maia$MAX_RESP_WINDOW, "Stimulus responseWindow")
  lev <- round(stim$level,0)
  .OpiEnv$maia$minCheck(lev, .OpiEnv$maia$MIN_DB, "Stimulus level")
  .OpiEnv$maia$maxCheck(lev, .OpiEnv$maia$MAX_DB, "Stimulus level")
  
  if (!is.null(nextStim)) 
    warning("opiPresent: nextStim ignored")
  
  msg <- "OPI-PRESENT-STATIC"
  msg <- paste(msg, '1', stim$x, stim$y, stim$color, 1, lev, "0.43", stim$duration, stim$responseWindow)
  
  presentBad <- TRUE
  presentCount <- 0
  while (presentBad) {
    presentCount <- presentCount + 1
    if (presentCount %% 10 == 0)
      warning(paste('opiPresent: I have tried presenting',presentCount,'times.'))
    
    writeLines(msg, .OpiEnv$maia$socket)
    res <- readLines(.OpiEnv$maia$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]
    
    presentBad <- s[1] > 0
  }
  
  # BUG - num_track_events, etc are for a single call to the protocolo, and not aggregated over the
  # 'presentBad' loop.
  
  return(list(
    err             =NULL,
    seen            =ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
    time            =as.numeric(s[3]), 
    time_hw         =as.numeric(s[4]),
    time_rec        =as.numeric(s[5]),
    time_resp       =as.numeric(s[6]),
    loc_x           =as.numeric(s[7]),
    loc_y           =as.numeric(s[8]),
    loc_x_px        =as.numeric(s[9]),
    loc_y_px        =as.numeric(s[10])
  ))
}
