
#' Print method for calSSB
#'
#' @param x calSSB object 
#' @param digits positive integer.  Minimum number of significant digits to be used for printing most numbers.
#' @param \dots further arguments sent to the underlying
#'
#' @return Invisibly returns the original object.
#' @keywords print
#' @export
print.calSSB <- function(x, digits = max(getOption("digits") - 3, 3), ...) {
  if(!is.null(x$estTM)){
    cat("---- estTM ----\n")
    print(x$estTM)
    cat("\n\n")
  }  
  
  cat("---- n =", attr(x,"n"),"    ny =",attr(x,"nY"),"\n\n")
  
  if(!is.null(colnames(x$y)))
    cat("---- y names ----\n",head(colnames(x$y),21),"\n\n")
  
  cat("---- Calibrated weights, w ----\n",head(x$w),"...",tail(x$w,1),"\n\n")
  
  cat("---- Summary of calSSB object ----\n")
  print(summary(x,digits = digits,...))
  invisible(x)
}


#' Print method for calSSBwide
#'
#' @param x calSSBwide object  
#' @param digits positive integer.  Minimum number of significant digits to be used for printing most numbers.
#' @param \dots further arguments sent to the underlying
#'
#' @return Invisibly returns the original object.
#' @keywords print
#' @export
print.calSSBwide <- function(x, digits = max(getOption("digits") - 3, 3), ...) {
  
  cat("---- ",NCOL(x$w)," waves of ",NROW(x$w)," units ---- \n\n")
  
  if(!is.null(colnames(x$w)))
    cat("---- wave names ----\n",head(colnames(x$w),21),"\n")
  
  cat("\n---- Summary of calSSBwide object ----\n")
  print(summary(x,digits = digits,...))
  cat("\n---- Summary of y ----\n")
  print(summary(x$y,digits = digits,...))
  if(!is.null(x$extra)){
    cat("\n---- Summary of extra ----\n")
    print(summary(x$extra,digits = digits,...))
  }
  
  invisible(x)
}




CheckDim <- function(x,nRow,nCol=1,nam="x",allowNULL=TRUE){
  if(allowNULL & is.null(x)) return(NULL)
  if(NROW(x) !=nRow | NCOL(x) !=nCol)
    stop(paste("Wrong dimension of",nam))
  NULL
}



#' Create or modify a CalSSB object
#' 
#' The elements of the CalSSB object are taken directly from the input parameters.
#'
#' @param x NULL or an  existing calSSB object
#' @param y y
#' @param w w
#' @param wGross wGross
#' @param resids resids
#' @param resids2 resids2
#' @param leverages leverages
#' @param leverages2 leverages2
#' @param samplingWeights samplingWeights
#' @param extra extra
#' @param id id 
#' @param wave wave 
#'
#' @return A CalSSB object. That is, an object of the type retuned by \code{\link{CalibrateSSB}}.
#' 
#' @note If x is a ReGenesees/cal.analytic object, this function is a wrapper to \code{\link{CalSSBobjReGenesees}}.
#' 
#' @export
#' 
#' @seealso \code{\link{CalibrateSSB}}, \code{\link{CalSSBobjReGenesees}}, \code{\link{WideFromCalibrate}},  \code{\link{PanelEstimation}}.
#'
#' @examples
#' #' # Generates data - two years
#' z <- AkuData(3000)  # 3000 in each quarter
#' zPop <- AkuData(10000)[, 1:7]
#' 
#' # Create a CalSSB object by CalibrateSSB
#' b <- CalibrateSSB(z, calmodel = "~ sex*age", partition = c("year", "q"), popData = zPop, 
#'                   y = c("unemployed", "workforce"))
#' 
#' # Modify the CalSSB object
#' a <- CalSSBobj(b, w = 10*b$w, wave = CrossStrata(z[, c("year", "q")]), id = z$id)
#' 
#' # Use the CalSSB object as input ...
#' PanelEstimation(WideFromCalibrate(a), "unemployed", linComb = PeriodDiff(8, 4))
#' 
#' # Create CalSSB object without x as input
#' CalSSBobj(y = b$y, w = 10*b$w, resids = b$resids, wave = CrossStrata(z[, c("year", "q")]), 
#'           id = z$id)
#' 
CalSSBobj <- function(x=NULL,y=NULL,w=NULL,wGross=NULL,resids=NULL,resids2=NULL,
                   leverages=NULL,leverages2=NULL,samplingWeights=NULL,extra=NULL,
                   id=NULL, wave=NULL){
  if(!is.null(x)){
    if(class(x)[1]=="cal.analytic"){
      if(!is.null(w))         warning("Input w ignored when ReGenesees")
      if(!is.null(wGross))    warning("Input wGross ignored when ReGenesees")
      if(!is.null(resids))    warning("Input resids ignored when ReGenesees")
      if(!is.null(resids2))   warning("Input resids2 ignored when ReGenesees")
      if(!is.null(leverages)) warning("Input leverages ignored when ReGenesees")
      if(!is.null(leverages)) warning("Input leverages2 ignored when ReGenesees")
      return(CalSSBobjReGenesees(x,
        y=y,samplingWeights=samplingWeights,extra=extra,id=id,wave=wave))
    }
    if(class(x)[1] != "calSSB")
      stop("x must be an object of class calSSB")
    n = attr(x,"n")
    nY = attr(x,"nY")
    CheckDim(y,n,1,"y")
    if(!is.null(y)) x$y = y
  }
  else{
    if(is.null(y)) stop("y needed in input when x=NULL")
    if(is.null(w)) stop("w needed in input when x=NULL")
    if(is.null(resids)) stop("resids needed in input when x=NULL")
    n = dim(y)[1]
    nY= dim(y)[2]
    x = structure(list(y=y), class = "calSSB", n=n, nY=nY)
  }
  CheckDim(w,n,1,"w")
  CheckDim(wGross,n,1,"wGross")
  CheckDim(resids,n,nY,"resids")
  CheckDim(resids2,n,nY,"resids2")
  CheckDim(leverages,n,1,"leverages")
  CheckDim(leverages2,n,1,"leverages2")
  CheckDim(samplingWeights,n,1,"samplingWeights")
  if(!is.null(extra))
    if(NROW(extra) != n) 
      stop(paste("extra must have ",n," rows"))
  CheckDim(id,n,1,"id")
  if(!NCOL(wave)==1)
    wave = CrossStrata(wave)
  CheckDim(wave,n,1,"wave")
  if(!is.null(w)) x$w = w
  if(!is.null(wGross)) x$wGross = wGross
  if(!is.null(resids)) x$resids = resids
  if(!is.null(resids2)) x$resids2 = resids2
  if(!is.null(leverages)) x$leverages = leverages
  if(!is.null(leverages2)) x$leverages2 = leverages2
  if(!is.null(samplingWeights)) x$samplingWeights = samplingWeights
  if(!is.null(extra)) x$extra = extra
  if(!is.null(id)) x$id = id
  if(!is.null(wave)) x$wave = wave
  x
}


#' Create a CalSSB object from a ReGenesees/cal.analytic object 
#'
#' @param x Output from ReGenesees::e.calibrate() (object of class cal.analytic)  
#' @param y formula or variable names 
#' @param samplingWeights NULL, TRUE (capture from x), formula, variable name or vector of data
#' @param extra NULL, formula, variable names or matrix of data
#' @param id NULL, TRUE (ids from x), formula, variable name or vector of data
#' @param wave NULL,  formula, variable name or vector of data
#'
#' @return A CalSSB object. That is, an object of the type retuned by \code{\link{CalibrateSSB}}.
#' @export
#' 
#' @seealso \code{\link{CalibrateSSB}}, \code{\link{CalSSBobj}}, \code{\link{WideFromCalibrate}},  \code{\link{PanelEstimation}}.
#' 
#' @examples
#' \dontrun{
#' # Generates data - two years
#' z <- AkuData(3000)  # 3000 in each quarter
#' zPop <- AkuData(10000)[, 1:7]
#' z$samplingWeights <- 1
#' z$ids <- 1:NROW(z)
#' 
#' # Create a ReGenesees/cal.analytic object
#' library("ReGenesees")
#' desReGenesees <- e.svydesign(z[z$R == 1, ], ids = ~ids, weights = ~samplingWeights)
#' popTemplate <- pop.template(data = desReGenesees, calmodel = ~sex * age, partition = ~year + q)
#' popTotals <- fill.template(universe = zPop, template = popTemplate)
#' calReGenesees <- e.calibrate(design = desReGenesees, df.population = popTotals)
#' 
#' # Create CalSSB objects from a ReGenesees/cal.analytic object
#' CalSSBobjReGenesees(calReGenesees, y = ~unemployed + workforce, id = TRUE, 
#'                     samplingWeights = TRUE, extra = ~famid)
#' a <- CalSSBobjReGenesees(calReGenesees, y = c("unemployed", "workforce"), 
#'                          id = "id", extra = "famid", wave = c("year", "q"))
#' 
#' # Use the CalSSB object as input ...
#' PanelEstimation(WideFromCalibrate(a), "unemployed", linComb = PeriodDiff(8, 4))
#' 
#' }
CalSSBobjReGenesees <- function(x,y, samplingWeights=NULL,extra=NULL,
                      id=NULL, wave=NULL){
  if (requireNamespace("ReGenesees", quietly = TRUE)) {
    get.residuals <- ReGenesees::get.residuals
  } else {
    stop("The package ReGenesees, is needed.")
  }
  z = NULL
  z$w <- weights(x)
  z$resids = get.residuals(x,asFormula(y), scale = "no")
  z$y = model.frame(asFormula(y), data=x$variables)
  n = dim(z$y)[1]
  nY= dim(z$y)[2]
  
  if(!is.null(samplingWeights)){
    if(is.logical(samplingWeights)){
      if(samplingWeights){
        wei      <- attr(x, "weights")
        wei.char <- all.vars(wei)
        samplingWeights <- substr(wei.char, 0, nchar(wei.char) - 4)
        } else
          samplingWeights <- NULL
        } 
  }
  if(!is.null(samplingWeights))
    if(class(samplingWeights)[1]=="formula" | (is.character(samplingWeights) & length(samplingWeights)==1 ))
      samplingWeights = model.frame(asFormula(samplingWeights), data=x$variables)[,]
  z$samplingWeights = samplingWeights
  
  if(!is.null(extra))
    if(class(extra)[1]=="formula" | (is.character(extra) & length(extra)<min(dim(x$variables)) ))
      extra = model.frame(asFormula(extra), data=x$variables)    
  z$extra = extra
    
  if(!is.null(id))
    if(is.logical(id)){
       if(id)
         id = model.frame(attr(x,"ids"),data=x$variables)[,]
       else
         id = NULL
    }
  
  if(!is.null(id))
    if(class(id)[1]=="formula" | (is.character(id) & length(id)==1))
      id = model.frame(asFormula(id), data=x$variables)[,]    
  z$id = id
  
  if(!is.null(wave))
      if(class(wave)[1]=="formula" | (is.character(wave) & length(wave)<min(dim(x$variables)) ))
        wave = model.frame(asFormula(wave), data=x$variables)[,]
  if(NCOL(wave)==1)
    z$wave = wave
  else
    z$wave = CrossStrata(wave)
    
    
  n = dim(z$y)[1]
  nY= dim(z$y)[2]
  structure(z, class = "calSSB", n=n, nY=nY)
}





GetSubset <- function(x, subset) {
  if (is.null(x))
    return(x)
  if (is.null(subset))
    return(x)
  if(is.null(ncol(x))) x=as.vector(x) ## Handle problem with AsIs-class in special cases
  if (is.vector(x))
    return(x[subset])
  return(x[subset, , drop = FALSE])
}


RemoveZeroCalSSB <- function(x){
    s = x$w!=0    
    for(i in 1: length(x))
      x[[i]] = GetSubset(x[[i]],s)  ## Unngå å gjøre dette for popTotals, estTM
    x    ################ Må endre nY atributt også
}
  
  



#' Calibration weighting and variance estimation for panel data
#' 
#' @encoding UTF8
#'
#' @param ... Input to CalibrateSSB() and PanelEstimation()
#'
#' @return Output from PanelEstimation()
#' @export
#' 
#' @seealso \code{\link{CalibrateSSB}}, \code{\link{PanelEstimation}}.
#'
#' @examples
#' z    = AkuData(3000)  # 3000 in each quarter
#' zPop = AkuData(10000)[,1:7]
#' lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
#' rownames(lc) = c("diffQ1","diffQ2","diffQ3","diffQ4","diffYearMean")
#' CalibrateSSBpanel(grossSample=z,calmodel="~ sex*age", partition=c("year","q"),popData=zPop, 
#'        y=c("unemployed","workforce"),id="id",wave=c("year","q"),
#'        numerator="unemployed",linComb=lc)
CalibrateSSBpanel = function(...){
  sysCall <- sys.call()
  panelArgs <-  names(sysCall) %in% formalArgs(PanelEstimation)
  CalibrateArgs <- !panelArgs 
  sysCallPanel <- as.call(c(list(as.name("PanelEstimation"),x="x"),as.list(sysCall[panelArgs])) )
  sysCall <- sysCall[!panelArgs]  
  sysCall[[1]] <- as.name("CalibrateSSB")
  parentFrame = parent.frame()
  sysCallPanel$x = WideFromCalibrate(eval(sysCall, envir=parentFrame))
  eval(sysCallPanel, envir=parentFrame)
}






#bB=RemoveZeroCalSSB(CalSSBobj(b,id=z$id))
#bWideB = WideFromCalibrate(bB,CrossStrata(z[b$w!=0 ,c("year","q")]))














