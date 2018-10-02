CalibratePackageReGenesees = function(netSample,calmodel=NULL,popTotals=NULL,y=NULL,by = NULL,partition=NULL,
                          popData=NULL,samplingWeights=NULL,bounds=c(-Inf,Inf),calfun="linear",
                          onlyTotals=FALSE,ids,...){
  #warning("The non-CRAN package, ReGenesees, is needed.")
  # Add this line to NAMESPACE when ReGenesees is on CRAN
  #importFrom(ReGenesees,e.svydesign,pop.template,fill.template,get.residuals,e.calibrate,svystatTM)
  # and add ReGenesees to Depends or Suggests in DESCRIPTION
  
  if (requireNamespace("ReGenesees", quietly = TRUE)) {
    e.svydesign   <- ReGenesees::e.svydesign
    pop.template  <- ReGenesees::pop.template
    fill.template <- ReGenesees::fill.template
    e.calibrate   <- ReGenesees::e.calibrate
    get.residuals <- ReGenesees::get.residuals
    svystatTM     <- ReGenesees::svystatTM
  } else {
    stop("The package ReGenesees, is needed.")
  }
  
  
  desReGenesees <- e.svydesign(netSample,ids=asFormula(ids), weights =asFormula(samplingWeights))
  
  if(is.null(popTotals)){
    if(is.null(partition))
      popTemplate <- pop.template(data=desReGenesees, calmodel=as.formula(calmodel))
    else
      popTemplate <- pop.template(data=desReGenesees, calmodel=as.formula(calmodel),partition=asFormula(partition))
    popTotals   <- fill.template(universe=popData,template= popTemplate)
  } else
    if(!is.null(partition)) warning("Partition as input has no effect when popTotals is specified and ReGenesees is used.")


  if(onlyTotals) return(popTotals)
  calReGenesees <- e.calibrate(design=desReGenesees, df.population=popTotals,bounds= bounds,calfun=calfun)
  w=weights(calReGenesees) #########   BARE NETTO ##################
  
  ###calReGenesees <<- calReGenesees ####################

  resids = get.residuals(calReGenesees,asFormula(y), scale = "no")

  estTM=NULL

  if(!is.null(y)){
    if(is.list(y) | is.list(by)){
      if(is.list(y) & is.list(by)) {if(length(y)!=length(by)) stop("length(y)==length(by) must be TRUE")}
      else{
        if(is.list(y)){
          if(is.null(by))
            by = vector("list",length(y))
          else{
            by_ = by
            by = y
            for(i in 1:length(y)) by[[i]] = by_
          }
        }else{
          y_ = y
          y = by
          for(i in 1:length(y)) y[[i]] = y_
        }

      }
      estTM = y
      for(i in 1:length(y)) estTM[[i]] = svystatTM(calReGenesees,y=asFormula(y[[i]]),by=asFormula(by[[i]]),...)

    } else
    {
      estTM =  svystatTM(calReGenesees,y=asFormula(y),by=asFormula(by),...)
    }

  }
  if(is.null(estTM)) return(list(popTotals=popTotals,w=w))
  list(popTotals=popTotals,w=w,estTM=estTM,resids=resids)
}

