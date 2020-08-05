
suppressWarnings(RNGversion("3.5.0"))
set.seed(1234)
z    <- AkuData(100)  
zPop <- AkuData(200)[,1:7]
z$samplingWeights = sample(10000:100000,NROW(z))

z$sex = as.character(z$sex)
zPop$age = as.character(zPop$age)


Csum <- function(a){
  x <- as.numeric(as.matrix(a))
  x <- x[!is.na(x)]
  c(length(x),sum(x),sum(x^2))
}

#z = tibble::as_tibble(z)
#zPop = tibble::as_tibble(zPop)


test_that("Examples", {
  
  
  #### CalibrateSSB Examples
  
  # Calibration using "survey"
  a <- CalibrateSSB(z, calmodel = "~ sex*age",
                    partition = c("year","q"),  # calibrate within quarter
                    popData = zPop, y = c("unemployed","workforce"),
                    by = c("year","q")) # Estimate within quarter
  expect_identical(Csum(a$popTotals),c(64,20096,32956616))
  expect_equal(Csum(a$w),c(800,1600,4428.729572165))
  expect_equal(Csum(a$estTM),c(32, 17414.751745, 32658559.6096))
  
  
  expect_identical(a$popTotals, CalibrateSSB(z, calmodel = "~ sex*age",
                    partition = c("year","q"),  # calibrate within quarter
                    popData = zPop, y = c("unemployed","workforce"),
                    by = c("year","q"), onlyTotals = TRUE))
  
  
  # Calibration, no package, popTotals as input
  b <- CalibrateSSB(z, popTotals=a$popTotals, calmodel="~ sex*age",
                    partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"))
  
  expect_identical(a$popTotals, b$popTotals)
  expect_equal(a$w, b$w)
  expect_equal(colSums(a$estTM[,3:4]), colSums(b$estTM))
  
  
  expect_identical(a$popTotals, CalibrateSSB(z, popTotals=a$popTotals, calmodel="~ sex*age",
                    partition = c("year","q"), usePackage = "none", 
                    y = c("unemployed","workforce"), onlyTotals = TRUE))
  
  
  #### PanelEstimation Examples
  
  bWide = WideFromCalibrate(b,CrossStrata(z[,c("year","q")]),z$id)
  
  # Define linear combination matrix
  lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
  rownames(lc) = c("diffQ1","diffQ2","diffQ3","diffQ4","diffYearMean")
  colnames(lc) = colnames(head(bWide$y[[1]]))

  # Unemployed: Totals and linear combinations
  d1=PanelEstimation(bWide,"unemployed",linComb=lc)  
  
  expect_equal(range(d1$wTot), c(200,200))
  expect_equal(Csum(c(d1$estimates, d1$linCombs, d1$varEstimates, d1$varLinCombs)),
               c(26, 113.4170829057, 1674.9380089581))
  
  d=PanelEstimation(bWide,numerator="unemployed",denominator="workforce",linComb=lc)
  
  expect_equal(Csum(unlist(d)), c(86, 3493.652535091372, 535118.167577150976))
  
  expect_warning(b2 <- CalibrateSSB(z,popData=zPop,calmodel="~ edu*sex + sex*age",
                    partition=c("year","q"), y=c("unemployed","workforce"),
                    leverageOutput=TRUE))
  

  expect_equal(Csum(log(unlist(b2)+1)), c(7352, 2206.516235572093, 3617.600635213678))
  
  
  b2Wide = WideFromCalibrate(b2,CrossStrata(z[,c("year","q")]),z$id,extra=z$famid)
  d2 = PanelEstimation(b2Wide,"unemployed",linComb=lc,group=1,estType = "robustModelGroup")
  
  expect_equal(Csum(unlist(d2)), c(42, 3329.254745316503, 642035.689654916176)) 
  
  g=PanelEstimation(bWide,numerator="unemployed",denominator="workforce",
                    linComb= LagDiff(2),linComb0=Period(8,4))
  
  expect_equal(Csum(unlist(g)), c(50, 3208.179712767641, 522114.947009256517))
  
  expect_equal(Csum(c(g$varEstimates, g$varLinCombs)),c(3, 2.554084342077243e-04, 2.771624841133405e-08))
  
})

test_that("samplingWeights", {
  
  a_popTotals = CalibrateSSB(z, calmodel = "~ age*sex",    # instead of sex*age
               partition = c("year","q"),  # calibrate within quarter
               popData = zPop, y = c("unemployed","workforce"),
               by = c("year","q"), onlyTotals = TRUE)
  
  # Calibration, "survey", popTotals as input
  bS <- CalibrateSSB(z, popTotals=a_popTotals, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "survey", y = c("unemployed","workforce"),
                     samplingWeights ="samplingWeights")
  
  # Calibration, no package, popTotals as input
  bN <- CalibrateSSB(z, popTotals=a_popTotals, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"),
                     samplingWeights ="samplingWeights")
  
  expect_equal(Csum(log(unlist(bS)+1.2)), c(3828,1877.483679157398, 2552.801866633156))
  expect_equal(bS,bN)
  
  
  # Calibration, no package, popTotals as input
  bN <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                      partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"),
                      samplingWeights ="samplingWeights")
  
  expect_equal(bS,bN)
  
  
  bS <- CalibrateSSB(z, calmodel="~ sex*age",
                      partition = c("year","q"), usePackage = "survey", y = c("unemployed","workforce"),
                      samplingWeights ="samplingWeights")
  
  
  bN <- CalibrateSSB(z, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"),
                     samplingWeights ="samplingWeights")
  
  expect_equal(Csum(log(unlist(bS)+1.2)), c(3828,9020.196061889004, 92562.450246460881))
  expect_equal(bS,bN)
  
  expect_identical(bS$popTotals, CalibrateSSB(z, calmodel="~ sex*age",
                  partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"),
                  samplingWeights ="samplingWeights",onlyTotals = TRUE))
  
  expect_identical(bS$popTotals, CalibrateSSB(z, calmodel="~ sex*age",
                  partition = c("year","q"), usePackage = "survey", y = c("unemployed","workforce"),
                  samplingWeights ="samplingWeights",onlyTotals = TRUE))

  
  zPop$age[sample(NROW(zPop),100)] = "4"
  
  expect_error(CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
               partition = c("year","q"), usePackage = "survey", y = c("unemployed","workforce"),
               samplingWeights ="samplingWeights"))
  
  zPop$age[zPop$age == "4"] = NA
  
  
  # Calibration, "survey", popTotals as input
  bS <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "survey", y = c("unemployed","workforce"),
                     samplingWeights ="samplingWeights")
  
  # Calibration, no package, popTotals as input
  bN <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"))
  
  expect_equal(Csum(log(unlist(bS)+1.2)), c(3828, 1878.749175681913, 2554.80550645861))
  expect_equal(Csum(log(unlist(bN)+1)), c(3828, 1299.31846833678, 2415.705455668723))
  
  expect_warning(bS <- CalibrateSSB(z,popData=zPop,calmodel="~ edu*sex + sex*age",
               partition=c("year","q"), y=c("unemployed","workforce"),
               leverageOutput=TRUE, samplingWeights ="samplingWeights"))
  
  
  expect_equal(Csum(log(unlist(bS)+1.1)), c(7352, 2728.768354842984, 3667.182228662961)) 
  
  expect_identical(bS$popTotals, suppressWarnings(
    CalibrateSSB(z,popData=zPop,calmodel="~ edu*sex + sex*age",
                 partition=c("year","q"), y=c("unemployed","workforce"),
                 leverageOutput=TRUE, samplingWeights ="samplingWeights",
                 usePackage = "none", onlyTotals = TRUE)))
  
  
  bSWide = WideFromCalibrate(bS,CrossStrata(z[,c("year","q")]),z$id)
  # Define linear combination matrix
  lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
  
  # Unemployed: Totals and linear combinations
  d1=PanelEstimation(bSWide,numerator="unemployed",denominator="workforce",linComb=lc, leveragePower = 1)  
  
  expect_equal(Csum(sqrt(unlist(d1)+15)), c(102, 817.3757246497714, 8722.4130549911861))
  
  
  # MERK: Variabelnavn i CalibrateSSB og vektor-input i WideFromCalibrate bør forklares bedre 
  
  expect_warning(d2 <- CalibrateSSBpanel(z, popData=zPop,calmodel="~ sex*edu + age*sex",  # edu*sex + sex*age" over
                        partition=c("year","q"), 
                        y=c("unemployed","workforce"),leverageOutput=TRUE,samplingWeights ="samplingWeights",
                        wave=c("year","q"), id = "id", usePackage = "none",
                        numerator="unemployed",denominator="workforce",linComb=lc, leveragePower = 1))
  
  expect_equal(d1,d2)
  
})


test_that("extra", {
  
  lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
  
  #tt = function(x)  tibble::as_tibble(x)
  
  z$yq = CrossStrata(z[,c("year","q")])
  zPop$yq = CrossStrata(zPop[,c("year","q")])
  
  a <- CalibrateSSBpanel(z, popData=zPop,calmodel="~ sex",  # edu*sex + sex*age" over
                          partition=c("yq"), 
                          y=c("unemployed"),leverageOutput=TRUE,samplingWeights ="samplingWeights",
                          wave=c("yq"), id = "id", usePackage = "none",
                          numerator="unemployed",linComb=lc, leveragePower = 1)

  expect_equal(Csum(log(unlist(a)+ 15)), c(34, 114.9679972860836, 479.3773963999))
  
  
  
  a <- CalibrateSSB(z, calmodel = "~ sex*age",
                    partition = c("year","q"),  # calibrate within quarter
                    popData = zPop, y = c("unemployed","workforce"),
                    by = c("year")) # Estimate within quarter
  
    
  expect_equal(Csum(log(unlist(a)+ 1)), c(3832, 1307.354796766325, 2450.963556820278))
  
  
  
})



if(require(ReGenesees)){ test_that("ReGenesees", {
  z$sex = as.factor(z$sex)
  zPop$age = as.factor(zPop$age)
  
  
  # Calibration, no package, popData as input
  bN <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"),
                     by = c("year","q"),
                     samplingWeights ="samplingWeights")
  
  # Calibration, "ReGenesees", popData as input
  bR <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "ReGenesees", y = c("unemployed","workforce"),
                     by = c("year","q"),
                     samplingWeights ="samplingWeights")

  expect_equal(bN$w, bR$w)
  expect_equal(as.data.frame(bR$popTotals[,3:8]),bN$popTotals[,3:8])
  estTM=merge(bR$estTM,bN$estTM)
  expect_equal(estTM[,"Total.workforce"],estTM[,"workforce"])
  
  
  # Residuals different when samplingWeights
  
  
  # Calibration, no package, popData as input, no samplingWeights
  bN <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"),
                     by = c("year","q"))
  
  # Calibration, "ReGenesees", popData as input, no samplingWeights
  bR <- CalibrateSSB(z, popData=zPop, calmodel="~ sex*age",
                     partition = c("year","q"), usePackage = "ReGenesees", y = c("unemployed","workforce"),
                     by = c("year","q"))
  
  expect_equal(bN$resids[,2], bR$resids[,2])
  
})}

