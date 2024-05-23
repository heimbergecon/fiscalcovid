  rm(list = ls()) #clear list
  
  #automatic installation of required packages
  packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
                "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
                "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
                "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")
  
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  ipak(packages)
  
  #load packages
  library(xlsx) #Excel-Paket laden
  library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
  library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
  library(sandwich)
  library(lmtest)
  library(getopt)
  library(CausalGAM)
  library(ggplot2)
  library(reshape2)
  library(xts)
  library(lattice)
  library(gridExtra)
  library(gtable)
  library(plm)
  library(lfe)
  library(lmtest)
  library(car)
  library(tis)
  library(foreign)
  library(MASS)
  library(quantreg)
  library(ggrepel)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(datasets)
  library(rio)
  library(psych)
  library(systemfit)
  library(foreign)
  library(MatchIt)
  library(CRTgeeDR)
  library(eurostat)
  library(plyr)
  library(zoo)
  library(ggthemes)
  library("robumeta")
  library("metafor")
  library("dplyr")
  library(clubSandwich)
  library(Hmisc)
  library(metafor)
  library(pracma)
  library(broom)
  library(sjPlot)
  library(here)
  library(data.table)
  
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  #Load data
  dat <- fread(here("data/datafinal_extend.csv"))
  #exclude Ireland and Norway because of outliers/data problems
  dat <- subset(dat, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN','CAN','CHE','GBR','JPN','KOR', 'NZL','USA' ,'CZE', 'DNK', 'HUN', 'POL', 'SWE'))
  dat <- dat %>%
    dplyr::group_by(ccode) %>%
    dplyr::mutate(lag_PDebt = lag(PDebt))

  dat <- ddply(dat,"ccode", transform,
                      diffoutputgap=c(NA,diff(outputgap))) #calculate population growth
  dat$outputgap
  
  #multiply fiscal variables by (-1) to facilitate more intuitive cyclicality interpretation
  dat$primarybalance <- dat$primarybalance*(-1)
  dat$CAPB <- dat$CAPB*(-1)
  dat$cyclicalcomponent <- dat$cyclicalcomponent*(-1)
  dat$diffprimarybalance <- dat$diffprimarybalance*(-1)
  dat$diffCAPB <- dat$diffCAPB*(-1)
  dat$diffcyclicalcomponent <- dat$diffcyclicalcomponent*(-1)
  
  
  #alternative instrument
  dat$alt_inst <- dat$US_outputgap_instrument * (dat$TaxRev/100)
  
  #full sample
  dat_full <- subset(dat, year %in% c('1995', '1996', '1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012', '2013', '2014', '2015','2016', '2017', '2018', '2019', '2020', '2021'))
  dat_EU <- subset(dat_full, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','ITA','LTU','LUX','MLT','NLD','PRT','SVK'))
  dat_Euro_full <- subset(dat_full, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
  dat_OECD <- subset(dat_full, ccode %in% c('CAN','CHE','GBR','JPN','KOR','NZL','USA','CZE', 'DNK', 'HUN', 'POL', 'SWE'))
  dat_EU_core <- subset(dat_full, ccode %in% c('AUT', 'BEL','DEU','FIN','NLD'))
  dat_EU_peri<- subset(dat_full, ccode %in% c('ESP','GRC','ITA','PRT'))
  dat_full_no_covid <- subset(dat_full, year %in% c('1995', '1996', '1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012', '2013', '2014', '2015','2016', '2017', '2018', '2019'))
  dat_EU_no_covid <- subset(dat_EU, year %in% c('1995', '1996', '1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012', '2013', '2014', '2015','2016', '2017', '2018', '2019'))
  dat_OECD_no_covid <- subset(dat_OECD, year %in% c('1995', '1996', '1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012', '2013', '2014', '2015','2016', '2017', '2018', '2019'))
  
  #descriptive statistics
  dat_descr <- select(dat_full, primarybalance, CAPB, cyclicalcomponent, outputgap, PDebt, Election)
  stargazer(as.data.frame(dat_descr))
  
  #pre and post-Covid
  dat_pre_Covid <- subset(dat_full, year %in% c('1995', '1996', '1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012', '2013', '2014', '2015','2016', '2017', '2018', '2019'))
  dat_Covid <- subset(dat_full, year %in% c('2020', '2021'))
  dat_fincrisis <- subset(dat_full, year %in% c('2008', '2009'))
  dat_Eurocrisis <- subset(dat_full, year %in% c('2011', '2012'))
  
  #EU countries only
  dat_EU_pre_Covid <- subset(dat_pre_Covid, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
  dat_EU_Covid <- subset(dat_Covid, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
  dat_EU_fincrisis <- subset(dat_fincrisis, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
  dat_EU_Eurocrisis <- subset(dat_Eurocrisis, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
  dat_OECD_fincrisis <- subset(dat_fincrisis, ccode %in% c('CAN','CHE','GBR','JPN','KOR','NZL','USA','CZE', 'DNK', 'HUN', 'POL', 'SWE'))
  dat_OECD_Eurocrisis <- subset(dat_Eurocrisis, ccode %in% c('CAN','CHE','GBR','JPN','KOR','NZL','USA','CZE', 'DNK', 'HUN', 'POL', 'SWE'))
  dat_OECD_Covid <- subset(dat_Covid, ccode %in% c('CAN','CHE','GBR','JPN','KOR','NZL','USA','CZE', 'DNK', 'HUN', 'POL', 'SWE'))
  
  #Eurozone countries only
  dat_Euro_pre_Covid <- subset(dat_pre_Covid, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
  dat_Euro_Covid <- subset(dat_Covid, ccode %in% c('AUT', 'BEL','CYP','DEU','ESP','EST','FIN','FRA','GRC','IRL','ITA','LTU','LUX','LVA','MLT','NLD','PRT','SVK','SVN'))
 
  #Instrumental variables
  library(AER)
  library(ivpack)
  #full sample with crisis dummies
  #fiscal balance

  reg_v2_FB_full_instrument <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_full)
  summary(reg_v2_FB_full_instrument)
  #weak instrument test
  summary(reg_v2_FB_full_instrument, vcov = sandwich)
  summary(reg_v2_FB_full_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_full_instrument, vcov.=vcovHAC(reg_v2_FB_full_instrument))
  
  reg_v2_CAB_full_instrument <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_full)
  summary(reg_v2_CAB_full_instrument)
  #weak instrument test
  summary(reg_v2_CAB_full_instrument, vcov = sandwich)
  summary(reg_v2_CAB_full_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_full_instrument, vcov.=vcovHAC(reg_v2_CAB_full_instrument))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_full)
  summary(reg_v2_non_CAB_full_instrument)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument <- list(coef_test(reg_v2_FB_full_instrument, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument <- list(coef_test(reg_v2_FB_full_instrument, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument <- list(coef_test(reg_v2_FB_full_instrument, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_full_instrument <- list(coef_test(reg_v2_CAB_full_instrument, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument <- list(coef_test(reg_v2_CAB_full_instrument, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument <- list(coef_test(reg_v2_CAB_full_instrument, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument <- list(coeftest(reg_v2_non_CAB_full_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument <- list(coeftest(reg_v2_non_CAB_full_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument <- list(coeftest(reg_v2_non_CAB_full_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #EU sample with crisis dummies
  #fiscal balance

  reg_v2_FB_EU_instrument <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_EU)
  summary(reg_v2_FB_EU_instrument)
  #weak instrument test
  summary(reg_v2_FB_EU_instrument, vcov = sandwich)
  summary(reg_v2_FB_EU_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_EU_instrument, vcov.=vcovHAC(reg_v2_FB_EU_instrument))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_EU)
  summary(reg_v2_CAPB_instrument)
  #weak instrument test
  summary(reg_v2_CAPB_instrument, vcov = sandwich)
  summary(reg_v2_CAPB_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument, vcov.=vcovHAC(reg_v2_CAPB_instrument))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU)
  summary(reg_v2_cyclicalcomponent_instrument)

  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument <- list(coef_test(reg_v2_FB_EU_instrument, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument <- list(coef_test(reg_v2_FB_EU_instrument, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument <- list(coef_test(reg_v2_FB_EU_instrument, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument <- list(coef_test(reg_v2_CAPB_instrument, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument <- list(coef_test(reg_v2_CAPB_instrument, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument <- list(coef_test(reg_v2_CAPB_instrument, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument <- list(coeftest(reg_v2_cyclicalcomponent_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument <- list(coeftest(reg_v2_cyclicalcomponent_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument <- list(coeftest(reg_v2_cyclicalcomponent_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_OECD_instrument <- plm(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD)
  summary(reg_v2_FB_OECD_instrument)

  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_OECD_instrument <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_OECD)
  summary(reg_v2_CAB_OECD_instrument)
  #weak instrument test
  summary(reg_v2_CAB_OECD_instrument, vcov = sandwich)
  summary(reg_v2_CAB_OECD_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_OECD_instrument, vcov.=vcovHAC(reg_v2_CAB_OECD_instrument))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_OECD_instrument <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + lag(PDebt) + Election , index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD)
  summary(reg_v2_non_CAB_OECD_instrument)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_OECD_instrument <- list(coeftest(reg_v2_FB_OECD_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_OECD_instrument <- list(coeftest(reg_v2_FB_OECD_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_FB_OECD_instrument <- list(coeftest(reg_v2_FB_OECD_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  ses.reg_v2_CAB_OECD_instrument <- list(coef_test(reg_v2_CAB_OECD_instrument, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_OECD_instrument <- list(coef_test(reg_v2_CAB_OECD_instrument, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_OECD_instrument <- list(coef_test(reg_v2_CAB_OECD_instrument, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_OECD_instrument <- list(coeftest(reg_v2_non_CAB_OECD_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_OECD_instrument <- list(coeftest(reg_v2_non_CAB_OECD_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_OECD_instrument <- list(coeftest(reg_v2_non_CAB_OECD_instrument, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #only Covid crisis dummy
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_full_instrument_CO <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_FB_full_instrument_CO)

  #weak instrument test
  summary(reg_v2_FB_full_instrument_CO, vcov = sandwich)
  summary(reg_v2_FB_full_instrument_CO, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_full_instrument_CO, vcov.=vcovHAC(reg_v2_FB_full_instrument_CO))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_full_instrument_CO <- ivreg(CAPB ~ lag(CAPB) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_CAB_full_instrument_CO)
  #weak instrument test
  summary(reg_v2_CAB_full_instrument_CO, vcov = sandwich)
  summary(reg_v2_CAB_full_instrument_CO, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_full_instrument_CO, vcov.=vcovHAC(reg_v2_CAB_full_instrument_CO))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument_CO <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_full)
  summary(reg_v2_non_CAB_full_instrument_CO)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument_CO <- list(coef_test(reg_v2_FB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument_CO <- list(coef_test(reg_v2_FB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument_CO <- list(coef_test(reg_v2_FB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val

  ses.reg_v2_CAB_full_instrument_CO <- list(coef_test(reg_v2_CAB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument_CO <- list(coef_test(reg_v2_CAB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument_CO <- list(coef_test(reg_v2_CAB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument_CO <- list(coeftest(reg_v2_non_CAB_full_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument_CO <- list(coeftest(reg_v2_non_CAB_full_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument_CO <- list(coeftest(reg_v2_non_CAB_full_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #EU sample with crisis dummies
  #fiscal balance
  reg_v2_FB_EU_instrument_CO <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_EU)
  summary(reg_v2_FB_EU_instrument_CO)
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_CO <- ivreg(CAPB ~ lag(CAPB) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_EU)
  summary(reg_v2_CAPB_instrument_CO)
  #weak instrument test
  summary(reg_v2_CAPB_instrument_CO, vcov = sandwich)
  summary(reg_v2_CAPB_instrument_CO, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument_CO, vcov.=vcovHAC(reg_v2_CAPB_instrument_CO))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_CO <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU)
  summary(reg_v2_cyclicalcomponent_instrument_CO)

  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_CO <- list(coef_test(reg_v2_FB_EU_instrument_CO, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_CO <- list(coef_test(reg_v2_FB_EU_instrument_CO, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_CO <- list(coef_test(reg_v2_FB_EU_instrument_CO, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_CO <- list(coef_test(reg_v2_CAPB_instrument_CO, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_CO <- list(coef_test(reg_v2_CAPB_instrument_CO, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_CO <- list(coef_test(reg_v2_CAPB_instrument_CO, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_CO <- list(coeftest(reg_v2_cyclicalcomponent_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_CO <- list(coeftest(reg_v2_cyclicalcomponent_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_CO <- list(coeftest(reg_v2_cyclicalcomponent_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_OECD_instrument_CO <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_OECD)
  summary(reg_v2_FB_OECD_instrument_CO)
  #weak instrument test
  summary(reg_v2_FB_OECD_instrument_CO, vcov = sandwich)
  summary(reg_v2_FB_OECD_instrument_CO, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_OECD_instrument_CO, vcov.=vcovHAC(reg_v2_FB_OECD_instrument_CO))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_OECD_instrument_CO <- ivreg(CAPB ~ lag(CAPB) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_OECD)
  summary(reg_v2_CAB_OECD_instrument_CO)
  #weak instrument test
  summary(reg_v2_CAB_OECD_instrument_CO, vcov = sandwich)
  summary(reg_v2_CAB_OECD_instrument_CO, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_OECD_instrument_CO, vcov.=vcovHAC(reg_v2_CAB_OECD_instrument_CO))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_OECD_instrument_CO <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD)
  summary(reg_v2_non_CAB_OECD_instrument_CO)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_OECD_instrument_CO <- list(coef_test(reg_v2_FB_OECD_instrument_CO, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_OECD_instrument_CO <- list(coef_test(reg_v2_FB_OECD_instrument_CO, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_OECD_instrument_CO <- list(coef_test(reg_v2_FB_OECD_instrument_CO, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_OECD_instrument_CO <- list(coef_test(reg_v2_CAB_OECD_instrument_CO, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_OECD_instrument_CO <- list(coef_test(reg_v2_CAB_OECD_instrument_CO, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_OECD_instrument_CO <- list(coef_test(reg_v2_CAB_OECD_instrument_CO, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_OECD_instrument_CO <- list(coeftest(reg_v2_non_CAB_OECD_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_OECD_instrument_CO <- list(coeftest(reg_v2_non_CAB_OECD_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_OECD_instrument_CO <- list(coeftest(reg_v2_non_CAB_OECD_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #only Covid crisis dummy
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_full_instrument_ND <- ivreg(primarybalance ~ lag(primarybalance) + outputgap  + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap) + US_outputgap_instrument + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_FB_full_instrument_ND)

  #weak instrument test
  summary(reg_v2_FB_full_instrument_ND, vcov = sandwich)
  summary(reg_v2_FB_full_instrument_ND, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_full_instrument_ND, vcov.=vcovHAC(reg_v2_FB_full_instrument_ND))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_full_instrument_ND <- ivreg(CAPB ~ lag(CAPB) + outputgap  + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap) + US_outputgap_instrument + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_CAB_full_instrument_ND)
  #weak instrument test
  summary(reg_v2_CAB_full_instrument_ND, vcov = sandwich)
  summary(reg_v2_CAB_full_instrument_ND, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_full_instrument_ND, vcov.=vcovHAC(reg_v2_CAB_full_instrument_ND))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument_ND <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_full)
  summary(reg_v2_non_CAB_full_instrument_ND)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument_ND <- list(coef_test(reg_v2_FB_full_instrument_ND, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument_ND <- list(coef_test(reg_v2_FB_full_instrument_ND, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument_ND <- list(coef_test(reg_v2_FB_full_instrument_ND, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val

  ses.reg_v2_CAB_full_instrument_ND <- list(coef_test(reg_v2_CAB_full_instrument_ND, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument_ND <- list(coef_test(reg_v2_CAB_full_instrument_ND, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument_ND <- list(coef_test(reg_v2_CAB_full_instrument_ND, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument_ND <- list(coeftest(reg_v2_non_CAB_full_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument_ND <- list(coeftest(reg_v2_non_CAB_full_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument_ND <- list(coeftest(reg_v2_non_CAB_full_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #EU sample with crisis dummies
  #fiscal balance
  reg_v2_FB_EU_instrument_ND <- ivreg(primarybalance ~ lag(primarybalance) + outputgap  + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap) + US_outputgap_instrument + factor(ccode) + lag(PDebt) + Election, data=dat_EU)
  summary(reg_v2_FB_EU_instrument_ND)
  
  #weak instrument test
  summary(reg_v2_FB_EU_instrument_ND, vcov = sandwich)
  summary(reg_v2_FB_EU_instrument_ND, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_EU_instrument_ND, vcov.=vcovHAC(reg_v2_FB_EU_instrument_ND))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_ND <- ivreg(CAPB ~ lag(CAPB) + outputgap  + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap) + US_outputgap_instrument + factor(ccode) + lag(PDebt) + Election, data=dat_EU)
  summary(reg_v2_CAPB_instrument_ND)
  #weak instrument test
  summary(reg_v2_CAPB_instrument_ND, vcov = sandwich)
  summary(reg_v2_CAPB_instrument_ND, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument_ND, vcov.=vcovHAC(reg_v2_CAPB_instrument_ND))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_ND <- plm(cyclicalcomponent ~ lag(cyclicalcomponent)  + outputgap + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU)
  summary(reg_v2_cyclicalcomponent_instrument_ND)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_ND <- list(coef_test(reg_v2_FB_EU_instrument_ND, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_ND <- list(coef_test(reg_v2_FB_EU_instrument_ND, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_ND <- list(coef_test(reg_v2_FB_EU_instrument_ND, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_ND <- list(coef_test(reg_v2_CAPB_instrument_ND, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_ND <- list(coef_test(reg_v2_CAPB_instrument_ND, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_ND <- list(coef_test(reg_v2_CAPB_instrument_ND, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_ND <- list(coeftest(reg_v2_cyclicalcomponent_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_ND <- list(coeftest(reg_v2_cyclicalcomponent_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_ND <- list(coeftest(reg_v2_cyclicalcomponent_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_OECD_instrument_ND <- ivreg(primarybalance ~ lag(primarybalance) + outputgap  + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap) + US_outputgap_instrument + factor(ccode) + lag(PDebt) + Election, data=dat_OECD)
  summary(reg_v2_FB_OECD_instrument_ND)
  #weak instrument test
  summary(reg_v2_FB_OECD_instrument_ND, vcov = sandwich)
  summary(reg_v2_FB_OECD_instrument_ND, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_OECD_instrument_ND, vcov.=vcovHAC(reg_v2_FB_OECD_instrument_ND))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_OECD_instrument_ND <- ivreg(CAPB ~ lag(CAPB) + outputgap  + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap) + US_outputgap_instrument + factor(ccode) + lag(PDebt) + Election, data=dat_OECD)
  summary(reg_v2_CAB_OECD_instrument_ND)
  #weak instrument test
  summary(reg_v2_CAB_OECD_instrument_ND, vcov = sandwich)
  summary(reg_v2_CAB_OECD_instrument_ND, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_OECD_instrument_ND, vcov.=vcovHAC(reg_v2_CAB_OECD_instrument_ND))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_OECD_instrument_ND <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD)
  summary(reg_v2_non_CAB_OECD_instrument_ND)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_OECD_instrument_ND <- list(coef_test(reg_v2_FB_OECD_instrument_ND, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_OECD_instrument_ND <- list(coef_test(reg_v2_FB_OECD_instrument_ND, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_OECD_instrument_ND <- list(coef_test(reg_v2_FB_OECD_instrument_ND, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_OECD_instrument_ND <- list(coef_test(reg_v2_CAB_OECD_instrument_ND, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_OECD_instrument_ND <- list(coef_test(reg_v2_CAB_OECD_instrument_ND, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_OECD_instrument_ND <- list(coef_test(reg_v2_CAB_OECD_instrument_ND, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_OECD_instrument_ND <- list(coeftest(reg_v2_non_CAB_OECD_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_OECD_instrument_ND <- list(coeftest(reg_v2_non_CAB_OECD_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_OECD_instrument_ND <- list(coeftest(reg_v2_non_CAB_OECD_instrument_ND, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #GMM
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_full_instrument_GMM <- pgmm(primarybalance ~ lag(primarybalance) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(primarybalance,k=2) + lag(primarybalance,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v2_FB_full_instrument_GMM)
  coeftest(reg_v2_FB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))
  
  summary( reg_v2_FB_full_instrument_GMM, vcov = sandwich, df = Inf, diagnostics = TRUE)
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_full_instrument_GMM <- pgmm(CAPB ~ lag(CAPB) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(CAPB,k=2) + lag(CAPB,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v2_CAB_full_instrument_GMM)

  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument_GMM <- pgmm(cyclicalcomponent ~ lag(cyclicalcomponent) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(cyclicalcomponent,k=2) + lag(cyclicalcomponent,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v2_non_CAB_full_instrument_GMM)

  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument_GMM <- list(coeftest(reg_v2_FB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument_GMM <- list(coeftest(reg_v2_FB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument_GMM <- list(coeftest(reg_v2_FB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  summary(reg_v2_FB_full_instrument_GMM)
  
  ses.reg_v2_CAB_full_instrument_GMM <- list(coeftest(reg_v2_CAB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument_GMM <- list(coeftest(reg_v2_CAB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument_GMM <- list(coeftest(reg_v2_CAB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument_GMM <- list(coeftest(reg_v2_non_CAB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument_GMM <- list(coeftest(reg_v2_non_CAB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument_GMM <- list(coeftest(reg_v2_non_CAB_full_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #EU sample with crisis dummies
  #fiscal balance
  reg_v2_FB_EU_instrument_GMM <- pgmm(primarybalance ~ lag(primarybalance) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(primarybalance,k=2) + lag(primarybalance,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_EU)
  summary(reg_v2_FB_EU_instrument_GMM)
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_GMM <- pgmm(CAPB ~ lag(CAPB) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(CAPB,k=2) + lag(CAPB,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_EU)
  summary(reg_v2_CAPB_instrument_GMM)

  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_GMM <- pgmm(cyclicalcomponent ~ lag(cyclicalcomponent) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(cyclicalcomponent,k=2) + lag(cyclicalcomponent,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_EU)
  summary(reg_v2_cyclicalcomponent_instrument_GMM)
  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_GMM <- list(coeftest(reg_v2_FB_EU_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_GMM <- list(coeftest(reg_v2_FB_EU_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_GMM <- list(coeftest(reg_v2_FB_EU_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_GMM <- list(coeftest(reg_v2_CAPB_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_GMM <- list(coeftest(reg_v2_CAPB_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_GMM <- list(coeftest(reg_v2_CAPB_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_GMM <- list(coeftest(reg_v2_cyclicalcomponent_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_GMM <- list(coeftest(reg_v2_cyclicalcomponent_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_GMM <- list(coeftest(reg_v2_cyclicalcomponent_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_OECD_instrument_GMM <- pgmm(primarybalance ~ lag(primarybalance) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(primarybalance,k=2) + lag(primarybalance,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_OECD)
  summary(reg_v2_FB_OECD_instrument_GMM)
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_OECD_instrument_GMM <- pgmm(CAPB ~ lag(CAPB) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(CAPB,k=2) + lag(CAPB,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_OECD)
  summary(reg_v2_CAB_OECD_instrument_GMM)

  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)

  reg_v2_non_CAB_OECD_instrument_GMM <- pgmm(cyclicalcomponent ~ lag(cyclicalcomponent) + CovidCrisis*outputgap + lag(PDebt) + Election | lag(cyclicalcomponent,k=2) + lag(cyclicalcomponent,k=3) +  CovidCrisis*lag(outputgap,k=2) + CovidCrisis*lag(outputgap,k=3) + lag(PDebt) + Election, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_OECD)
  summary(reg_v2_non_CAB_OECD_instrument_GMM)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_OECD_instrument_GMM <- list(coeftest(reg_v2_FB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_OECD_instrument_GMM <- list(coeftest(reg_v2_FB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_OECD_instrument_GMM <- list(coeftest(reg_v2_FB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_OECD_instrument_GMM <- list(coeftest(reg_v2_CAB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_OECD_instrument_GMM <- list(coeftest(reg_v2_CAB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_OECD_instrument_GMM <- list(coeftest(reg_v2_CAB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_OECD_instrument_GMM <- list(coeftest(reg_v2_non_CAB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_OECD_instrument_GMM <- list(coeftest(reg_v2_non_CAB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_OECD_instrument_GMM <- list(coeftest(reg_v2_non_CAB_OECD_instrument_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

  #alternative instrument
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_full_instrument_CO_alt_int <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap)*CovidCrisis + alt_inst*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_FB_full_instrument_CO_alt_int)
  
  #weak instrument test
  summary(reg_v2_FB_full_instrument_CO_alt_int, vcov = sandwich)
  summary(reg_v2_FB_full_instrument_CO_alt_int, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_full_instrument_CO_alt_int, vcov.=vcovHAC(reg_v2_FB_full_instrument_CO_alt_int))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_full_instrument_CO_alt_int <- ivreg(CAPB ~ lag(CAPB) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap)*CovidCrisis + alt_inst*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_CAB_full_instrument_CO_alt_int)
  #weak instrument test
  summary(reg_v2_CAB_full_instrument_CO_alt_int, vcov = sandwich)
  summary(reg_v2_CAB_full_instrument_CO_alt_int, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_full_instrument_CO_alt_int, vcov.=vcovHAC(reg_v2_CAB_full_instrument_CO_alt_int))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument_CO_alt_int <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_full)
  summary(reg_v2_non_CAB_full_instrument_CO_alt_int)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_full_instrument_CO_alt_int, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_full_instrument_CO_alt_int, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_full_instrument_CO_alt_int, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_full_instrument_CO_alt_int <- list(coef_test(reg_v2_CAB_full_instrument_CO_alt_int, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument_CO_alt_int <- list(coef_test(reg_v2_CAB_full_instrument_CO_alt_int, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument_CO_alt_int <- list(coef_test(reg_v2_CAB_full_instrument_CO_alt_int, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument_CO_alt_int <- list(coeftest(reg_v2_non_CAB_full_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument_CO_alt_int <- list(coeftest(reg_v2_non_CAB_full_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument_CO_alt_int <- list(coeftest(reg_v2_non_CAB_full_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #EU sample with crisis dummies
  #fiscal balance
  reg_v2_FB_EU_instrument_CO_alt_int <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap)*CovidCrisis + alt_inst*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_EU)
  summary(reg_v2_FB_EU_instrument_CO_alt_int)
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_CO_alt_int <- ivreg(CAPB ~ lag(CAPB) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap)*CovidCrisis + alt_inst*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_EU)
  summary(reg_v2_CAPB_instrument_CO_alt_int)
  #weak instrument test
  summary(reg_v2_CAPB_instrument_CO_alt_int, vcov = sandwich)
  summary(reg_v2_CAPB_instrument_CO_alt_int, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument_CO_alt_int, vcov.=vcovHAC(reg_v2_CAPB_instrument_CO_alt_int))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_CO_alt_int <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU)
  summary(reg_v2_cyclicalcomponent_instrument_CO_alt_int)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_EU_instrument_CO_alt_int, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_EU_instrument_CO_alt_int, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_EU_instrument_CO_alt_int, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_CO_alt_int <- list(coef_test(reg_v2_CAPB_instrument_CO_alt_int, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_CO_alt_int <- list(coef_test(reg_v2_CAPB_instrument_CO_alt_int, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_CO_alt_int <- list(coef_test(reg_v2_CAPB_instrument_CO_alt_int, vcov = "CR0", cluster = dat_EU$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_CO_alt_int <- list(coeftest(reg_v2_cyclicalcomponent_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_CO_alt_int <- list(coeftest(reg_v2_cyclicalcomponent_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_CO_alt_int <- list(coeftest(reg_v2_cyclicalcomponent_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_OECD_instrument_CO_alt_int <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) +  lag(outputgap)*CovidCrisis + alt_inst*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_OECD)
  summary(reg_v2_FB_OECD_instrument_CO_alt_int)
  #weak instrument test
  summary(reg_v2_FB_OECD_instrument_CO_alt_int, vcov = sandwich)
  summary(reg_v2_FB_OECD_instrument_CO_alt_int, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_OECD_instrument_CO_alt_int, vcov.=vcovHAC(reg_v2_FB_OECD_instrument_CO_alt_int))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_OECD_instrument_CO_alt_int <- ivreg(CAPB ~ lag(CAPB) + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) +  lag(outputgap)*CovidCrisis + alt_inst*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_OECD)
  summary(reg_v2_CAB_OECD_instrument_CO_alt_int)
  #weak instrument test
  summary(reg_v2_CAB_OECD_instrument_CO_alt_int, vcov = sandwich)
  summary(reg_v2_CAB_OECD_instrument_CO_alt_int, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_OECD_instrument_CO_alt_int, vcov.=vcovHAC(reg_v2_CAB_OECD_instrument_CO_alt_int))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_OECD_instrument_CO_alt_int <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD)
  summary(reg_v2_non_CAB_OECD_instrument_CO_alt_int)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_OECD_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_OECD_instrument_CO_alt_int, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_OECD_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_OECD_instrument_CO_alt_int, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_OECD_instrument_CO_alt_int <- list(coef_test(reg_v2_FB_OECD_instrument_CO_alt_int, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_OECD_instrument_CO_alt_int <- list(coef_test(reg_v2_CAB_OECD_instrument_CO_alt_int, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_OECD_instrument_CO_alt_int <- list(coef_test(reg_v2_CAB_OECD_instrument_CO_alt_int, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_OECD_instrument_CO_alt_int <- list(coef_test(reg_v2_CAB_OECD_instrument_CO_alt_int, vcov = "CR0", cluster = dat_OECD$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_OECD_instrument_CO_alt_int <- list(coeftest(reg_v2_non_CAB_OECD_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_OECD_instrument_CO_alt_int <- list(coeftest(reg_v2_non_CAB_OECD_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_OECD_instrument_CO_alt_int <- list(coeftest(reg_v2_non_CAB_OECD_instrument_CO_alt_int, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #Eurozone sub-samples core/periphery
  
  #core
  #fiscal balance
  
  reg_v2_FB_EU_instrument_core <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_EU_core)
  summary(reg_v2_FB_EU_instrument_core)
  #weak instrument test
  summary(reg_v2_FB_EU_instrument_core, vcov = sandwich)
  summary(reg_v2_FB_EU_instrument_core, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_EU_instrument_core, vcov.=vcovHAC(reg_v2_FB_EU_instrument_core))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_core <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_EU_core)
  summary(reg_v2_CAPB_instrument_core)
  #weak instrument test
  summary(reg_v2_CAPB_instrument_core, vcov = sandwich)
  summary(reg_v2_CAPB_instrument_core, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument_core, vcov.=vcovHAC(reg_v2_CAPB_instrument_core))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_core <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU_core)
  summary(reg_v2_cyclicalcomponent_instrument_core)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_core <- list(coef_test(reg_v2_FB_EU_instrument_core, vcov = "CR0", cluster = dat_EU_core$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_core <- list(coef_test(reg_v2_FB_EU_instrument_core, vcov = "CR0", cluster = dat_EU_core$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_core <- list(coef_test(reg_v2_FB_EU_instrument_core, vcov = "CR0", cluster = dat_EU_core$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_core <- list(coef_test(reg_v2_CAPB_instrument_core, vcov = "CR0", cluster = dat_EU_core$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_core <- list(coef_test(reg_v2_CAPB_instrument_core, vcov = "CR0", cluster = dat_EU_core$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_core <- list(coef_test(reg_v2_CAPB_instrument_core, vcov = "CR0", cluster = dat_EU_core$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_core <- list(coeftest(reg_v2_cyclicalcomponent_instrument_core, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_core <- list(coeftest(reg_v2_cyclicalcomponent_instrument_core, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_core <- list(coeftest(reg_v2_cyclicalcomponent_instrument_core, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #peri
  #fiscal balance
  
  reg_v2_FB_EU_instrument_peri <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_EU_peri)
  summary(reg_v2_FB_EU_instrument_peri)
  #weak instrument test
  summary(reg_v2_FB_EU_instrument_peri, vcov = sandwich)
  summary(reg_v2_FB_EU_instrument_peri, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_EU_instrument_peri, vcov.=vcovHAC(reg_v2_FB_EU_instrument_peri))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_peri <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + lag(outputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode), data=dat_EU_peri)
  summary(reg_v2_CAPB_instrument_peri)
  #weak instrument test
  summary(reg_v2_CAPB_instrument_peri, vcov = sandwich)
  summary(reg_v2_CAPB_instrument_peri, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument_peri, vcov.=vcovHAC(reg_v2_CAPB_instrument_peri))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_peri <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU_peri)
  summary(reg_v2_cyclicalcomponent_instrument_peri)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_peri <- list(coef_test(reg_v2_FB_EU_instrument_peri, vcov = "CR0", cluster = dat_EU_peri$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_peri <- list(coef_test(reg_v2_FB_EU_instrument_peri, vcov = "CR0", cluster = dat_EU_peri$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_peri <- list(coef_test(reg_v2_FB_EU_instrument_peri, vcov = "CR0", cluster = dat_EU_peri$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_peri <- list(coef_test(reg_v2_CAPB_instrument_peri, vcov = "CR0", cluster = dat_EU_peri$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_peri <- list(coef_test(reg_v2_CAPB_instrument_peri, vcov = "CR0", cluster = dat_EU_peri$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_peri <- list(coef_test(reg_v2_CAPB_instrument_peri, vcov = "CR0", cluster = dat_EU_peri$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_peri <- list(coeftest(reg_v2_cyclicalcomponent_instrument_peri, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_peri <- list(coeftest(reg_v2_cyclicalcomponent_instrument_peri, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_peri <- list(coeftest(reg_v2_cyclicalcomponent_instrument_peri, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #without Covid-19 crisis year
  #fiscal balance

  reg_v2_FB_full_instrument_no_covid <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + factor(ccode), data=dat_full_no_covid)
  summary(reg_v2_FB_full_instrument_no_covid)
  #weak instrument test
  summary(reg_v2_FB_full_instrument_no_covid, vcov = sandwich)
  summary(reg_v2_FB_full_instrument_no_covid, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_full_instrument_no_covid, vcov.=vcovHAC(reg_v2_FB_full_instrument_no_covid))
  
  reg_v2_CAB_full_instrument_no_covid <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + factor(ccode), data=dat_full_no_covid)
  summary(reg_v2_CAB_full_instrument_no_covid)
  #weak instrument test
  summary(reg_v2_CAB_full_instrument_no_covid, vcov = sandwich)
  summary(reg_v2_CAB_full_instrument_no_covid, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_full_instrument_no_covid, vcov.=vcovHAC(reg_v2_CAB_full_instrument_no_covid))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument_no_covid <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_full_no_covid)
  summary(reg_v2_non_CAB_full_instrument_no_covid)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument_no_covid <- list(coef_test(reg_v2_FB_full_instrument_no_covid, vcov = "CR0", cluster = dat_full_no_covid$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument_no_covid <- list(coef_test(reg_v2_FB_full_instrument_no_covid, vcov = "CR0", cluster = dat_full_no_covid$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument_no_covid <- list(coef_test(reg_v2_FB_full_instrument_no_covid, vcov = "CR0", cluster = dat_full_no_covid$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_full_instrument_no_covid <- list(coef_test(reg_v2_CAB_full_instrument_no_covid, vcov = "CR0", cluster = dat_full_no_covid$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument_no_covid <- list(coef_test(reg_v2_CAB_full_instrument_no_covid, vcov = "CR0", cluster = dat_full_no_covid$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument_no_covid <- list(coef_test(reg_v2_CAB_full_instrument_no_covid, vcov = "CR0", cluster = dat_full_no_covid$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument_no_covid <- list(coeftest(reg_v2_non_CAB_full_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument_no_covid <- list(coeftest(reg_v2_non_CAB_full_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument_no_covid <- list(coeftest(reg_v2_non_CAB_full_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #EU sample with crisis dummies
  #fiscal balance
  
  reg_v2_FB_EU_instrument_no_covid <- ivreg(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + factor(ccode), data=dat_EU_no_covid)
  summary(reg_v2_FB_EU_instrument_no_covid)
  #weak instrument test
  summary(reg_v2_FB_EU_instrument_no_covid, vcov = sandwich)
  summary(reg_v2_FB_EU_instrument_no_covid, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_FB_EU_instrument_no_covid, vcov.=vcovHAC(reg_v2_FB_EU_instrument_no_covid))
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAPB_instrument_no_covid <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis + factor(ccode), data=dat_EU_no_covid)
  summary(reg_v2_CAPB_instrument_no_covid)
  #weak instrument test
  summary(reg_v2_CAPB_instrument_no_covid, vcov = sandwich)
  summary(reg_v2_CAPB_instrument_no_covid, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAPB_instrument_no_covid, vcov.=vcovHAC(reg_v2_CAPB_instrument_no_covid))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_cyclicalcomponent_instrument_no_covid <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_EU_no_covid)
  summary(reg_v2_cyclicalcomponent_instrument_no_covid)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_EU_instrument_no_covid <- list(coef_test(reg_v2_FB_EU_instrument_no_covid, vcov = "CR0", cluster = dat_EU_no_covid$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_EU_instrument_no_covid <- list(coef_test(reg_v2_FB_EU_instrument_no_covid, vcov = "CR0", cluster = dat_EU_no_covid$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_EU_instrument_no_covid <- list(coef_test(reg_v2_FB_EU_instrument_no_covid, vcov = "CR0", cluster = dat_EU_no_covid$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAPB_instrument_no_covid <- list(coef_test(reg_v2_CAPB_instrument_no_covid, vcov = "CR0", cluster = dat_EU_no_covid$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAPB_instrument_no_covid <- list(coef_test(reg_v2_CAPB_instrument_no_covid, vcov = "CR0", cluster = dat_EU_no_covid$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAPB_instrument_no_covid <- list(coef_test(reg_v2_CAPB_instrument_no_covid, vcov = "CR0", cluster = dat_EU_no_covid$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_cyclicalcomponent_instrument_no_covid <- list(coeftest(reg_v2_cyclicalcomponent_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_cyclicalcomponent_instrument_no_covid <- list(coeftest(reg_v2_cyclicalcomponent_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_cyclicalcomponent_instrument_no_covid <- list(coeftest(reg_v2_cyclicalcomponent_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #full sample with crisis dummies
  #fiscal balance
  reg_v2_FB_OECD_instrument_no_covid <- plm(primarybalance ~ lag(primarybalance) + outputgap*FinancialCrisis + outputgap*EuroCrisis + factor(ccode) + lag(PDebt) + Election | lag(primarybalance) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + factor(ccode), index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD_no_covid)
  summary(reg_v2_FB_OECD_instrument_no_covid)
  
  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_OECD_instrument_no_covid <- ivreg(CAPB ~ lag(CAPB) + outputgap*FinancialCrisis + outputgap*EuroCrisis + factor(ccode) + lag(PDebt) + Election | lag(CAPB) + lag(outputgap)*FinancialCrisis + US_outputgap_instrument*FinancialCrisis  + lag(outputgap)*EuroCrisis +  US_outputgap_instrument*EuroCrisis + factor(ccode), data=dat_OECD_no_covid)
  summary(reg_v2_CAB_OECD_instrument_no_covid)
  #weak instrument test
  summary(reg_v2_CAB_OECD_instrument_no_covid, vcov = sandwich)
  summary(reg_v2_CAB_OECD_instrument_no_covid, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_OECD_instrument_no_covid, vcov.=vcovHAC(reg_v2_CAB_OECD_instrument_no_covid))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_OECD_instrument_no_covid <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*FinancialCrisis + outputgap*EuroCrisis + lag(PDebt) + Election , index=c("ccode", "year"), model="within", effect="individual", data=dat_OECD_no_covid)
  summary(reg_v2_non_CAB_OECD_instrument_no_covid)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_OECD_instrument_no_covid <- list(coeftest(reg_v2_FB_OECD_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_OECD_instrument_no_covid <- list(coeftest(reg_v2_FB_OECD_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_FB_OECD_instrument_no_covid <- list(coeftest(reg_v2_FB_OECD_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  ses.reg_v2_CAB_OECD_instrument_no_covid <- list(coef_test(reg_v2_CAB_OECD_instrument_no_covid, vcov = "CR0", cluster = dat_OECD_no_covid$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_OECD_instrument_no_covid <- list(coef_test(reg_v2_CAB_OECD_instrument_no_covid, vcov = "CR0", cluster = dat_OECD_no_covid$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_OECD_instrument_no_covid <- list(coef_test(reg_v2_CAB_OECD_instrument_no_covid, vcov = "CR0", cluster = dat_OECD_no_covid$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_OECD_instrument_no_covid <- list(coeftest(reg_v2_non_CAB_OECD_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_OECD_instrument_no_covid <- list(coeftest(reg_v2_non_CAB_OECD_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_OECD_instrument_no_covid <- list(coeftest(reg_v2_non_CAB_OECD_instrument_no_covid, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #Covid crisis dummy, change in CAPB, change in output gap
  #full sample with crisis dummies
  #fiscal balance

  #cyclically-adjusted fiscal balance (discretionary fiscal policy)
  reg_v2_CAB_full_instrument_CO <- ivreg(diffCAPB ~ lag(diffCAPB) + diffoutputgap*CovidCrisis + factor(ccode) + lag(PDebt) + Election | lag(diffCAPB) +  lag(diffoutputgap)*CovidCrisis + US_outputgap_instrument*CovidCrisis + factor(ccode) + lag(PDebt) + Election, data=dat_full)
  summary(reg_v2_CAB_full_instrument_CO)
  #weak instrument test
  summary(reg_v2_CAB_full_instrument_CO, vcov = sandwich)
  summary(reg_v2_CAB_full_instrument_CO, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coeftest(reg_v2_CAB_full_instrument_CO, vcov.=vcovHAC(reg_v2_CAB_full_instrument_CO))
  
  #non-cyclically-adjusted fiscal balance (automatic fiscal stabilisers)
  reg_v2_non_CAB_full_instrument_CO <- plm(cyclicalcomponent ~ lag(cyclicalcomponent) + outputgap*CovidCrisis + lag(PDebt) + Election, index=c("ccode", "year"), model="within", effect="individual", data=dat_full)
  summary(reg_v2_non_CAB_full_instrument_CO)
  
  #preparation for stargazer tables
  ses.reg_v2_FB_full_instrument_CO <- list(coef_test(reg_v2_FB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_FB_full_instrument_CO <- list(coef_test(reg_v2_FB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_FB_full_instrument_CO <- list(coef_test(reg_v2_FB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_CAB_full_instrument_CO <- list(coef_test(reg_v2_CAB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_CAB_full_instrument_CO <- list(coef_test(reg_v2_CAB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v2_CAB_full_instrument_CO <- list(coef_test(reg_v2_CAB_full_instrument_CO, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v2_non_CAB_full_instrument_CO <- list(coeftest(reg_v2_non_CAB_full_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v2_non_CAB_full_instrument_CO <- list(coeftest(reg_v2_non_CAB_full_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
  pvals.reg_v2_non_CAB_full_instrument_CO <- list(coeftest(reg_v2_non_CAB_full_instrument_CO, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
  
  #Tables
  #Table 1; baseline results: no Covid dummy
  stargazer(reg_v2_CAB_full_instrument_ND, reg_v2_non_CAB_full_instrument_ND, reg_v2_CAPB_instrument_ND, reg_v2_cyclicalcomponent_instrument_ND, reg_v2_CAB_OECD_instrument_ND, reg_v2_non_CAB_OECD_instrument_ND, t=list(unlist(tvals.reg_v2_CAB_full_instrument_ND), unlist(tvals.reg_v2_non_CAB_full_instrument_ND), unlist(tvals.reg_v2_CAPB_instrument_ND), unlist(tvals.reg_v2_cyclicalcomponent_instrument_ND), unlist(tvals.reg_v2_CAB_OECD_instrument_ND), unlist(tvals.reg_v2_non_CAB_OECD_instrument_ND)), se=list(unlist(ses.reg_v2_CAB_full_instrument_ND), unlist(ses.reg_v2_non_CAB_full_instrument_ND), unlist(ses.reg_v2_CAPB_instrument_ND), unlist(ses.reg_v2_cyclicalcomponent_instrument_ND), unlist(ses.reg_v2_CAB_OECD_instrument_ND), unlist(ses.reg_v2_non_CAB_OECD_instrument_ND)), p=list(unlist(pvals.reg_v2_CAB_full_instrument_ND), unlist(pvals.reg_v2_non_CAB_full_instrument_ND), unlist(pvals.reg_v2_CAPB_instrument_ND), unlist(pvals.reg_v2_cyclicalcomponent_instrument_ND), unlist(pvals.reg_v2_CAB_OECD_instrument_ND), unlist(pvals.reg_v2_non_CAB_OECD_instrument_ND)))
  
  #Table 2; with Covid dummy
  stargazer(reg_v2_CAB_full_instrument_CO, reg_v2_non_CAB_full_instrument_CO, reg_v2_CAPB_instrument_CO, reg_v2_cyclicalcomponent_instrument_CO, reg_v2_CAB_OECD_instrument_CO, reg_v2_non_CAB_OECD_instrument_CO, t=list(unlist(tvals.reg_v2_CAB_full_instrument_CO), unlist(tvals.reg_v2_non_CAB_full_instrument_CO), unlist(tvals.reg_v2_CAPB_instrument_CO), unlist(tvals.reg_v2_cyclicalcomponent_instrument_CO), unlist(tvals.reg_v2_CAB_OECD_instrument_CO), unlist(tvals.reg_v2_non_CAB_OECD_instrument_CO)), se=list(unlist(ses.reg_v2_CAB_full_instrument_CO), unlist(ses.reg_v2_non_CAB_full_instrument_CO), unlist(ses.reg_v2_CAPB_instrument_CO), unlist(ses.reg_v2_cyclicalcomponent_instrument_CO), unlist(ses.reg_v2_CAB_OECD_instrument_CO), unlist(ses.reg_v2_non_CAB_OECD_instrument_CO)), p=list(unlist(pvals.reg_v2_CAB_full_instrument_CO), unlist(pvals.reg_v2_non_CAB_full_instrument_CO), unlist(pvals.reg_v2_CAPB_instrument_CO), unlist(pvals.reg_v2_cyclicalcomponent_instrument_CO), unlist(pvals.reg_v2_CAB_OECD_instrument_CO), unlist(pvals.reg_v2_non_CAB_OECD_instrument_CO)))
  
  #Table 3; Euro and financial crisis dummies included
  stargazer(reg_v2_CAB_full_instrument, reg_v2_non_CAB_full_instrument, reg_v2_CAPB_instrument, reg_v2_cyclicalcomponent_instrument, reg_v2_CAB_OECD_instrument, reg_v2_non_CAB_OECD_instrument, t=list(unlist(tvals.reg_v2_CAB_full_instrument), unlist(tvals.reg_v2_non_CAB_full_instrument), unlist(tvals.reg_v2_CAPB_instrument), unlist(tvals.reg_v2_cyclicalcomponent_instrument), unlist(tvals.reg_v2_CAB_OECD_instrument), unlist(tvals.reg_v2_non_CAB_OECD_instrument)), se=list(unlist(ses.reg_v2_CAB_full_instrument), unlist(ses.reg_v2_non_CAB_full_instrument), unlist(ses.reg_v2_CAPB_instrument), unlist(ses.reg_v2_cyclicalcomponent_instrument), unlist(ses.reg_v2_CAB_OECD_instrument), unlist(ses.reg_v2_non_CAB_OECD_instrument)), p=list(unlist(pvals.reg_v2_CAB_full_instrument), unlist(pvals.reg_v2_non_CAB_full_instrument), unlist(pvals.reg_v2_CAPB_instrument), unlist(pvals.reg_v2_cyclicalcomponent_instrument), unlist(pvals.reg_v2_CAB_OECD_instrument), unlist(pvals.reg_v2_non_CAB_OECD_instrument)))

  #Table for the appendix: GMM
  stargazer(reg_v2_CAB_full_instrument_GMM, reg_v2_non_CAB_full_instrument_CO, reg_v2_CAPB_instrument_GMM, reg_v2_cyclicalcomponent_instrument_CO, reg_v2_CAB_OECD_instrument_GMM, reg_v2_non_CAB_OECD_instrument_CO, t=list(unlist(tvals.reg_v2_CAB_full_instrument_GMM), unlist(tvals.reg_v2_non_CAB_full_instrument_CO), unlist(tvals.reg_v2_CAPB_instrument_GMM), unlist(tvals.reg_v2_cyclicalcomponent_instrument_CO), unlist(tvals.reg_v2_CAB_OECD_instrument_GMM), unlist(tvals.reg_v2_non_CAB_OECD_instrument_CO)), se=list(unlist(ses.reg_v2_CAB_full_instrument_GMM), unlist(ses.reg_v2_non_CAB_full_instrument_CO), unlist(ses.reg_v2_CAPB_instrument_GMM), unlist(ses.reg_v2_cyclicalcomponent_instrument_CO), unlist(ses.reg_v2_CAB_OECD_instrument_GMM), unlist(ses.reg_v2_non_CAB_OECD_instrument_CO)), p=list(unlist(pvals.reg_v2_CAB_full_instrument_GMM), unlist(pvals.reg_v2_non_CAB_full_instrument_CO), unlist(pvals.reg_v2_CAPB_instrument_GMM), unlist(pvals.reg_v2_cyclicalcomponent_instrument_CO), unlist(pvals.reg_v2_CAB_OECD_instrument_GMM), unlist(pvals.reg_v2_non_CAB_OECD_instrument_CO)))
  
  #Table robustness check: other instrument
  stargazer(reg_v2_CAB_full_instrument_CO_alt_int, reg_v2_non_CAB_full_instrument_CO_alt_int, reg_v2_CAPB_instrument_CO_alt_int, reg_v2_cyclicalcomponent_instrument_CO_alt_int, reg_v2_CAB_OECD_instrument_CO_alt_int, reg_v2_non_CAB_OECD_instrument_CO_alt_int, t=list(unlist(tvals.reg_v2_CAB_full_instrument_CO_alt_int), unlist(tvals.reg_v2_non_CAB_full_instrument_CO_alt_int), unlist(tvals.reg_v2_CAPB_instrument_CO_alt_int), unlist(tvals.reg_v2_cyclicalcomponent_instrument_CO_alt_int), unlist(tvals.reg_v2_CAB_OECD_instrument_CO_alt_int), unlist(tvals.reg_v2_non_CAB_OECD_instrument_CO_alt_int)), se=list(unlist(ses.reg_v2_CAB_full_instrument_CO_alt_int), unlist(ses.reg_v2_non_CAB_full_instrument_CO_alt_int), unlist(ses.reg_v2_CAPB_instrument_CO_alt_int), unlist(ses.reg_v2_cyclicalcomponent_instrument_CO_alt_int), unlist(ses.reg_v2_CAB_OECD_instrument_CO_alt_int), unlist(ses.reg_v2_non_CAB_OECD_instrument_CO_alt_int)), p=list(unlist(pvals.reg_v2_CAB_full_instrument_CO_alt_int), unlist(pvals.reg_v2_non_CAB_full_instrument_CO_alt_int), unlist(pvals.reg_v2_CAPB_instrument_CO_alt_int), unlist(pvals.reg_v2_cyclicalcomponent_instrument_CO_alt_int), unlist(pvals.reg_v2_CAB_OECD_instrument_CO_alt_int), unlist(pvals.reg_v2_non_CAB_OECD_instrument_CO_alt_int)))
  
  #Table robustness check: core vs. periphery
  stargazer(reg_v2_CAPB_instrument, reg_v2_cyclicalcomponent_instrument, reg_v2_CAPB_instrument_core, reg_v2_cyclicalcomponent_instrument_core, reg_v2_CAPB_instrument_peri, reg_v2_cyclicalcomponent_instrument_peri, t=list(unlist(tvals.reg_v2_CAPB_instrument), unlist(tvals.reg_v2_cyclicalcomponent_instrument), unlist(tvals.reg_v2_CAPB_instrument_core), unlist(tvals.reg_v2_cyclicalcomponent_instrument_core), unlist(tvals.reg_v2_CAPB_instrument_peri), unlist(tvals.reg_v2_cyclicalcomponent_instrument_peri)), se=list(unlist(ses.reg_v2_CAPB_instrument), unlist(ses.reg_v2_cyclicalcomponent_instrument), unlist(ses.reg_v2_CAPB_instrument_core), unlist(ses.reg_v2_cyclicalcomponent_instrument_core), unlist(ses.reg_v2_CAPB_instrument_peri), unlist(ses.reg_v2_cyclicalcomponent_instrument_peri)), p=list(unlist(pvals.reg_v2_CAPB_instrument), unlist(pvals.reg_v2_cyclicalcomponent_instrument), unlist(pvals.reg_v2_CAPB_instrument_core), unlist(pvals.reg_v2_cyclicalcomponent_instrument_core), unlist(pvals.reg_v2_CAPB_instrument_peri), unlist(pvals.reg_v2_cyclicalcomponent_instrument_peri)))
  
  #Table robustness check: 1995-2019, no Covid crisis dummy
  stargazer(reg_v2_CAB_full_instrument_no_covid, reg_v2_non_CAB_full_instrument_no_covid, reg_v2_CAPB_instrument_no_covid, reg_v2_cyclicalcomponent_instrument_no_covid, reg_v2_CAB_OECD_instrument_no_covid, reg_v2_non_CAB_OECD_instrument_no_covid, t=list(unlist(tvals.reg_v2_CAB_full_instrument_no_covid), unlist(tvals.reg_v2_non_CAB_full_instrument_no_covid), unlist(tvals.reg_v2_CAPB_instrument_no_covid), unlist(tvals.reg_v2_cyclicalcomponent_instrument_no_covid), unlist(tvals.reg_v2_CAB_OECD_instrument_no_covid), unlist(tvals.reg_v2_non_CAB_OECD_instrument_no_covid)), se=list(unlist(ses.reg_v2_CAB_full_instrument_no_covid), unlist(ses.reg_v2_non_CAB_full_instrument_no_covid), unlist(ses.reg_v2_CAPB_instrument_no_covid), unlist(ses.reg_v2_cyclicalcomponent_instrument_no_covid), unlist(ses.reg_v2_CAB_OECD_instrument_no_covid), unlist(ses.reg_v2_non_CAB_OECD_instrument_no_covid)), p=list(unlist(pvals.reg_v2_CAB_full_instrument_no_covid), unlist(pvals.reg_v2_non_CAB_full_instrument_no_covid), unlist(pvals.reg_v2_CAPB_instrument_no_covid), unlist(pvals.reg_v2_cyclicalcomponent_instrument_no_covid), unlist(pvals.reg_v2_CAB_OECD_instrument_no_covid), unlist(pvals.reg_v2_non_CAB_OECD_instrument_no_covid)))