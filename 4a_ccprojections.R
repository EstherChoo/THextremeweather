library(mgcv)
library(tidyverse)
library(MASS)

folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)

#load cleaned data - shld be province, year, month, extremeTP, extremeHeat, popln (use same as past)
ccmodels <- c("MIROC6", "CMCC", "IPSL")
for(ccmodel in ccmodels){
  finalccdata <- read.csv(paste0("./data/", ccmodel, "_finalccdata.csv"))
  finalccdata$rh_mean <- finalccdata$rh_mean / 100
  if(ncol(finalccdata)==8){
    finalccdata <- cbind(NA, finalccdata)
  }
  df <- read.csv("./data/finaldata.csv")
  
  #for loop
  modelfiles <- list.files("./out", pattern="_out.Rdata")
  provs <- unique(finalccdata$Province)
  ssps <- unique(finalccdata$SSP)
  
  ###combined effect of extreme heat, dry and wet weather
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    
    gamtest <- store_model
      
    for(prov in provs){
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])

          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi >= 1.5 | spi <= -1.5 | extremeHeat>0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)
      #currdata$Pop=pastdf$Pop[1]
      
      ccdata$worldPop <- ccdata$futPop #here
      ccdata$worldPopDens <- ccdata$Popdens
      
      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_allCCpred.csv"))
  }
  
  ###combined effect of extreme heat
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(extremeHeat>0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Pop, Popdens, rh_mean), mean)
  
  
      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_heatCCpred.csv"))
  }
  
  ###impact of extreme dry weather
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi <= -1.5)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)
      
      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_dryCCpred.csv"))
  }
  
  ###impact of extreme wet weather
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi >= 1.5)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)
      
      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_wetCCpred.csv"))
  }
  
  
  #SI: impact of extreme dry + hot weather only
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi <= -1.5 & extremeHeat>0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)
      currdata$Province = prov; currdata$Pop=df_filt$Pop[1]; currdata$rh_mean=df_filt$rh_mean[1]
      
      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_dryheatCCpred.csv"))
  }
  ###SI: impact of extreme wet + hot weather only
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi >= 1.5 & extremeHeat > 0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)

      
      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_wetheatCCpred.csv"))
  }
  
  ###SI: impact of extreme wet + no hot weather only
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi >= 1.5 & extremeHeat== 0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)

      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_wetnoheatCCpred.csv"))
  }
  
  ###SI: impact of extreme dry + no hot weather only
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi <= -1.5 & extremeHeat== 0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)

      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_drynoheatCCpred.csv"))
  }
  
  ###SI: impact of extreme dry + no hot weather only
  for(file in modelfiles){
    load(paste0("./out/", file))
    dis <- strsplit(file, "_")[[1]][1]
    ccdatapred <- matrix(nrow=0, ncol=7)
    gamtest <- store_model
    
    for(prov in provs){
      pastcases <- gamtest$fitted.values
      pastdf <- filter(df, Province==prov)
      
      ccdata <- matrix(nrow=0, ncol=21)
      for(ssp in ssps){
        allperiod <- matrix(nrow=0, ncol=21)
        for(period in unique(finalccdata$Period)){
          df_filt <- filter(finalccdata, Province==prov, SSP==ssp, Period==period) %>%
            arrange(Month)
          df_filt <- cbind(df_filt, spi_1=df_filt[c(12, 1:11), "spi"])
          df_filt <- cbind(df_filt, spi_2=df_filt[c(11:12, 1:10), "spi"])
          df_filt <- cbind(df_filt, spi_3=df_filt[c(10:12, 1:9), "spi"])
          
          df_filt <- cbind(df_filt, extremeHeat_1=df_filt[c(12, 1:11), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_2=df_filt[c(11:12, 1:10), "extremeHeat"])
          df_filt <- cbind(df_filt, extremeHeat_3=df_filt[c(10:12, 1:9), "extremeHeat"])
          
          allperiod <- rbind(allperiod, df_filt)
        }
        ccdata <- rbind(ccdata, allperiod)
      }
      ccdata <- ccdata %>% filter(spi >= -1.5 & spi <= 1.5 & extremeHeat== 0)
      
      #change monthly historical avg of each province spi_123 and extremeheat_123
      currdata <- pastdf %>% group_by(Month, Province) %>%
        summarise_at(vars(spi, spi_1, spi_2, spi_3, extremeHeat, extremeHeat_1, extremeHeat_2, extremeHeat_3, Popdens, Pop, rh_mean), mean)

      baseline <- predict.gam(gamtest, currdata, se.fit=T, type="response")
      pred <- predict.gam(gamtest, ccdata, se.fit=T, type="response")
      ccdata <- cbind(ccdata, "pred"=pred$fit)
      ccdata[which(ccdata$pred<0), "pred"] <- 0
      ccdata <- cbind(ccdata, "predHi"=pred$fit + (1.96* pred$se.fit))
      ccdata <- cbind(ccdata, "predLo"=pred$fit - (1.96* pred$se.fit))
      ccdata[which(ccdata$predLo<0), "predLo"] <- 0
      
      currpred <- currdata[1:2]
      currpred$baseline <- baseline$fit
      currpred[which(currpred$baseline<0), "baseline"] <- 0
      currpred <- cbind(currpred, "baselineHi"=baseline$fit + (1.96* baseline$se.fit))
      currpred <- cbind(currpred, "baselineLo"=baseline$fit - (1.96* baseline$se.fit))
      currpred[which(currpred$baselineLo<0), "baselineLo"] <- 0
      ccdata <- merge(ccdata, currpred, by=c("Province", "Month"))
      
      ccdata <- dplyr::select(ccdata, Province, Period, Month, SSP, pred, predHi, predLo, baseline, baselineHi, baselineLo)
      ccdatapred <- rbind(ccdatapred, ccdata)
    }
    write.csv(ccdatapred, file=paste0("./out/", dis, "_", ccmodel, "_heatnodrynowetCCpred.csv"))
  }
}

###calculate annual excess risk in country
for(type in c("all", "heat", "wet", "dry", "dryheat", "wetheat", "drynoheat", "wetnoheat", "heatnodrynowet")){
  for(model in c("MIROC6", "CMCC", "IPSL")){
    ccfiles <- list.files("./out/", pattern=paste0(model, "_", type, "CCpred.csv"))
    print(model)
    print(type)
    excessrisk <- list()
    for(file in ccfiles){ 
      proj <- read.csv(paste0("./out/", file))
      annualproj <- proj %>% 
        group_by(Period, SSP) %>%
        dplyr::summarise(across(pred:baselineLo, sum))
      
      dis <- strsplit(file, "_")[[1]][1]
      
      annualproj[which(annualproj$baseline == 0), "baseline"] <- 1
      annualproj$pred <- (annualproj$pred - annualproj$baseline) / annualproj$baseline *100
      annualproj$predHi <- (annualproj$predHi - annualproj$baseline)/ annualproj$baseline *100
      annualproj$predLo <- (annualproj$predLo - annualproj$baseline)/ annualproj$baseline *100
      annualproj[is.na(annualproj$predLo), "predLo"] <- 0
      annualproj <- as.data.frame(annualproj)
      
      excessrisk[[file]] <- annualproj
    }
    save(excessrisk, file=paste0("./out/", model, "_", type, "excessrisk.Rdata"))
  }
}

