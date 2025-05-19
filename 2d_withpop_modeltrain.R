library(mgcv)
library(tidyverse)
library(MASS)

folder <- "C:/Users/esthe/OneDrive - Nanyang Technological University/TH_extr_weat"
folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)
set.seed(123)

#import data#
df <- read.csv("./data/finaldata.csv")
df_filt <- filter(df, Year<2020)
df_filt$Province <- as.factor(df_filt$Province)

#data check#
var(df$DF)
mean(df$DF)
hist(df$DF, breaks=100)

#looptyloop
diseases <- colnames(df)[11:17]
provs <- unique(df$Province)

gam_aic <- matrix(data=NA, nrow=1, ncol=length(diseases))
glm_aic <- matrix(data=NA, nrow=1, ncol=length(diseases))
colnames(glm_aic) <- diseases
colnames(gam_aic) <- diseases

load("./out/lags.Rdata")
df_filt["rh_mean"] <- unlist(df_filt["rh_mean"])
for(dis in diseases){
  store_rr_tp <- list()
  store_rr_heat <- list()
  store_paf_tp <- list()
  store_paf_heat <- list()
    
  heatlag <- grep(colnames(df_filt), pattern="extremeHeat_", value=T)
  heatlagglm <- heatlag %>% paste0(collapse = "+")
  heatlag <- paste0("s(", heatlag %>% paste0(collapse = ") + s("), ")")
  spilag <- grep(colnames(df_filt), pattern="spi_", value=T)
  spilagglm <- spilag %>% paste0(collapse = "+")
  spilag <- paste0("s(", spilag %>% paste0(collapse = ") + s("), ")")
  
  hlag <- strsplit(lags[[dis]], "_")[[1]][1]
  slag <- strsplit(lags[[dis]], "_")[[1]][2]
  
  heatlag <- paste0("s(extremeHeat) +", heatlag)
  heatlag <- strsplit(heatlag, "\\+")[[1]][1:hlag]
  heatlag <- paste(heatlag, collapse="+")
  spilag <- paste0("s(spi) +", spilag)
  spilag <- strsplit(spilag, "\\+")[[1]][1:slag]
  spilag <- paste(spilag, collapse="+")

  gamtest <- gam(as.formula(paste0(dis, "~offset(log(worldPop)) + s(rh_mean, bs=\"re\") + s(Month, bs=\"re\") + s(Province, bs=\"re\") + s(worldPopDens)", "+", heatlag, "+", spilag)),
                 data=df_filt, family=nb(), method="REML")
  glmtest <- glm.nb(as.formula(paste0(dis, "~rh_mean + offset(log(worldPop))", "+ Month + Province + worldPopDens +", heatlagglm, "+", spilagglm)),
                        data=df_filt)
  
    gam_aic[1, dis] <- gamtest$aic
    glm_aic[1, dis] <- glmtest$aic
  
    # sink(file=paste0("./out/summary/", dis, "_summary.txt"), type="output")
    # print(summary(store_model[[dis]]))
    # sink()
    
    store_model <- gamtest
    store_glm <- glmtest
    
    #create new data to predict with
    #calculate y by predict(x, 0, mean, mean, mean), results - IRR over number of days of exp
    
    for(prov in provs){
      df_prov <- filter(df_filt, Province==prov)
      heatxcutoff <- max(df_prov$extremeHeat)
      baseline <- list(spi=0, spi_1=mean(df_prov$spi_1), spi_2=mean(df_prov$spi_2),  spi_3=mean(df_prov$spi_3), 
                       extremeHeat=0, extremeHeat_1=mean(df_prov$extremeHeat_1), extremeHeat_2=mean(df_prov$extremeHeat_2), extremeHeat_3=mean(df_prov$extremeHeat_3),
                       mean(df_prov[[paste0(dis, "_1")]]), mean(df_prov[[paste0(dis, "_2")]]), mean(df_prov[[paste0(dis, "_3")]]),
                       rh_mean = mean(df_prov$rh_mean), 
                       worldPop=mean(df_prov$worldPop),  worldPopDens=mean(df_prov$worldPopDens), Province=prov, Month=1)
      names(baseline)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
      
      tpexp <- list(spi=seq(-3, 3, 0.5), spi_1=rep(mean(df_prov$spi_1), 13), spi_2=rep(mean(df_prov$spi_2),13),  spi_3=rep(mean(df_prov$spi_3),13),
                    extremeHeat=rep(0, 13), extremeHeat_1=rep(mean(df_prov$extremeHeat_1),13), extremeHeat_2=rep(mean(df_prov$extremeHeat_2),13), extremeHeat_3=rep(mean(df_prov$extremeHeat_3),13),
                    rep(mean(df_prov[[paste0(dis, "_1")]]),13), rep(mean(df_prov[[paste0(dis, "_2")]]),13), rep(mean(df_prov[[paste0(dis, "_3")]]),13),
                    rh_mean = rep(mean(df_prov$rh_mean), 13), worldPop=rep(mean(df_prov$worldPop),13), worldPopDens=rep(mean(df_prov$worldPopDens),13), Province=rep(prov,13), Month=rep(1,13))
      names(tpexp)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
      
      pred0 <- predict.gam(gamtest, baseline, se.fit=T, type="link")
      baselinep <- c(exp(pred0$fit))
      
      if(heatxcutoff>=5){
        heatexp <- list(spi=rep(0,heatxcutoff), spi_1=rep(mean(df_prov$spi_1), heatxcutoff), spi_2=rep(mean(df_prov$spi_2),heatxcutoff),  spi_3=rep(mean(df_prov$spi_3),heatxcutoff),
                        extremeHeat=1:heatxcutoff, extremeHeat_1=rep(mean(df_prov$extremeHeat_1),heatxcutoff), extremeHeat_2=rep(mean(df_prov$extremeHeat_2),heatxcutoff), extremeHeat_3=rep(mean(df_prov$extremeHeat_3),heatxcutoff),
                        rep(mean(df_prov[[paste0(dis, "_1")]]),heatxcutoff), rep(mean(df_prov[[paste0(dis, "_2")]]),heatxcutoff), rep(mean(df_prov[[paste0(dis, "_3")]]),heatxcutoff),
                        rh_mean = rep(mean(df_prov$rh_mean), heatxcutoff), worldPop=rep(mean(df_prov$worldPop),heatxcutoff), worldPopDens=rep(mean(df_prov$worldPopDens),heatxcutoff), 
                        Province=rep(prov,heatxcutoff), Month=rep(1,heatxcutoff))
        names(heatexp)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
        
        predheat <- predict.gam(gamtest, heatexp, se.fit=T, type="link")
        
        mean_heat <- c(exp(predheat$fit))
        mean_heat[mean_heat<0] <- 0
        
        irr_log <- log(mean_heat) - log(baselinep)
        irr_hi <- exp(irr_log + 1.96 * sqrt(1/mean_heat + 1/baselinep)) 
        irr_me <- exp(irr_log)
        irr_lo <- exp(irr_log - 1.96 * sqrt(1/mean_heat + 1/baselinep))
        
        store_rr_heat[[prov]] <- list("mean"= irr_me, "lo"=irr_lo, "hi"=irr_hi)
        
        #worldPopulation attributable fraction: (exposed - baseline)/exposed
        paf_me <- 1 - baselinep/mean_heat
        paf_me[which(paf_me > 1)] <- 1
        paf_hi <- paf_me + 1.96 * sqrt( mean_heat^2 / baselinep^3 + mean_heat / baselinep^2)
        paf_hi[which(paf_hi > 1)] <- 1
        paf_lo <- paf_me - 1.96 * sqrt( mean_heat^2 / baselinep^3 + mean_heat / baselinep^2)
        paf_lo[which(paf_lo > 1)] <- 1
        
        store_paf_heat[[prov]] <- list("mean"=paf_me, "lo"=paf_lo, "hi"=paf_hi)
      }

      predtp <- predict.gam(gamtest, tpexp, se.fit=T, type="link")
    
      
      mean_tp <- c(exp(predtp$fit))
      mean_tp[mean_tp<0] <- 0
      
      ##delta method for boundaries: estimate variance of IRR/paf
      irr_log <- log(mean_tp) - log(baselinep)
      irr_hi <- exp(irr_log + 1.96 * sqrt(1/mean_tp + 1/baselinep)) 
      irr_me <- exp(irr_log)
      irr_lo <- exp(irr_log - 1.96 * sqrt(1/mean_tp + 1/baselinep)) 
      
      store_rr_tp[[prov]] <- list("mean"= irr_me, "lo"=irr_lo, "hi"=irr_hi)
      
      #worldPopulation attributable fraction: (exposed - baseline)/exposed
      paf_me <- 1 - baselinep/mean_tp
      paf_me[which(paf_me > 1)] <- 1
      paf_hi <- paf_me + 1.96 * sqrt( mean_tp^2 / baselinep^3 + mean_tp / baselinep^2)
      paf_hi[which(paf_hi > 1)] <- 1
      paf_lo <- paf_me - 1.96 * sqrt( mean_tp^2 / baselinep^3 + mean_tp / baselinep^2)
      paf_lo[which(paf_lo > 1)] <- 1
      
      store_paf_tp[[prov]] <- list("mean"=paf_me, "lo"=paf_lo, "hi"=paf_hi)
    }
    
    save(list=c("store_model", "store_glm", "store_rr_heat", "store_rr_tp", "store_paf_tp", "store_paf_heat"), file=paste0("./out/withpop/", dis, "_out.Rdata"))

  }



