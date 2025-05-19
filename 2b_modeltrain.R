library(mgcv)
library(tidyverse)
library(MASS)
library(overdisp)

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

#overdispersion test
sink(file="./out/overdisp.txt")
for(i in 1:7){
  print(overdisp(df_filt, dependent.position=10+i, predictor.position=c(5,9,10,12,18:20, 20+i, 30, 31, 31+i, 41, 42, 42+i)))
}
sink()

test <- overdisp(df_filt, dependent.position=10+i, predictor.position=c(5,9,10,12,18:20, 20+i, 30, 31, 31+i, 41, 42, 42+i))

for(dis in diseases){
  store_rr_tp <- list()
  store_rr_heat <- list()
  store_paf_tp <- list()
  store_paf_heat <- list()
    # #max df exceeding unique covariate combinations
    # k_heat <- 10
    # nfact_heat <- length(levels(as.factor(df_filt$extremeHeat)))
    # if (nfact_heat < 10){
    #   k_heat <- nfact_heat
    # }
    
    # ar <- grep(colnames(df_filt), pattern=paste0(dis, "_"), value=T)
    # arglm <- ar %>% paste0(collapse = "+")
    
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

    gamtest <- gam(as.formula(paste0(dis, "~offset(log(Pop)) + s(rh_mean) + s(Month) + s(Province, bs=\"re\") + s(Popdens)", "+", heatlag, "+", spilag)),
                   data=df_filt, family=nb(), method="REML")
    glmtest <- glm.nb(as.formula(paste0(dis, "~rh_mean + offset(log(Pop))", "+ Month + Province + Popdens +", heatlagglm, "+", spilagglm)),
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
                       Pop=mean(df_prov$Pop),  Popdens=mean(df_prov$Popdens), Province=prov, Month=1)
      names(baseline)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
      
      tpexp <- list(spi=seq(-3, 3, 0.5), spi_1=rep(mean(df_prov$spi_1), 13), spi_2=rep(mean(df_prov$spi_2),13),  spi_3=rep(mean(df_prov$spi_3),13),
                    extremeHeat=rep(0, 13), extremeHeat_1=rep(mean(df_prov$extremeHeat_1),13), extremeHeat_2=rep(mean(df_prov$extremeHeat_2),13), extremeHeat_3=rep(mean(df_prov$extremeHeat_3),13),
                    rep(mean(df_prov[[paste0(dis, "_1")]]),13), rep(mean(df_prov[[paste0(dis, "_2")]]),13), rep(mean(df_prov[[paste0(dis, "_3")]]),13),
                    rh_mean = rep(mean(df_prov$rh_mean), 13), Pop=rep(mean(df_prov$Pop),13), Popdens=rep(mean(df_prov$Popdens),13), Province=rep(prov,13), Month=rep(1,13))
      names(tpexp)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
      
      pred0 <- predict.gam(gamtest, baseline, se.fit=T, type="link")
      baselinep <- c(exp(pred0$fit))
      
      if(heatxcutoff>=5){
        heatexp <- list(spi=rep(0,heatxcutoff), spi_1=rep(mean(df_prov$spi_1), heatxcutoff), spi_2=rep(mean(df_prov$spi_2),heatxcutoff),  spi_3=rep(mean(df_prov$spi_3),heatxcutoff),
                        extremeHeat=1:heatxcutoff, extremeHeat_1=rep(mean(df_prov$extremeHeat_1),heatxcutoff), extremeHeat_2=rep(mean(df_prov$extremeHeat_2),heatxcutoff), extremeHeat_3=rep(mean(df_prov$extremeHeat_3),heatxcutoff),
                        rep(mean(df_prov[[paste0(dis, "_1")]]),heatxcutoff), rep(mean(df_prov[[paste0(dis, "_2")]]),heatxcutoff), rep(mean(df_prov[[paste0(dis, "_3")]]),heatxcutoff),
                        rh_mean = rep(mean(df_prov$rh_mean), heatxcutoff), Pop=rep(mean(df_prov$Pop),heatxcutoff), Popdens=rep(mean(df_prov$Popdens),heatxcutoff), 
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
        
        #population attributable fraction: (exposed - baseline)/exposed
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
      irr_hi <- exp(irr_log + 1.96 * sqrt(1/mean_tp^2 + 1/baselinep^2)) 
      irr_me <- exp(irr_log)
      irr_lo <- exp(irr_log - 1.96 * sqrt(1/mean_tp^2 + 1/baselinep^2)) 
      
      store_rr_tp[[prov]] <- list("mean"= irr_me, "lo"=irr_lo, "hi"=irr_hi)
      
      #population attributable fraction: (exposed - baseline)/exposed
      paf_me <- 1 - baselinep/mean_tp
      paf_me[which(paf_me > 1)] <- 1
      paf_hi <- paf_me + 1.96 * sqrt( 1/baselinep^2 + mean_tp^2/baselinep^4 )
      paf_hi[which(paf_hi > 1)] <- 1
      paf_lo <- paf_me - 1.96 * sqrt( 1/baselinep^2 + mean_tp^2/baselinep^4 )
      paf_lo[which(paf_lo > 1)] <- 1
      
      store_paf_tp[[prov]] <- list("mean"=paf_me, "lo"=paf_lo, "hi"=paf_hi)
    }
    
    save(list=c("store_model", "store_glm", "store_rr_heat", "store_rr_tp", "store_paf_tp", "store_paf_heat"), file=paste0("./out/", dis, "_out.Rdata"))

  }

write.csv(gam_aic, "./out/tables/gam_aic.csv")
write.csv(glm_aic, "./out/tables/glm_aic.csv")


#compare AIC val of gam and glm

for(dis in 1:7){
    if(is.na(gam_aic[prov,dis])) next
    if(gam_aic[prov, dis] > glm_aic[prov,dis])
      print(paste(prov, dis))
  }

#remove DF - Sing Buri and INFLUENZA - Pathum Thani model (poor performance)
# load("./out/DF_out.Rdata")
# store_model[['Sing Buri']] <- NULL
# store_paf_heat[['Sing Buri']] <- NULL
# store_paf_tp[['Sing Buri']] <- NULL
# store_rr_heat[['Sing Buri']] <- NULL
# store_rr_tp[['Sing Buri']] <- NULL
# problem <- c(problem, "Sing Buri")
# save(list=c("store_model", "store_rr_heat", "store_rr_tp", "store_paf_tp", "store_paf_heat", "problem"), file=paste0("./out/DF_out.Rdata"))
# 
# load("./out/INFLUENZA_out.Rdata")
# store_model[['Pathum Thani']] <- NULL
# store_paf_heat[['Pathum Thani']] <- NULL
# store_paf_tp[['Pathum Thani']] <- NULL
# store_rr_heat[['Pathum Thani']] <- NULL
# store_rr_tp[['Pathum Thani']] <- NULL
# problem <- c(problem, "Pathum Thani")
# save(list=c("store_model", "store_rr_heat", "store_rr_tp", "store_paf_tp", "store_paf_heat", "problem"), file=paste0("./out/INFLUENZA_out.Rdata"))

##adjust paf values - -1 <= PAF <= 1
for(dis in diseases){
  load(paste0("./out/", dis, "_out.Rdata"))
  for(prov in names(store_paf_heat)){
    for(val in names(store_paf_heat[[prov]])){
      store_paf_heat[[prov]][[val]][store_paf_heat[[prov]][[val]] > 1] <- 1
      store_paf_heat[[prov]][[val]][store_paf_heat[[prov]][[val]] < -1] <- -1
      store_paf_tp[[prov]][[val]][store_paf_tp[[prov]][[val]] > 1] <- 1
      store_paf_tp[[prov]][[val]][store_paf_tp[[prov]][[val]] < -1] <- -1
    }
  }
  save(list=c("store_model", "store_rr_heat", "store_rr_tp", "store_paf_tp", "store_paf_heat"), file=paste0("./out/", dis, "_out.Rdata"))
}


##testing interaction terms
# gamtest <- gam(as.formula(paste0(dis, "~rh_mean + offset(log(Pop)) + s(Month, bs=\"cr\") + s(Province, bs=\"re\") + s(Pop)", "+", heatlag, "+", spilag)),
#                data=df_filt, family=nb(), method="REML")
# gamint <- gam(as.formula(paste0(dis, "~rh_mean + offset(log(Pop)) + s(Month, bs=\"cr\") + s(Province, bs=\"re\") + s(Pop)", "+", heatlag, "+", spilag,
#                                 " + ti(extremeHeat, spi) + ti(extremeHeat_1, spi_1) + ti(extremeHeat_2, spi_2) + ti(extremeHeat_3, spi_3)")),
#                data=df_filt, family=nb(), method="REML")
# 
# fitted <- predict.gam(gamtest, type="response")
# fittedint <- predict.gam(gamtest, type="response")



