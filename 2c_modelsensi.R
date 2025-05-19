library(mgcv)
library(tidyverse)
library(MASS)

folder <- "/Users/esthe/OneDrive - Nanyang Technological University/TH_extr_weat"
folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)
set.seed(123)


#import data#
df <- read.csv("./data/finaldata.csv")
# df <- filter(df, Year < 2017) #leave 2020-2021 for validation

###########################################
### Sensitivity analysis 1: removing RH ###
###########################################

#looptyloop
diseases <- colnames(df)[11:17]
provs <- unique(df$Province)

removed_provs <- list()
noRH_gam_aic <- matrix(data=NA, nrow=1, ncol=length(diseases))
colnames(noRH_gam_aic) <- diseases

spline_gam_aic <- matrix(data=NA, nrow=1, ncol=length(diseases))
colnames(spline_gam_aic) <- diseases

load("./out/lags.Rdata")

for(dis in diseases){
    # #remove province from analysis if more than 70% of the monthly counts are 0
    # if( length(which(df_filt[dis]==0)) / nrow(df_filt) > 0.7 ){
    #   problem <- c(problem, prov)
    #   next
    # }
    # 
    # #check if there are enough observations for each level - trim axis
    # counts <- df_filt[c("extremeHeat", dis)] %>%
    #   group_by(extremeHeat) %>%
    #   summarise(n=n())
    # heatxcutoff <- 0
    # for(i in 1:nrow(counts)){
    #   if(counts[i, "n"] < 4) break
    #   heatxcutoff <- heatxcutoff+1
    # }
    # 
    # if(heatxcutoff<5) {
    #   problem <- c(problem, prov)
    #   next }
    # 
    
    # #max df exceeding unique covariate combinations
    # k_heat <- 10
    # nfact_heat <- length(levels(as.factor(df_filt$extremeHeat)))
    # if (nfact_heat < 10){
    #   k_heat <- nfact_heat
    # }

    heatlag <- grep(colnames(df_filt), pattern="extremeHeat_", value=T)
    heatlag <- paste0("s(", heatlag %>% paste0(collapse = ") + s("), ")")
    spilag <- grep(colnames(df_filt), pattern="spi_", value=T)
    spilag <- paste0("s(", spilag %>% paste0(collapse = ") + s("), ")")
    
    hlag <- strsplit(lags[[dis]], "_")[[1]][1]
    slag <- strsplit(lags[[dis]], "_")[[1]][2]
    
    heatlag <- paste0("s(extremeHeat) +", heatlag)
    heatlag <- strsplit(heatlag, "\\+")[[1]][1:hlag]
    heatlag <- paste(heatlag, collapse="+")
    spilag <- paste0("s(spi) +", spilag)
    spilag <- strsplit(spilag, "\\+")[[1]][1:slag]
    spilag <- paste(spilag, collapse="+")
    
    gamtest <- gam(as.formula(paste0(dis, "~ offset(log(Pop))", "+", heatlag, "+", spilag)),
                   data=df_filt, family=nb(), method="REML")
    
    noRH_gam_aic[1, dis] <- gamtest$aic
}

write.csv(noRH_gam_aic, "./out/tables/noRH_gam_aic.csv")

################################################
### Sensitivity analysis 2: different spline ###
################################################

for(dis in diseases){
    # #remove province from analysis if more than 70% of the monthly counts are 0
    # if( length(which(df_filt[dis]==0)) / nrow(df_filt) > 0.7 ){
    #   problem <- c(problem, prov)
    #   next
    # }
    # 
    # #check if there are enough observations for each level - trim axis
    # counts <- df_filt[c("extremeHeat", dis)] %>%
    #   group_by(extremeHeat) %>%
    #   summarise(n=n())
    # heatxcutoff <- 0
    # for(i in 1:nrow(counts)){
    #   if(counts[i, "n"] < 4) break
    #   heatxcutoff <- heatxcutoff+1
    # }
    # 
    # if(heatxcutoff<5) {
    #   problem <- c(problem, prov)
    #   next }
    # 
    # 
    # #max df exceeding unique covariate combinations
    # k_heat <- 10
    # nfact_heat <- length(levels(as.factor(df_filt$extremeHeat)))
    # if (nfact_heat < 10){
    #   k_heat <- nfact_heat
    # }
    
    spl <- "cr"
    
    heatlag <- grep(colnames(df_filt), pattern="extremeHeat_", value=T)
    heatlagglm <- heatlag %>% paste0(collapse = "+")
    heatlag <- paste0("s(", heatlag %>% paste0(collapse = ", bs=spl) + s("), ", bs=spl)")
    spilag <- grep(colnames(df_filt), pattern="spi_", value=T)
    spilagglm <- spilag %>% paste0(collapse = "+")
    spilag <- paste0("s(", spilag %>% paste0(collapse = ",bs=spl) + s("), ",bs=spl)")
    
    hlag <- strsplit(lags[[dis]], "_")[[1]][1]
    slag <- strsplit(lags[[dis]], "_")[[1]][2]
    
    heatlag <- paste0("s(extremeHeat, bs=spl) +", heatlag)
    heatlag <- strsplit(heatlag, "\\+")[[1]][1:hlag]
    heatlag <- paste(heatlag, collapse="+")
    spilag <- paste0("s(spi, bs=spl) +", spilag)
    spilag <- strsplit(spilag, "\\+")[[1]][1:slag]
    spilag <- paste(spilag, collapse="+")
    
    gamtest <- gam(as.formula(paste0(dis, "~rh_mean + offset(log(Pop)) + s(Month, bs=\"cr\") + s(Province, bs=\"re\") + s(Pop)", "+", heatlag, "+", spilag)),
                   data=df_filt, family=nb(), method="REML")
    
    spline_gam_aic[1, dis] <- gamtest$aic
    
}

# write.csv(spline_gam_aic, "./out/tables/spline_gam_aic.csv")
# 
# 
# #################################################
# ### Sensitivity analysis 3: adding popdensity ###
# #################################################
# 
# load("./out/lags.Rdata")
# 
# for(dis in diseases){
#   store_rr_tp <- list()
#   store_rr_heat <- list()
#   store_paf_tp <- list()
#   store_paf_heat <- list()
#     
#     heatlag <- grep(colnames(df_filt), pattern="extremeHeat_", value=T)
#     heatlagglm <- heatlag %>% paste0(collapse = "+")
#     heatlag <- paste0("s(", heatlag %>% paste0(collapse = ") + s("), ")")
#     spilag <- grep(colnames(df_filt), pattern="spi_", value=T)
#     spilagglm <- spilag %>% paste0(collapse = "+")
#     spilag <- paste0("s(", spilag %>% paste0(collapse = ") + s("), ")")
#     
#     hlag <- strsplit(lags[[dis]], "_")[[1]][1]
#     slag <- strsplit(lags[[dis]], "_")[[1]][2]
#     
#     heatlag <- paste0("s(extremeHeat) +", heatlag)
#     heatlag <- strsplit(heatlag, "\\+")[[1]][1:hlag]
#     heatlag <- paste(heatlag, collapse="+")
#     spilag <- paste0("s(spi) +", spilag)
#     spilag <- strsplit(spilag, "\\+")[[1]][1:slag]
#     spilag <- paste(spilag, collapse="+")
#     
#     gamtest <- gam(as.formula(paste0(dis, "~s(rh_mean) + offset(log(Pop)) + s(Month, bs=\"cr\") + s(Province, bs=\"re\") + s(Pop) + Popdens + ", heatlag, "+", spilag)),
#                    data=df_filt, family=nb(), method="REML")
#     
#     #gam_aic[prov, dis] <- gamtest$aic
# 
#     
#     
#     #sink(file=paste0("./out/summary/", dis, "_summary.txt"), type="output")
#     #print(summary(store_model[[prov]]))
#     #sink()
#     
#     store_model <- gamtest
#     
#     #create new data to predict with
#     #calculate y by predict(x, 0, mean, mean, mean), results - IRR over number of days of exp
#     
#     for(prov in provs){
#       df_prov <- filter(df_filt, Province==prov)
#       heatxcutoff <- max(df_prov$extremeHeat)
#       baseline <- list(spi=0, spi_1=mean(df_prov$spi_1), spi_2=mean(df_prov$spi_2),  spi_3=mean(df_prov$spi_3), 
#                        extremeHeat=0, extremeHeat_1=mean(df_prov$extremeHeat_1), extremeHeat_2=mean(df_prov$extremeHeat_2), extremeHeat_3=mean(df_prov$extremeHeat_3),
#                        mean(df_prov[[paste0(dis, "_1")]]), mean(df_prov[[paste0(dis, "_2")]]), mean(df_prov[[paste0(dis, "_3")]]),
#                        rh_mean = mean(df_prov$rh_mean), 
#                        Pop=mean(df_prov$Pop), Province=prov, Month=1, Popdens=mean(df_prov$Popdens))
#       names(baseline)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
#       
#       tpexp <- list(spi=seq(-3, 3, 0.5), spi_1=rep(mean(df_prov$spi_1), 13), spi_2=rep(mean(df_prov$spi_2),13),  spi_3=rep(mean(df_prov$spi_3),13),
#                     extremeHeat=rep(0, 13), extremeHeat_1=rep(mean(df_prov$extremeHeat_1),13), extremeHeat_2=rep(mean(df_prov$extremeHeat_2),13), extremeHeat_3=rep(mean(df_prov$extremeHeat_3),13),
#                     rep(mean(df_prov[[paste0(dis, "_1")]]),13), rep(mean(df_prov[[paste0(dis, "_2")]]),13), rep(mean(df_prov[[paste0(dis, "_3")]]),13),
#                     rh_mean = rep(mean(df_prov$rh_mean), 13), Pop=rep(mean(df_prov$Pop),13),Province=rep(prov,13), Month=rep(1,13), Popdens=rep(mean(df_prov$Popdens), 13))
#       names(tpexp)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
#       
#       pred0 <- predict.gam(gamtest, baseline, se.fit=T, type="link")
#       
#       if(heatxcutoff>=5){
#         heatexp <- list(spi=rep(0,heatxcutoff), spi_1=rep(mean(df_prov$spi_1), heatxcutoff), spi_2=rep(mean(df_prov$spi_2),heatxcutoff),  spi_3=rep(mean(df_prov$spi_3),heatxcutoff),
#                         extremeHeat=1:heatxcutoff, extremeHeat_1=rep(mean(df_prov$extremeHeat_1),heatxcutoff), extremeHeat_2=rep(mean(df_prov$extremeHeat_2),heatxcutoff), extremeHeat_3=rep(mean(df_prov$extremeHeat_3),heatxcutoff),
#                         rep(mean(df_prov[[paste0(dis, "_1")]]),heatxcutoff), rep(mean(df_prov[[paste0(dis, "_2")]]),heatxcutoff), rep(mean(df_prov[[paste0(dis, "_3")]]),heatxcutoff),
#                         rh_mean = rep(mean(df_prov$rh_mean), heatxcutoff), Pop=rep(mean(df_prov$Pop),heatxcutoff),Province=rep(prov,heatxcutoff), Month=rep(1,heatxcutoff), Popdens=rep(mean(df_prov$Popdens), heatxcutoff))
#         names(heatexp)[9:11] <- paste0(dis, c("_1", "_2", "_3"))
#         
#         predheat <- predict.gam(gamtest, heatexp, se.fit=T, type="link")
#         
#         mean_heat <- c(exp(predheat$fit))
#         mean_heat[mean_heat<0] <- 0
#         lo_heat <- c(exp(predheat$fit - (1.96 * predheat$se.fit)))
#         lo_heat[lo_heat<0] <- 0
#         hi_heat <- c(exp(predheat$fit + (1.96* predheat$se.fit)))
#         hi_heat[hi_heat<0] <- 0
#         
#         store_rr_heat[[prov]] <- list("mean"=mean_heat/baselinep, "lo"=lo_heat/baselinep, "hi"=hi_heat/baselinep)
#         
#         #population attributable fraction: (exposed - baseline)/exposed
#         store_paf_heat[[prov]] <- list("mean"=(mean_heat-baselinep)/mean_heat, "lo"=(lo_heat-baselinep)/lo_heat, "hi"=(hi_heat-baselinep)/hi_heat)
#       }
#       
#       predtp <- predict.gam(gamtest, tpexp, se.fit=T, type="link")
#       
#       baselinep <- c(exp(pred0$fit))
#       
#       mean_tp <- c(exp(predtp$fit))
#       mean_tp[mean_tp<0] <- 0
#       lo_tp <- c(exp(predtp$fit - (1.96 * predtp$se.fit)))
#       lo_tp[lo_tp<0] <- 0
#       hi_tp <- c(exp(predtp$fit + (1.96* predtp$se.fit)))
#       hi_tp[hi_tp<0] <- 0
#       
#       store_rr_tp[[prov]] <- list("mean"= mean_tp/baselinep, "lo"=lo_tp/baselinep, "hi"=hi_tp/baselinep)
#       
#       #population attributable fraction: (exposed - baseline)/exposed
#       store_paf_tp[[prov]] <- list("mean"= (mean_tp-baselinep)/mean_tp, "lo"=(lo_tp-baselinep)/lo_tp, "hi"=(hi_tp-baselinep)/hi_tp)
#       
#     }
#     
#     save(list=c("store_model", "store_glm", "store_rr_heat", "store_rr_tp", "store_paf_tp", "store_paf_heat"), file=paste0("./out/withpop/", dis, "_out.Rdata"))
# }
