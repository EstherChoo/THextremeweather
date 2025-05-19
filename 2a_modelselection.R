library(mgcv)
library(tidyverse)
library(MASS)
library(terra)
library(spdep)

folder <- "/Users/esthe/OneDrive - Nanyang Technological University/TH_extr_weat"
folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)

#import data#
df <- read.csv("./data/finaldata.csv")
df <- df_filt
df_filt$Province <- as.factor(df_filt$Province)

#looptyloop
diseases <- colnames(df)[11:17]
provs <- unique(df$Province)
modellags <- c()
for(i in 0:3){
  for(j in 0:3){
    modellags <- c(modellags, paste0(i, "_", j)) #first num is heat lags, second num is spi lags
  }
}

# th.shp <- shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
# 
# th.shp1 <- st_as_sf(th.shp)
# merged <- st_union(th.shp1[c(4,35),])
# merged <- st_sf(merged)
# th.shp1$geometry[35] <- merged
# th.shp1 <- th.shp1[-4,]
# th.shp <- st_as_sfc(th.shp1)
# 
# nb <- poly2nb(th.shp, row.names=th.shp1$ADM1_EN)
# names(nb) <- attr(nb, "region.id")

lags <- list()
gam_aic <- matrix(data=NA, nrow=length(diseases), ncol=16); rownames(gam_aic) <- diseases
gam_rmse <- matrix(data=NA, nrow=length(diseases), ncol=16); rownames(gam_rmse) <- diseases
colnames(gam_aic) <- modellags
colnames(gam_rmse) <- modellags

for(dis in diseases){
    #circumvent max df exceeding unique covariate combinations
    # k_tp <- 10; k_heat <- 10
    # nfact_heat <- length(levels(as.factor(df_filt$extremeHeat)))                         
    # if (nfact_heat < 10){
    #   k_heat <- nfact_heat
    # }
    
    heatlag0 <- grep(colnames(df_filt), pattern="extremeHeat_", value=T)
   # heatlag0 <- paste0("s(", heatlag0 %>% paste0(collapse = ", k=k_heat) + s("), ", k=k_heat)")
    heatlag0 <- paste0("s(", heatlag0 %>% paste0(collapse = ") + s("), ")")
    spilag0 <- grep(colnames(df_filt), pattern="spi_", value=T)
    spilag0 <- paste0("s(", spilag0 %>% paste0(collapse = ") + s("), ")")
    
    # ar <- grep(colnames(df_filt), pattern=paste0(dis, "_"), value=T)
    # arglm <- ar %>% paste0(collapse = "+")

    i <- 1
    for(hlag in 1:4){
      for(slag in 1:4){
        heatlag <- paste0("s(extremeHeat) +", heatlag0)
        heatlag <- strsplit(heatlag, "\\+")[[1]][1:hlag]
        heatlag <- paste(heatlag, collapse="+")
        spilag <- paste0("s(spi) +", spilag0)
        spilag <- strsplit(spilag, "\\+")[[1]][1:slag]
        spilag <- paste(spilag, collapse="+")
        
        gamtest <- gam(as.formula(paste0(dis, "~s(rh_mean) + offset(log(Pop)) + s(Month) + s(Province, bs=\"re\") + s(Pop)", "+", heatlag, "+", spilag)),
                       data=df_filt, family=nb(), method="REML")
        fitted <- predict.gam(gamtest, type="response")
        
        gam_rmse[dis, i] <- sqrt(mean(fitted - df_filt[[dis]])^2)
      
        gam_aic[dis, i] <- gamtest$aic
        i <- i + 1
      }
    }

# glmtest <- glmmPQL(DF ~ offset(log(Pop)) + DF_1, random=(~1|Province),  data=df_filt, family=gaussian())
# glmfitted <- predict(glmtest)
  
  print(dis)
  print(names(which.min(gam_aic[dis,])))
  print(names(which.min(gam_rmse[dis,])))
  print("=====================")
  
  lags[[dis]] <- names(which.min(gam_aic[dis,]))
}


write.csv(gam_rmse, paste0("./out/tables/lags/gam_rmse.csv"))
write.csv(gam_aic, paste0("./out/tables/lags/gam_aic.csv"))

save(lags, file= "./out/lags.Rdata")





