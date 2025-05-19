library(tidyverse)
library(readxl)
library(matrixStats)
library(SPEI)

folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)

#clim: year / month / province / ah / rh / extreme low n high rainfall (0/1) / extreme heat (0/1)

#load climate data
data <- read.csv(file="./data/TH_DisClim_month.csv")
data <- data %>% dplyr::select(!c(CONJUNC, DHF, DSS, CHIKV, HFM))
disease <- data[1:9]
disease$Month <- as.numeric(disease$Month)
disease <- arrange(disease, Year, Month, Province)
clim <- data %>% dplyr::select(c(1:3, ah_mean, rh_mean, t2m_mean, tp_mean))
clim$Month <- as.numeric(clim$Month)
clim <- arrange(clim, Year, Month, Province)

#add tb and cholera (alr cleaned)
chol <- read.csv(file="./data/cases/Monthly cases_CHOLERA.csv")
tb <- read.csv(file="./data/cases/Monthly cases_TB.csv")
chol$Month <- as.numeric(chol$Month)
chol <- arrange(chol, Year, Month, Province)
disease$CHOL <- chol$Cases
tb$Month <- as.numeric(tb$Month)
tb <- arrange(tb, Year, Month, Province)
disease$TB <- tb$Cases

#add lepto and melioid (had to clean from raw)
lepto <- read.csv(file="./data/cases/lepto_cases.csv")
melioid <- read.csv(file="./data/cases/melioid_cases.csv")
malaria <- read.csv(file="./data/cases/malaria_cases.csv")
lepto <- filter(lepto, Year < 2022)
melioid <- filter(melioid, Year < 2022)
malaria <- filter(malaria, Year < 2022)
lepto$Month <- as.numeric(lepto$Month)
lepto <- arrange(lepto, Year, Month, Province)
disease$LEPTO <- c(lepto$Cases)
melioid$Month <- as.numeric(melioid$Month)
melioid <- arrange(melioid, Year, Month, Province)
disease$MELIOID <- c(melioid$Cases)
malaria$Month <- as.numeric(malaria$Month)
malaria <- arrange(malaria, Year, Month, Province)
disease$MALARIA <- c(malaria$Cases)

write.csv(disease, file="./data/finalcases.csv")

#SPI
##this section here for rainmonthly vvvvvv
rain <- read.csv("./data/tp_mean.csv") #bo's era5 data is 6-hourly from jan 01 2003
raindaily <- matrix(NA, nrow=6940, ncol=78)
for(i in 2:78){
  raindaily[,i] <- colSums(matrix(as.numeric(unlist(rain[i]*6)), nrow=4))
}
raindaily <- as.data.frame(raindaily)
colnames(raindaily)[2:78] <- colnames(rain[2:78])
raindaily[1] <- seq.Date(from=as.Date("2003-01-01"), length.out=6940, by="day")

raindaily$Year <- format(raindaily[1], "%Y")
raindaily$Month <- format(raindaily[1], "%m")

raindaily["Nong.Khai"] <- (raindaily["Nong.Khai"] + raindaily["Bueng.Kan"]) / 2
raindaily$Bueng.Kan <- NULL

raindaily <- pivot_longer(raindaily, cols=Amnat.Charoen:Yasothon, names_to="Province", values_to="TP")

rainmonthly <- raindaily %>%
  group_by(Year$V1, Month$V1, Province) %>%
  summarise(meantp = mean(TP))
##this section here for rainmonthly ^^^^^^

spimonthly <- rainmonthly[-(1:nrow(rainmonthly)),]

for(prov in unique(rainmonthly$Province)){
  df <- filter(rainmonthly, Province==prov)
  spi1 <- spi(df$meantp, 1)
  df$meantp <- spi1$fitted
  spimonthly <- rbind(spimonthly, df)
}
colnames(spimonthly) <- c("Year", "Month", "Province", "SPI")
spimonthly <- arrange(spimonthly, Year, Month, Province)
spimonthly[which(spimonthly$SPI)==-Inf,"SPI"] <- -3
clim$spi <- c(spimonthly$SPI)

clim[which(clim$SPI)==-Inf,"SPI"] <- -3

#extreme rainfall
# rainnatl <- rowMeans(raindaily[2:78])
# tpcutoff <- quantile(rainnatl, 0.95) #95th percentile daily rainfall on a national scale
# #0.00212m ##0.0127m
# 
# raindaily["Nong.Khai"] <- (raindaily["Nong.Khai"] + raindaily["Bueng.Kan"]) / 2
# raindaily$Bueng.Kan <- NULL
# 
# raindaily <- pivot_longer(raindaily, cols=Amnat.Charoen:Yasothon, names_to="Province", values_to="TotalPrecip")
# raindaily$extreme <- as.numeric(raindaily$TotalPrecip >= tpcutoff)
# raindaily <- raindaily %>% group_by(Province, Year, Month) %>% summarise(extremeTP=sum(extreme))
# 
# raindaily <- arrange(raindaily, Year, Month, Province)
# clim$extremeTP <- c(raindaily$extremeTP)

#extreme heat

temp <- read.csv("./data/t2m_max.csv", row.names=NULL) #bo's era5 data is 6-hourly from jan 01 2003

tempdaily <- matrix(NA, nrow=6940, ncol=78)
for(i in 2:78){
  tempdaily[,i] <- colMaxs(matrix(as.numeric(unlist(temp[i])), nrow=4))
}

tempdaily <- as.data.frame(tempdaily)
colnames(tempdaily)[2:78] <- colnames(temp[2:78])
tempdaily[1] <- seq.Date(from=as.Date("2003-01-01"), length.out=6940, by="day")
tempdaily$Year <- format(tempdaily[[1]], "%Y")
tempdaily$Month <- format(tempdaily[[1]], "%m")

tempdaily["Nong.Khai"] <- (tempdaily["Nong.Khai"] + tempdaily["Bueng.Kan"]) / 2
tempdaily$Bueng.Kan <- NULL

#calculate wet bulb temperature
#rh <- read.csv("./data/rh_mean.csv", row.names=NULL)
# rhdaily <- matrix(NA, nrow=6940, ncol=78)
# for(i in 2:78){
#   rhdaily[,i] <- colMeans(matrix(as.numeric(unlist(rh[i])), nrow=4))
# }
# 
# rhdaily <- as.data.frame(rhdaily)
# colnames(rhdaily)[2:78] <- colnames(rh[2:78])
# rhdaily[1] <- seq.Date(from=as.Date("2003-01-01"), length.out=6940, by="day")
# rhdaily$Year <- format(rhdaily[[1]], "%Y")
# rhdaily$Month <- format(rhdaily[[1]], "%m")
# 
# rhdaily["Nong.Khai"] <- (rhdaily["Nong.Khai"] + rhdaily["Bueng.Kan"]) / 2
# rhdaily$Bueng.Kan <- NULL
# 
# rhdaily <- pivot_longer(rhdaily, cols=Amnat.Charoen:Yasothon, names_to="Province", values_to="RH")
# 
# tempdaily$Temp <- tempdaily$Temp - 273.15
# rhdaily$RH <- rhdaily$RH * 100
# wbt <- tempdaily$Temp * atan(0.152 * (rhdaily$RH + 8.3136)^(1/2)) + atan(tempdaily$Temp + rhdaily$RH) - atan(rhdaily$RH - 1.6763) +
#   0.00391838 *(rhdaily$RH)^(3/2) * atan(0.0231 * rhdaily$RH) 
# 
# tempdaily$wbt <- wbt
# tempdaily$extreme31 <- as.numeric(tempdaily$wbt > 31)
#tempdaily <- tempdaily %>% group_by(Province, Year, Month) %>% summarise(extremeHeat31=sum(extreme31))

####
tempdaily[,2:77] <- lapply(tempdaily[,2:77], as.numeric)
tempnatl <- rowMeans(tempdaily[2:77])
atcutoff <- quantile(tempnatl, 0.90) #90th percentile daily temp on a national scale for 3 or more days - 34.93

tempdaily <- pivot_longer(tempdaily, cols=Amnat.Charoen:Yasothon, names_to="Province", values_to="Temp")
tempdaily$extreme <- as.numeric(tempdaily$Temp >= atcutoff)
tempdaily$Temp <- tempdaily$Temp - 273.15
tempdaily <- arrange(tempdaily, Province, V1)
newtempdaily <- tempdaily[0,]

#check for at least 3 consecutive days of extreme heat
for(prov in unique(tempdaily$Province)){
  tempdailyprov <- tempdaily %>%
    filter(Province==prov)
  lengths <- rle(tempdailyprov$extreme)
  tempdailyprov$extreme <- 0
  row <- 1
  for(i in 1:length(lengths[["lengths"]])){
    if(lengths[["values"]][i]==0 | lengths[["values"]][i]==1 & lengths[["lengths"]][i]<3){
      row <- row + lengths[["lengths"]][i]
    }else{
      tempdailyprov$extreme[(row+2):(row + lengths[["lengths"]][i] - 1)] <- 1
      row <- row + lengths[["lengths"]][i]
    }
  }
  newtempdaily <- rbind(newtempdaily, tempdailyprov)
}

newtempdaily <- newtempdaily %>% group_by(Province, Year, Month) %>% summarise(extremeHeat=sum(extreme))

newtempdaily <- arrange(newtempdaily, Year, Month, Province)
clim$extremeHeat <- c(newtempdaily$extremeHeat)

write.csv(clim, file="./data/climate.csv")



#get popln data 
pop <- read.csv("./data/THpopln_prov.csv")
clim <- read.csv("./data/climate.csv")[-1]
cases <- read.csv("./data/finalcases.csv")[-1]
  
#final full dataset
df <- merge(clim, cases, by=c("Year", "Month", "Province"))
df <- merge(df, pop, by=c("Year", "Month", "Province"))
df["Pop"] <- as.numeric(df[["Pop"]])
provs <- unique(df$Province)

newdf <- matrix(nrow=0, ncol=53)
for(prov in provs){
  filt <- filter(df, Province==prov) %>%
    arrange(Year, Month)
  for(i in 1:3){
    lags <- dplyr::select(filt, spi, extremeHeat, DF:MELIOID) %>%
      dplyr::lag(n=i, default=NA)
    colnames(lags) <- paste0(colnames(lags), "_", i)
    filt <- cbind(filt, lags)
  }
  newdf <- rbind(newdf, filt)
}

newdf <- na.omit(newdf)

write.csv(newdf, "./data/finaldata.csv", row.names=F)

##added later on
df <- read.csv("./data/finaldata.csv")

th.shp <- shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
area <- raster::area(th.shp)
area <- area/10^6
areadf <- data.frame(Province=th.shp@data[["ADM1_EN"]], Area=area)
df <- merge(df, areadf, by="Province")
df <- mutate(df, Popdens=Pop/Area)
df <- mutate(df, Popdenscat=case_when(Popdens<=100~"Low",
                                      Popdens>100 & Popdens<=300~"Moderate",
                                      Popdens>300 & Popdens<=1500~"High",
                                      Popdens>1501~"Very High"))

write.csv(df, "./data/finaldata.csv", row.names=F)

###adding worldpop data
shp <- shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
allpop <- data.frame(matrix(nrow=0, ncol=3))

for(file in list.files("./data/worldpop/")){
  ras <- raster(paste0("./data/worldpop/", file))
  pop <- extract(ras, shp, sum, na.rm=T)
  popdf <- data.frame("Province"=shp@data[["ADM1_EN"]], "worldPop"=pop, "Year"=strsplit(file, "_")[[1]][3])
  allpop <- rbind(allpop, popdf)
}


df <- read.csv("./data/finaldata.csv")
df1 <- merge(df, allpop, by=c("Province", "Year"))
df1 <- mutate(df1, worldPopDens = worldPop/Area)

df1 <- df1[-which(colnames(df1) == c("CHOL") |  colnames(df1) == c("TB"))]

write.csv(df1, "./data/finaldata.csv", row.names=F)




