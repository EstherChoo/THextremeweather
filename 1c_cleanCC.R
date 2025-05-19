library(tidyverse)
library(SPEI)
library(zoo)
library(sf)
library(raster)
library(ncdf4)
library(terra)
library(sf)
library(crsuggest)
library(plyr)

folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)

#load in climate change data
files <- list.files("./data/rawclimchan")
ccmodel <- "IPSL" #change manually: MIROC6, CMCC, IPSL
files <- grep(ccmodel, files, value=T)
precfiles <- grep("prec", files, value=T)
tempfiles <- grep("tmax", files, value=T)
tminfiles <- grep("tmin", files, value=T)

#cleaning data into proper format
totaltp <- data.frame(Adm=NA, Time_P=NA, Month=NA, Obj_data=NA)
totaltp <- totaltp[-1,]
for(file in precfiles){
  df <- read.csv(paste0("./data/rawclimchan/", file))
  tp <- df %>% 
    group_by(Adm, Time_P, Month, SSPs) %>%
    summarise(tp=mean(Obj_data))
  totaltp <- rbind(totaltp, tp)
}
totaltp$Year <- lapply(strsplit(totaltp$Time_P, "-"), "[", 1)

#cleaning data into proper format
totalheat <- data.frame(Adm=NA, Time_P=NA, Month=NA, Obj_data=NA)
totalheat <- totalheat[-1,]
for(file in tempfiles){
  df <- read.csv(paste0("./data/rawclimchan/", file))
  heat <- df %>% 
    group_by(Adm, Time_P, Month, SSPs) %>%
    summarise(heat=mean(Obj_data))
  totalheat <- rbind(totalheat, heat)
}

totalheat$Year <- lapply(strsplit(totalheat$Time_P, "-"), "[", 1)

#cleaning data into proper format
totaltmin <- data.frame(Adm=NA, Time_P=NA, Month=NA, Obj_data=NA)
totaltmin <- totaltmin[-1,]
for(file in tminfiles){
  df <- read.csv(paste0("./data/rawclimchan/", file))
  heat <- df %>% 
    group_by(Adm, Time_P, Month, SSPs) %>%
    summarise(heat=mean(Obj_data))
  totaltmin <- rbind(totaltmin, heat)
}

totaltmin$Year <- lapply(strsplit(totaltmin$Time_P, "-"), "[", 1)

###calc SPI
nk <- totaltp[which(totaltp$Adm=="Nong Khai"),]
bk <- totaltp[which(totaltp$Adm=="Bueng Kan"),]
nk <- arrange(nk, Time_P, Month, SSPs)
bk <- arrange(bk, Time_P, Month, SSPs)

#merge nong khai and bueng kan - provinces that split in 2011
nk$tp <- (nk$tp + bk$tp) /2
totaltp <- totaltp[-which(totaltp$Adm == "Bueng Kan"),]
totaltp <- totaltp[-which(totaltp$Adm == "Nong Khai"),]
totaltp <- rbind(totaltp, nk)


#cleaning historical precip data to input to calc SPI
{
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
}

rainmonthly$Province <- gsub(x=rainmonthly$Province, "\\.", " ") 
rainmonthly[which(rainmonthly$Province=="Bangkok Metropolis"), "Province"] <- "Bangkok"

spi_cc <- totaltp[-(1:nrow(totaltp)),]
for(prov in unique(totaltp$Adm)){
  for(ssp in unique(totaltp$SSPs)){
    df <- filter(totaltp, Adm==prov, SSPs==ssp)
    histdf <- filter(rainmonthly, Province==prov)
    prec <- c(histdf$meantp*1000, df$tp)
    prec <- ts(prec, start=c(2003,1), frequency=12)
    
    days <- days_in_month(as.yearmon(prec)) #get mean monthly tp
    n <- length(prec)
    prec[(n-47):n] <- prec[(n-47):n]/days[(n-47):n]
    
    spi1 <- spi(prec, 1, ref.start=c(2003, 1), ref.end=c(2021,12))
    df$tp <- spi1$fitted[(n-47):n]
    spi_cc <- rbind(spi_cc, df)
  }
}
colnames(spi_cc) <- c("Province", "Period", "Month", "SSP", "spi", "Year")
spi_cc[which(is.na(spi_cc$spi)),"spi"] <- 0

###use past data to glm to impute monthly wet days with total rainfall
#cos spi needs monthly wet days but we dont have that data
hist_tp_6h <- read.csv("./data/tp_mean.csv")
hist_tp <- matrix(NA, nrow=6940, ncol=78)
for(i in 2:78){
  hist_tp[,i] <- colSums(matrix(as.numeric(unlist(hist_tp_6h[i])), nrow=4))
}

hist_tp <- as.data.frame(hist_tp)
colnames(hist_tp)[2:78] <- colnames(hist_tp_6h[2:78])
hist_tp[1] <- seq.Date(from=as.Date("2003-01-01"), length.out=6940, by="day")
hist_tp$Year <- format(hist_tp[1], "%Y")
hist_tp$Month <- format(hist_tp[1], "%m")

hist_tp <- pivot_longer(hist_tp, cols=Amnat.Charoen:Yasothon, names_to="Province", values_to="TotalPrecip")
hist_tp$wet <- as.numeric(hist_tp$TotalPrecip > 0.1/1000)
hist_tp <- hist_tp %>% group_by(Province, Year, Month) %>% summarise(wetdays=sum(wet), tp=sum(TotalPrecip))

model <- glm(data=hist_tp, wetdays ~ tp, family=poisson())
filler <- rep(NA, 12)

#just some cleaning so that the historical and future dataset provinces match up
df <- st_read("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
df <- df[,c(3,5)]
df$Adm_Co <- paste0("0", substr(df$ADM1_PCODE, 3,4))
df <- as.data.frame(df)
totaltp[which(totaltp$Adm=="Bangkok Metropolis"), "Adm"] <- "Bangkok"
totaltmin[which(totaltmin$Adm=="Bangkok Metropolis"), "Adm"] <- "Bangkok"
totalheat[which(totalheat$Adm=="Bangkok Metropolis"), "Adm"] <- "Bangkok"

###format text files needed for modawec to run
#modawaec needs prec, tmin, tmax, wetdays, sd (just put 999)
#row years and months
#each prov and ssp is 1 set of files and 1 line in rdwtrun.dat
for(ssp in unique(totaltp$SSPs)){
  for(prov in unique(totaltp$Adm)){
    tmax_filt <- filter(totalheat, SSPs==ssp, Adm==prov) %>%
      ungroup() %>%
      dplyr::select(Year, Month, heat) %>%
      pivot_wider(names_from=Month, values_from=heat)
    
    prec_filt <- filter(totaltp, SSPs==ssp, Adm==prov) %>%
      ungroup() %>%
      dplyr::select(Year, Month, tp) %>%
      pivot_wider(names_from=Month, values_from=tp)  
    
    tmin_filt <- filter(totaltmin, SSPs==ssp, Adm==prov) %>%
      ungroup() %>%
      dplyr::select(Year, Month, heat) %>%
      pivot_wider(names_from=Month, values_from=heat)
    
    wetdaydf <- filter(totaltp, SSPs==ssp, Adm==prov)
    wet <- round(predict(model, newdata=data.frame("tp"=wetdaydf$tp/1000), type="response"), 0)
    wet[which(wet>31)] <- 31
    wetday_filt <- cbind(wetdaydf[c(3,6)], wet=wet) %>%
      pivot_wider(names_from=Month, values_from=wet) 
    
    ext_filt <- as.data.frame(matrix(data="999.", nrow=8, ncol=12))
    code <- df[which(df$ADM1_EN==prov), "Adm_Co"]
    sspn <- substr(ssp, 4,6)
    filename <- paste0("./MODAWEC_program/", ccmodel, "/", code, sspn)
    
    prec_filt <- apply(prec_filt, 2,as.character)
    write.table(prec_filt, paste0(filename, ".pcp"), row.names = F, col.names = F, quote = F, sep = '\t')
    cat(file=paste0(filename, ".pcp"), "\n", append = T)
    
    tmax_filt <- apply(tmax_filt, 2, as.character)
    write.table(tmax_filt, paste0(filename, ".tmx"), row.names = F, col.names = F, quote = F, sep = '\t')
    cat(file=paste0(filename, ".tmx"), "\n", append = T)
    
    tmin_filt <- apply(tmin_filt, 2, as.character)
    write.table(tmin_filt, paste0(filename, ".tmn"), row.names = F, col.names = F, quote = F, sep = '\t')
    cat(file=paste0(filename, ".tmn"), "\n", append = T)
    
    wetday_filt <- apply(wetday_filt, 2, as.character)
    write.table(wetday_filt, paste0(filename, ".wtd"), row.names = F, col.names = F, quote = F, sep = '\t')
    cat(file=paste0(filename, ".wtd"), "\n", append = T)
    
    ext_filt <- apply(ext_filt, 2, as.character)
    write.table(ext_filt, paste0(filename, ".ext"), row.names = F, col.names = F, quote = F, sep = '\t')
    cat(file=paste0(filename, ".ext"), "\n", append = T)
    
    dat_file <- paste0(code, sspn, "  ", code, sspn, "  ", 0.75, "      ", 0.5)
    write.table(dat_file, file=paste0("./MODAWEC_program/", ccmodel, "/RDWTRUN.DAT"), append=T, row.names = F, col.names = F, quote = F)
 
  }
}

modawec_files <- grep(list.files(paste0("./MODAWEC_program/", ccmodel)), pattern="DLY", value=T)
dailytmax <- data.frame(Province=NA, Period=NA, Month=NA, Day=NA, SSP=NA, Value=NA) ; dailytmax <- dailytmax[-1,]
dailytmin <- data.frame(Province=NA,Period=NA, Month=NA, Day=NA, SSP=NA, Value=NA) ; dailytmin <- dailytmin[-1,]
dailytp <- data.frame(Province=NA, Period=NA, Month=NA, Day=NA, SSP=NA, Value=NA) ; dailytp <- dailytp[-1,]
for(file in modawec_files){
  code <- as.numeric(substr(file, 1, 3))
  code <- paste0("TH", code)
  ssp <- paste0("ssp", substr(file,4,6))
  prov <- df[which(df$ADM1_PCODE==code), "ADM1_EN"]
  out <- read.table(paste0(("./MODAWEC_program/"), ccmodel, "/", file))
  out$V1 <- gsub(2021, "2021-2040", out$V1)
  out$V1 <- gsub(2022, "2041-2060", out$V1)
  out$V1 <- gsub(2023, "2061-2080", out$V1)
  out$V1 <- gsub(2024, "2081-2100", out$V1)
  dailytmax <- rbind(dailytmax, cbind("Province"=prov, "Period"=out$V1, "Month"=out$V2, "Day"=out$V3, "SSP"=ssp, "Value"=out$V4))
  dailytmin <- rbind(dailytmin, cbind("Province"=prov, "Period"=out$V1, "Month"=out$V2, "Day"=out$V3, "SSP"=ssp, "Value"=out$V5))
  dailytp <- rbind(dailytp, cbind("Province"=prov, "Period"=out$V1, "Month"=out$V2, "Day"=out$V3, "SSP"=ssp, "Value"=out$V6))
}

write.csv(dailytmax, paste0("./data/", ccmodel, "_CCdailytmax.csv"), row.names=F)
write.csv(dailytmin,  paste0("./data/", ccmodel, "_CCdailytmin.csv"), row.names=F)
write.csv(dailytp,  paste0("./data/", ccmodel, "_CCdailytp.csv"), row.names=F)

dailytmax <- read.csv(paste0("./data/", ccmodel, "_CCdailytmax.csv"))
dailytmin <- read.csv(paste0("./data/", ccmodel, "_CCdailytmin.csv"))
dailytp <- read.csv(paste0("./data/", ccmodel, "_CCdailytp.csv"))

###extreme heat
atcutoff <- 34.93 #90th percentile daily temp on a national scale for 3 or more days - 30.14 (calculated before hand)
tempdaily <- dailytmax

#merge bueng kan into nong khai
nk <- tempdaily[which(tempdaily$Province=="Nong Khai"),]
bk <- tempdaily[which(tempdaily$Province=="Bueng Kan"),]
nk <- arrange(nk, Period, Month, Day, SSP)
bk <- arrange(bk, Period, Month, Day, SSP)

nk$Value <- (nk$Value + bk$Value) /2
tempdaily <- tempdaily[-which(tempdaily$Province == "Bueng Kan" | tempdaily$Province == "Nong Khai"),]
tempdaily <- rbind(tempdaily, nk)

tempdaily$extreme <- as.numeric(tempdaily$Value >= atcutoff)
tempdaily <- arrange(tempdaily, Province, SSP, Period, Month, Day)
newtempdaily <- tempdaily[0,]

#check for at least 3 consecutive days of extreme heat
for(ssp in unique(tempdaily$SSP)){
  for(prov in unique(tempdaily$Province)){
    tempdailyprov <- tempdaily %>%
      filter(Province==prov, SSP==ssp)
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
}
  
newtempdaily <- newtempdaily %>% group_by(Province, Period, Month, SSP) %>% summarise(extremeHeat=sum(extreme))

###prepare data to add into gam function
newtempdaily <- arrange(newtempdaily, Province, Period, Month, SSP)
spi_cc <- arrange(spi_cc, Province, Period, Month, SSP)
cc_clim <- cbind(newtempdaily, "spi"=spi_cc$spi)
data <- read.csv("./data/finaldata.csv")

#make province names match
cc_prov <- unique(cc_clim$Province)
corr_prov <- unique(data$Province)
which(!(cc_prov %in% corr_prov))
cc_clim[which(cc_clim$Province=="Bangkok Metropolis"), "Province"] <- "Bangkok"

#add average of each province over hist data
vars <- data %>% 
  group_by(Province) %>%
  summarise(rh_mean=mean(rh_mean), Pop=mean(Pop))

#merge all data tgt
finalccdata <- merge(cc_clim, vars, by="Province")

write.csv(finalccdata, file=paste0("./data/", ccmodel, "_finalccdata.csv"), row.names = F)

###adding new pop (added retrospectively cos of methodology changes)
ccmodels <- c("MIROC6", "CMCC", "IPSL")
#clean population data
th.shp <- shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
ssps <- c("ssp126", "ssp245", "ssp370", "ssp585")

allpopdata <- data.frame(matrix(ncol=4, nrow=0))
for(i in 4){
  popnc <- terra::rast(paste0("./data/thssp", 4, ".nc"))
  th.sf <- st_as_sf(th.shp)
  th.sf <- st_transform(th.sf, crs(popnc))
  th.v <- terra::vect(th.sf)
  nccrop <- terra::crop(popnc, th.sf)
  popdata <- terra::extract(nccrop, th.sf, sum, na.rm=T)
  popdata$Province <- th.sf$ADM1_EN
  colnames(popdata) <- c("ID", as.character(2020:2100), "Province")
  popdata$`2021-2040` <- rowMeans(popdata[3:22])
  popdata$`2041-2060` <- rowMeans(popdata[23:42])
  popdata$`2061-2080` <- rowMeans(popdata[43:62])
  popdata$`2081-2100` <- rowMeans(popdata[63:82])
  popdata$SSP <-  ssps[i]
  popdata <- popdata[83:88]
  popdata <- pivot_longer(popdata, cols=starts_with("20"), names_to="Period", values_to="Pop")
  allpopdata <- rbind(allpopdata, popdata)
}

write.csv(allpopdata, "./data/futureTHpop.csv", row.names=F)

#calc pop density (added retrospectively cos of methodology changes)
th.shp <- shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
area <- raster::area(th.shp)
area <- area/10^6
areadf <- data.frame(Province=th.shp@data[["ADM1_EN"]], Area=area)
allpopdata <- merge(allpopdata, areadf, by="Province")
allpopdata <- mutate(allpopdata, Popdens=Pop/Area)
allpopdata <- mutate(allpopdata, Popdenscat=case_when(Popdens<=100~"Low",
                                                      Popdens>100 & Popdens<=300~"Moderate",
                                                      Popdens>300 & Popdens<=1500~"High",
                                                      Popdens>1501~"Very High"))
allpopdata <- rename(allpopdata, futPop=Pop)

for(ccmodel in ccmodels){
  df <- read.csv(paste0("./data/", ccmodel, "_finalccdata.csv"))
  df$Pop <- NULL
  df1 <- merge(df, allpopdata, by=c("Province", "SSP", "Period"))

  write.csv(df1, file=paste0("./data/", ccmodel, "_finalccdata.csv"), row.names = F)
}

###clean future relative humidity (added retrospectively cos of methodology changes)
{future_rh <- data.frame(matrix(NA, nrow=0, ncol=4))
rhfiles <- list.files(path="./data/future_rh")

for(i in c(1,4, 5)){
  file <- rhfiles[i]
  nc <- terra::rast(paste0("./data/future_rh/", file))
  th.shp <- st_as_sf(shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp"))
  th.sf <- st_transform(th.shp, crs(nc))
  nccrop <- terra::crop(nc, th.sf)
  rhdata <- terra::extract(nccrop, th.sf, mean, na.rm=T)
  
  rhdata <- rhdata[-1]
  rhdata$Province <- th.sf$ADM1_EN
  rhdata <- pivot_longer(rhdata, cols=1:1032, names_to="ind")
  rhdata$Year <- rep(rep(2015:2100, each=12), 77)
  rhdata$Month <- rep(1:12, length.out=nrow(rhdata))
  rhdata <- rhdata[-which(rhdata$Year < 2021),]
  rhdata <- arrange(rhdata, Province, Month, Year)
  
  test <- as.data.frame(rowMeans(matrix(ncol=20, rhdata$value)))
  colnames(test) <- "rh"
  test$Month <- rep(1:12, each=4, length.out=nrow(test))
  test$Province <- rep(th.sf$ADM1_EN, each=48)
  test$Period <- rep(c("2021-2040", "2041-2060", "2061-2080", "2081-2100"), length.out=nrow(test))
  test$SSP <- strsplit(file, "_")[[1]][4]
  
  future_rh <- rbind(future_rh, test)
}

#ssp245 special case due to dirty data
file1 <- rhfiles[2]
file2 <- rhfiles[3]
nc <- terra::rast(paste0("./data/future_rh/", file1))
th.shp <- st_as_sf(shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp"))
th.sf <- st_transform(th.shp, crs(nc))
nccrop <- terra::crop(nc, th.sf)
rhdata <- terra::extract(nccrop, th.sf, mean, na.rm=T)

nc <- terra::rast(paste0("./data/future_rh/", file2))
th.shp <- st_as_sf(shapefile("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp"))
th.sf <- st_transform(th.shp, crs(nc))
nccrop <- terra::crop(nc, th.sf)
rhdata1 <- terra::extract(nccrop, th.sf, mean, na.rm=T)

rhdata1 <- rhdata1[-1]
rhdata <- rhdata[-1]

rhdata <- cbind(rhdata, rhdata1)
colnames(rhdata) <- 1:1032
rhdata$Province <- th.sf$ADM1_EN
rhdata <- pivot_longer(rhdata, cols=1:1032, names_to="ind")
rhdata$Year <- rep(rep(2015:2100, each=12), 77)
rhdata$Month <- rep(1:12, length.out=nrow(rhdata))
rhdata <- rhdata[-which(rhdata$Year < 2021),]
test <- as.data.frame(rowMeans(matrix(ncol=20, rhdata$value)))
colnames(test) <- "rh"
test$Month <- rep(1:12, each=4, length.out=nrow(test))
test$Province <- rep(th.sf$ADM1_EN, each=48)
test$Period <- rep(c("2021-2040", "2041-2060", "2061-2080", "2081-2100"), length.out=nrow(test))
test$SSP <- strsplit(file1, "_")[[1]][4]

future_rh <- rbind(future_rh, test)
}

#merge nongkai and buengkhan
nk <- filter(future_rh, Province=="Nong Khai")
bk <- filter(future_rh, Province=="Bueng Kan")
future_rh[which(future_rh$Province=="Nong Khai"), "rh"] <- (bk$rh + nk$rh) / 2
future_rh <- future_rh[-which(future_rh$Province=="Bueng Kan"),]
colnames(future_rh)[1] <- "rh_mean"

for(ccmodel in ccmodels){
  df <- read.csv(paste0("./data/", ccmodel, "_finalccdata.csv"))
  df$rh_mean <- NULL
  df1 <- merge(df, future_rh, by=c("Province", "SSP", "Period", "Month"))
  
  write.csv(df1, file=paste0("./data/", ccmodel, "_finalccdata.csv"), row.names = F)
}



