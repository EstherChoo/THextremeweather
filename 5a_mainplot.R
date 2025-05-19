library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggrepel)
library(scales)
library(egg)
library(matrixStats)
library(cowplot)
library(raster)
library(sf)

folder <- "C:/Users/esthe/OneDrive - Nanyang Technological University/TH_extr_weat"
folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)
sapply(paste0("./code/func/", list.files("./code/func")), source)

###plot1: climate vars by province, ssp and period
{th.shp <- sf::st_read("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
  
  spicc <- read.csv("./data/MIROC6_finalccdata.csv")
  spicc <- dplyr::select(spicc, Province:Period, spi)
  spimincc <- spicc %>%
    group_by(Period, SSP, Province) %>% 
    summarise(SPI=min(spi))
  spimaxcc <- spicc %>%
    group_by(Period, SSP, Province) %>% 
    summarise(SPI=max(spi))
  
  tmaxcc <- read.csv("./data/finalccdata.csv")
  tmaxcc <- dplyr::select(tmaxcc, Province:Period, extremeHeat)
  tmaxcc <- tmaxcc %>%
    group_by(Period, SSP, Province) %>%
    summarise(`No. of Extreme Heat Days` =mean(extremeHeat))
  
  tmax.shp <- merge(th.shp, tmaxcc, all.y=T, by.y="Province", by.x="ADM1_EN")
  spimin.shp <- merge(th.shp, spimincc, all.y=T, by.y="Province", by.x="ADM1_EN")
  spimax.shp <- merge(th.shp, spimaxcc, all.y=T, by.y="Province", by.x="ADM1_EN")
  
  # na.shp <- tmax.shp[is.na(tmax.shp$extremeHeat),]
  # na.shp1 <- na.shp[rep(seq_len(nrow(na.shp)), each = 16), ]
  # periods <- unique(tmaxcc$Period)
  # ssps <- unique(tmaxcc$SSP)
  # na.shp1$Period <- rep(periods, 4, length.out=nrow(na.shp1))
  # na.shp1$SSP <- rep(ssps, each=4, length.out=nrow(na.shp1))
  # tmax.shp1 <- rbind(tmax.shp, na.shp1)
  # tmax.shp1 <- tmax.shp1[!is.na(tmax.shp1$Period),]
  # 
  # na.shp <- spimin.shp[is.na(spimin.shp$spi),]
  # na.shp1 <- na.shp[rep(seq_len(nrow(na.shp)), each = 16), ]
  # periods <- unique(spicc$Period)
  # ssps <- unique(tmaxcc$SSP)
  # na.shp1$Period <- rep(periods, 4, length.out=nrow(na.shp1))
  # na.shp1$SSP <- rep(ssps, each=4, length.out=nrow(na.shp1))
  # spimin.shp1 <- rbind(spimin.shp, na.shp1)
  # spimim.shp1 <- spimin.shp1[!is.na(spimin.shp1$Period),]
  # spimax.shp1 <- rbind(spimax.shp, na.shp1)
  # spimax.shp1 <- spimax.shp1[!is.na(spimax.shp1$Period),]
  
  spiminplot <- ggplot(spimin.shp) +
    geom_sf(aes(fill=SPI), linewidth=0, color="white") +
    facet_grid(SSP~Period) +
    theme_bw() +
    theme(axis.line = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") + 
    labs(title="Minimum Monthly SPI") +
    scale_fill_gradient2(low="navy", mid="lavender", high="red3", midpoint=0)
  
  spimaxplot <- ggplot(spimax.shp) +
    geom_sf(aes(fill=SPI), linewidth=0, color="white") +
    facet_grid(SSP~Period) +
    theme_bw() +
    theme(axis.line = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") + 
    labs(title="Maximum Monthly SPI") +
    scale_fill_gradient2(low="navy", mid="lavender", high="red3", midpoint=0)
  
  tmaxplot <- ggplot(tmax.shp) +
    geom_sf(aes(fill=`No. of Extreme Heat Days`), linewidth=0, color="white") +
    facet_grid(SSP~Period) +
    theme_bw() +
    theme(axis.line = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") + 
    labs(title="Average Monthly Extreme Heat Days") +
    scale_fill_gradient2(low="lavender", high="red3")
  
  png(filename="./plots/1_climatefull.png", width=3400, height=2400, res=300)
  plot_grid(spiminplot, spimaxplot, tmaxplot, nrow=1, labels="AUTO")
  dev.off()
}

###plot2:
png(filename="./plots/2_rships.png", width=3500, height=2000, res=300)
SFplotter()
dev.off()


###plot3_dis: combine all

#manually add bins for sfprojplot
{
  ccfiles <-list.files("./out", pattern="_MIROC6_all")
  dis <- unlist(lapply(strsplit(ccfiles, "_"), "[[", 1))
  cuts <- list()
  
  for(i in 1:length(ccfiles)){
    file <- ccfiles[i]
    proj <- read.csv(paste0("./out/", file))
    annualproj <- proj %>% 
      group_by(Period, SSP, Province) %>%
      dplyr::summarise(across(pred:baselineLo, sum))
    
    filt <- annualproj %>% 
      group_by(Period, SSP, Province) %>%
      mutate(`Excess Risk (%)`=(pred-baseline)/baseline * 100)
    assign(paste0("er", i), filt$`Excess Risk (%)`)
  }
  

  cuts[[dis[1]]] <- cut(er1, breaks=c(seq(-75, 100, 25)),dig.lab=5)
  cuts[[dis[2]]] <- cut(er2, breaks=c(seq(-50, 50, 25)) ,dig.lab=5)
  cuts[[dis[3]]] <- cut(er3, breaks=c(seq(-60, 60, 20)), dig.lab=5)
  cuts[[dis[4]]] <- cut(er4, breaks=c(seq(-30, 30, 10)), dig.lab=5)
  cuts[[dis[5]]] <- cut(er5, breaks=c(seq(-50, 50, 25)), dig.lab=5)
  cuts[[dis[6]]] <- cut(er6, breaks=c(seq(-25, 150, 25)), dig.lab=5)
  cuts[[dis[7]]] <- cut(er7, breaks=c(seq(-10,15,5)), dig.lab=5)

  for(i in 1:length(cuts)){
    levels(cuts[[i]]) <- gsub("\\(", "", levels(cuts[[i]]))
    levels(cuts[[i]]) <- gsub("]", "", levels(cuts[[i]]))
    levels(cuts[[i]]) <- gsub(",", " to ", levels(cuts[[i]]))
  }
  
  save(cuts, file="./out/ercuts.Rdata")   
}

load("./out/ercuts.Rdata")
types <- c("all", "heat", "dry", "wet")
typedict <- c("all"="all extreme events", "wet"="extreme wet weather", 
              "dry"="extreme dry weather", "heat"="extreme heat")
diseases <- c("DF", "ENCEP", "INFLUENZA", "MALARIA", "PNEU", "LEPTO", "MELIOID")
disdict <- c("DF"="Dengue", "ENCEP"= "Japanese Encephalitis", "INFLUENZA"="Influenza", "MALARIA"="Malaria",
             "PNEU"="Pneumonia", "LEPTO"="Leptospirosis", "MELIOID"="Melioidosis")

for(dis in diseases){
  annualERplot <- annualERplotter(dis, types, ncol=2, "MIROC6")
  sfprojplot <- sfprojplotter(dis, "ssp126")
  p1 <- plot_grid(annualERplot, sfprojplot, ncol=2, rel_widths = c(0.45, 0.55), labels=c("", "E"))
  title <- ggdraw() + draw_label(disdict[dis])
  png(filename=paste0("./plots/s7_provall/3_", dis, "plot.png"), width=3400, height=2500, res=300)
  print(plot_grid(title, p1, ncol=1, rel_heights = c(0.02, 1)))
  dev.off()
}

