library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(scales)
library(egg)
library(matrixStats)
library(cowplot)
library(sf)

folder <- "C:/Users/esthe/OneDrive - Nanyang Technological University/TH_extr_weat"
folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)
sapply(paste0("./code/func/", list.files("./code/func")), source)

df <- read.csv("./data/finaldata.csv")
diseases <- colnames(df)[11:17]
provinces <- unique(df$Province)
disdict <- c("DF"="Dengue", "ENCEP"= "Japanese Encephalitis", "INFLUENZA"="Influenza", "MALARIA"="Malaria",
             "PNEU"="Pneumonia", "LEPTO"="Leptospirosis", "MELIOID"="Melioidosis")

ccmodels <- c("MIROC6", "IPSL", "CMCC")
##s1: tmax and prec future climate

for(ccmodel in ccmodels){
  spicc <- read.csv(paste0("./data/", ccmodel, "_finalccdata.csv"))
  spicc <- dplyr::select(spicc, Province:Month, spi)
  spicc <- spicc %>%
    group_by(Period, Month, SSP) %>% 
    summarise(spi=mean(spi))
  
  spihist <- read.csv("./data/finaldata.csv")
  spihist <- dplyr::select(spihist, Province:Month, spi)
  spihist$date <- as.Date(paste0(spihist$Year, "-", spihist$Month, "-01"))
  spihist <- spihist %>%
    group_by(date, spi) %>% 
    summarise(spi=mean(spi))
  
  #clean daily max temp
  tmaxhist <- read.csv("./data/finaldata.csv")
  tmaxhist <- dplyr::select(tmaxhist, Province:Month, extremeHeat)
  tmaxhist$date <- as.Date(paste0(tmaxhist$Year, "-", tmaxhist$Month, "-01"))
  tmaxhist <- tmaxhist %>%
    group_by(date) %>%
    summarise(extremeHeat=mean(extremeHeat))
  
  tmaxcc <- read.csv("./data/MIROC6_finalccdata.csv")
  tmaxcc <- dplyr::select(tmaxcc, Province:Month, extremeHeat)
  tmaxcc <- tmaxcc %>%
    group_by(Period, Month, SSP) %>%
    summarise(extremeHeat=mean(extremeHeat))
  tmaxcc$date <- as.Date(paste0(2021, "-", tmaxcc$Month, "-", 01))
  
  monbreaks <- as.Date(c("01-01-2021", "01-03-2021", "01-06-2021", "01-09-2021", "01-12-2021"), format="%d-%m-%Y")
  
  tmaxcc$SSP <- gsub("ssp", "SSP", tmaxcc$SSP)
  spicc$SSP <- gsub("ssp", "SSP", spicc$SSP)
  
  #tmax plot
  tmaxplot1 <- ggplot() +
    geom_point(data=tmaxhist, aes(y=extremeHeat, x=date)) +
    theme_minimal() +
    scale_x_date(labels=date_format("%y"), date_breaks="1 year") +
    labs(x="Year", y="Number of Extreme Heat Days") +
    ylim(0,30)+
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks.x=element_line(color='black'),
          plot.margin = unit(c(1,0,1,1), 'lines'),
          axis.text.x = element_text(hjust=-0.4))
  
  tmaxplot2 <- ggplot() + 
    geom_line(data=filter(tmaxcc, Period=="2021-2040"), aes(y=extremeHeat, x=as.Date(date), color=SSP)) +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_date(labels = date_format("%b"), date_breaks="3 months") +
    labs(x="2021-2040", y="") +
    ylim(0,30) +
    theme_minimal() +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks.x=element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  tmaxplot3 <- ggplot() + 
    geom_line(data=filter(tmaxcc, Period=="2041-2060"), aes(y=extremeHeat, x=as.Date(date), color=SSP)) +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_date(labels = date_format("%b"), date_breaks="3 months") +
    theme_minimal() +
    ylim(0,30) +
    labs(x="2041-2060", y="") +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks.x=element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  tmaxplot4 <- ggplot() + 
    geom_line(data=filter(tmaxcc, Period=="2061-2080"), aes(y=extremeHeat, x=as.Date(date), color=SSP)) +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_date(labels = date_format("%b"), date_breaks="3 month") +
    theme_minimal() +
    ylim(0,30) +
    labs(x="2061-2080", y="") +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks.x=element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  tmaxplot5 <- ggplot() + 
    geom_line(data=filter(tmaxcc, Period=="2081-2100"), aes(y=extremeHeat, x=as.Date(date), color=SSP)) +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    theme_minimal() +
    scale_x_date(labels = date_format("%b"), date_breaks="3 months") +
    labs(x="2081-2100", y="") +
    ylim(0,30) +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks.x=element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,1,1,0), 'lines'))
  
  
  #prec plot
  precplot1 <- ggplot() +
    geom_line(data=spihist, aes(y=spi, x=date)) +
    theme_minimal() +
    labs(y="SPI", x="Year") +
    scale_x_date(labels=date_format("%y"), date_breaks="1 year") +
    scale_y_continuous(breaks=-4:4, limits=c(-4,4)) +
    geom_hline(yintercept=1, colour="royalblue") +
    geom_hline(yintercept=-1, colour="royalblue") +
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks=element_line(color='black'),
          plot.margin = unit(c(1,0,1,1), 'lines'),
          axis.text.x = element_text(hjust=-0.4))
  
  precplot2 <- ggplot() + 
    geom_line(data=filter(spicc, Period=="2021-2040"), aes(y=spi, x=Month, color=SSP)) +
    geom_hline(yintercept=1, colour="royalblue") +
    geom_hline(yintercept=-1, colour="royalblue") +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_continuous(breaks=seq(1,12,3), labels = month.abb[seq(1,12,3)]) +
    labs(x="2021-2040", y="") +
    scale_y_continuous(breaks=-4:4, limits=c(-4,4)) +
    theme_minimal() +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="top",
          axis.ticks =element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  precplot3 <- ggplot() + 
    geom_line(data=filter(spicc, Period=="2041-2060"), aes(y=spi, x=Month, color=SSP)) +
    geom_hline(yintercept=1, colour="royalblue") +
    geom_hline(yintercept=-1, colour="royalblue") +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_continuous(breaks=seq(1,12,3), labels = month.abb[seq(1,12,3)]) +
    labs(x="2041-2060", y="") +
    scale_y_continuous(breaks=-4:4, limits=c(-4,4)) +
    theme_minimal() +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks =element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  precplot4 <- ggplot() + 
    geom_line(data=filter(spicc, Period=="2061-2080"), aes(y=spi, x=Month, color=SSP)) +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_continuous(breaks=seq(1,12,3), labels = month.abb[seq(1,12,3)]) +
    labs(x="2061-2080", y="") +
    geom_hline(yintercept=1, colour="royalblue") +
    geom_hline(yintercept=-1, colour="royalblue") +
    scale_y_continuous(breaks=-4:4, limits=c(-4,4)) +
    theme_minimal() +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks =element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  precplot5 <- ggplot() + 
    geom_line(data=filter(spicc, Period=="2081-2100"), aes(y=spi, x=Month, color=SSP)) +
    scale_colour_manual(values=c("darkseagreen4", "purple1", "coral", "indianred")) +
    scale_x_continuous(breaks=seq(1,12,3), labels = month.abb[seq(1,12,3)]) +
    labs(x="2081-2100", y="") +
    scale_y_continuous(breaks=-4:4, limits=c(-4,4)) +
    geom_hline(yintercept=1, colour="royalblue") +
    geom_hline(yintercept=-1, colour="royalblue") +
    theme_minimal() +
    theme(axis.line.x = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          axis.ticks =element_line(color='black'),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,0,1,0), 'lines'))
  
  climprojplot <- ggarrange(precplot1, precplot2, precplot3, precplot4, precplot5,
                            tmaxplot1, tmaxplot2, tmaxplot3, tmaxplot4, tmaxplot5,
                            labels=c("A", "","","","","B","","","",""),
                            nrow=2, widths=c(0.8,0.2,0.2,0.2,0.2))
  
  ggsave(filename=paste0("./plots/s1_", ccmodel, "_natlclimate.jpeg"), climprojplot, device="jpeg", units="in", width=10, height=7)
}

ccmodels <- c("MIROC6", "IPSL", "CMCC")

for(ccmodel in ccmodels){
  th.shp <- sf::st_read("./data/shp/tha_admbnda_adm1_rtsd_20190221.shp")
  
  spicc <- read.csv(paste0("./data/", ccmodel, "_finalccdata.csv"))
  spicc <- dplyr::select(spicc, Province:Period, spi)
  spimincc <- spicc %>%
    group_by(Period, SSP, Province) %>% 
    summarise(SPI=min(spi))
  spimaxcc <- spicc %>%
    group_by(Period, SSP, Province) %>% 
    summarise(SPI=max(spi))
  
  tmaxcc <- read.csv(paste0("./data/", ccmodel, "_finalccdata.csv"))
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
  
  png(filename=paste0("./plots/s1_", ccmodel, "climatefull.png"), width=3400, height=2400, res=300)
  plot_grid(spiminplot, spimaxplot, tmaxplot, nrow=1, labels="AUTO")
  dev.off()
}

##s2 - heat days IRR
heatIRRplotter()

##s3 - SPI IRR
rainIRRplotter()

##s4/s5 - association
barplot <- list()
for(dis in diseases){
  HEAT <- heatbarplotter(dis)
  RAIN <- rainbarplotter(dis)
  title <- ggdraw() + draw_label(disdict[dis], fontface='bold', size=12)
  p1 <- plot_grid(HEAT, RAIN, nrow=1)
  barplot[[dis]] <- plot_grid(title, p1, ncol=1, rel_heights = c(0.03, 1))
}


for(dis in diseases){
  png(file=paste0("./plots/s4_assoc/", dis, "_assoc.png"), width=8, height=11, units="in", res=300)
  print(barplot[[dis]])
  dev.off()
}



#s5 - other cc models
types <- c("all", "heat", "dry", "wet")
typedict <- c("all"="all extreme events", "wet"="extreme wet weather", 
              "dry"="extreme dry weather", "heat"="extreme heat")
diseases <- c("DF", "ENCEP", "INFLUENZA", "MALARIA", "PNEU", "LEPTO", "MELIOID")


for(dis in diseases){
  mirocplot <- annualERplotter(dis, types, ncol=1, "MIROC6")
  ipslplot <- annualERplotter(dis, types, ncol=1, "IPSL")
  cmccplot <- annualERplotter(dis, types, ncol=1, "CMCC")
  
  miroctitle <- ggdraw() + draw_label("MIROC6")
  ipsltitle <- ggdraw() + draw_label("IPSL-CM6A-LR")
  cmcctitle <- ggdraw() + draw_label("CMCC-ESM2")
  p <- plot_grid(miroctitle, ipsltitle, cmcctitle, mirocplot, ipslplot, cmccplot, vjust=0, ncol=3, nrow=2, rel_heights=c(0.02,1))
  
  png(file=paste0("./plots/s5_CCmodel/", dis, "-CCmodels.png"), width=12, height=10, pointsize=10, units="in", res=300)
  print(plot_grid(ggdraw()+draw_label(disdict[dis]), p, ncol=1, rel_heights=c(0.02,1)))
  dev.off()
}


##s6 - combination
type1 <- c("heat", "dryheat", "wetheat", "heatnodrynowet")
type2 <- c("dry", "dryheat", "drynoheat")
type3 <- c("wet", "wetheat", "wetnoheat")
types <- list(type1, type2, type3)
typedict <- c("dryheat" = "extreme heat \n & extreme dry weather", "wetheat"="extreme heat \n & extreme wet weather",
              "heat" = "extreme heat", "dry" = "extreme dry weather", "wet" = "extreme wet weather",
              "heatnodrynowet" = "extreme heat \n & normal dry/wet weather",
              "drynoheat" = "extreme dry weather \n & no extreme heat", "wetnoheat" = "extreme wet weather \n & no extreme heat")

for(j in 1:3){
  plotlist <- list()
  plotlabel <- list()
  title <- c(F, F, F, F, F, F, T)
  i <- 1
  ##add label of disease to plot
  for(dis in diseases){
    plotlist[[dis]] <- annualERsuppplotter(dis, types[[j]], ncol=length(types[[j]]), "MIROC6", title[i])
    plotlist[[dis]] <- plot_grid(ggdraw() + draw_label(disdict[dis], angle=90), plotlist[[dis]], rel_widths=c(0.02,1))
    i <- i + 1
  }
  
  png(file=paste0("./plots/S6_combi-", j ,".png"), width=12, height=18, units="in", res=300)
  print(plot_grid(plots=plotlist, ncol=1))
  dev.off()
}


# pdf(file="./plots/S5_combi.pdf", paper="a4r", width=11, height=9)
# print(plot_grid(plotlist[[1]], plotlist[[2]], plotlist[[3]], nrow=3))
# print(plot_grid(plotlist=plotlist[4:6], nrow=3))
# print(plot_grid(plotlist=plotlist[7], nrow=3))
# dev.off()


# for(dis in diseases){
#   png(file=paste0("./plots/s6_combi/", dis, "-combi.png"), width=11, height=9, units="in", res=300)
#   print(plotlist[[dis]])
#   dev.off()
# }

##projection plots with future population

types <- c("all", "heat", "dry", "wet")
typedict <- c("all"="all extreme events", "wet"="extreme wet weather", 
              "dry"="extreme dry weather", "heat"="extreme heat")
diseases <- c("DF", "ENCEP", "INFLUENZA", "MALARIA", "PNEU", "LEPTO", "MELIOID")
disdict <- c("DF"="Dengue", "ENCEP"= "Japanese Encephalitis", "INFLUENZA"="Influenza", "MALARIA"="Malaria",
             "PNEU"="Pneumonia", "LEPTO"="Leptospirosis", "MELIOID"="Melioidosis")

for(dis in diseases){
  annualERplot <- annualERplotter_pop(dis, types, ncol=2)
  title <- ggdraw() + draw_label(disdict[dis])
  png(filename=paste0("./plots/s5_popdensadded/", dis, "plot.png"), width=2400, height=1800, res=300)
  print(plot_grid(title, annualERplot, ncol=1, rel_heights = c(0.02, 1)))
  dev.off()
}

sfprojplotter_old("DF")

for(dis in diseases){
  sfprojplot <- sfprojplotter_old(dis)
  title <- ggdraw() + draw_label(disdict[dis])
  png(filename=paste0("./plots/s7_prov/", dis, "plot.png"), width=3400, height=2500, res=300)
  print(plot_grid(title, sfprojplot, ncol=1, rel_heights = c(0.02, 1)))
  dev.off()
}



