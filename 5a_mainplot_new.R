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

png("./plots/3_overallER.png", width=18, height=11, units="in", res=300)
print(plot1_overallER("MIROC6"))
dev.off()

png("./plots/CMCC_overallER.png", width=18, height=11, units="in", res=300)
print(plot1_overallER("CMCC"))
dev.off()

png("./plots/IPSL_overallER.png", width=18, height=11, units="in", res=300)
print(plot1_overallER("IPSL"))
dev.off()

plot1_overallER <- function(model){
  load(paste0("./out/", model, "_allexcessrisk.Rdata"))
  
  disdict <- c("DF"="Dengue", "ENCEP"= "JEV", "INFLUENZA"="Influenza", "MALARIA"="Malaria",
               "PNEU"="Pneumonia", "LEPTO"="Leptospirosis", "MELIOID"="Melioidosis")
  
  
  ER <- matrix(nrow=0, ncol=6)
  i <- 1
  for(df in excessrisk){
    df <- cbind(df, disease=names(excessrisk)[i])
    ER <- rbind(ER, df)
    i <- i + 1
  }
  
  ER$disease <- unlist(lapply(strsplit(ER$disease, "_"), "[", 1))
  ER$disease <- disdict[ER$disease]

  ER$label <- paste0(round(ER$pred, 2), "%")
  ind <- which(ER$Period=="2021-2040")
  ER$label[ind] <- paste0(ER$disease[ind], ", " ,round(ER$pred[ind], 2), "%")
  
  ER$Period[which(ER$Period=="2021-2040")] <- "'21-'40"
  ER$Period[which(ER$Period=="2041-2060")] <- "'41-'60"
  ER$Period[which(ER$Period=="2061-2080")] <- "'61-'80"
  ER$Period[which(ER$Period=="2081-2100")] <- "'81-'00"

  ER$Period <- as.factor(ER$Period)
  
  ER$sig <- "Significant"
  ER[which(ER$predLo < 0 & ER$predHi > 0), "sig"] <- "Non-Significant"
  ER$type <- ""
  ER$type[which(ER$disease=="JEV"|ER$disease=="Dengue"|ER$disease=="Malaria")] <- "Vector-Borne Disease"
  ER$type[which(ER$disease=="Influenza"|ER$disease=="Pneumonia")] <- "Air-Borne Disease"
  ER$type[which(ER$disease=="Melioidosis"|ER$disease=="Leptospirosis")] <- "Water-Borne Disease"
  
  # ER$SSP[which(ER$SSP=="ssp126")] <- "SSP126"
  # ER$SSP[which(ER$SSP=="ssp245")] <- "SSP245"
  # ER$SSP[which(ER$SSP=="ssp370")] <- "SSP370"
  # ER$SSP[which(ER$SSP=="ssp585")] <- "SSP585"
  

  
  df126 <- filter(ER, SSP=="ssp126")
  df245 <- filter(ER, SSP=="ssp245")
  df370 <- filter(ER, SSP=="ssp370")
  df585 <- filter(ER, SSP=="ssp585")

  p1 <- ggplot(df126) +
    facet_grid(rows=vars(type)) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=0, ymax=Inf, fill="tomato", alpha=0.01) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=-Inf, ymax=0, fill="skyblue", alpha=0.01) +
    geom_vline(xintercept=unique(df126$Period), colour="gray50") +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_point(aes(x=Period, y=pred, colour=disease, shape=sig), size=4, stroke=2, alpha=0.8) +
    geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.7) + 
    geom_label_repel(aes(label=label, x=Period, y=pred, colour=disease), segment.color = NA, force=2.5, alpha=0.9, nudge_x=0.2, fontface="bold", direction="y", size=5, show.legend=F) +
    #geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.4) +
    theme_bw() +
    theme(strip.background =element_blank(),
          strip.text = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.title=element_blank(),
          legend.position="bottom",
          legend.key.size=unit(1.5,"cm"),
          plot.margin=unit(c(1,0,1,1), 'lines'),
          axis.text= element_text(size=14),
          axis.title = element_text(size=16),
          legend.text=element_text(size=14)) +
    labs(x=paste("SSP126"), y="Excess Risk (%)") +
    scale_colour_manual(values=brewer.pal(7, "Dark2")) +
    scale_shape_manual(values=c("Significant"=15, "Non-Significant"=1)) +
    scale_y_continuous(trans = "pseudo_log", breaks=c(-20,-10,-5,-2.5,0,2.5,5, 10))
    
  
  p2 <- ggplot(df245) +
    facet_grid(rows=vars(type)) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=0, ymax=Inf, fill="tomato", alpha=0.01) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=-Inf, ymax=0, fill="skyblue", alpha=0.01) +
    geom_vline(xintercept=unique(df126$Period), colour="gray50") +
    geom_hline(yintercept=0, linetype="dashed") +
    #geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.8) + 
    geom_point(aes(x=Period, y=pred, colour=disease, shape=sig), size=4, stroke=2, alpha=0.8) +
    geom_label_repel(aes(label=label, x=Period, y=pred, colour=disease), segment.color = NA, force=2.5, alpha=0.9,nudge_x=0.2, fontface="bold", direction="y", size=5, show.legend=F) +
    geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.4) +
    theme_bw() +
    theme(strip.background =element_blank(),
          strip.text = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.margin=unit(c(1,0,1,0), 'lines'),
          legend.position="none",
          axis.title = element_text(size=16),
          axis.text= element_text(size=14)) +
    labs(x=paste("SSP245"), y="") +
    scale_colour_manual(values=brewer.pal(7, "Dark2")) +
    scale_shape_manual(values=c("Significant"=15, "Non-Significant"=1)) +
    scale_y_continuous(trans = "pseudo_log", breaks=c(-20, -10,-5,-2.5,0,2.5,5, 10, 20))
    #scale_y_continuous(breaks=seq(-25,30, 10), limits=c(-26, 30))
  
  p3 <- ggplot(df370) +
    facet_grid(rows=vars(type)) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=0, ymax=Inf, fill="tomato", alpha=0.01) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=-Inf, ymax=0, fill="skyblue", alpha=0.01) +
    geom_vline(xintercept=unique(df126$Period), colour="gray50") +
    geom_hline(yintercept=0, linetype="dashed") +
    #geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.8) + 
    geom_point(aes(x=Period, y=pred, colour=disease, shape=sig), size=4, stroke=2, alpha=0.8) +
    geom_label_repel(aes(label=label, x=Period, y=pred, colour=disease), segment.color = NA, force=2.5, alpha=0.9, nudge_x=0.2, fontface="bold", direction="y", size=5, show.legend=F) +
    geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.4) +
    theme_bw() +
    theme(strip.background =element_blank(),
          strip.text = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none",
          plot.margin=unit(c(1,0,1,0), 'lines'),
          axis.title = element_text(size=16),
          axis.text= element_text(size=14)) +
    labs(x=paste("SSP370"), y="") +
    scale_colour_manual(values=brewer.pal(7, "Dark2")) +
    scale_shape_manual(values=c("Significant"=15, "Non-Significant"=1)) +
    scale_y_continuous(trans ="pseudo_log", breaks=c(-20, -10,-5,-2.5,0,2.5,5, 10, 20))
    #scale_y_continuous(breaks=seq(-25,30, 10), limits=c(-26, 30))
  
  p4 <- ggplot(df585) +
    facet_grid(rows=vars(type)) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=0, ymax=Inf, fill="tomato", alpha=0.01) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=-Inf, ymax=0, fill="skyblue", alpha=0.01) +
    geom_vline(xintercept=unique(df126$Period), colour="gray50") +
    geom_hline(yintercept=0, linetype="dashed") +
    #geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.8) + 
    geom_point(aes(x=Period, y=pred, colour=disease, shape=sig), size=4, stroke=2, alpha=0.8) +
    geom_label_repel(aes(label=label, x=Period, y=pred, colour=disease), segment.color = NA, force=2.5, alpha=0.9, nudge_x=0.2, fontface="bold", direction="y", size=5, show.legend=F) +
    geom_line(aes(x=Period, y=pred, colour=disease, group=disease), lwd=1, alpha=0.4) +
    theme_bw() +
    theme(strip.background =element_rect(fill="grey90"),
          strip.text = element_text(size = 16, face = "bold"),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.margin=unit(c(1,1,1,0), 'lines'),
          legend.position="none",
          axis.title = element_text(size=16),
          axis.text= element_text(size=14)) +
    labs(x=paste("SSP585"), y="") +
    scale_colour_manual(values=brewer.pal(7, "Dark2")) +
    scale_shape_manual(values=c("Significant"=15, "Non-Significant"=1)) +
    scale_y_continuous(trans = "pseudo_log", breaks=c(-20, -10,-5,-2.5,0,2.5,5, 10, 20))
    #scale_y_continuous(breaks=seq(-25,30, 10), limits=c(-26, 30))
  
  leg <- ggpubr::get_legend(p1)
  plots <- plot_grid(p1+theme(legend.position="none"), p2, p3, p4, nrow=1)
  finalplot <- plot_grid(plots, leg, nrow=2, rel_heights=c(1,0.15))
  
  return(finalplot)
}

plot2_ERweather <- function(model){
  ###prepare dataframe###
  types <- c("heat", "dry", "wet")
  
  disdict <- c("DF"="Dengue", "ENCEP"= "JEV", "INFLUENZA"="Influenza", "MALARIA"="Malaria",
               "PNEU"="Pneumonia", "LEPTO"="Leptospirosis", "MELIOID"="Melioidosis")
  
  finalER <- matrix(nrow=0, ncol=6)
  for(type in types){
    load(paste0("./out/", model, "_", type, "excessrisk.Rdata"))
    
    i <- 1
    ER <- matrix(nrow=0, ncol=6)
    for(df in excessrisk){
      df <- cbind(df, disease=names(excessrisk)[i])
      ER <- rbind(ER, df)
      i <- i + 1
    }
    
    ER$type <- type
    
    ER$disease <- unlist(lapply(strsplit(ER$disease, "_"), "[", 1))
    ER$disease <- disdict[ER$disease]
    
    ER$Period[which(ER$Period=="2021-2040")] <- "'21-'40"
    ER$Period[which(ER$Period=="2041-2060")] <- "'41-'60"
    ER$Period[which(ER$Period=="2061-2080")] <- "'61-'80"
    ER$Period[which(ER$Period=="2081-2100")] <- "'81-'00"
    
    ER$Period <- as.factor(ER$Period)
    
    ER$sig <- "Significant"
    ER[which(ER$predLo < 0 & ER$predHi > 0), "sig"] <- "Non-Significant"
  
    
    finalER <- rbind(finalER, ER)
  }
    
  finalER$SSP <- gsub(pattern="ssp", replacement="SSP", finalER$SSP)

  plotlist1 <- list()
  for(dis in c("Dengue", "JEV", "Malaria")){
    for(ty in c("heat", "dry", "wet")){
      plotlist1[[paste0(dis, ty)]] <- local({
        test <- dplyr::filter(finalER, disease==dis)
        test <- dplyr::filter(test, type==ty)
        ggplot(test) +
          facet_wrap(~SSP, strip.position="bottom", nrow=1) +
          geom_point(aes(x=Period, y=pred, colour=SSP, shape=sig), size=5) +
          geom_line(aes(x=Period, y=pred, colour=SSP, group=SSP)) +
          geom_errorbar(aes(x=Period, ymin=predLo, y=pred, ymax=predHi, colour=SSP), width=0.5, linewidth=1) +
          geom_hline(yintercept=0, linetype="dashed", colour="gray50") +
          labs(x="", y="ER (%)", title=paste0(dis, " (Extreme ", ty, ")")) +
          scale_shape_manual(values=c("Significant"=16, "Non-Significant"=1)) +
          theme_classic()+
          theme(plot.background = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.spacing = unit(0, "cm"),
                strip.placement="outside",
                strip.background = element_blank(),
                strip.text = element_text(size = 16, face = "bold"),
                legend.position="none",
                axis.title = element_text(size=16),
                axis.text.x = element_text(angle = 45, hjust=1),
                plot.title=element_text(size=16),
                axis.text= element_text(size=13),
                legend.text=element_text(size=14),
                legend.title=element_blank())
      })
    #  }

    }
  }
  
  finalplot1 <- ggpubr::ggarrange(plotlist=plotlist1, nrow=3, ncol=3, common.legend=T, labels="AUTO")
  
  return(finalplot1)
  
}

png("./plots/4_ERweather-vect.png", width=19, height=11, units="in", res=300)
print(plot2_ERweather("MIROC6"))
dev.off()

png("./plots/CMCC_ERweather-vect.png", width=19, height=11, units="in", res=300)
print(plot2_ERweather("CMCC"))
dev.off()

png("./plots/IPSL_ERweather-vect.png", width=19, height=11, units="in", res=300)
print(plot2_ERweather("IPSL"))
dev.off()

plot3_ERweather <- function(model){
  ###prepare dataframe###
  types <- c("heat", "dry", "wet")
  
  disdict <- c("DF"="Dengue", "ENCEP"= "JEV", "INFLUENZA"="Influenza", "MALARIA"="Malaria",
               "PNEU"="Pneumonia", "LEPTO"="Leptospirosis", "MELIOID"="Melioidosis")
  
  finalER <- matrix(nrow=0, ncol=6)
  for(type in types){
    load(paste0("./out/", model, "_", type, "excessrisk.Rdata"))
    
    i <- 1
    ER <- matrix(nrow=0, ncol=6)
    for(df in excessrisk){
      df <- cbind(df, disease=names(excessrisk)[i])
      ER <- rbind(ER, df)
      i <- i + 1
    }
    
    ER$type <- type
    
    ER$disease <- unlist(lapply(strsplit(ER$disease, "_"), "[", 1))
    ER$disease <- disdict[ER$disease]
    
    ER$Period[which(ER$Period=="2021-2040")] <- "'21-'40"
    ER$Period[which(ER$Period=="2041-2060")] <- "'41-'60"
    ER$Period[which(ER$Period=="2061-2080")] <- "'61-'80"
    ER$Period[which(ER$Period=="2081-2100")] <- "'81-'00"
    
    ER$Period <- as.factor(ER$Period)
    
    ER$sig <- "Significant"
    ER[which(ER$predLo < 0 & ER$predHi > 0), "sig"] <- "Non-Significant"
    
    
    finalER <- rbind(finalER, ER)
  }
  
  finalER$SSP <- gsub(pattern="ssp", replacement="SSP", finalER$SSP)
  
  plotlist2 <- list()
  for(dis in c("Influenza", "Pneumonia", "Leptospirosis", "Melioidosis")){
    for(ty in c("heat", "dry", "wet")){
      plotlist2[[paste0(dis, ty)]] <- local({
        test <- dplyr::filter(finalER, disease==dis)
        test <- dplyr::filter(test, type==ty)
        ggplot(test) +
          facet_wrap(~SSP, strip.position="bottom", nrow=1) +
          geom_point(aes(x=Period, y=pred, colour=SSP, shape=sig), size=5) +
          geom_line(aes(x=Period, y=pred, colour=SSP, group=SSP)) +
          geom_errorbar(aes(x=Period, ymin=predLo, y=pred, ymax=predHi, colour=SSP), width=0.5, linewidth=1) +
          geom_hline(yintercept=0, linetype="dashed", colour="gray50") +
          labs(x="", y="ER (%)", title=paste0(dis, " (Extreme ", ty, ")")) +
          scale_shape_manual(values=c("Significant"=16, "Non-Significant"=1)) +
          theme_classic()+
          theme(plot.background = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.spacing = unit(0, "cm"),
                strip.placement="outside",
                strip.background = element_blank(),
                strip.text = element_text(size = 16, face = "bold"),
                axis.title = element_text(size=16),
                axis.text.x = element_text(angle = 45, hjust=1),
                plot.title=element_text(size=16),
                axis.text= element_text(size=13),
                legend.text=element_text(size=14),
                legend.title=element_blank())
      })
      #  }
      
    }
  }
  
  test <- dplyr::filter(finalER, disease=="Influenza")
  test <- dplyr::filter(test, type=="wet")
  leg <- get_legend(ggplot(test) +
               facet_wrap(~SSP, strip.position="bottom", nrow=1) +
               geom_point(aes(x=Period, y=pred, colour=SSP, shape=sig), size=5) +
               geom_line(aes(x=Period, y=pred, colour=SSP, group=SSP)) +
               geom_errorbar(aes(x=Period, ymin=predLo, y=pred, ymax=predHi, colour=SSP), width=0.5, linewidth=1) +
               labs(x="", y="ER (%)", title=paste0(dis, " (Extreme ", ty, ")")) +
               scale_shape_manual(values=c("Significant"=16, "Non-Significant"=1)) +
                 theme_bw() +
              theme(legend.title=element_blank(),
                    legend.text=element_text(size=14),
                    legend.position="top")
  )
  
  finalplot2 <- ggpubr::ggarrange(plotlist=plotlist2, nrow=4, ncol=3, common.legend=T, labels="AUTO")
  
  return(finalplot2)
  
}

png("./plots/5_ERweather-air-food.png", width=19, height=15, units="in", res=300)
print(plot3_ERweather("MIROC6"))
dev.off()

png("./plots/CMCC_ERweather-air-food.png", width=19, height=15, units="in", res=300)
print(plot3_ERweather("CMCC"))
dev.off()

png("./plots/IPSL_ERweather-air-food.png", width=19, height=15, units="in", res=300)
print(plot3_ERweather("IPSL"))
dev.off()

plot4 <- function(dis, ssp){
  proj <- read.csv(paste0("./out/", dis, "_MIROC6_allCCpred.csv"))
  
  annualproj <- proj %>% 
    filter(SSP==ssp) %>%
    group_by(Period, Province) %>%
    dplyr::summarise(across(pred:baselineLo, sum))
  
  annualer <- annualproj %>% 
    group_by(Period, Province) %>%
    mutate(`Excess Risk (%)`=(pred-baseline)/baseline * 100)
  
  #select top 10 provinces with highest incidence historical
  df <- read.csv("./data/finaldata.csv")
  top <- df %>%
    group_by(Province) %>%
    summarise(monthlydf=mean(get(dis))) %>%
    arrange(desc(monthlydf))
  
  annualer <- filter(annualer, Province %in% top$Province[1:10])
  annualer$label <- paste0(annualer$Province, ", " ,round(annualer$`Excess Risk (%)`, 2), "%")
  
  annualer$sig <- "Significant"
  erLo <- (annualer$predLo-annualer$baseline)/annualer$baseline
  erHi <- (annualer$predHi-annualer$baseline)/annualer$baseline
  annualer[which(erLo < 0 & erHi > 0), "sig"] <- "Non-Significant"
  #annualer[which(is.na(annualer$er)), "sig"] <- "Significant"
  
  points <- ggplot(annualer) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=0, ymax=Inf, fill="tomato", alpha=0.01) +
    geom_rect(xmin=-Inf,xmax=Inf, ymin=-Inf, ymax=0, fill="skyblue", alpha=0.01) +
    geom_vline(xintercept=unique(annualer$Period), colour="gray50") +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_point(aes(x=Period, y=`Excess Risk (%)`, colour=sig), size=3, stroke=2, alpha=0.8) +
    geom_label_repel(aes(label=label, x=Period, y=`Excess Risk (%)`, colour=sig), segment.color = NA, force=2.5, alpha=0.9, nudge_x=0.2, fontface="bold", direction="y", size=5, show.legend=F) +
    geom_line(aes(x=Period, y=`Excess Risk (%)`, group=Province), lwd=1, alpha=0.4, colour="royalblue") + 
    theme_bw() +
    scale_colour_manual(values=c("Significant"="brown", "Non-Significant"="Black")) +
    scale_y_continuous(n.breaks=8) +
    labs(title=toupper(ssp)) +
    theme(strip.background =element_blank(),
          strip.text = element_blank(),
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.title=element_blank(),
          legend.position="top",
          axis.text= element_text(size=14),
          axis.title = element_text(size=20),
          plot.title=element_text(size=16),
          legend.text=element_text(size=16))

  maps <- sfprojplotter(dis, ssp)

  finalplot4 <- plot_grid(points, maps, nrow=2, rel_widths=c(1,1.2), labels="AUTO")
}

png("./plots/4_dengue.png", width=19, height=15, units="in", res=300)
print(plot4("DF", "ssp245"))
dev.off()

png("./plots/test_5_influenza.png", width=19, height=15, units="in", res=300)
print(plot4("INFLUENZA", "ssp245"))
dev.off()





