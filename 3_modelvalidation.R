library(mgcv)
library(tidyverse)
library(MASS)

#validation
folder <- "/Users/esthe/OneDrive - Nanyang Technological University/TH_extr_weat"
folder <- "/Users/estherchoo/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/TH_extr_weat"
setwd(folder)
set.seed(123)

#import data#
full_df <- read.csv("./data/finaldata.csv")

diseases <- colnames(df)[11:17]
provs <- unique(df$Province)

store_error <- matrix(nrow=length(provs)+1, ncol=length(diseases))
store_error_glm <- matrix(nrow=length(provs)+1, ncol=length(diseases))
rownames(store_error) <- c(provs, "median")
colnames(store_error) <- diseases
rownames(store_error_glm) <- c(provs, "median")
colnames(store_error_glm) <- diseases

#cross-validated mean absolute error (cases per year)
for(dis in diseases){
  load(paste0("./out/", dis, "_out.Rdata"))
  
  for(prov in provs){
    totalmae <- 0
    totalmaeglm <- 0
    
    for(y in 2017:2021){
      df <- filter(full_df, Year<y) #leave one-year-out cross validation
      df0 <- filter(full_df, Year==y)
      df_filt <- df %>% filter(Province==prov) #filter by prov
      df0_filt <- df0 %>% filter(Province==prov)
      
      pred <- round(predict.gam(store_model, df0_filt, type="response"))
      #predglm <- round(predict(store_glm, df0_filt, type="response"))
      totalmae <- totalmae + (sum(abs(pred - df0_filt[,dis])))
      #totalmaeglm <- totalmaeglm + (sum(abs(predglm-df0_filt[,dis])))
      
    }
    store_error[prov, dis] <- totalmae/5
    #store_error_glm[prov, dis] <- totalmaeglm/5
  }
  store_error["median", dis] <- median(store_error[1:76, dis])
  store_error_glm["median", dis] <- median(store_error_glm[1:76, dis])
}

write.csv(store_error, file="./out/tables/validation.csv")
write.csv(store_error_glm, file="./out/tables/glm_validation.csv")

##compare with mean cases
cases <- list()
for(i in colnames(df)[11:17]){ ##province lvl
  annual <- df %>%  
    group_by(Province) %>%
    dplyr::summarise(sum(get(i)))
  
  print(i)
  cases[[i]] <- annual[[2]]/18
  print(mean(annual[[2]]/18))
}

for(dis in diseases){
  plotdf <- as.data.frame(cases[[dis]])
  plotdf$Province <- annual$Province
  colnames(plotdf)[1] <- "actual"
  errordf <- data.frame(store_error[-77,])
  errordf$Province <- rownames(store_error[1:76,])
  errordf <- errordf[,c(dis, "Province")]
  plotdf <- merge(plotdf, errordf)
  colnames(plotdf)[2:3] <- c("Mean Cases", "Error")
  plotdf$Level <- "Mean Cases > Error"
  plotdf$Level[which(plotdf$Error > plotdf$`Mean Cases`)] <- "Error > Mean Cases"
  plotdf$`Difference` <- plotdf$Error - plotdf$`Mean Cases`
  plotdf <- arrange(plotdf, Difference, decreasing=T)
  plotdf$Province <- factor(plotdf$Province, levels=plotdf$Province)
  
  ggplot(plotdf, aes(x=Difference, y=Province, label=Province)) + 
    geom_bar(stat='identity', aes(fill=Level), width=.5)  +
    scale_fill_manual(name="",
                      values = c("Mean Cases > Error"="#00ba38", "Error > Mean Cases"="#f8766d")) + 
    labs(y="", title= disdict[dis]) +
    theme_bw() +
    theme(axis.line = element_blank())
  
  ggsave(paste0("./plots/s_errorbyprov/", dis, ".png"), device="png", units="px", width=2000, height=2500)
}

for(dis in diseases){
  plotdf <- as.data.frame(cases[[dis]])
  plotdf$Province <- annual$Province
  colnames(plotdf)[1] <- "actual"
  errordf <- data.frame(store_error[-77,])
  errordf$Province <- rownames(store_error[1:76,])
  errordf <- errordf[,c(dis, "Province")]
  plotdf <- merge(plotdf, errordf)
  colnames(plotdf)[2:3] <- c("Mean Cases", "Error")
  plotdf$Level <- "Mean Cases > Error"
  plotdf$Level[which(plotdf$Error > plotdf$`Mean Cases`)] <- "Error > Mean Cases"
  plotdf <- pivot_longer(plotdf, c("Mean Cases", "Error"), names_to="Type")
  levels <- plotdf %>% 
    filter(Type=="Error") %>%
    arrange(value, decreasing=T)
  levels <- levels$Province
  plotdf$Province <- factor(plotdf$Province, levels=levels)
  
  
  ggplot(plotdf) + 
    geom_col(aes(y=Province, x=value, fill=Type), position="dodge") +
    labs(y="", x="Cases", title= disdict[dis]) +
    theme_bw() +
    theme(axis.line = element_blank()) +
    scale_fill_manual(name="", values = c("Mean Cases"="royalblue", "Error"="#f8766d"))
  
  ggsave(paste0("./plots/s_errorbyprov/1-", dis, ".png"), device="png", units="px", width=2000, height=2500)
}  


# 
# 
# 
# 
# ggplot(plotdf) + 
#   geom_col(aes(y=Province, x=Error), fill="brown3") +
#   geom_point(aes(y=Province, x=`Mean Cases`), colour="steelblue") +
#   geom_segment(aes(y=Province, x=Error, xend=`Mean Cases`, group=Province, colour=Level)) +
#   scale_colour_manual(values=c("Error > Mean Cases"="black", "Mean Cases > Error" = "darkgreen"))
# 
# ggplot(plotdf)+
#   geom_col(aes(y=Province, x=value, fill=Type), position="dodge")
# 
# ggplot(plotdf) +
#   geom_point(aes(y=Province, x=value, colour=Type))  +
#   geom_line(aes(y=Province, x=value, group=Province, color=Level)) +
#   scale_colour_manual(values=c("Error > Mean Cases"="firebrick", "Mean Cases > Error" = "lightblue",
#                                "Mean Cases" = "lightblue", "Error" = "firebrick"))



###mae of model with population density
#prob can leave this out since we now include population in model
# store_error_pop <- matrix(nrow=length(provs)+1, ncol=length(diseases))
# rownames(store_error_pop) <- c(provs, "median")
# colnames(store_error_pop) <- diseases
# 
# for(dis in diseases){
#   load(paste0("./out/withpop/", dis, "_out.Rdata"))
#   
#   for(prov in provs){
#     totalmae <- 0
#     
#     for(y in 2017:2021){
#       df <- filter(full_df, Year<y) #leave one-year-out cross validation
#       df0 <- filter(full_df, Year==y)
#       df_filt <- df %>% filter(Province==prov) #filter by prov
#       df0_filt <- df0 %>% filter(Province==prov)
#       
#       pred <- round(predict.gam(store_model, df0_filt, type="response"))
#       predglm <- round(predict(store_glm, df0_filt, type="response"))
#       
#       totalmae <- totalmae + (sum(abs(pred - df0_filt[,dis])))
# 
#     }
#     store_error_pop[prov, dis] <- totalmae/5
# 
#   }
#   store_error_pop["median", dis] <- median(store_error_pop[1:76, dis])
# }
# 
# write.csv(store_error_pop, file="./out/tables/pop_validation.csv")

