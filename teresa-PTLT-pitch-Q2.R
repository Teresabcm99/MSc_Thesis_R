library("eyetrackingR")
library("eyelinker")
library('readxl')
#library("xlsx")
library("edfReader")
library('imager')
library("Matrix")
library("lme4")
library("ggplot2")
library("ggpubr")
library("png")
library("tidyverse")
library("jpeg")
library("grid")
library("seewave")
library("cowplot")
library("gridExtra")
library("reshape2")
library("RColorBrewer")
library("stringr")
library("Hmisc")
library(lubridate)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(reshape2)
library(dbplyr)
library(latticeExtra)
library(tidyr)
library(patchwork)
library(readxl)
library(matrixStats)

datapath_PI_1="insert datapath"
datapath_PI_3="insert datapath"
datapath_PI_4="insert datapath"
datapath_PI_5="insert datapath"
datapath_PI_6="insert datapath"
datapath_PI_7="insert datapath"
datapath_PI_8="insert datapath"
datapath_PH_10="insert datapath"
datapath_PH_6="insert datapath"
datapath_PH_9="insert datapath"
datapth_PH_11="insert datapath"
datapth_PH_8="insert datapath"
datapath_PH_13="insert datapath"

#Once data is loaded, create list in order to run all of the files at 1

Path_PI<- list(datapath_PI_1, datapath_PI_3,datapath_PI_4,datapath_PI_5, datapath_PI_6, datapath_PI_7,datapath_PI_8)
Path_PH<-list(datapath_PH_10,datapath_PH_6, datapath_PH_9,datapth_PH_11, datapth_PH_8,datapath_PH_13)
all<-append(Path_PI, Path_PH)

#order of presentation, versions (in order to match participants)

names = c('PI01', 'PI03', 'PI04', 'PI05', 'PI06', 'PI07', 'PI08','PH10','PH06', 'PH09', 'PH11','PH08', 'PH13')
versions = c(1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2)


titles_1 = c("SPA2", "Vsq1", "SPA1", "Vsq2")
titles_2 = c("Vsq1", "SPA2", "Vsq2", "SPA1")


df <- read.csv("datapath",header=TRUE, sep = ";") #; to read separated by comas file
# Version 2 means 1st video AV es: [2] VSq1, [4] SPA2, [6] Vsq2, [8] SPA1
# Version 1 means: [2] SPA2, [4] VSq1, [6] SPA1, [8] Vsq2


#ROI
xleft= 310
xright=700
ybottom=670
ytop=480


e_xleft=310
e_xright=700
e_ybottom=480
e_ytop=280
p =0 
PLT_group <- data.frame(p)

for (k in 1:length(all)){
  
  data<-read.asc(all[[k]],samples= TRUE, events = TRUE, parse_all =  FALSE)
  
  
  # name=substring(all[13], 71, 78)
 #taking vector time from the .asci files
  
  times=data$raw$time 
  times_AD_S = times[data$raw$block%in% c(2,4,6,8)] # Takes only audio-visual trials
  
  # Hacer que empiece el vector desde uno
  times_AD_S = times_AD_S - times_AD_S[1] +1;
  
  x=data$raw$xp
  
  y=data$raw$yp


  mouth_eyes=data.frame(x1=c(xleft,e_xleft),x2=c(xright,e_xright), y1=c(ybottom,e_ybottom), y2=c(ytop,e_ytop))
  
  #Creating dataframe
  PLT_Value<-c("")
  StartTime=0
  StartTime_local=0
  EndTime<-c("")
  Bloque<-c("")
  blo <- c("")
  # rm(PLTs)
  
  PLT_all<-data.frame(PLT_Value, Bloque)
  PLTs<-data.frame(PLT_Value)
  PLT_ordered = matrix()
  
  

  
  # Hacer que empiece el vector desde uno,
  
  # Go block by block
  times = data$raw$time 
  times = times - times[1]+1 # To ensure each block starts with 1, as this is the index we use to go through the x and y axes
  block = times
  
# to divide each bock into two milisecond segments
  
  for (i in seq(2, 8, by = 2)){
    block = times[data$raw$block %in% i]
    #print(i)
    #print(block)
    x_block = data$raw$xp[data$raw$block %in% i]
    y_block = data$raw$yp[data$raw$block %in% i]
    #plot(x_block, y_block, xlim = c(0, 1110), ylim = c(0, 950))
    
    # To store data from participants with info on pitch according to order of presentation #remember same rows and columns for rbind
    if(versions[k]==1){
      
      # SPA 1= name vid
      # SPA 2 = name vid2
      # VSQ 1 = name vid
      # VSQ 2= name vid 2
      pitch_range = rbind(df$`Spanish 2 Pitch Range`,  df$`Basque 1 Pitch Range`, df$`Spanish 1 Pitch Range`, df$`Basque 2 Pitch Range`)
      pitch_height = rbind(df$`Spanish 2 Pitch Height`,  df$`Basque 1 Pitch Height`, df$`Spanish1 Pitch Height`, df$`Basque2 Pitch Height`)
    }else{
      
      # titles_pitch = c("Basque 1", "Spanish 2", "Basque 2", "Spanish 1")
      pitch_range = rbind(df$`Basque 1 Pitch Range`, df$`Spanish 2 Pitch Range`, df$`Basque 2 Pitch Range`,  df$`Spanish 1 Pitch Range`)
      pitch_height = rbind(df$`Basque 1 Pitch Height`,df$`Spanish 2 Pitch Height`,   df$`Basque2 Pitch Height`, df$`Spanish1 Pitch Height`)
    }
    
    for (j in seq(1, length(block)-1000, by = 1000)){
      
      #calcula_PLT_3(times_AD_Sq[t0], times_AD_S[t1])
      #print(times_AD_S[j])
      t0 = block[j]
      print(t0)
      t1 = block[j+1000]
      
      t0_local = j
      t1_local = j + 1000;
      
      xwindow=x_block[t0_local:t1_local] #defining vectors, according to space & time
      ywindow = y_block[t0_local:t1_local]
      
      #plot(xwindow, ywindow, xlim = c(0, 1110), ylim = c(0, 950))
      
     
      
      mouth = xwindow<xright & xwindow>xleft & ywindow> ytop & ywindow< ybottom
      mouth[mouth == TRUE] = 1
      Def_mouth = sum(mouth,na.rm=TRUE)
      #print(Def_mouth)
      
      #eyes
      eyes= ywindow >e_xleft &  ywindow< e_xright & ywindow> e_ytop & ywindow < e_ybottom 
      eyes[eyes==TRUE]=1
      Def_eyes= sum(eyes,na.rm=TRUE)
      #print(Def_eyes)
      
      #PTLT our var is the mouth, so mouth against total
      Total = sum(Def_mouth + Def_eyes)
      
      plt= sum(Def_mouth)/Total
      #print(plt)
      
      if(i == 2 && j == 1){
        StartTime = t0/1000;
        StartTime_local = 0;
        PLTs<-plt;
        blo = i;
      }else{
        if (j == 1){
          StartTime_local = rbind(StartTime_local, 0);
        }else{
          StartTime_local = rbind(StartTime_local, t0_local/1000); 
        }
        StartTime = rbind(StartTime, t0/1000); 
        PLTs<-rbind(PLTs,plt)
        #print(PLTs)
        blo = rbind(blo, i);
      }
      
      #print(PLTs)
      # plot(PLTs)
    }
  }
  
  trial_title = blo;
  if(versions[k]==1){
    title_order = titles_1;
    trial_title[trial_title==2] = titles_1[1]
    trial_title[trial_title==4] = titles_1[2]
    trial_title[trial_title==6] = titles_1[3]
    trial_title[trial_title==8] = titles_1[4]
  }else{
    title_order = titles_2;
    trial_title[trial_title==2] = titles_2[1]
    trial_title[trial_title==4] = titles_2[2]
    trial_title[trial_title==6] = titles_2[3]
    trial_title[trial_title==8] = titles_2[4]
  }
  
  #print(versions[k])  
  # Get the collapsed PLTs by trial: 
  trial_2 = which(trial_title == "Vsq1")
  #print(trial_2)
  trial_4 = which(trial_title == "Vsq2")
  #print(trial_4)
  trial_6 = which(trial_title == "SPA1")
  #print(trial_6)
  trial_8 = which(trial_title == "SPA2")
  #print(trial_8)
  
  PLT_ordered[1:15] = PLTs[trial_2]
  PLT_ordered[16:30] = PLTs[trial_4]
  PLT_ordered[31:45] = PLTs[trial_6]
  PLT_ordered[46:64] = PLTs[trial_8]
  
  PLT_group_or <- cbind(PLT_group, PLT_ordered)
  PLT_group <- cbind(PLT_group, PLTs)
  #print(PLT_group)
  PLT_all<-data.frame(StartTime_local, StartTime, PLT_group, blo, trial_title)
 
  
  PLT_all$PLT_Value[PLT_all$PLTs==NaN] = NA
  
  # PLT_subset = subset(PLT_all, !is.na(PLT_all))
  # To see it clearly without NA values:
  # PLT_subset = drop_na(PLT_all)
  
  # }
}  
  


# For plotting all subjects' data, better without the drop_na

# rows from [1:15] = (VSQ 1)
# rows from  [16:30] =  (VSQ 2)
# rows from  [31:45] =  (SPA 1)
# rows from  [46:60] =  (SPA 2)

#data visualzation of all of the videos plotting accroding to mean of group, and sd deviation of group

egunon = 1:15
eguzkia = 16:30
buenos_dias = 31:45
salio_sol = 46:60

HL = 2:8
TH = 9:14

xx = subset(PLT_all$StartTime_local, blo == 2)

PLT_group_n <- as.numeric(unlist(PLT_group))

df_egunon <- data.frame(Mean_HI = rowMeans(PLT_group_or[egunon,HI], na.rm = TRUE), Mean_NH = rowMeans(PLT_group_or[egunon,NH], na.rm = TRUE), sd_HI = rowSds(as.matrix(PLT_group_or[egunon, HI]), na.rm = TRUE), sd_NH = rowSds(as.matrix(PLT_group_or[egunon, NH]), na.rm=TRUE))
df_eguzkia <- data.frame(Mean_HI = rowMeans(PLT_group_or[eguzkia,HI], na.rm = TRUE), Mean_NH = rowMeans(PLT_group_or[eguzkia,NH], na.rm = TRUE), sd_HI = rowSds(as.matrix(PLT_group_or[eguzkia, HI]), na.rm = TRUE), sd_NH = rowSds(as.matrix(PLT_group_or[eguzkia, NH]), na.rm=TRUE))
df_buenos_dias <- data.frame(Mean_HI = rowMeans(PLT_group_or[buenos_dias,HI], na.rm = TRUE), Mean_NH = rowMeans(PLT_group_or[buenos_dias,NH], na.rm = TRUE), sd_HI = rowSds(as.matrix(PLT_group_or[buenos_dias, HI]), na.rm = TRUE), sd_NH = rowSds(as.matrix(PLT_group_or[buenos_dias, NH]), na.rm=TRUE))
df_salio_sol <- data.frame(Mean_HI = rowMeans(PLT_group_or[salio_sol,HI], na.rm = TRUE), Mean_NH = rowMeans(PLT_group_or[salio_sol,NH], na.rm = TRUE), sd_HI = rowSds(as.matrix(PLT_group_or[salio_sol, HI]), na.rm = TRUE), sd_NH = rowSds(as.matrix(PLT_group_or[salio_sol, NH]), na.rm=TRUE))



g2 <- ggplot() +
  geom_path(data = df_egunon,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  + 
  geom_ribbon(aes(ymin=df_egunon$Mean_HI-(df_egunon$sd_HI/2), ymax=df_egunon$Mean_HI+(df_egunon$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_egunon,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_egunon$Mean_NH-(df_egunon$sd_NH/2), ymax=df_egunon$Mean_NH+(df_egunon$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "VSQ 1", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.1.pitch.range`/max(df$`Basque.1.pitch.height`), color = "Pitch range"),size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.1.pitch.height`/max(df$`Basque.1.pitch.height`), color = "Pitch height"), size=1, linetype = "solid") + 
  theme_classic() + labs(title = "VSQ 1", x = "Time", y = "Proportion of looks to the mouth")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Basque.1.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g2)

g4 <- ggplot() +
  geom_path(data = df_eguzkia,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_eguzkia$Mean_HI-(df_eguzkia$sd_HI/2), ymax=df_eguzkia$Mean_HI+(df_eguzkia$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_eguzkia,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_eguzkia$Mean_NH-(df_eguzkia$sd_NH/2), ymax=df_eguzkia$Mean_NH+(df_eguzkia$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "VSQ2", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.2.pitch.range`/max(df$`Basque.2.pitch.height`), color = "Pitch range"), size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.2.pitch.height`/max(df$`Basque.2.pitch.height`), color = "Pitch height" ),size =1, linetype = "solid") + 
  theme_classic() + labs(title = "VSQ 2", x = "Time", y = "Proportion of looks to the mouth")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Basque.2.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g4)


g6 <- ggplot() +
  geom_path(data = df_buenos_dias,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_buenos_dias$Mean_HI-(df_buenos_dias$sd_HI/2), ymax=df_buenos_dias$Mean_HI+(df_buenos_dias$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_buenos_dias,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_buenos_dias$Mean_NH-(df_buenos_dias$sd_NH/2), ymax=df_buenos_dias$Mean_NH+(df_buenos_dias$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "SPA1", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.1.pitch.range`/max(df$`Spanish.1.pitch.height`), color = "Pitch range"), size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.1.pitch.height`/max(df$`Spanish.1.pitch.height`), color = "Pitch height"), size=1, linetype = "solid",) + 
  theme_classic() + labs(title = "SPA   1", x = "Time", y = "Proportion of looks to the mouth")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Spanish.1.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g6)



g8 <- ggplot() +
  geom_path(data = df_salio_sol,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_salio_sol$Mean_HI-(df_salio_sol$sd_HI/2), ymax=df_salio_sol$Mean_HI+(df_salio_sol$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_salio_sol,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_salio_sol$Mean_NH-(df_salio_sol$sd_NH/2), ymax=df_salio_sol$Mean_NH+(df_salio_sol$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "SPA2", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.2.pitch.range`/max(df$`Spanish.2.pitch.height`), color = "Pitch range"),size=1 )+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.2.pitch.height`/max(df$`Spanish.2.pitch.height`), color = "Pitch height"), size =1, linetype = "solid") + 
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Spanish.2.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g8)

g_all <- ggarrange(g2, g4, g6, g8, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom") #arrange and put all plots in one
print(g_all)

ggsave("ptlt_pitch.eps", width = 12, height = 5) #store plots

write.csv( df_salio_sol,'salio_sol.csv', sep="")




g10 <- ggplot() +
  geom_path(data = df_egunon,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  + 
  geom_ribbon(aes(ymin=df_egunon$Mean_HI, ymax=df_egunon$Mean_HI, x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_egunon,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_egunon$Mean_NH, ymax=df_egunon$Mean_NH, x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "VSQ 1", x = "Time", y = "Mean Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.1.pitch.range`/max(df$`Basque.1.pitch.height`), color = "Pitch range"),size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.1.pitch.height`/max(df$`Basque.1.pitch.height`), color = "Pitch height"), size=1, linetype = "solid") + 
  theme(legend.position="none")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Basque.1.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g10)



g12 <- ggplot() +
  geom_path(data = df_eguzkia,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_eguzkia$Mean_HI, ymax=df_eguzkia$Mean_HI, x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_eguzkia,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_eguzkia$Mean_NH, ymax=df_eguzkia$Mean_NH, x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "VSQ 2", x = "Time", y = "Mean Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.2.pitch.range`/max(df$`Basque.2.pitch.height`), color = "Pitch range"), size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.2.pitch.height`/max(df$`Basque.2.pitch.height`), color = "Pitch height" ),size =1, linetype = "solid") + 
  theme(legend.position="none")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Basque.2.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g12)


g14 <- ggplot() +
  geom_path(data = df_buenos_dias,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_buenos_dias$Mean_HI, ymax=df_buenos_dias$Mean_HI, x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_buenos_dias,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_buenos_dias$Mean_NH, ymax=df_buenos_dias$Mean_NH, x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "SPA 1", x = "Time", y = " Mean Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.1.pitch.range`/max(df$`Spanish.1.pitch.height`), color = "Pitch range"), size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.1.pitch.height`/max(df$`Spanish.1.pitch.height`), color = "Pitch height"), size=1, linetype = "solid",) + 
  theme(legend.position="none")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Spanish.1.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g14)


g16 <- ggplot() +
  geom_path(data = df_salio_sol,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_salio_sol$Mean_HI, ymax=df_salio_sol$Mean_HI, x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_salio_sol,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_salio_sol$Mean_NH, ymax=df_salio_sol$Mean_NH, x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "SPA 2", x = "Time", y = "Mean Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.2.pitch.range`/max(df$`Spanish.2.pitch.height`), color = "Pitch range"),size=1 )+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.2.pitch.height`/max(df$`Spanish.2.pitch.height`), color = "Pitch height"), size =1, linetype = "solid") + 
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Spanish.2.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g16)

g_all <- ggarrange(g10, g12, g14, g16, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
print(g_all)






g18 <- ggplot() +
  geom_path(data = df_egunon,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  + 
  geom_ribbon(aes(ymin=(df_egunon$sd_HI/2), ymax=(df_egunon$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_egunon,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=(df_egunon$sd_NH/2), ymax=(df_egunon$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "VSQ 1", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.1.pitch.range`/max(df$`Basque.1.pitch.height`), color = "Pitch range"),size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.1.pitch.height`/max(df$`Basque.1.pitch.height`), color = "Pitch height"), size=1, linetype = "solid") + 
  theme(legend.position="none")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Basque.1.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g18)



g4 <- ggplot() +
  geom_path(data = df_eguzkia,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_eguzkia$Mean_HI-(df_eguzkia$sd_HI/2), ymax=df_eguzkia$Mean_HI+(df_eguzkia$sd_HI/2), x=1:15, fill = "HI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_eguzkia,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_eguzkia$Mean_NH-(df_eguzkia$sd_NH/2), ymax=df_eguzkia$Mean_NH+(df_eguzkia$sd_NH/2), x=1:15, fill = "NH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "VSQ2", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.2.pitch.range`/max(df$`Basque.2.pitch.height`), color = "Pitch range"), size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Basque.2.pitch.height`/max(df$`Basque.2.pitch.height`), color = "Pitch height" ),size =1, linetype = "solid") + 
  theme(legend.position="none")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Basque.2.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g4)


g6 <- ggplot() +
  geom_path(data = df_buenos_dias,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=df_buenos_dias$Mean_HI-(df_buenos_dias$sd_HI/2), ymax=df_buenos_dias$Mean_HI+(df_buenos_dias$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_buenos_dias,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=df_buenos_dias$Mean_NH-(df_buenos_dias$sd_NH/2), ymax=df_buenos_dias$Mean_NH+(df_buenos_dias$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "SPA 1", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.1.pitch.range`/max(df$`Spanish.1.pitch.height`), color = "Pitch range"), size=1)+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.1.pitch.height`/max(df$`Spanish.1.pitch.height`), color = "Pitch height"), size=1, linetype = "solid",) + 
  theme(legend.position="none")+
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Spanish.1.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g6)


g20 <- ggplot() +
  geom_path(data = df_salio_sol,
            aes(x = 1:15, y = Mean_HI, color = "PI", size = 1))  +
  geom_ribbon(aes(ymin=(df_salio_sol$sd_HI/2), ymax=(df_salio_sol$sd_HI/2), x=1:15, fill = "PI"), fill = "blue3", alpha = 0.051) + 
  geom_path(data = df_salio_sol,
            aes(x = 1:15, y = Mean_NH, color = "PH", size = 1)) + 
  geom_ribbon(aes(ymin=(df_salio_sol$sd_NH/2), ymax=(df_salio_sol$sd_NH/2), x=1:15, fill = "PH"), fill = "darkred", alpha = 0.051) + 
  theme_classic() + labs(title = "SPA 2", x = "Time", y = "Proportion of looks to the mouth")+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.2.pitch.range`/max(df$`Spanish.2.pitch.height`), color = "Pitch range"),size=1 )+
  geom_path(data = df,
            aes(x = 1:15, y = `Spanish.2.pitch.height`/max(df$`Spanish.2.pitch.height`), color = "Pitch height"), size =1, linetype = "solid") + 
  scale_y_continuous(sec.axis = sec_axis(~.*max(df$`Spanish.2.pitch.height`), name = "Pitch [Hz]")) + 
  scale_color_manual(name='',
                     breaks=c('PI', 'PH', 'Pitch range', 'Pitch height'),
                     values=c('PI'='blue3', 'PH'='darkred', 'Pitch range' = 'black', 'Pitch height' = 'springgreen4'))
print(g20)

g_all <- ggarrange(g2, g4, g6, g8, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
print(g_all)