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

native= c(1,1,1,1,0,1,1,1,1,1,1,0,1)# 1= Native ESP #HI06 =1

#Matrix to define parameters of dataframe 
df= data.frame(matrix(ncol = 4))
header<- c('Subject','Discard','Title','Todo')
colnames(df) <- header

xleft= 310
xright=700
ybottom=750
ytop=480



e_xleft=310
e_xright=700
e_ybottom=480
e_ytop=280

Subject<-(names)
#Primer for recorre los participantes

for (k in 1:length(all)){
  data<-read.asc(all[[k]],samples= TRUE, events = TRUE, parse_all =  FALSE)
  x=data$raw$xp
  #x=data$raw$xp[data$raw$block %in%c(2,4,6,8)]
  #xwindow=x[0:1000]
  y=data$raw$yp
  AOI=data.frame(x1=c(xleft,e_xleft),x2=c(xright,e_xright), y1=c(ybottom,e_ybottom), y2=c(ytop,e_ytop))
  Cond=data$raw$block %in% c(1,2,3,4,5,6,7,8)
  
  #Segundo for recorre los bloques
  for (i in seq(1,8)){
    block = data$raw$block[data$raw$block %in% i]
    #print(i)
    #print(block)
    #Datos de los ejes x e y
    x_block = data$raw$xp[data$raw$block %in% i]
    x_block_all= sum(x_block,na.rm = T)
    
    y_block = data$raw$yp[data$raw$block %in% i]
    y_block_all=sum(y_block, na.rm = T)
    
    #Datos fuera, data outside area of interest
    out_box = x_block<xleft | x_block>xright | y_block< e_ytop | y_block> ybottom
    out_box[out_box == TRUE] = 1
    na_out_box = out_box[is.na(out_box)]
    Count_box = sum(out_box, na.rm = T)
    print(Count_box)
    
    Discard<- (Count_box)
    
    #Datos dentro, datos used for PTLT
    in_box = x_block>xleft | x_block<xright | y_block> e_ytop | y_block< ybottom
    in_box[in_box == TRUE] = 1
    na_in_box = in_box[is.na(in_box)]
    Count_in_box = sum(in_box, na.rm = T)
    print(Count_in_box)
    
    Todo<- (Count_box+Count_in_box)
    #Este if adjudica el orden de los trials según orden presentación  
    
    if(versions[k]==1)
    {
      title_order = titles_1;
    }
    else
    {
      title_order = titles_2;
    }
    
    trial = title_order[i]
    
    #Creamos el data frame combined (importante same number of rows and columns) 
    v<-data.frame(names[k], Discard, trial,Todo)
    colnames(v) <- header
    #length(Discard) <- length(df) <- max(c(length(Discard), length(df)))
    df=rbind(df,v)
    
  }
}

write.csv(df,"Discard.csv", row.names=TRUE)
