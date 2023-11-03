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

datapath_PI_1= "insert datapath"

native= c(1)
version=c(2)


  data<-read.asc(datapath_PI_1,samples= TRUE, events = TRUE, parse_all =  FALSE)
  #name=substring(all_path[element], 71, 78)
  #data2<-read.asc(all_path[element],samples=TRUE,events=TRUE, parse_all=FALSE)
  image="datapath to store + extension (.png/.jpg)"
   mimage<-readPNG(image)
  img<-rasterGrob(mimage,interpolate = TRUE)
  
  # Defining areas of Interest
  xleft= 310
  xright=700
  ybottom=670
  ytop=480
  
  
  e_xleft=310
  
  e_xright=700
  e_ybottom=480
  e_ytop=280
  
  mouth_eyes=data.frame(x1=c(xleft,e_xleft),x2=c(xright,e_xright), y1=c(ybottom,e_ybottom), y2=c(ytop,e_ytop))
  Cond=data$fix$block %in% c(1,3,5,7)
  #lang=data$fix$block %in% c(1,2,5,6)
  langue=data$fix$block%in% c(1,2,5,6)
  
  
  if(version[element]==1){
    langue=data$fix$block%in% c(1,2,5,6)
    langue[langue==TRUE]="SPA"
    langue[langue==FALSE]="VSQ"
  }else{
    langue=data$fix$block%in% c(1,2,5,6)
    langue[langue==TRUE]="VSQ"
    langue[langue==FALSE]="SPA"
  }
  
  Cond[Cond==TRUE]="visual"
  Cond[Cond==FALSE]="audiovisual"
  
  exposure=langue
  
  if(native[element]==1){ 
    
    exposure[langue=="SPA"]="Dom"  #labeling the filters according to participants data
    exposure[langue=="VSQ"]="Non-Dom"
  }else{
    exposure[langue=="VSQ"]="Non-Dom" 
    exposure[langue=="SPA"]="Dom"
  }
  
  
  all_data=cbind(data$fix,Cond,exposure,langue)
  #  print(all_data)
  
  plot<-ggplot()
  plot<-plot +annotation_custom(img,xmin = -Inf, ymin=-Inf, ymax=Inf)+ geom_point(data = all_data,aes(x=axp, y=ayp,size=dur,color=eye))+scale_x_continuous(expand = c(0,0),limits = c(0,data$info$screen.x))+scale_y_reverse(expand=c(0,0),limits=c(data$info$screen.y,0))+
    coord_fixed()+geom_rect(data=mouth_eyes,mapping = aes(xmin=x1, xmax=x2, ymin=y1,ymax=y2),color="black",alpha=0.5)+facet_wrap(~Cond+exposure)
  print(plot)
  ggsave(paste(name, "_plot.png"), plot,device = "png", path="storing datapath", scale = 5, limitsize = FALSE )


