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



datapath_PI_3="/insert datapath"
data<-read.asc(datapath_HI_3,samples= TRUE, events = TRUE, parse_all =  FALSE)
image="/dtapath to save the `photo in"
mimage<-readPNG(image)
img<-rasterGrob(mimage,interpolate = TRUE)
  
#Establishing the coordinates from which to take data (Areas of Interest)
#Remember eye tracker y axis goes from top to bottom and x axis goes from left to right
xleft= 310
xright=700
ybottom=670
ytop=480
  
  
e_xleft=310
e_xright=700
e_ybottom=480
e_ytop=280

#data frame containing only the areas of interest
  
mouth_eyes=data.frame(x1=c(xleft,e_xleft),x2=c(xright,e_xright), y1=c(ybottom,e_ybottom), y2=c(ytop,e_ytop))
Cond=data$fix$block %in% c(1,3,5,7)
lang=data$fix$block %in% c(1,2,5,6)
langue=data$fix$block%in% c(1,2,5,6)
  
  
  
  
Cond[Cond==TRUE]="visual"
Cond[Cond==FALSE]="audiovisual"
lang[lang==TRUE]= "Spanish"
lang[lang==FALSE]="EUSKERA"
  
all_data=cbind(data$fix,Cond,lang)
print(all_data)

#plot data on top of an image of the speaker's face, to help visualization
plot<-ggplot()
plot<-plot +annotation_custom(img,xmin = -Inf, ymin=-Inf, ymax=Inf)+ geom_point(data = all_data,aes(x=axp, y=ayp,size=dur,color=eye))+scale_x_continuous(expand = c(0,0),limits = c(0,data$info$screen.x))+scale_y_reverse(expand=c(0,0),limits=c(data$info$screen.y,0))+
coord_fixed()+geom_rect(data=mouth_eyes,mapping = aes(xmin=x1, xmax=x2, ymin=y1,ymax=y2),color="black",alpha=0.5)+facet_wrap(~Cond+lang)
  
print(plot)
  
  
  #Counter Mouth VS Eyes (PTL) For All conditions (Overall total looking time)
  
mouth_eyes=data.frame(x1=c(xleft,e_xleft),x2=c(xright,e_xright), y1=c(ybottom,e_ybottom), y2=c(ytop,e_ytop))
Cond=data$fix$block %in% c(1,3,5,7)
lang=data$fix$block %in% c(1,2,5,6)
  
  
Cond[Cond==TRUE]="visual"
Cond[Cond==FALSE]="audiovisual"
lang[lang==TRUE]= "Spanish"
lang[lang==FALSE]="EUSKERA"
  
langue[langue==TRUE]="Native" 
langue[langue==FALSE]="Non-Native"
  
  
all_data=cbind(data$fix,Cond,lang)
print(all_data%in%c(1,3,5,7))
  
print (all_data)
  #length
  #se pueden usar los symb > <
  
  #Mouth, define length of parameters of area of interest and count and add all the data points in that area
mouth = all_data$axp<xright & all_data$axp>xleft & all_data$ayp> ytop & all_data$ayp< ybottom
mouth[mouth == TRUE] = 1
Def_mouth = sum(mouth)
  
  
  
  #Eyes, define length of parameters of area of interest and count and add all the data points in that area
  
eyes= all_data$axp >e_xleft &  all_data$axp< e_xright & all_data$ayp> e_ytop & all_data$ayp < e_ybottom
eyes[eyes==TRUE]=1
Def_eyes= sum(eyes)
print(Def_eyes)
  
  
  #Calculating the proportion of total looking time to the mouth, then do the same with looks to the eyes
Total = sum(Def_mouth + Def_eyes)
  
PLT_O= sum(Def_mouth)/Total
  
#Creating data frame for all conditions + assigning values 
 
  
mouth_eyes=data.frame(x1=c(xleft,e_xleft),x2=c(xright,e_xright), y1=c(ybottom,e_ybottom), y2=c(ytop,e_ytop))
Cond=data$fix$block %in% c(1,3,5,7)
langue=data$fix$block %in% c(3,4,7,8)
  
  
Cond[Cond==TRUE]="visual"
Cond[Cond==FALSE]="audiovisual"
lang[lang==TRUE]= "Spanish"
lang[lang==FALSE]="EUSKERA"
  
langue[langue==TRUE]="Native" 
langue[langue==FALSE]="Non-Native"
  
  
all_data=cbind(data$fix,Cond,langue)
  
#Counter Mouth VS Eyes (PTL) For Visual and Native
  
  
#Mouth & Eyes, give coordinates and establish which condition (recording) are you obtaining data from
  
mouth= all_data$axp<xright & all_data$axp>xleft  & all_data$ayp> ytop & all_data$ayp< ybottom & all_data$Cond=="visual" & all_data$langue=="Native"
mouth[mouth == TRUE]= 1
Def_mouth= sum(mouth)
print(Def_mouth)
  
eyes= all_data$axp >e_xleft &  all_data$axp< e_xright & all_data$ayp> e_ytop & all_data$ayp < e_ybottom & all_data$Cond=="visual" & all_data$langue=="Native"
eyes[eyes==TRUE]=1
Def_eyes= sum(eyes)
print(Def_eyes)


Total = sum(Def_mouth + Def_eyes)
PLT_VN= sum(Def_mouth)/Total

#Eyes 5 Mouth 23 PLT 0.46



#PLT Condtion Visual Non-Native, give coordinates and establish which condition (recording) are you obtaining data from

mouth= all_data$axp<xright & all_data$axp>xleft  & all_data$ayp> ytop & all_data$ayp< ybottom & all_data$Cond=="visual" & all_data$langue=="Non-Native"
mouth[mouth == TRUE]= 1
Def_mouth= sum(mouth)
print(Def_mouth)

eyes= all_data$axp >e_xleft &  all_data$axp< e_xright & all_data$ayp> e_ytop & all_data$ayp < e_ybottom & all_data$Cond=="visual" & all_data$langue=="Non-Native"
eyes[eyes==TRUE]=1
Def_eyes= sum(eyes)
print(Def_eyes)


Total = sum(Def_mouth + Def_eyes)
PLT_VNN= sum(Def_mouth)/Total

#Eyes 14 Mouth 12 PLT 0.82



#PLT Condtion audiovisual Non-Native, give coordinates and establish which condition (recording) are you obtaining data from

mouth= all_data$axp<xright & all_data$axp>xleft  & all_data$ayp> ytop & all_data$ayp< ybottom & all_data$Cond=="audiovisual" & all_data$langue=="Non-Native"
mouth[mouth == TRUE]= 1
Def_mouth= sum(mouth)
print(Def_mouth)

eyes= all_data$axp >e_xleft &  all_data$axp< e_xright & all_data$ayp> e_ytop & all_data$ayp < e_ybottom & all_data$Cond=="audiovisual" & all_data$langue=="Non-Native"
eyes[eyes==TRUE]=1
Def_eyes= sum(eyes)
print(Def_eyes)


Total = sum(Def_mouth + Def_eyes)
PLT_AVNN= sum(Def_mouth)/Total

#Eyes 12 Mouth 13 PLT 0.52777



#PLT Condtion audiovisual Non-Native, give coordinates and establish which condition (recording) are you obtaining data from

mouth= all_data$axp<xright & all_data$axp>xleft  & all_data$ayp> ytop & all_data$ayp< ybottom & all_data$Cond=="audiovisual" & all_data$langue=="Native"
mouth[mouth == TRUE]= 1
Def_mouth= sum(mouth)
print(Def_mouth)

eyes= all_data$axp >e_xleft &  all_data$axp< e_xright & all_data$ayp> e_ytop & all_data$ayp < e_ybottom & all_data$Cond=="audiovisual" & all_data$langue=="Native"
eyes[eyes==TRUE]=1
Def_eyes= sum(eyes)
print(Def_eyes)


Total = sum(Def_mouth + Def_eyes)
PLT_AVN= sum(Def_mouth)/Total

#create data frame to store values, to export onto a csv file

PI=data.frame(name, PLT_AVN,PLT_AVNN,PLT_VN, PLT_VNN )
names(PI) <- c('Subject','AV_Nat', 'AV_NNat', 'V_NAt', 'V_NNat')
df=rbind(df,PI)
#rm(mouth)
#rm(eyes)
