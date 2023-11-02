library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyverse)

p=Police.Fatalities
head(p)
str(p)
summary(p)

#Test of Independence between the manner of death of the victim & how they were armed

p1=p%>%filter(p$Manner_of_death!='',p$Armed!='')
a1=p1%>%select(Manner_of_death,Armed)
a2_1=a1%>%filter(Armed=='Gun')
bind10=cbind(a2_1,Armed2='Gun')
a2_2=a1%>%filter(Armed=='Knife')
bind11=cbind(a2_2,Armed2='Knife')
a2_3=a1%>%filter(Armed!='Gun',Armed!='Knife')
bind12=cbind(a2_3,Armed2='Other')
a2_bind=rbind(bind10,bind11,bind12)%>%select(Armed2,Manner_of_death)
y1=table(a2_bind)
chi_y1=chisq.test(y1); chi_y1
chi_y1$expected
z1=data.frame(y1[,1]+y1[,4],y1[,2],y1[,3])
chi_z1=chisq.test(z1); chi_z1

#State-wise Comparison

p2=p%>%filter(State!='')

sh=function(x) {
  u=unique(x)
  u[which.max(tabulate(match(x,u)))]
  }
u1=unique(p2$State)
u1[which.max(tabulate(match(p2$State,u1)))]

sl=function(x) {
  u=unique(x)
  u[which.min(tabulate(match(x,u)))]
}
u2=unique(p2$State)
u2[which.min(tabulate(match(p2$State,u2)))]

a3<-ggplot(p2,aes(State))
a3+geom_bar(fill='purple')

#Age-wise Comparison
p3=p%>%filter(p$Age!='')
a4<-ggplot(p3,aes(Age))
a4+geom_freqpoly(binwidth=1,color='red',size=1)

#Test of Independence between the Age & Race of the victim
p4=p%>%filter(p$Age!='',p$Race!='')
a5<-ggplot(p4,aes(Race,Age))
a5+geom_boxplot()
a6=p4%>%select(Age,Race)
a7=a6%>%filter(Age<18)
a8=a7%>%count(Race)
bind1=rbind(list('Asian',0),a8)
a9=a6%>%filter(Age>=18&Age<59)
a10=a9%>%count(Race)
a11=a6%>%filter(Age>=60)
a12=a11%>%count(Race)
y2=data.frame(bind1$n,a10$n,a12$n)
chi_y2=chisq.test(y2); chi_y2
chi_y2$expected
t2=t(y2); t2
z2=data.frame(t2[,1]+t2[,2],t2[,3]+t2[,4]+t2[,5],t2[,6])
chi_z2=chisq.test(z2); chi_z2

#Arms & Race Comparison
p5=p%>%filter(p$Armed!='',p$Race!='')
a13=p5%>%select(Armed,Race)
a14=a13%>%filter(Armed=='Unarmed')
a15=a13%>%filter(Armed!='Unarmed')
a16<-ggplot(a14,aes(Race))
a16+geom_bar(fill='blue')
a17<-ggplot(a15,aes(Race))
a17+geom_bar(fill='gold')

#Gender & Race Comparison
p6=p%>%filter(p$Gender!='',p$Race!='')
a18=p6%>%select(Gender,Race)
a19<-ggplot(a18,aes(Race,fill=Gender))
a19+geom_bar(position='fill')
y3=table(p6$Gender,p6$Race)
chi_y3=chisq.test(y3); chi_y3
chi_y3$expected
z3=data.frame(y3[,1],y3[,2],y3[,3],y3[,4]+y3[,5],y3[,6])
chi_z3=chisq.test(z3); chi_z3

#Age & Race Comparison
bind2=cbind(a7,Age_Group='Less than 18 years')
bind3=cbind(a9,Age_Group='Between 18 & 60 years')
bind4=cbind(a11,Age_Group='60 years & above')
a20=rbind(bind2,bind3,bind4)
a21<-ggplot(a20,aes(Race,fill=Age_Group))
a21+geom_bar(position='dodge')

#Mental Illness & Race Comparison
p7=p%>%filter(p$Mental_illness!='',p$Race!='')
a22=p7%>%select(Mental_illness,Race)
a23=a22%>%filter(Race=='White')
bind5=cbind(a23,Race2='White')
a24=a22%>%filter(Race!='White')
bind6=cbind(a24,Race2='Other')
a25=rbind(bind5,bind6)
a26<-ggplot(a25,aes(Race2,fill=Mental_illness))
a26+geom_bar(position='fill')

#Test of Independence of whether the Mental Illness of the victim depends on their Race
y4=table(a24)
chi_y4=chisq.test(y4); chi_y4

#Year & Race Comparison
p8=p%>%filter(p$Date!='',p$Race!='')
a27=p8%>%select(Date,Race)
Y_2000=a27%>%filter(grepl('2000',Date))
bind_2000=cbind(Y_2000,Year='2000')
Y_2004=a27%>%filter(grepl('2004',Date))
bind_2004=cbind(Y_2004,Year='2004')
Y_2008=a27%>%filter(grepl('2008',Date))
bind_2008=cbind(Y_2008,Year='2008')
Y_2012=a27%>%filter(grepl('2012',Date))
bind_2012=cbind(Y_2012,Year='2012')
Y_2016=a27%>%filter(grepl('2016',Date))
bind_2016=cbind(Y_2016,Year='2016')
Y_bind=rbind(bind_2000,bind_2004,bind_2008,bind_2012,bind_2016)
ggplot(Y_bind,aes(Year,fill=Race))+geom_bar(position='dodge')

#Details of how the victims were armed
p9=p%>%filter(p$Armed!='Toy weapon',p$Armed!='')
a28=p9%>%select(Race,Armed)
a29=ggplot(a28,aes(Race,fill=Armed))
a29+geom_bar(position='dodge')

#Gun, Knife, Vehicle, Toy Weapon, Unarmed (Most used arms by the victims)
p10=p%>%filter(p$Armed=='Gun'|p$Armed=='Knife'|p$Armed=='Vehicle'|p$Armed=='Toy Weapon'|p$Armed=='Toy weapon'|p$Armed=='Unarmed')
a45=p10%>%select(Armed)
a46=a45%>%filter(Armed=='Toy Weapon'|Armed=='Toy weapon')
bind13=cbind(a46,Armed2='Toy Weapon')
a47=a45%>%filter(Armed!='Toy Weapon',Armed!='Toy weapon')
bind14=cbind(a47,Armed2=a47[,1])
a48=rbind(bind13,bind14)
a30=ggplot(a48,aes(Armed2))
a30+geom_bar(fill='coral')

#Toy Weapon, Race
p11=p%>%filter(p$Armed=='Toy Weapon'|p$Armed=='Toy weapon',p$Race!='')
a31=p11%>%select(Armed,Race)
u3=unique(p11$Race)
u3[which.max(tabulate(match(p11$Race,u1)))]
a32<-ggplot(a31,aes(Race))
a32+geom_bar(fill='magenta')

#Toy Weapon, Illness
p12=p%>%filter(p$Armed=='Toy Weapon'|p$Armed=='Toy weapon',p$Mental_illness!='')
a33=p12%>%select(Armed,Mental_illness)
a34<-ggplot(a33,aes(Mental_illness))
a34+geom_bar(fill='tan')

#Toy Weapon, Age
p13=p%>%filter(p$Armed=='Toy Weapon'|p$Armed=='Toy weapon',p$Age!='')
a35=p13%>%select(Armed,Age)
a36=a35%>%filter(Age<18)
bind7=cbind(a36,Age_Group='Less than 18 years')
a37=a35%>%filter(Age>=18&Age<59)
bind8=cbind(a37,Age_Group='Between 18 & 60 years')
a38=a35%>%filter(Age>=60)
bind9=cbind(a37,Age_Group='60 years & above')
a39=rbind(bind7,bind8,bind9)
a40<-ggplot(a39,aes(Age_Group))
a40+geom_bar(fill='cyan')

#Graph on Manner of Death
p14=p%>%filter(p$Manner_of_death!='')
a41=p14%>%select(Manner_of_death)
a42<-ggplot(a41,aes(Manner_of_death))
a42+geom_bar(fill='maroon')

#Test of Independence between Gender & Mental Illness
p15=p%>%filter(p$Gender!='',p$Mental_illness!='')
a43=p15%>%select(Gender,Mental_illness)
y5=table(a43)
chi_y5=chisq.test(y5); chi_y5

#Test of Independence between whether the victim tried to flee & whether they had any mental illness
p16=p%>%filter(p$Flee!='',p$Mental_illness!='')
a44=p16%>%select(Flee,Mental_illness)
y6=table(a44)
chi_y6=chisq.test(y6); chi_y6