# For Guo
filenames=list.files("/Users/jinsf/Documents/paper/rusefulcode/pretem/zuo/",
                     pattern="*.txt" )
#length(filenames)
f = file.path("/Users/jinsf/Documents/paper/rusefulcode/pretem/zuo",filenames)
for (i in 1:65){
    tem=read.table(f[i])
    guo=tem[which(tem$V8 < 32000),]
    guo1=aggregate(V8~V5+V6,data=guo, FUN="sum")
    guo1=guo1[order(guo1$V5,guo1$V6),]
   # write.table(guo1,filenames[i])
    ts.plot(guo1$V8)
}



filenames=
  list.files("/Users/jinsf/Documents/paper/rusefulcode/pretem/旱田初略数据/summaries/",
             pattern="*.txt"   )

f = file.path("/Users/jinsf/Documents/paper/rusefulcode/pretem/旱田初略数据/summaries",filenames)

filenames
fileorder=seq(3,31,by=2)

file1=read.table(f[1],skip=2)

for (i in fileorder){
  tem=read.table(f[i],skip=2)
  file1=rbind(file1,tem)
}

write.csv(file1,"Dry.csv")





library(lubridate)



dry=read.csv("rice2.csv")
dry$V3=ymd(dry$V3)
dry=transform(dry,
              Year=year(dry$V3),
              month=month(dry$V3),
              day=day(dry$V3))
dry1=aggregate(cbind(V18,V21)~Year+month+day,
                 data=dry,FUN="mean")
dry1=dry1[order(dry1$Year,dry1$month,dry1$day),]

write.csv(dry1,"rice21.csv")





jiusheng=read.csv("jiusheng.csv",header = T)
library(reshape2)
jiusheng=melt(jiusheng,id="Community")
library(ggplot2)
ggplot2::ggplot(jiusheng, aes(x="",y=value,fill=Community))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y") +
  facet_wrap(~variable,nrow=2)+
  labs(x="",y="")
ggsave("jiusheng.pdf")





library(ggplot2)
sun=read.csv("sun.csv",header = T)
sun=melt(sun,id="x")

ggplot(sun,aes(x=variable,y=value,fill=x))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_y_continuous(labels = NULL)+
  labs(y="",x="")

ggsave("sun.pdf",height = 10,width = 15, units = "cm")





filenames=list.files("/Users/jinsf/Documents/paper/rusefulcode/temp/",
                     pattern=NULL)
f = file.path("/Users/jinsf/Documents/paper/rusefulcode/temp/",
              filenames)
mohe_all=matrix(NA,nrow=3000,ncol = 27)
n=1
for (i in 1:length(f)){
  sub_file_name=list.files(f[i],pattern = NULL)
  sub_f=file.path(f[i],sub_file_name)
  if (length(sub_f) > 0){
    for (j in 1:length(sub_f)){
      file_need=scan(sub_f[j],skip=2)
      # file_need=read.table(sub_f[j],skip=2,sep="\t")
      location_mohe=which(file_need==50136)
      if (length(location_mohe) > 0) {
        Date=substr(sub_f[j],57,64)
        Prop=file_need[location_mohe:(location_mohe+25)]
        mohe_all[n,1]=Date
        mohe_all[n,2:27]=Prop
        n=n+1
      } else {
        Date=substr(sub_f[j],57,64)
        mohe_all[n,1]=Date
        mohe_all[n,2:27]=NA
        n=n+1 
      }
    }
  } else {
    i=i+1
  }
}    
write.csv(mohe_all,"mohe_all.csv")      

mohe_temp=as.data.frame(mohe_all)
mohe_temp=mohe_temp[,c(1,21)]

mohe_temp=transform(mohe_temp,
                    Day=substr(V1,1,6),
                    Time=substr(V1,7,8))

mohe_temp$Day=as.numeric(as.vector(mohe_temp$Day))
mohe_temp$V21=as.numeric(as.vector(mohe_temp$V21))

mohe_temp_final=aggregate(V21~Day,data=mohe_temp, FUN="mean")

write.csv(mohe_temp_final,"mohe_daily_temp.csv")