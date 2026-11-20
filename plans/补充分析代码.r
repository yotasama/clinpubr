library(data.table)
library(readr)
library(dplyr)
library(dtplyr)
library(openxlsx)
library(tibble)
library(stringr)
library(tableone)
library(clinpubr)
rm(list=ls())
years = c(2024:2021)
var.table=read.xlsx('重点字段表格.xlsx')
var.table$vars=tolower(var.table$vars)
var.typ.num=var.table$vars[var.table$datatype=='数字']
var.typ.dt=var.table$vars[var.table$datatype%in%c('日期时间','日期')]
var.typ.chr=var.table$vars[var.table$datatype%in%c('字符','集合')]

# 
# for(year in years){
#   diag.subdf=fread(paste0("../data/combined/",year,'_diag.csv')) %>%as.data.frame
#   诊断编码df=select(diag.subdf,all_of(var.table$vars[grepl('诊断编码',var.table$label)]))

#   rm(diag.subdf)
#   gc()
#   res.df=fread(paste0(year,'_processed.csv'))%>%as.data.frame
#   gc()
  
#   主要诊断人群=c('IgA死亡','IgA死亡有活检',paste0('IgA死亡CKD',1:5),'IgA死亡CKD未分期',
#            'IgA旧诊断住肾科','IgA旧诊断非住肾科')
#   主要诊断df=data.frame(matrix(NA,nrow=3*length(主要诊断人群),ncol=52))
#   colnames(主要诊断df)[1:3]=c('人群','平均年龄','诊断（数量）')
#   for(i in 1:length(主要诊断人群)){
#     if(i==2){
#       flag=res.df$活检&res.df$IgA死亡
#     }else if(i %in%3:7){
#       flag=res.df$IgA死亡&(res.df$CKD.group%in%paste0('CKD',i-2,'期'))
#     }else if(i==8){
#       flag=res.df$IgA死亡&(!res.df$CKD.group%in%c('CKD1期','CKD2期','CKD3期','CKD4期','CKD5期'))
#     }else{
#       flag=res.df[,主要诊断人群[i]]
#     }
#     if(sum(flag,na.rm = T)>0){
#       tmp=sort(table(unlist(诊断编码df[flag,])),decreasing = T)[1:50]
#       主要诊断df[(i*3-2),-c(1,2)]=names(tmp)
#       主要诊断df[(i*3-1),-c(1,2)]=tmp
#       主要诊断df[(i*3-1),1]=主要诊断人群[i]
#       主要诊断df[(i*3-1),2]=mean(res.df$年龄[flag],na.rm=T)
#     }
#   }
#   write.xlsx(主要诊断df,paste0('../result/其他结果表格/',year,'_死亡诊断与人数v2.xlsx'))
#   rm(res.df)
#   rm(诊断编码df)
#   gc()
# }

df_list=list()
for(year in years){
  other.df=fread(paste0("../data/combined/",year,'_other.csv'),
                 colClasses = list(character="b13c")) %>%as.data.frame
  出院时间=as.Date(other.df$b15)
  rm(other.df)
  gc()
  res.df0=fread(paste0(year,'_processed.csv'))%>%as.data.frame
  gc()
  res.df0$CKD.group=factor(res.df0$CKD.group,levels = c('CKD','CKD1期','CKD2期',
                                                        'CKD3期','CKD4期','CKD5期'),ordered=T)
  res.df0$病理类型[res.df0$病理类型=='']=NA
  res.df0$性别[res.df0$性别=='']=NA
  res.df0=res.df0[order(出院时间),]
  res.df=res.df0%>%
    group_by(患者id)%>%
    summarise(年龄=na_max(年龄),
              CKD.group=na_max(CKD.group),
              病理类型 = last(病理类型,na_rm = T),
              性别=last(性别),
              死亡=any(死亡)
    )
  fwrite(res.df,paste0(year,"tmp.csv"))
}

df_list=list()
i=1
for(year in years){
  res.df=fread(paste0(year,"tmp.csv"))%>%as.data.frame
  res.df$年龄组=cut(res.df$年龄,c(0:20,Inf)*5,right=F)
  for(type in c("病理类型","CKD分期")){
    if(type == "病理类型"){
      tmp=res.df%>%
        group_by(病理类型,性别,年龄组)
    }else if(type == "CKD分期"){
      tmp=res.df%>%
        group_by(CKD.group,性别,年龄组)
    }
    tmp=tmp%>%
      summarise(住院人数=n(),死亡人数=sum(死亡,na.rm = T))
    tmp$分组方式=type
    tmp$年份=year
    colnames(tmp)[1]='组别'
    df_list[[i]]=tmp
    i=i+1
  }
}
df_all=bind_rows(df_list)
write.xlsx(df_all,paste0('../result/其他结果表格/分组死亡数.xlsx'))
