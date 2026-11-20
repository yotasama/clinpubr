library(LaF)
library(openxlsx)
library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(clinpubr)

# 数据准备----
setDTthreads(5)
rm(list=ls())
dir_d=function(x){
  paste0("../data/",x)
}

dir_r=function(x){
  paste0("../result/",x)
}

data_file = dir_d('sbx_250918.csv')
years = 2021:2024

# t1=read.xlsx(dir_d("字段说明.xlsx"))
# t1=merge(t1,read.csv("子表格变量分组.csv"),all.x=T)
# write.xlsx(t1,"重点字段表格.xlsx")

# 重点字段表格，在字段表格基础上添加列：keep（是否重要项目），Subtable（变量分组）
var.table=read.xlsx('重点字段表格.xlsx')
var.table$vars=tolower(var.table$vars)

var.typ.num=var.table$vars[var.table$datatype=='数字']
var.typ.dt=var.table$vars[var.table$datatype%in%c('日期时间','日期')]
var.typ.chr=var.table$vars[var.table$datatype%in%c('字符','集合')]
Sys.setlocale("LC_TIME", "C")

Subtables=list(diag=c('a14','pro_sname',var.table$vars[var.table$Subtable=='diag']),
               other=var.table$vars[var.table$Subtable=='other'],
               surgery=var.table$vars[var.table$Subtable=='surgery'])

# 简单查看表格
x=read.csv(data_file, nrows=100)
# y=read.csv(dir_d('分割/ori_0.csv'),nrows=100)
# xx=readLines(data_file,n=5,encoding = "UTF-8-sig")


# LaF读文件
# column_types=sapply(x,class)
# column_types=replace_elements(column_types,c("character","numeric"),c("string","double"))
# laf <- laf_open_csv(data_file,column_names = colnames(x),
#                    #  column_types=column_types, skip=1)
#                     column_types=rep("character",ncol(x)))
# begin(laf)
# current_line(laf)
# first_block <- next_block(laf, nrows=1e3)
# 
# goto(laf,2)
# current_line(laf)
# sec_block <- next_block(laf, nrows=999)
# second_block <- next_block(laf, nrows=6e4)
# laf[1:5,]
# laf <- laf_open_csv(data_file, # column_names = colnames(x),
#                     # column_types=column_types, skip=1)
#                     column_types=rep("character",ncol(x)))
# 
# test=laf[1:1e5,Subtables[["surgery"]]]

# 数据横切----
# 每个数据横块的简单处理
table.prepro=function(table){
  tmp=table %>%
    # 日期处理
    mutate(across(any_of(var.typ.dt),function(x){
      x=sub(":.*$","",x)
      x[grepl('1900',x)]=NA
      as.Date(x, "%d%b%Y")
    })) %>% 
    # 数值处理
    mutate(across(any_of(var.typ.num),as.numeric)) %>%
    # 字符处理
    mutate(across(any_of(var.typ.chr),~na_if(.x,"-1")))%>%
    mutate(across(any_of(var.typ.chr),~na_if(.x,"")))
  tmp
}

# 原表格太大，无法直接读进内存，先逐行读取数据进行横切，并只保留重要变量
f=file(data_file,'r',encoding ='utf8')
cols.names=scan(f,quote ='\"',sep=',', nlines=1, what='character')

# 读取数据，横切并保存
nchunk=0
nlines=100000#此处修改数据块大小
while({tmp=scan(f,sep=',',quote ='\"',nlines=nlines,
                what='character',allowEscapes=T,skipNul=T);
       length(tmp)>1}){
  if(length(tmp) %% length(cols.names) != 0) stop("check loaded results!")
  
  tmp=as.data.frame(matrix(tmp,ncol=length(cols.names),byrow = TRUE))
  if(nrow(tmp)>nlines){
    print(paste0('chunk',nchunk,'has wrong number of lines!!!'))
  }
  colnames(tmp)=cols.names

# laf <- laf_open_csv(data_file,column_names = colnames(x),
#                     column_types=rep("character",ncol(x)))
# goto(laf,2)
# nchunk=0
# nlines=1e6#此处修改数据块大小
# while({tmp=next_block(laf, nrows=nlines);
#        nrow(tmp)>0}){
  # fwrite(tmp,dir_d(paste0('分割/ori_',nchunk,'.csv')))
  tmp=table.prepro(tmp)
  
  # 按年\类别分割保存
  for(i in 2021:2024){
    for(st in names(Subtables))
      fwrite(tmp[year(tmp$b15)==i,Subtables[[st]]],
             dir_d(paste0('分割/',i,'_',nchunk,"_",st,'.csv')))
  }
  # df=rbind(df,tmp)
  nchunk=nchunk+1
  if(nchunk%%20==0)
    print(nchunk)
  gc()
  
  #测试用
  if(nchunk>=2){
    break
  }
}
closeAllConnections()

cb_f=function (path = ".", pattern = NULL, add_file_name = FALSE, 
          unique_only = TRUE, reader_fun = read.csv, ...) 
{
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (length(files) > 0) {
    tmpl=list()
    for (f in files) {
      tmp <- reader_fun(f)#, ...)
      if (add_file_name) {
        base_col <- "origin_file"
        original_cols <- colnames(tmp)
        new_col_name <- base_col
        while (new_col_name %in% original_cols) {
          new_col_name <- paste0(new_col_name, "_")
        }
        tmp <- cbind(temp_col = basename(f), tmp)
        colnames(tmp)[1] <- new_col_name
      }
      tmpl=append(tmpl,list(tmp))
    }
    dat=rbindlist(tmpl)
    if (unique_only) {
      dat <- unique(dat)
    }
    return(dat)
  }
  else {
    return(NULL)
  }
}

# 合并纵切
# 2021 8749667
# 2022 9711275
# 2023 11656969
# 2024 13244587
for(year in 2021:2024){
  gc()
  for(st in names(Subtables)){
    df=cb_f(path=dir_d("分割"),pattern = paste0(year,'.*',st,".csv"),
            reader_fun = fread,unique_only = FALSE)
    fwrite(df,dir_d(paste0("combined/",year,'_',st,'.csv')))
    print(nrow(df))
  }
}
