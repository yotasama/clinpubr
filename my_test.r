load_all()
library(openxlsx)
data(cancer, package = "survival")
x1=factor(c("a","y","x"),levels=c("a","y","x"))
x2=factor(c("a","z","x"),levels=c("a","z","x"))
y=c(x1,x2)

cor_table(cancer,"spearman")
cor_table=function(data, method){
  cor.table=data.frame(matrix(NA,nrow=ncol(data),ncol=ncol(data)))
  colnames(cor.table)=colnames(data)
  for(i in 1:ncol(data)){
    cor.table[i,i]=colnames(data)[i]
  }
  for(i in 1:(ncol(data)-1)){
    for(j in (i+1):ncol(data)){
      ct=cor.test(data[,i],data[,j],method = method)
      ct.res=c(ct$estimate,ct$p.value)
      if(ct.res[2]<0.001){
        ct.res=paste0(format(ct.res[1],digits=1,nsmall=3),' p<0.001')
      }else{
        ct.res=format(ct.res,digits=1,nsmall=3)
        ct.res=stringr::str_remove(ct.res," ")
        ct.res=paste0(ct.res[1],' p=',ct.res[2])
      }
      cor.table[i,j]=ct.res
    }
  }
  cor.table[is.na(cor.table)]=''
  write.csv(cor.table,'cor_table.csv', row.names = FALSE)
}

install.packages(c("rlang","cli","stringi","purrr","Rcpp","commonmark","xfun","xml2","zoo","data.table","mvtnorm","quantreg","fs","mime","jsonlite","parallelly","future","prodlim","Hmisc","readxl","gld","rms","DescTools"))
