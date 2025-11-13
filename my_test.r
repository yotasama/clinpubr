# frequent tasks for R package development----
check()
test()
load_all()
install()
document()

check(remote = TRUE, manual = TRUE)
check_win_devel()

build_manual()
build_readme()
use_version('minor')
submit_cran()

# codes to be processed--------
## correlation plot----
library(export)
library(corrplot)
vars_to_show=c('Cystc','CKD.EPI..2009.','CKD.EPI.CrCy.2021..ori.',"IgG",'IgA')
tmp=cor(data[,vars_to_show],use="pairwise.complete.obs")
corrplot(corr=tmp,order = "AOE",type="upper",
         tl.col = "red",
         tl.pos = "d")
corrplot(corr=tmp,add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")
export::graph2png(file="corrplot.png",width=4,height=4)

## Survival results----
library(timeROC)
library(Hmisc)
library(survival)

res=rcorr.cens(data$Cystc,
               Surv(data$Time,data$Outcome))
c_index=res["C Index"]
if(c_index<0.5){
  c_index=1-c_index
}
class(res)
print(c_index)

time_roc <- timeROC(T=data$Time,
                    delta=data$Outcome,
                    marker=data$Cystc, 
                    cause=1,
                    weighting="marginal",
                    times=c(12,36,60),
                    ROC=TRUE)

print(time_roc) 

colors=c("red","blue","green")
for(i in 1:length(time_roc$times)){
  plot(time_roc, time=time_roc$times[i], col=colors[i], title=FALSE,add=i!=1)
}
legend("bottomright",
       legend=paste0(time_roc$times," months AUC=",sprintf("%.3f",time_roc$AUC)),
       col=colors,
       lwd=2)
export::graph2png(file="timeROC_Cystc_multiple.png",width=4,height=4)

# test codes----
list.dirs()
file.copy()