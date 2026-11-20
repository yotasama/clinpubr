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

# todo: 处理分组NA值，出院方式变分类变量
# 新生儿体重字段弃用，数值太离谱
years = c(2024:2021)
# 重点字段表格 添加列：keep（是否重要项目），Subtable（变量分组）
var.table=read.xlsx('重点字段表格.xlsx')
var.table$vars=tolower(var.table$vars)
var.typ.num=var.table$vars[var.table$datatype=='数字']
var.typ.dt=var.table$vars[var.table$datatype%in%c('日期时间','日期')]
var.typ.chr=var.table$vars[var.table$datatype%in%c('字符','集合')]

# 各种编码、函数----
# 读入诊断、手术编码
CKD.diag.all=c('A02.200x004+N16.0*', 'A06.500x001+J99.8*', 'A18.100x025+N29.1*', 
               'A18.103+N29.1*', 'A18.104+N29.1*', 'A18.105+N29.1*', 
               'A27.900x005', 'A43.802', 'A52.700x012+N08.0*', 'A52.700x022', 
               'A98.500', 'A98.500x001+N08.0*', 'B01.800x002+N08.0*', 
               'B18.103+N08.0*', 'B18.205+N08.0*', 'B18.904+N08.0*', 
               'B26.800x008+N08.0*', 'B52.000', 'B52.000x001', 
               'B52.000x002+N08.0*', 'B83.800x008', 'B90.101', 'B90.102+N29.1*', 
               'C85.900x043', 'C90.000x003+N16.1*', 'C90.000x007+N08.1*', 
               'C90.000x013+N16.1*', 'C95.900x007+N16.1*', 'C95.900x017+N16.1*', 
               'D64.900x005', 'D69.000x006+N08.2*', 'D69.005+N08.2*', 
               'D75.100x010', 'D89.101+N08.2*', 'D89.800x011', 'E10.200', 
               'E10.200x011+N08.3*', 'E10.200x012+N08.3*', 'E10.200x013+N08.3*', 
               'E10.200x014+N08.3*', 'E10.200x015+N08.3*', 'E10.200x023+N08.3*', 
               'E10.200x024+N08.3*', 'E10.200x025+N08.3*', 'E10.200x026+N08.3*', 
               'E10.200x028+N08.3*', 'E10.200x029+N08.3*', 'E10.200x031+N29.8*',
               'N03.000', 'N03.100', 'N03.200', 'N03.200x001', 'N03.300', 
               'N03.300x001', 'N03.300x002', 'N03.400', 'N03.500', 'N03.500x001', 
               'N03.500x002', 'N03.500x003', 'N03.501', 'N03.502', 'N03.503', 
               'N03.600', 'N03.601', 'N03.700', 'N03.800', 'N03.800x001', 
               'N03.800x003', 'N03.800x004', 'N03.801', 'N03.900', 'N03.900x001', 
               'N03.900x002', 'N03.900x003', 'N03.900x004', 'N03.900x005', 
               'N03.900x006', 'N03.900x007', 'N04.000', 'N04.000x002', 'N04.001', 
               'N04.100', 'N04.101', 'N04.102', 'N04.200', 'N04.200x001', 
               'N04.200x002', 'N04.300', 'N04.300x001', 'N04.300x003', 'N04.400', 
               'N04.400x001', 'N04.500', 'N04.501', 'N04.502', 'N04.600', 
               'N04.601', 'N04.700', 'N04.800', 'N04.800x002', 'N04.801', 
               'N04.900', 'N04.901', 'N04.902', 'N04.903', 'N05.000', 
               'N05.000x001', 'N05.000x003', 'N05.000x004', 'N05.100', 
               'N05.100x001', 'N05.100x004', 'N05.101', 'N05.200', 
               'N05.200x002', 'N05.201', 'N05.300', 'N05.301', 'N05.400', 
               'N05.500', 'N05.500x002', 'N05.501', 'N05.600', 'N05.700', 
               'N05.700x001', 'N05.701', 'N05.800', 'N05.800x003', 'N05.800x004', 
               'N05.800x005', 'N05.801', 'N05.802', 'N05.803', 'N05.900x002', 
               'N05.900x003', 'N05.900x006', 'N05.900x007', 'N05.900x008', 
               'N05.900x009', 'N06.000', 'N06.001', 'N06.100', 'N06.200', 
               'N06.300', 'N06.400', 'N06.500', 'N06.700', 'N07.000', 'N07.100', 
               'N07.200', 'N07.300', 'N07.400', 'N07.500', 'N07.600', 'N07.700', 
               'N07.800', 'N07.900', 'N07.900x001', 'N11.000', 'N11.000x001', 
               'N11.100', 'N11.800', 'N11.800x002', 'N11.800x003', 'N11.801', 
               'N11.802', 'N11.900', 'N11.900x001', 'N11.900x003', 'N11.901', 
               'N12.x00', 'N12.x00x004', 'N12.x00x005', 'N12.x00x006', 
               'N12.x00x007', 'N12.x00x008', 'N12.x01', 'N12.x02', 'N12.x03', 
               'N13.000', 'N13.100', 'N13.100x001', 'N13.200', 'N13.201', 
               'N13.202', 'N13.203', 'N13.204', 'N13.300', 'N13.300x003', 
               'N13.300x005', 'N13.300x011', 'N13.301', 'N13.500', 'N13.500x009', 
               'N13.501', 'N13.600', 'N13.600x001', 'N13.600x002', 'N13.601', 
               'N13.603', 'N13.604', 'N13.801', 'N14.000', 'N14.100', 'N14.101', 
               'N14.102', 'N14.200', 'N14.200x002', 'N14.200x003', 'N14.201', 
               'N14.300', 'N14.301', 'N14.400', 'N15.000', 'N15.100', 'N15.101', 
               'N15.102', 'N15.800', 'N15.801', 'N15.900', 'N15.900x002', 
               'N15.900x003', 'N15.900x004', 'N15.901', 'N17.002', 'N17.100x003', 
               'N17.200x003', 'N18.000', 'N18.000x001', 'N18.001', 
               'N18.002+D63.8*', 'N18.800', 'N18.800x013', 'N18.800x014', 
               'N18.800x015', 'N18.800x016', 'N18.800x017', 'N18.800x018', 
               'N18.800x019', 'N18.800x020', 'N18.800x021', 'N18.801', 'N18.802', 
               'N18.803', 'N18.804', 'N18.810+D63.8*', 'N18.900', 'N18.900x003', 
               'N18.900x004', 'N18.900x005', 'N18.900x007', 'N18.900x011', 
               'N18.900x012', 'N18.900x013', 'N18.901', 'N19.x00', 'N19.x00x002', 
               'N19.x02', 'N20.000', 'N20.000x001', 'N20.000x002', 'N20.002', 
               'N20.200', 'N20.900x001', 'N20.901', 'N23.x00', 'N25.000', 
               'N25.000x007', 'N25.000x011', 'N25.001', 'N25.002+M90.8*', 
               'N25.003+M90.8*', 'N25.004', 'N25.100', 'N25.800', 'N25.801', 
               'N25.802', 'N25.803', 'N25.804', 'N25.805', 'N25.806', 'N25.900', 
               'N26.x00', 'N26.x00x001', 'N26.x01', 'N26.x02', 'N27.000', 
               'N27.100', 'N27.900', 'N28.000', 'N28.000x003', 'N28.000x007', 
               'N28.001', 'N28.002', 'N28.003', 'N28.004', 'N28.005', 'N28.100', 
               'N28.101', 'N28.102', 'N28.800', 'N28.800x002', 'N28.800x004', 
               'N28.800x020', 'N28.800x024', 'N28.800x028', 'N28.800x030', 
               'N28.801', 'N28.802', 'N28.803', 'N28.804', 'N28.805', 'N28.806', 
               'N28.807', 'N28.808', 'N28.809', 'N28.810', 'N28.811', 'N28.812', 
               'N28.813', 'N28.814', 'N28.815', 'N28.816', 'N28.818', 'N28.819', 
               'N28.820', 'N28.837', 'N28.900', 'N28.900x004', 'N28.900x010', 
               'N28.900x011', 'N28.900x012', 'N28.900x013', 'N28.900x014', 
               'N28.900x015', 'N28.900x016', 'N28.900x017', 'N28.900x018', 
               'N28.900x019', 'N28.900x020', 'N28.900x022', 'N28.900x023', 
               'N28.900x024', 'N28.900x025', 'N28.900x026', 'N28.901', 
               'N39.800x001', 'N99.000', 'N99.001', 'O03.300x041', 'O03.800x041', 
               'O04.800x041', 'O08.400', 'O08.400x003', 'O08.401', 'O10.200', 
               'O10.201', 'O10.300', 'O10.301', 'O23.000', 'O23.001', 
               'O26.800x011', 'O26.800x012', 'O26.800x013', 'O26.801', 'O26.802', 
               'O26.803', 'O26.804', 'O35.800x009', 'O35.800x010', 'O35.800x023', 
               'O35.817', 'O90.400x002', 'O90.800x005', 'O99.426', 'O99.800x315', 
               'O99.800x415', 'O99.800x417', 'O99.810', 'P00.100', 'P00.101', 
               'P96.000', 'P96.802', 'Q27.100', 'Q27.200', 'Q27.200x002', 
               'Q27.200x003', 'Q27.305', 'Q27.800x039', 'Q27.806', 'Q52.800x004', 
               'Q55.201', 'Q60.000', 'Q60.100', 'Q60.200', 'Q60.300', 'Q60.400', 
               'Q60.500', 'Q60.501', 'Q61.000', 'Q61.100', 'Q61.101', 'Q61.200', 
               'Q61.201', 'Q61.300', 'Q61.300x002', 'Q61.400', 'Q61.400x001', 
               'Q61.400x002', 'Q61.500', 'Q61.501', 'Q61.800', 'Q61.801', 
               'Q61.900', 'Q62.000', 'Q62.000x002', 'Q62.100x001', 'Q62.101', 
               'Q62.300', 'Q62.300x902', 'Q62.300x904', 'Q62.501', 'Q62.700', 
               'Q63.000', 'Q63.001', 'Q63.002', 'Q63.100', 'Q63.101', 'Q63.102', 
               'Q63.103', 'Q63.200', 'Q63.201', 'Q63.202', 'Q63.203', 'Q63.300', 
               'Q63.300x002', 'Q63.301', 'Q63.302', 'Q63.800', 'Q63.800x101', 
               'Q63.800x902', 'Q63.801', 'Q63.900', 'Q63.901', 'Q85.900x013', 
               'Q85.900x029', 'Q85.903', 'Q87.800x301', 'Q87.800x911', 'R39.200', 
               'R39.200x001', 'R93.400x002', 'R93.401', 'R93.402', 'R93.403', 
               'R94.400', 'R94.401', 'S35.400', 'S35.400x001', 'S35.400x002', 
               'S35.401','S35.402', 'S37.000', 'S37.000x012', 'S37.000x013', 
               'S37.000x015', 'S37.000x016', 'S37.000x021', 'S37.000x022', 
               'S37.000x023', 'S37.000x031', 'S37.000x041', 'S37.001', 'S37.002', 
               'S37.003', 'S37.004', 'S37.010', 'S37.011', 'T79.500x002', 
               'T81.000x038', 'T81.018', 'T81.032', 'T82.400x002', 'T82.400x003', 
               'T82.400x004', 'T82.700x007', 'T82.700x008', 'T82.700x009', 
               'T82.700x010', 'T82.700x011', 'T83.001', 'T83.002', 'T83.804', 
               'T86.100', 'T86.100x001', 'T86.100x002', 'T86.100x003', 'T86.102', 
               'T86.103', 'T86.104', 'T86.105', 'T86.106', 'T86.107', 'Z43.601', 
               'Z43.602', 'Z46.601', 'Z46.800x001', 'Z46.800x002', 'Z49.000x002', 
               'Z49.000x003', 'Z49.000x004', 'Z49.000x005', 'Z84.100', 'Z90.500', 
               'Z94.000', 'Z94.001', 'Z94.002', 'Z98.800x210', 'Z99.200', 
               'Z99.200x002', 'N08.0*', 'N08.2*', 'N08.3*', 'N08.4*', 'N08.5*', 
               'N08.8*', 'N16.0*', 'N16.2*', 'N16.3*', 'N16.4*', 'N16.5*', 
               'N16.8*', 'N29.0*', 'N29.1*', 'N29.8*', 'Y60.200', 'Y61.200', 
               'Y62.200', 'Y84.100')
活检编码=c('55.23')
糖肾编码=c('E14.200','E11.200','E10.200')
糖尿病编码=c('E14','E11','E10')
IgAN编码=c('N02.002','N02.101','N02.201','N02.302','N02.701','N02.801',
         'N02.800x002','N04.300x003','N00.801')
MN编码=c('N05.201','N04.2')
LN编码=c('M32.100x001', 'M32.101+N08.5*', 'M32.102+N16.4*')
DN编码=糖肾编码
FSGS编码=c('N00.100', 'N00.100x001', 'N01.100', 'N02.100', 'N02.102', 'N03.100', 
         'N04.100', 'N04.101', 'N04.102', 'N05.100', 'N05.100x001', 'N05.100x004', 
         'N05.101', 'N06.100', 'N07.100')
MCD编码=c('N04.001', 'N05.000x001', 'N05.000x004')
CTN编码=c('N11.800', 'N11.900', 'N11.901', 'N12.x00', 'N12.x00x005', 'N12.x00x006', 
        'N12.x00x007', 'N12.x00x008', 'N12.x01', 'N12.x03', 'N15.800', 'N15.900',
        'C95.900x007+N16.1*', 'C95.900x017+N16.1*', 'M35.006+N16.4*', 'O08.400x003', 
        'N16.1*', 'N16.2*', 'N16.3*', 'N16.4*', 'N16.8*')
感染编码=c('J96.000','J96.100','J96.900','J98.414','J12.900','J15.900','J18.800',
       'J18.900','J18.901','J18.903')
流产编码=paste0('O0',c(0:8))
妊娠期高血压编码=paste0('O1',c(0:6))
分娩编码=paste0('O8',c(0:4))
新生儿结局编码=paste0('P0',c(0:8))
人群分组=c('总体','活检','住肾科','非住肾科','诊断全糖肾','诊断2糖肾','诊断1糖肾','诊断活检全糖肾',
       '诊断活检2糖肾','诊断活检1糖肾','糖尿病活检全糖肾',
       '糖尿病活检2糖肾','糖尿病活检1糖肾','IgAN','IgA新诊断','IgA旧诊断',
       'IgA死亡','IgA旧诊断住肾科','IgA旧诊断非住肾科','全糖肾死亡',
       '1糖肾死亡','2糖肾死亡','老年','产妇','新生儿')
# 函数，从列表中判断是否含有特定诊断/手术
get.diag=function(diags,diag.df,partial.match=T,replace.repetition.quantifiers=T){
  if(partial.match){
    if(replace.repetition.quantifiers){
      diags=str_replace_all(diags, c("\\+" = "\\\\+", "\\*" = "\\\\*"))
    }
    pattern=paste0(diags,collapse='|')
    hit_mat=vapply(diag.df,function(col) str_detect(col,pattern),logical(nrow(diag.df)))
  }else{
    hit_mat=vapply(diag.df,function(col) col %in% diags,logical(nrow(diag.df)))
  }
  rowSums(hit_mat,na.rm=T)>0
}


# 函数，取操作对应的首个日期
get.date.surgery=function(codes,code.df,date.df,partial.match=T){
  res=rep(NA,nrow(code.df))
  if(partial.match){
    codes=str_replace_all(codes, c("\\+" = "\\\\+", "\\*" = "\\\\*"))
  }
  for(i in 1:nrow(code.df)){
    if(partial.match){
      tmp=grep(paste0(codes,collapse='|'),code.df[i,])[1]
    }else{
      tmp=which(code.df[i,]%in%codes)[1]
    }
    res[i]=date.df[i,tmp]
  }
  res
}

# 实际循环----
for(year in years){
  if(!file.exists(paste0(year,'_processed.csv'))){
    other.subdf=fread(paste0("../data/combined/",year,'_other.csv')) %>%as.data.frame
    res.df=data.frame(province.group=factor(other.subdf$pro_sname,
                                            levels=c("北京市","天津市","河北省", "山西省", "内蒙古自治区",
                                                     "辽宁省","吉林省","黑龙江省",
                                                     "上海市","江苏省","浙江省","安徽省","福建省","江西省","山东省",
                                                     "河南省","湖北省","湖南省","广东省","广西壮族自治区","海南省",
                                                     "重庆市","四川省","贵州省","云南省","西藏自治区",
                                                     "陕西省","甘肃省","青海省","宁夏回族自治区","新疆维吾尔自治区","兵团"),
                                            labels=c("北京","天津","河北","山西","内蒙古",
                                                     "辽宁","吉林","黑龙江",
                                                     "上海","江苏","浙江","安徽","福建","江西","山东",
                                                     "河南","湖北","湖南","广东","广西","海南",
                                                     "重庆","四川","贵州","云南","西藏",
                                                     "陕西","甘肃","青海","宁夏","新疆","新疆")))
    
    rm(other.subdf)
    gc()
    
    
    diag.subdf=fread(paste0("../data/combined/",year,'_diag.csv')) %>%as.data.frame
    #算地理分组----
    
    #算CKD分期----
    诊断编码df=select(diag.subdf,all_of(var.table$vars[grepl('诊断编码',var.table$label)]))
    诊断名称df=select(diag.subdf,all_of(var.table$vars[grepl('诊断名称',var.table$label)]))
    rm(diag.subdf)
    gc()
    #IgA分类
    tmp.df=data.frame(matrix(NA,nrow=sum(res.df$IgAN),ncol=4))
    colnames(tmp.df)=c('牛津','Lee氏','Haas','其他')
    分类关键词=list(c('牛津','IgA[[:print:]]*M'),c('Lee','lee','LEE'),c('Ha'),
               c('IgA[[:print:]]*级','IgA[[:print:]]*型','级[[:print:]]*IgA','型[[:print:]]*IgA'))
    for(i in 1:4){
      tmp.df[,i]=get.diag(分类关键词[[i]],诊断名称df[res.df$IgAN,],replace.repetition.quantifiers = F)
    }
    IgA类型=rep('未分类',sum(res.df$IgAN))
    IgA类型[tmp.df$其他]='其他'
    IgA类型[tmp.df$牛津]='单用牛津'
    IgA类型[tmp.df$Lee氏]='单用Lee氏'
    IgA类型[tmp.df$Haas]='单用Haas'
    IgA类型[tmp.df$牛津&tmp.df$Lee氏]='牛津+Lee氏'
    IgA类型[tmp.df$牛津&tmp.df$Haas]='牛津+Haas'
    IgA类型[tmp.df$Lee氏&tmp.df$Haas]='其他'
    IgA类型=factor(IgA类型)
    res.df$IgA类型=factor(NA,levels=levels(IgA类型))
    res.df$IgA类型[res.df$IgAN]=factor(IgA类型)
    rm(诊断名称df)
    gc()
    
    CKD.group=rep(NA,nrow(res.df))
    CKD.group[get.diag(CKD.diag.all,诊断编码df,partial.match = F)]='CKD'
    CKD.group[get.diag('N18.801',诊断编码df)]='CKD1期'
    CKD.group[get.diag('N18.802',诊断编码df)]='CKD2期'
    CKD.group[get.diag('N18.803',诊断编码df)]='CKD3期'
    CKD.group[get.diag('N18.804',诊断编码df)]='CKD4期'
    CKD.group[get.diag('N18.001',诊断编码df)]='CKD5期'
    res.df$CKD.group=factor(CKD.group,levels = c('CKD','CKD1期','CKD2期',
                                                 'CKD3期','CKD4期','CKD5期'),ordered=T)
    res.df$有CKD=!is.na(res.df$CKD.group)
    rm(CKD.group)
    gc()
    
    #按诊断、手术分人群----
    surgery.subdf=fread(paste0("../data/combined/",year,'_surgery.csv')) %>%as.data.frame
    gc()
    手术编码df=surgery.subdf[,var.table$vars[grepl('操作编码',var.table$label)]]
    手术日期df=surgery.subdf[,var.table$vars[grepl('操作日期',var.table$label)]]
    rm(list=c('surgery.subdf'))
    gc()
    res.df$活检=get.diag(活检编码,手术编码df)
    res.df$介入止血=get.diag('39.7900x020',手术编码df)
    res.df$肾切除=get.diag(c('55.3','55.5'),手术编码df)
    res.df$输血治疗=get.diag('99.0401',手术编码df)
    活检日期=as.Date(get.date.surgery(活检编码,手术编码df[res.df$活检,],手术日期df[res.df$活检,]))
    rm(手术编码df)
    rm(手术日期df)
    gc()
    res.df$流产=get.diag(流产编码,诊断编码df)
    res.df$妊娠期高血压=get.diag(妊娠期高血压编码,诊断编码df)
    res.df$分娩=get.diag(分娩编码,诊断编码df)
    res.df$产妇= res.df$流产|res.df$妊娠期高血压|res.df$分娩
    res.df$新生儿结局=get.diag(新生儿结局编码,诊断编码df)
    res.df$诊断全糖肾=get.diag(糖肾编码,诊断编码df)
    res.df$诊断2糖肾=get.diag('E11.200',诊断编码df)
    res.df$诊断1糖肾=get.diag('E10.200',诊断编码df)
    res.df$诊断活检全糖肾=res.df$诊断全糖肾&res.df$活检
    res.df$诊断活检2糖肾=res.df$诊断2糖肾&res.df$活检
    res.df$诊断活检1糖肾=res.df$诊断1糖肾&res.df$活检
    res.df$全糖尿病=get.diag(糖尿病编码,诊断编码df)
    res.df$`2糖尿病`=get.diag('E11',诊断编码df)
    res.df$`1糖尿病`=get.diag('E10',诊断编码df)
    res.df$全糖尿病CKD=res.df$`全糖尿病`&res.df$有CKD
    res.df$`2糖尿病CKD`=res.df$`2糖尿病`&res.df$有CKD
    res.df$`1糖尿病CKD`=res.df$`1糖尿病`&res.df$有CKD
    res.df$糖尿病活检全糖肾=res.df$全糖尿病&res.df$活检
    res.df$糖尿病活检2糖肾=res.df$`2糖尿病`&res.df$活检
    res.df$糖尿病活检1糖肾=res.df$`1糖尿病`&res.df$活检
    res.df$IgAN=get.diag(IgAN编码,诊断编码df)
    
    #具体诊断
    #病理分型整理
    tmp.df=data.frame(matrix(NA,nrow=nrow(res.df),ncol=7))
    colnames(tmp.df)=c('IgAN','MN','LN','DN','FSGS','MCD','CTN')
    分类编码=list(IgAN编码,MN编码,LN编码,DN编码,FSGS编码,MCD编码,CTN编码)
    for(i in 1:ncol(tmp.df)){
      tmp.df[,i]=get.diag(分类编码[[i]],诊断编码df)
    }
    病理类型=rep(NA,nrow(res.df))
    病理类型[res.df$活检]='其他'
    tmp=rowSums(tmp.df)
    for(i in which(tmp==1)){
      病理类型[i]=colnames(tmp.df)[unlist(tmp.df[i,])]
    }
    病理类型[tmp>1]='其他'
    res.df$病理类型=as.factor(病理类型)
    rm(病理类型)
    
    #IgA具体诊断整理
    tmp.df=data.frame(matrix(NA,nrow=nrow(res.df),ncol=length(IgAN编码)))
    colnames(tmp.df)=IgAN编码
    for(i in 1:ncol(tmp.df)){
      tmp.df[,i]=get.diag(IgAN编码[i],诊断编码df)
    }
    IgAN具体编码=rep(NA,nrow(res.df))
    tmp=rowSums(tmp.df)
    for(i in which(tmp==1)){
      IgAN具体编码[i]=colnames(tmp.df)[unlist(tmp.df[i,])]
    }
    IgAN具体编码[tmp>1]='多种'
    res.df$IgAN具体编码=as.factor(IgAN具体编码)
    rm(IgAN具体编码)
    gc()
    
    res.df$合并糖网=get.diag('H36.0*',诊断编码df)
    res.df$合并高血压=get.diag('I10.x00',诊断编码df)
    res.df$合并难治性高血压=get.diag('I10.x12',诊断编码df)
    res.df$合并高胆固醇血症=get.diag('E78.000',诊断编码df)
    res.df$合并冠状动脉粥样硬化性心脏病=get.diag('I25.103',诊断编码df)
    res.df$合并脑梗死=get.diag('I63.',诊断编码df)
    res.df$合并脑出血=get.diag(c('I60.','I61.','I62.'),诊断编码df)
    res.df$合并脑卒中=res.df$合并脑梗死|res.df$合并脑出血
    res.df$血透=get.diag('Z49.101',诊断编码df)
    res.df$腹透=get.diag('Z49.201',诊断编码df)
    res.df$其他透析=get.diag('Z49.200',诊断编码df)
    res.df$感染=get.diag(感染编码,诊断编码df)
    res.df$股骨头坏死=get.diag(c('M87.002','M87.102'),诊断编码df)
    
    other.subdf=fread(paste0("../data/combined/",year,'_other.csv'),
                      colClasses = list(character="b13c")) %>%as.data.frame
    gc()
    直接用=c('level','grade','property','ownership','a12c','a14','b20','d01',
          'd09','d22','d23','d24','d25','thesameperson')
    重命名=c('机构级别','机构等次','机构类型','所有制形式','性别','年龄',
          '实际住院天数','住院总费用','住院总费用其中自付金额','中医治疗费',
          '西药费','中成药费','中草药费','患者id')
    
    res.df$死亡=other.subdf$b34c=='5'
    res.df$活检等待天数=NA
    res.df$活检等待天数[res.df$活检]=as.numeric(活检日期-as.Date(other.subdf$b12[res.df$活检]))
    res.df$活检至出院天数=NA
    res.df$活检至出院天数[res.df$活检]=as.numeric(as.Date(other.subdf$b15[res.df$活检])-活检日期)
    res.df$IgA新诊断=res.df$IgAN&res.df$活检
    res.df$IgA旧诊断=res.df$IgAN&(!res.df$活检)
    res.df$IgA死亡=res.df$死亡&res.df$IgAN
    res.df$全糖肾死亡=res.df$死亡&res.df$诊断全糖肾
    res.df$`1糖肾死亡`=res.df$死亡&res.df$诊断1糖肾
    res.df$`2糖肾死亡`=res.df$死亡&res.df$诊断2糖肾
    res.df$住肾科=(other.subdf$b13c=='306')&(other.subdf$b16c=='306')
    res.df$非住肾科=is.na(res.df$住肾科)|!res.df$住肾科
    res.df$IgA旧诊断住肾科=res.df$IgA旧诊断&res.df$住肾科
    res.df$IgA旧诊断非住肾科=res.df$IgA旧诊断&res.df$非住肾科
    res.df$离院方式=as.factor(as.numeric(other.subdf$b34c))
    #res.df$住三级肾科=(res.df$住肾科)&(other.subdf$level=='三级')
    
    非住肾科所住科室df=data.frame(matrix(NA,nrow=4,ncol=21))
    tmp=sort(table(other.subdf$b13c[res.df$IgA旧诊断非住肾科]),decreasing = T)[1:20]
    非住肾科所住科室df[1,]=c('科室代码',names(tmp))
    非住肾科所住科室df[2,]=c('入院',tmp)
    tmp=sort(table(other.subdf$b16c[res.df$IgA旧诊断非住肾科]),decreasing = T)[1:20]
    非住肾科所住科室df[3,]=c('科室代码',names(tmp))
    非住肾科所住科室df[4,]=c('出院',tmp)
    write.xlsx(非住肾科所住科室df,paste0('../result/其他结果表格/',year,'_旧IgA非住肾科所住科室与人数.xlsx'))
    
    other.subdf=other.subdf[,直接用]
    colnames(other.subdf)=重命名
    res.df=cbind(res.df,other.subdf)
    res.df$机构所有制和级别=paste0(res.df$所有制形式,res.df$机构级别)
    res.df$年龄[(res.df$年龄<0)|(res.df$年龄>120)]=NA
    res.df$新生儿=res.df$年龄==0
    res.df$新生儿结局=res.df$新生儿结局&res.df$新生儿
    res.df$老年=res.df$年龄>=60
    res.df$age.group=cut(res.df$年龄,c(0,seq(20,80,10),Inf),right=F,include.lowest=T)
    res.df$性别=factor(res.df$性别,levels=c(1,2),labels=c('男','女'))
    for(i in grep('费',colnames(res.df))){
      res.df[which(res.df[,i]<=0),i]=NA
    }
    for(i in grep('天数',colnames(res.df))){
      res.df[which(res.df[,i]<0),i]=NA
    }
    for(i in grep('天数',colnames(res.df))){
      res.df[which(res.df[,i]>40000),i]=NA
    }
    res.df$自付金额占比=res.df$住院总费用其中自付金额/res.df$住院总费用
    res.df$中医治疗费占比=res.df$中医治疗费/res.df$住院总费用
    res.df$西药费占比=res.df$西药费/res.df$住院总费用
    res.df$中成药费占比=res.df$中成药费/res.df$住院总费用
    res.df$中草药费占比=res.df$中草药费/res.df$住院总费用
    for(i in grep('占比',colnames(res.df))){
      res.df[which(res.df[,i]>1),i]=NA
    }
    rm(other.subdf)
    gc()
    
    for(i in 1:ncol(res.df)){
      if(colnames(res.df)[i]%in%人群分组){
        res.df[is.na(res.df[,i]),i]=F
      }
    }
    
    #剩余统计
    主要诊断人群=c('IgA死亡','IgA死亡有活检',paste0('IgA死亡CKD',1:5),'IgA死亡CKD未分期',
            'IgA旧诊断住肾科','IgA旧诊断非住肾科')
    主要诊断df=data.frame(matrix(NA,nrow=3*length(主要诊断人群),ncol=52))
    colnames(主要诊断df)[1:3]=c('人群','平均年龄','诊断（数量）')
    for(i in 1:length(主要诊断人群)){
      if(i==2){
        flag=res.df$活检&res.df$IgA死亡
      }else if(i %in%3:7){
        flag=res.df$IgA死亡&(res.df$CKD.group%in%paste0('CKD',i-2,'期'))
      }else if(i==8){
        flag=res.df$IgA死亡&(!res.df$CKD.group%in%c('CKD1期','CKD2期','CKD3期','CKD4期','CKD5期'))
      }else{
        flag=res.df[,主要诊断人群[i]]
      }
      if(sum(flag,na.rm = T)>0){
        tmp=sort(table(unlist(诊断编码df[flag,])),decreasing = T)[1:50]
        主要诊断df[(i*3-2),-c(1,2)]=names(tmp)
        主要诊断df[(i*3-1),-c(1,2)]=tmp
        主要诊断df[(i*3-1),1]=主要诊断人群[i]
        主要诊断df[(i*3-1),2]=mean(res.df$年龄[flag],na.rm=T)
      }
    }
    write.xlsx(主要诊断df,paste0('../result/其他结果表格/',year,'_死亡诊断与人数v2.xlsx'))
    rm(诊断编码df)
    gc()
    
    res.df0=res.df
    fwrite(res.df,paste0(year,'_processed.csv'))
  }else{
    res.df0=fread(paste0(year,'_processed.csv'))%>%as.data.frame
  }
  Sys.sleep(60)
  gc()
  
  #输出结果----
  res.df0$CKD.group=factor(res.df0$CKD.group,levels = c('CKD','CKD1期','CKD2期',
                                               'CKD3期','CKD4期','CKD5期'),ordered=T)

  众数列=c('province.group','IgA类型','病理类型','IgAN具体编码',
        '住肾科','非住肾科','离院方式','机构级别','机构等次','机构类型','所有制形式','性别',
        '机构所有制和级别','年龄','age.group')
  任一列=c('有CKD','活检','介入止血',
            '肾切除','输血治疗','流产','妊娠期高血压','分娩','产妇','新生儿结局',
            '诊断全糖肾','诊断2糖肾','诊断1糖肾','诊断活检全糖肾','诊断活检2糖肾',
            '诊断活检1糖肾','全糖尿病','2糖尿病','1糖尿病','全糖尿病CKD','2糖尿病CKD',
            '1糖尿病CKD','糖尿病活检全糖肾','糖尿病活检2糖肾','糖尿病活检1糖肾','IgAN',
            '合并糖网','合并高血压','合并难治性高血压',
            '合并高胆固醇血症','合并冠状动脉粥样硬化性心脏病','合并脑梗死','合并脑出血',
            '合并脑卒中','血透','腹透','其他透析','感染','股骨头坏死','死亡',
            'IgA新诊断','IgA旧诊断','IgA死亡','全糖肾死亡','1糖肾死亡',
            '2糖肾死亡','IgA旧诊断住肾科','IgA旧诊断非住肾科',
            '新生儿','老年')
  最大列=c('CKD.group')
  均值列=c('活检等待天数','活检至出院天数','实际住院天数',
        '自付金额占比','中医治疗费占比','西药费占比','中成药费占比','中草药费占比')
  总和列=c('住院总费用','住院总费用其中自付金额','中医治疗费','西药费','中成药费','中草药费')
  
  for(count_type in c('人数')){# ,'人次')){
    if(count_type=='人次'){
      res.df=res.df0
    }else{
      res.df=res.df0%>%
        group_by(患者id)
      tmp1=res.df %>%
        summarise(across(any_of(众数列),~first_mode(.x)))
      Sys.sleep(5)
      gc()
      print('f1')
      tmp2=res.df %>%
        summarise(across(any_of(任一列),~any(.x,na.rm=T)))
      Sys.sleep(5)
      gc()
      print('f2')
      tmp3=res.df %>%
        summarise(across(any_of(最大列),na_max))
      Sys.sleep(5)
      gc()
      print('f3')
      tmp4=res.df %>%
        summarise(across(any_of(均值列),~mean(.x,na.rm=T)))
      Sys.sleep(5)
      gc()
      print('f4')
      tmp5=res.df %>%
        summarise(across(any_of(总和列),~sum(.x,na.rm=T)))
      Sys.sleep(5)
      gc()
      print('f5')
      res.df=cbind(tmp1,tmp2,tmp3,tmp4,tmp5)%>%as.data.frame()
      Sys.sleep(5)
      gc()
      res.df$IgA旧诊断=!res.df$IgA新诊断
      res.df$IgA旧诊断住肾科=res.df$IgA旧诊断&res.df$住肾科
      res.df$IgA旧诊断非住肾科=res.df$IgA旧诊断&res.df$非住肾科
    }
    gc()
    cols.show=c()
    tmp.df=NULL
    for(i in 1:ncol(res.df)){
      if(!colnames(res.df)[i]%in%c('患者id','总体')){
        cols.show=c(cols.show,colnames(res.df)[i])
        if(is.numeric(res.df[,i])){
          tmp.df=cbind(tmp.df,res.df[,i])
          cols.show=c(cols.show,paste0(colnames(res.df)[i],'_as.nonnorm'))
        }
      }
    }
    nonnorm=cols.show[grepl('_as.nonnorm',cols.show)]
    colnames(tmp.df)=nonnorm
    res.df=cbind(res.df,tmp.df)
    rm(tmp.df)
    Sys.sleep(10)
    gc()
    # 生成表格
    for(人群 in 人群分组){
      for(strata in c("age.group","province.group","CKD.group","性别")){
        tryCatch(
          {if(grepl('糖',人群)){
            tab4 = CreateTableOne(vars = cols.show, strata = strata,test=F,includeNA = T,
                                  data = res.df[res.df[,人群]&res.df$住肾科,], addOverall = TRUE)
          }else if(人群=='总体'){
            tab4 = CreateTableOne(vars = cols.show, strata = strata,test=F,includeNA = T,
                                  data = res.df, addOverall = TRUE)
          }else{
            tab4 = CreateTableOne(vars = cols.show, strata = strata,test=F,includeNA = T,
                                  data = res.df[res.df[,人群],], addOverall = TRUE)
          }
            tab4Mat = print(tab4, nonnormal = nonnorm, exact = "extent", quote = FALSE,
                            noSpaces = TRUE, printToggle = FALSE)
            write.csv(tab4Mat, file = paste0('../result/主要结果表格/GB2312编码表格/',count_type,'_',year,'_',人群,'_',strata,'.csv'),fileEncoding = "GB2312")
            write.csv(tab4Mat, file = paste0('../result/主要结果表格/UTF8编码表格/',count_type,'_',year,'_',人群,'_',strata,'.csv'))},
          error = function(e) print(e)
          
        )
        
      }
      gc()
    }
    tab4 = CreateTableOne(vars = c('死亡'), strata = c("age.group","CKD.group"),
                          test=F,includeNA = T,data = res.df, addOverall = TRUE)
    tab4Mat = print(tab4, nonnormal = c(), exact = "extent", quote = FALSE,
                    noSpaces = TRUE, printToggle = FALSE)
    write.csv(tab4Mat, file = paste0('../result/其他结果表格/',count_type,'_',year,'_死亡统计_年龄CKD.csv'),fileEncoding = "GB2312")
    tab4 = CreateTableOne(vars = c('死亡'), strata = c("age.group","性别"),
                          test=F,includeNA = T,data = res.df, addOverall = TRUE)
    tab4Mat = print(tab4, nonnormal = c(), exact = "extent", quote = FALSE,
                    noSpaces = TRUE, printToggle = FALSE)
    write.csv(tab4Mat, file = paste0('../result/其他结果表格/',count_type,'_',year,'_死亡统计_年龄性别.csv'),fileEncoding = "GB2312")
    rm(res.df)
    Sys.sleep(5)
    gc()
  }
  rm(res.df0)
  Sys.sleep(60)
  gc()
}
