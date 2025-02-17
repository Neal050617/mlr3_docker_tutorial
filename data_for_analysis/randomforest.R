#!/usr/bin/env Rscript
# set_lib_paths.R

# v0.0.1 基于mlr3框架、黄志磊v0.0.0版本修改为一键化脚本，略微做了写修改，后续再优化
# 20240423 roc_pict函数有时会出现两个最优零界点，所以把点的取值从列表取值改成矩阵坐标取值;03.Probibality_of_disease.xlsx导出原始asv编号，features

#.libPaths(c("/work/software/app/R-4.3.3/lib64/R/library"))
#.libPaths(/usr/local/R-4.3.3/lib64/R/library)
getOption("repos")
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
## install.packages("remotes")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")#;BiocManager::install(version = "3.18")
if (!("mlr3extralearners" %in% installed.packages())) {remotes::install_github("mlr-org/mlr3extralearners@*release")}
if (!("aplot" %in% installed.packages()))             {remotes::install_github("YuLab-SMU/aplot")}
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(optparse,tidyverse, openxlsx, mlr3verse, data.table, mltools, mlr3tuningspaces, future, readxl,
               treeshap, kernelshap, shapviz, mlr3extralearners, ranger, randomForest, pROC, patchwork)

if (TRUE){
  option_list <- list(
    # 输入
    make_option(c("-i", "--input"), type="character", default="rarefac.ASV_genus.xls", help="输入的OTU表格"),
    make_option(c("-g", "--map"),   type="character", default="map-group.txt",         help="分组文件"),
    make_option(c("-c", "--color"), type="character", default="none", help="指定颜色：color.txt"),
    make_option(c(      "--part"),  type="character", default="0.7",                   help="咋分的样本，2/3还是3/4，或者0.8"),
    make_option(c(      "--split"), type="character", default="none",                  help="客户指定train和testsplit.map-group.txt"),
    make_option(c(      "--min"),   type="numeric",   default=3,                       help="最小特征数"),
    make_option(c(      "--max"),   type="numeric",   default=20,                      help="最大特征数"),
    # 添加筛选条件
    make_option(c("-u", "--unif"),  type="logical",   default=T,                       help="要不要归一化"),
    make_option(c(      "--gp"),    type="character", default="none",                  help="control-test顺序指定"),
    #make_option(c("-t", "--test"),  type="character", default="rarefac.Wilcoxon_rank_sum_unpaired.ALL.xls", help="差异"),
    make_option(c(      "--pv"),    type="double",    default=0.05,                    help="显著性筛选"),
    make_option(c(      "--pj"),    type="double",    default=1,                       help="pvalue.adjust.fdr显著性筛选"),
    make_option(c(      "--select"),type="character", default="none",                  help="select.list"),
    # 外部验证
    make_option(c("--valid"),       type="character", default="valid.rarefac.ASV_genus.xls", help="排列组合"),
    make_option(c("--map2"),        type="character", default="map2.txt",              help="显著性标记划分阈值？"),
    make_option(c("-s", "--seed"),  type="numeric",   default="123",                   help="设置种子,默认1234"),
    # 路径设置
    make_option(c("--cores"),         type="double",  default=8,                       help="准备给多少线程分析啊兄弟"),
    make_option(c("-o", "--outdir"),type="character", default="",                    help="输出文件夹默认为当前目录")
  )
  opts <- parse_args(OptionParser(option_list=option_list))
}
future::plan(multisession , workers = ifelse(opts$cores!=0, floor(opts$cores), ceiling(availableCores()/6)))

set.seed(opts$seed, kind = "Mersenne-Twister")
options("endocing"="UTF-8")
options(scipen = 5000)
options(dplyr.summarise.inform = FALSE)
nodeid.tbl_tree <- utils::getFromNamespace("nodeid.tbl_tree", "tidytree")
rootnode.tbl_tree <- utils::getFromNamespace("rootnode.tbl_tree", "tidytree")
offspring.tbl_tree <- utils::getFromNamespace("offspring.tbl_tree", "tidytree")
offspring.tbl_tree_item <- utils::getFromNamespace(".offspring.tbl_tree_item", "tidytree")
child.tbl_tree <- utils::getFromNamespace("child.tbl_tree", "tidytree")
parent.tbl_tree <- utils::getFromNamespace("parent.tbl_tree", "tidytree")

# 01.Function -------------------------------------------------------------
mytheme <- function(){
  theme_bw() + 
    theme(plot.title=element_text(size=rel(1),hjust=0.5),
          
          axis.title=element_text(size=rel(1)),
          axis.text.x=element_text(size=rel(1),angle = 90, 
                                   vjust = 0.5, hjust = 0.5,color = "black"),
          
          panel.grid.major=element_line(color="white"),
          panel.grid.minor=element_line(color="white"),
          
          legend.title=element_blank(),
          legend.text = element_text(size = 8),
          legend.key.size = unit(.4,'cm')
    )
}

roc_pict <- function( pod, title = ''){
  # pod = pod_test ; path = './训练集roc曲线'
  rocobj <- roc(pod[,2], pod[,4], # cases=pod[,4][pod[,2]=='OR'],
                #levels=c('OR','HC'),controls=pod[,4][pod[,2]=='HC'],  # 可以设置实验组或对照组
                smooth = F )      # 曲线是否光滑，当光滑时，无法计算置信区间
  
  # 计算临界点/阈值
  cutOffPoint <- coords(rocobj, "best")
  cutOffPointText <- paste0(round(cutOffPoint[1,1],3),"\n(",round(cutOffPoint[1,2],3),",",round(cutOffPoint[1,3],3),")")
  
  # 计算AUC值
  auc<-auc(rocobj)[1]
  # AUC的置信区间
  auc_low<-ci(rocobj,of="auc")[1]
  auc_high<-ci(rocobj,of="auc")[3]
  
  # 计算置信区间
  ciobj <- ci.se(rocobj,specificities=seq(0, 1, 0.01))
  data_ci<-ciobj[1:101,1:3]
  data_ci<-as.data.frame(data_ci)
  x=as.numeric(rownames(data_ci))
  data_ci<-data.frame(x,data_ci)
  
  # 绘图
  p <- ggroc(rocobj, color="black", size=0.5, legacy.axes = F ) + # FALSE时 横坐标为1-0 specificity；TRUE时 横坐标为0-1 1-specificity
    geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1),        # 绘制对角线
                 colour='grey', linetype = 'longdash') +
    geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.),fill = 'lightblue',alpha=0.5)+  # 绘制置信区间,当legacy.axes=TRUE时， 把x=x改为x=1-x
    geom_point(aes(x = cutOffPoint[1,2],y = cutOffPoint[1,3]),color = 'red')+ # 绘制临界点/阈值
    # geom_text(aes(x = cutOffPoint[[2]],y = cutOffPoint[[3]],label=cutOffPointText,fontface = "plain"),size=4,vjust=-1)+# 添加临界点/阈值文字标
    ggtitle(title) + mytheme() + #theme_bw()+
    annotate("text", x = cutOffPoint[1,2]*.9, y =  cutOffPoint[1,3]*.9, 
             label = cutOffPointText,hjust = 0.5, vjust = 0, size = 4) +
    annotate("text", x = 0.2, y = 0.1, hjust = 0.6, vjust = 0.2, size = 4,
             label = paste0("AUC: ", round(auc,4), "\n","95% CI: ", round(auc_low,4), "-",round(auc_high,4)))
  
  plot_bx <- pod[,c(2,5)] %>% rename_all(~c("group","prob"))
  q <- ggplot(plot_bx) +
    stat_boxplot(aes(x=group,y=prob), geom='errorbar', linetype=1, width=0.5)+  #whiskers
    geom_boxplot(aes(x=group,y=prob,fill=group),show.legend = FALSE) +
    scale_fill_manual(values=sc$V2) + ggtitle(title) + mytheme()
  
  #ggsave(paste0(path,'.pdf'),p,width = 12.5,height = 12,units = 'in',dpi = 300)
  #ggsave(paste0(path,'.png'),p,width = 12,height = 12,units = 'in',dpi = 600 )
  return(list(p,q))
}

biomaker_select <- function(
    biomaker_num ,
    task_parm = task,
    split_gp_parm = split_gp,
    ex_valid = T, # 是否有外部验证
    train_re = T, # 是否要训练集结果
    workers= 8, # 开始并行
    folds = 5L, # 几折
    term_evals = 100, # 超参数训练多少次
    name_value_parm=names_values,# featrue重命名
    out=opts$outdir) # 输出文件夹
{
  biomaker_num_fix <- ifelse(biomaker_num < 10, paste0("0", biomaker_num), as.character(biomaker_num))
  #dir <- file.path(out,paste0(biomaker_num_fix,'_features'))
  #print(dir)
  if(!dir.exists(out)) dir.create(out)
  # biomaker_num  = 10
  tsk_right = task_parm$clone()
  tsk_right$select(select_name[1:biomaker_num])
  
  set.seed(123, kind = "Mersenne-Twister")

  ## 指定算法与优化超参数
  learner_rf = lrn("classif.ranger",
                   predict_type = "prob",
                   importance = 'impurity',
                   predict_sets = c("train", "test"))
  tune_ps_rf = ps(
    num.trees = p_int(lower = 500, upper = 1000),
    max.depth =  p_int(lower = 3, upper = 5),
    min.node.size = p_int(lower = 3, upper =  10),
    mtry = p_int(lower = ceiling(sqrt(biomaker_num)), upper = min(biomaker_num,ceiling(sqrt(biomaker_num))+2) )
  )
  
  learner_rf = auto_tuner(
    tuner = tnr("random_search"),
    learner = learner_rf,
    resampling = rsmp("cv", folds = folds),
    measure = msr("classif.auc"),
    search_space = tune_ps_rf ,
    term_evals =  term_evals,
  )
  plan("multisession",workers=workers)
  nbrOfWorkers()
  start<-Sys.time()
  print(start)
  train = learner_rf$train(tsk_right, row_ids = split_gp_parm$train)
  end<-Sys.time()
  print(end)
  runningtime<-end-start
  print(runningtime)
  plan(sequential)

  prediction_test = learner_rf$predict(tsk_right, row_ids = split_gp_parm$test)
  # 是否修改阈值
  # prediction_test$set_threshold(0.6666667) 提高了 先放这，有人是提出了不同的方法
  # prediction_test$score(msr("classif.acc"))  0.8285714 原来 0.7几
  # prediction_test$score(msr("classif.auc"))
  measures = msrs(c("classif.auc","classif.acc", "classif.tpr",'classif.fpr'))
  
  test_measures = prediction_test$score(measures) %>% t()%>%
    as.data.frame(.,row.names="test_measures")%>%
    mutate(bio_num = biomaker_num)%>%
    rownames_to_column()%>%
    select(rowname,bio_num,everything())
  
  roc_plot <- list()
  
  # 是否要训练集的结果
  if (train_re == T){
    prediction_train = learner_rf$predict(tsk_right, row_ids = split_gp_parm$train)
    train_measures = prediction_train$score(measures) %>% t()%>%
      as.data.frame(.,row.names="train_measures")%>%
      mutate(bio_num = biomaker_num)%>%
      rownames_to_column()%>%
      select(rowname,bio_num,everything())
    pod_train = prediction_train$print() %>% as.data.frame() #pod_test = prediction_test$print() %>% as.data.frame()
    
    xlsx::write.xlsx2(pod_train, file=file.path(out,paste0(biomaker_num_fix,".Probibality_of_disease.xlsx")),sheetName = "train",
                      col.names = TRUE,row.names = FALSE,append = FALSE)
    
    roc_plot[["train"]] <- roc_pict(pod = pod_train,title="train")
    test_measures = rbind(train_measures, test_measures)
  }

  # https://ayueme.github.io/R_clinical_model/roc-bootstrap.html
  # autoplot(prediction_test, type = "roc")
  
  # POD值
  pod_test = prediction_test$print() %>% as.data.frame()
  xlsx::write.xlsx2(pod_test, file=file.path(out,paste0(biomaker_num_fix,".Probibality_of_disease.xlsx")),sheetName = "test",
                    col.names = TRUE,row.names = FALSE,append = TRUE)
  
  roc_plot[["test"]] <- roc_pict(pod = pod_test,title="test")
  # 如果有外部验证
  if(ex_valid == T){
    external_valid = learner_rf$predict_newdata(external_data)
    measures = msrs(c("classif.auc","classif.acc", "classif.tpr",'classif.fpr'))
    external_measures = external_valid$score(measures) %>% t()%>%
      as.data.frame(.,row.names="external_measures") %>%
      mutate(bio_num = biomaker_num) %>%
      rownames_to_column() %>%
      select(rowname,bio_num,everything())
    
    test_measures = rbind(test_measures,external_measures)
    
    pod_external = external_valid$print() %>% as.data.frame()
    xlsx::write.xlsx2(pod_external, file=file.path(out,paste0(biomaker_num_fix,".Probibality_of_disease.xlsx")),sheetName = "external",
                      col.names = TRUE,row.names = FALSE,append = TRUE)
    
    roc_plot[["external"]] <- roc_pict(pod =  pod_external,title="external")
  }

  # 特征重要性
  learner_rf$importance() %>% as.data.frame() %>% rownames_to_column() %>% 
    rename_all(~c("new_colnames","MeanDecreaseAccuracy")) %>% inner_join(names_values,.) %>% dplyr::select(-old_colnames) %>% 
    write_tsv(file.path(out,paste0(biomaker_num_fix,".varImpPlot.xls")))
  # 最优超参数
  learner_rf$tuning_result$learner_param_vals[[1]]
  
  ### 特征重要性数据 改成原来的名字
  data_var_imp = enframe(
    learner_rf$importance(),
    name = "variable",
    value = "importance"
  )
  data_var_imp$old_colnames <- map_chr(data_var_imp$variable, function(x) {
    feature_row <- name_value_parm[name_value_parm$new_colnames == x, ]
    feature_row$old_colnames
  })
  
  data_var_imp$old_colnames <-  factor(data_var_imp$old_colnames,levels = rev(data_var_imp$old_colnames))
  data_var_imp$old_colnames_n <- Add_breaks(data_var_imp$old_colnames)
  data_var_imp$old_colnames_n <- factor(data_var_imp$old_colnames_n,levels = rev(data_var_imp$old_colnames_n))
  ### 特征重要性图 可以美化哈
  w <- plot_p_feat(data_var_imp,plot=F)
  ## 合并出图
  ComBn_plot(roc_plot,w=w,out_parm=out,biomaker_num_fix_parm=biomaker_num_fix,parm=learner_rf$tuning_result$learner_param_vals[[1]])
  
  return(list(test_measures,learner_rf$tuning_result$learner_param_vals[[1]]))
}

ComBn_plot <- function(P,w,out_parm=opts$outdir,biomaker_num_fix_parm=biomaker_num_fix,
                       parm=learner_rf$tuning_result$learner_param_vals[[1]]){
  # P <- roc_plot
  if (length(P) == 3){
  layout <- "
  AABBGGGGG
  CCDDGGGGG
  EEFFHHHHH"
  }else if (length(P) == 3){
    layout <- "
  AABBGGGGGHH
  CCDDGGGGGHH"
  }else if (length(P) == 3){
    layout <- "AABBGGGGGHH"
  }
  
  Q <- P[[1]][[1]] + P[[1]][[2]]
  if (length(P) > 1){ for (i in 2:length(P)){ Q <- Q + P[[i]][[1]] + P[[i]][[2]]} }
  Q <- Q + w + plot_layout(design = layout,guides = 'collect') + 
    plot_annotation(tag_levels = c('A', '1')) + # ncol = 2, byrow = TRUE
    gridExtra::tableGrob(t(as.data.frame(parm)))
  ggsave(file.path(out_parm,paste0(biomaker_num_fix_parm,".roc.pdf")),Q,dpi = 300,device = cairo_pdf,
         width = 18,
         height = 4*length(P))
}

# 曲线找拐点
knee_point <- function(y){
  kk <- boxplot.stats(y)$conf[[2]]
  return(kk)
}

# 太长的话加换行符
Add_breaks <- function(x){# x <- data_var_imp$old_colnames_n
  xixi <- unique(as.character(x))
  kk <- knee_point(nchar(xixi))
  if (max(nchar(xixi))-kk < 5){
    hehe <- xixi
  }else{
    haha <- floor(kk)
    hehe <- map_chr(xixi,function(x) {# x <- xixi[9]
      xx <- strsplit(x,'')[[1]]
      ifelse(length(xx) > haha,str_c(str_c(xx[1:haha],collapse = ''),"\n",
                                     str_c(xx[(haha+1):length(xx)],collapse = '')),x)
    })
  }
  return(hehe)
}

# importance plot
plot_p_feat <- function(data_var_imp,out=opts$outdir,plot=T){
  p_feat <- ggplot(data_var_imp, aes( x = old_colnames_n, y = importance, fill = importance)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    ylab("Variable Importance") + xlab("") + #ggtitle("Information Value Summary") +
    guides(fill = "none") +
    scale_fill_gradient(low = "#327eba", high = "#e06663")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  if (isTRUE(plot))
    ggsave(file.path(out,'00.MeanDecreaseAccuracy.pdf'),p_feat,width = 10,height = 6)
  return(p_feat)
}

# 02.Read_in --------------------------------------------------------------
if (opts$outdir == "") { opts$outdir <- getwd() }
if (!isTRUE(dir.exists(opts$outdir))){dir.create(opts$outdir)}
if (!isTRUE(dir.exists(file.path(opts$outdir,paste0("ROC.",opts$seed))))){dir.create(file.path(opts$outdir,paste0("ROC.seed",opts$seed)))}

mapcol <- c("#61d04f","#df536b","#377EB8","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF") # ,"#4DAF4A","#E41A1C" RColorBrewer::brewer.pal(n = 8,name = "Set1")
Gp <- read_tsv(opts$map) %>% rename_all(~c("SampleID","group")) %>% mutate(group = fct_inorder(group))

# 指定分组顺序，要求对照组在前，实验组在后，重要
if (opts$gp != "none"){
  gp <- strsplit(opts$gp,split = "-")[[1]]
  if (length(gp) == 1) stop("Length of variable gp is equal to 1. Program terminated.")
  Gp <- Gp %>% filter(group %in% gp) %>% mutate(group = factor(group,levels=gp))
  if (length(unique(Gp$group)) == 1)  stop("Length of variable Gp  is equal to 1. Program terminated.")
}else{
  gp <- levels(Gp$group)
}

# 指定分组颜色，对照组为绿色，实验组为红色
if (opts$color != "none"){# opts$color = c("color.txt")
  sc <- read.table(opts$color,sep="\t",comment.char = "",check.names = FALSE)
  sc <- sc[which(as.vector(sc[,1]) %in% unique(unlist(Gp$group))),]
  Gp$group <- factor(Gp$group,levels = as.vector(sc[,1]))
  Gp <- Gp %>% arrange(group)
} else{
  sc <- cbind(levels(Gp$group),mapcol[1:nlevels(Gp$group)]) %>% 
    as.data.frame(stringsAsFactors = FALSE)
}

# 列是物质名 行是样本名，最后一列是group分组，其他都可以自行选择喜欢的
Data <- read_tsv(opts$input) %>% rename_at(1,~"Features")
if (isTRUE(opts$unif)){ Data <- Data %>% mutate(across(where(is.numeric), ~ . / sum(.))) }
Data <- Data %>% 
  pivot_longer(!Features) %>% pivot_wider(names_from = "Features",values_from = "value") %>% rename_at(1,~"SampleID") %>%
  inner_join(Gp)

## 注意！！！ 因为列名即物质名 大多都比较奇怪，现在的算法不支持这种奇怪的名字
## data external_data 都需要处理，此外data external_data的物质名顺序需一致
## 此外在特征重要性中需要物质名，所以下面是处理方式
names_values <- tibble(
  old_colnames = colnames(Data),
  new_colnames = c("SampleID",paste0("feature_", seq(1, length(old_colnames)-2)),'group')
)

colnames(Data) <-  names_values$new_colnames

## 最初的随机森林代码，拿到特征重要性，排序，然后循环
rf_vari_imp = ranger(formula = group~., # group 
                     data = Data[,-1], # data
                     num.trees = 1000,
                     importance = "permutation",seed = 42) #permutation
sort_var_imp = sort(rf_vari_imp[["variable.importance"]],decreasing = T)
select_name = names(sort(rf_vari_imp[["variable.importance"]],decreasing = T)[1:20])

### 特征重要性数据 改成原来的名字
data_var_imp = enframe(rf_vari_imp[["variable.importance"]][select_name], 
                       name = "variable", value = "importance")
data_var_imp$old_colnames <- map_chr(data_var_imp$variable, function(x) {
  feature_row <- names_values[names_values$new_colnames == x, ]
  feature_row$old_colnames
})
data_var_imp$old_colnames <-  factor(data_var_imp$old_colnames,levels = rev(data_var_imp$old_colnames))
data_var_imp$old_colnames_n <- Add_breaks(data_var_imp$old_colnames)
data_var_imp$old_colnames_n <- factor(data_var_imp$old_colnames_n,levels = rev(data_var_imp$old_colnames_n))
### 特征重要性图 可以美化哈
plot_p_feat(data_var_imp,out=file.path(opts$outdir,paste0("ROC.seed",opts$seed)))

# 03.建模准备 ------------------------------------------------------------------

# 参数
# biomaker_num 就是i
# positive 二分类需要,此外，如果因子化的字符串变量，直接输入字符串即可
# 比如 positive = 'OR'
# term_evals
# return tab2

task <- as_task_classif(Data,target = 'group',positive = gp[1])

if(grepl("/",opts$part)){
  aa <- strsplit(opts$part,'/')[[1]][1] %>% as.numeric ; bb <- strsplit(opts$part,'/')[[1]][2] %>% as.numeric 
}else{
  aa <- strsplit(MASS::fractions(opts$part %>% as.numeric) %>% as.character,'/')[[1]][1] %>% as.numeric 
  bb <- strsplit(MASS::fractions(opts$part %>% as.numeric) %>% as.character,'/')[[1]][2] %>% as.numeric 
}
zz <- aa/bb

if (opts$split == "none"){
  split_gp <- partition(task = task,ratio = zz,stratify = T)
  
  bind_rows(map(split_gp,as_tibble), .id = "Split") %>% mutate(value = Data$SampleID[value]) %>% 
    rename_at(2,~"SampleID") %>%
    left_join(Data[,c("SampleID","group")] %>% mutate(group = as.character(group)),.) %>%
    write_tsv(file.path(opts$outdir,"split.map-group.txt"))
}else{
  #split_gp_pre <- Data %>% dplyr::select(SampleID, group) %>% group_by(group) %>% nest() %>%
  #  mutate(
  #    num = map_dbl(data, ~nrow(.x)), num2 = map_dbl(num, ~floor(.x * zz)),
  #    data2 = map2(data, num2, ~dplyr::slice_sample(.x, n = .y, replace = FALSE)),
  #    data3 = map2(data,data2,~setdiff(pull(.x,SampleID), pull(.y,SampleID))),
  #    data = map(data, ~factor(pull(.x, SampleID), levels = pull(.x, SampleID)))
  #  ) %>% mutate(train = map(data2, ~factor(pull(.x, SampleID), levels = Data$SampleID) %>% as.numeric()),
  #         test = map(data3, ~factor(.x, levels = Data$SampleID) %>% as.numeric()))
  #split_gp <- list(train=unlist(split_gp_pre$train),test=unlist(split_gp_pre$test))
  
  split_gp_pre <- Data[,c("SampleID","group")] %>% 
    left_join(read_tsv(opts$split) %>% rename_all(~c("SampleID","group","Split"))) %>% 
    mutate(Split = factor(Split,levels=c("train","test"))) %>%
    group_by(group) %>% nest() %>% 
    mutate(data2 = map(data,~dplyr::filter(.x,Split == "train") %>% dplyr::select(SampleID)),
           data3 = map(data,~dplyr::filter(.x,Split == "test") %>% dplyr::select(SampleID))
           ) %>% 
    mutate(train = map(data2, ~factor(pull(.x, SampleID), levels = Data$SampleID) %>% as.numeric()),
           test =  map(data3, ~factor(pull(.x, SampleID), levels = Data$SampleID) %>% as.numeric()))
  split_gp <- list(train=unlist(split_gp_pre$train),test=unlist(split_gp_pre$test))
  
}

# 04.建模 -------------------------------------------------------------------
# 外部验证
if (opts$valid != "none" & opts$map2 != "none"){
  Gp2 <- read_tsv(opts$map2) %>% rename_all(~c("SampleID","group")) %>% filter(group %in% gp) %>% mutate(group = factor(group,levels=gp))
  if (length(unique(Gp2$group)) == 1)  stop("Length of variable Gp2  is equal to 1. Program terminated.")
  
  external_data <- read_tsv(opts$valid) %>% rename_at(1,~"Features")
  if (isTRUE(opts$unif)){ external_data <- external_data %>% mutate(across(where(is.numeric), ~ . / sum(.))) }
  external_data <- left_join(names_values,external_data,by=c("old_colnames"="Features")) %>%
    dplyr::select(-old_colnames) %>% slice(-1,-n()) %>% 
    pivot_longer(!new_colnames) %>% pivot_wider(names_from = "new_colnames",values_from = "value") %>% 
    mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
    rename_at(1,~"SampleID") %>% inner_join(Gp2)
}

d1 <- map(opts$min:opts$max,function(i){
  biomaker_select(biomaker_num=i,task = task,ex_valid = (opts$valid!="none"),
    workers= opts$cores,
    out=file.path(opts$outdir,paste0("ROC.seed",opts$seed)))
})

map(d1,function(x) x[[1]]) %>% bind_rows() %>%
  write_tsv(file.path(opts$outdir,paste0("ROC.seed",opts$seed),"00.All_meaures.xls"))

map(d1,function(x) x[[2]] %>% as_tibble() %>% add_column(bio_num = unique(x[[1]]$bio_num),.before = 3)) %>% bind_rows() %>%
  write_tsv(file.path(opts$outdir,paste0("ROC.seed",opts$seed),"00.Optimal_hyperparameters.xls"))

# d2 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = F)
# d3 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = T,train_re = F)
# d4 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = F,train_re = F)
# d2 <- map_dfc(3:4,biomaker_select,task = task,ex_valid = F)

