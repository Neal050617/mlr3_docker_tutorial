library(tidyverse)
library(openxlsx)
library(mlr3verse)
library(data.table)
library(mltools)
library(mlr3tuningspaces)
library(future)
library(readxl)
library(treeshap)
library(kernelshap)
library(shapviz)
library(mlr3extralearners)
library(ranger)
library(randomForest)
library(pROC)
roc_pict = function(
    pod,
    path = ''
){
  rocobj <- roc(pod[,2], pod[,4],
                #levels=c('OR','HC'),
                # controls=pod[,4][pod[,2]=='HC'],  # 可以设置实验组或对照组
                # cases=pod[,4][pod[,2]=='OR'],
                smooth = F       # 曲线是否光滑，当光滑时，无法计算置信区间
  ) 
  # 计算临界点/阈值
  cutOffPoint <- coords(rocobj, "best")
  cutOffPointText <- paste0(round(cutOffPoint[1],3),"\n(",round(cutOffPoint[2],3),",",round(cutOffPoint[3],3),")")
  
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
  p <- ggroc(rocobj,
             color="black",
             size=0.5,
             legacy.axes = F # FALSE时 横坐标为1-0 specificity；TRUE时 横坐标为0-1 1-specificity
  )+
    geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1),        # 绘制对角线
                 colour='grey', 
                 linetype = 'longdash'
    ) +
    geom_ribbon(data = data_ci,                                # 绘制置信区间
                aes(x=x,ymin=X2.5.,ymax=X97.5.),               # 当legacy.axes=TRUE时， 把x=x改为x=1-x
                fill = 'lightblue',
                alpha=0.5)+
    geom_point(aes(x = cutOffPoint[[2]],y = cutOffPoint[[3]]),color = 'red')+ # 绘制临界点/阈值
    # geom_text(aes(x = cutOffPoint[[2]],y = cutOffPoint[[3]],
    #               label=cutOffPointText,fontface = "plain"),
    #           size=4,vjust=-1)+# 添加临界点/阈值文字标
    theme_bw()+
    annotate("text", x = cutOffPoint[[2]] , y =  cutOffPoint[[3]] + 0.1, 
             label = cutOffPointText,
             hjust = 0.5, vjust = 0, size = 4)+
    
    annotate("text", x = 0.2, y = 0.1, label = paste0("AUC: ", round(auc,4), "\n",
                                                      "95% CI: ", round(auc_low,4), "-",round(auc_high,4)),
             hjust = 0.6, vjust = 0.2, size = 4)
  
  ggsave (
    paste0(path,'.pdf'),p,
    width = 12,height = 7.5,units = 'in',dpi = 600
  )
  ggsave (
    paste0(path,'.png'),p,
    width = 12,height = 7.5,units = 'in',dpi = 600
  )
  
}


biomaker_select = function(
    biomaker_num ,
    task = task,
    ex_valid = T, # 是否有外部验证
    train_re = T, # 是否要训练集结果
    workers= 8, # 开始并行
    folds = 5L, # 几折
    term_evals = 100) # 超参数训练多少次
{
  dir <- paste0('./',biomaker_num,'个代谢物建模')
  print(dir)
  if(!dir.exists(dir)) dir.create(dir)
  old.wd <- getwd()
  setwd(dir)
  # biomaker_num  = 10
  tsk_right = task$clone()
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
  train = learner_rf$train(tsk_right, row_ids = split$train)
  end<-Sys.time()
  print(end)
  runningtime<-end-start
  print(runningtime)
  plan(sequential)
  # 特征重要性
  learner_rf$importance()
  # 最优超参数
  learner_rf$tuning_result$learner_param_vals[[1]]
  
  
  prediction_test = learner_rf$predict(tsk_right, row_ids = split$test)
  # 是否修改阈值
  # prediction_test$set_threshold(0.6666667) 提高了 先放这，有人是提出了不同的方法
  # prediction_test$score(msr("classif.acc"))  0.8285714 原来 0.7几
  # prediction_test$score(msr("classif.auc"))
  measures = msrs(c("classif.auc","classif.acc", "classif.tpr",'classif.fpr'))
  
  test_measures = prediction_test$score(measures)
  # 是否要训练集的结果
  if (train_re == T){
    prediction_train = learner_rf$predict(tsk_right, row_ids = split$train)
    train_measures = prediction_train$score(measures)
    pod_train =prediction_train$print() %>% as.data.frame() #pod_test = prediction_test$print() %>% as.data.frame()
    write.xlsx(pod_train,'./训练集概率表.xlsx')
    roc_pict(pod = pod_train,path = './训练集roc曲线')
    test_measures = rbind(train_measures, test_measures)
  }
  
  
  
  
  
  # https://ayueme.github.io/R_clinical_model/roc-bootstrap.html
  # autoplot(prediction_test, type = "roc")
  
  # POD值
  pod_test = prediction_test$print() %>% as.data.frame()
  write.xlsx(pod_test,'./测试集预测概率表.xlsx')
  
  # 以前老脚本的画法
  # pdf(pdf.dir3)
  # roc <- roc(pod_test[,2],pod_test[,4],plot=T,col="black",ci=F,auc.polygon=F,print.thres=F,print.auc=F,percent=F,xlab="Specificity(%)",ylab="Sensitivity(%)")
  # # 秩和检验
  # 
  # # 置信区间
  # sens.ci <- ci.se(roc)
  # plot(sens.ci, type="s", col="lightblue",border="white")
  # # 标注
  # legend("bottomright",bty="n",paste0("AUC: ",round(ci(roc)[2],4),"\n","95% CI: ",round(ci(roc)[1],4),"-",round(ci(roc)[3],4)))
  # tt <- paste0(round(coords(roc,"best")[1,1],4),"\n","(",round(coords(roc,"best")[1,2],4),",",round(coords(roc,"best")[1,3],4),")")
  # points(coords(roc,"best")[1,2],coords(roc,"best")[1,3],pch=16,col="red",cex=1.5,font=1)
  # text(coords(roc,"best")[1,2]-0.1,coords(roc,"best")[1,3]-0.1,tt,cex=1.5,pos=4,col="black")
  # dev.off()
  
  
  
  #roc
  
  roc_pict(pod = pod_test,path = './测试集roc曲线')
  # 如果有外部验证
  if(ex_valid == T){
    external_valid = learner_rf$predict_newdata(external_data)
    measures = msrs(c("classif.auc","classif.acc", "classif.tpr",'classif.fpr'))
    external_measures = external_valid$score(measures)
    measures = rbind(test_measures,external_measures)
    
    pod_external = external_valid$print() %>% as.data.frame()
    write.xlsx(pod_external,'./外部测试集预测概率表.xlsx')
    roc_pict(pod =  pod_external,path = './外部测试集roc曲线')
    
    test_measures2 = measures %>%
      as.data.frame()%>%
      mutate(bio_num = biomaker_num)%>%
      rownames_to_column()%>%
      select(rowname,bio_num,everything())
    setwd(old.wd)
    return( test_measures2 )
    
  }
  if(train_re == T){
    test_measures1 = test_measures %>%
      # t()%>%
      as.data.frame()%>%
      mutate(bio_num = biomaker_num)%>%
      rownames_to_column()%>%
      select(rowname,bio_num,everything())
  }else{
    test_measures1 = test_measures %>%
      t()%>%
      as.data.frame()%>%
      mutate(bio_num = biomaker_num)%>%
      rownames_to_column()%>%
      select(rowname,bio_num,everything())
  }
  
  
  
  setwd(old.wd)
  return( test_measures1 )
} 



### 从这里开始-----------------------
# 数据可以自行选择excel csv读取 
# 最后的需求 列是物质名 行是样本名，最后一列是group分组，其他都可以自行选择喜欢的
data = read.xlsx('./data.xlsx',sheet = 2,rowNames = T)
external_data = read.xlsx('./valid.xlsx',sheet = 2,rowNames = T)  %>% 
  select(colnames(data))

## 注意！！！ 因为列名即物质名 大多都比较奇怪，现在的算法不支持这种奇怪的名字
## data external_data 都需要处理，此外data external_data的物质名顺序需一致
## 此外在特征重要性中需要物质名，所以下面是处理方式
ex = T # or F 可以考虑写个函数
if (ex == T){
  old_colnames = colnames(data)
  old_external_data_colnames = colnames(external_data)
  if(sum(old_colnames == old_external_data_colnames) != length(old_colnames)){
    stop('data与external_data物质名不匹配，请检查')
  }
  
  new_colnames = paste0("feature_", seq(1, length(old_colnames)-1))
  new_colnames = c(new_colnames,'group')
  
  names_values <- data.frame(
    old_colnames = old_colnames,
    new_colnames = new_colnames
  )
  
  colnames(data) =  new_colnames
  colnames(external_data) = new_colnames
  # group是因子或者01都可以
  data$group = factor(data$group,levels = c("NoCSPH",'CSPH'))
  external_data$group = factor(external_data$group,levels = c("NoCSPH",'CSPH'))
}else{
  old_colnames = colnames(data)
  new_colnames = paste0("feature_", seq(1, length(old_colnames)-1))
  new_colnames = c(new_colnames,'group')
  
  names_values <- data.frame(
    old_colnames = old_colnames,
    new_colnames = new_colnames
  )
  
  colnames(data) =  new_colnames
 
  # group是因子或者01都可以
  data$group = factor(data$group,levels = c("NoCSPH",'CSPH'))
  
}




set.seed(123, kind = "Mersenne-Twister")

## 最初的随机森林代码，拿到特征重要性，排序，然后循环
rf_vari_imp = ranger(formula = group~., # group 
                     data = data, # data
                     num.trees = 1000,
                     importance = "permutation",seed = 42) #permutation
sort_var_imp = sort(rf_vari_imp[["variable.importance"]],decreasing = T)
select_name = names(sort(rf_vari_imp[["variable.importance"]],decreasing = T)[1:20])

### 特征重要性数据 改成原来的名字
data_var_imp = enframe(
  rf_vari_imp[["variable.importance"]][select_name],
  name = "variable",
  value = "importance"
)
data_var_imp$old_colnames <- map_chr(data_var_imp$variable, function(x) {
  feature_row <- names_values[names_values$new_colnames == x, ]
  feature_row$old_colnames
})

data_var_imp$old_colnames =  factor(data_var_imp$old_colnames,levels = rev(data_var_imp$old_colnames))


### 特种重要性图 可以美化哈
p_feat <- ggplot(
  data_var_imp,
  aes(
    x = old_colnames, 
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  #ggtitle("Information Value Summary") +
  guides(fill = "none") +
  scale_fill_gradient(low = "#327eba", high = "#e06663")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(p_feat,'')

# 参数
# biomaker_num 就是i
# positive 二分类需要,此外，如果因子化的字符串变量，直接输入字符串即可
# 比如 positive = 'OR'
# term_evals
# return tab2

# 固定种子
set.seed(123, kind = "Mersenne-Twister")

# 
task = as_task_classif(data,target = 'group',positive = 'CSPH')

split = partition(task = task,ratio = 0.7,stratify = T) 

d1 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = T) 
# d2 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = F)
# d3 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = T,train_re = F)
# d4 <- map_dfr(3:5,biomaker_select,task = task,ex_valid = F,train_re = F)
#d2 <- map_dfc(3:4,biomaker_select,task = task,ex_valid = F)
