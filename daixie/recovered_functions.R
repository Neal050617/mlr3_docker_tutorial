

# 函数: Add_breaks 
function(x){# x <- data_var_imp$old_colnames_n
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


# 函数: child.tbl_tree 
function (.data, .node, ...) 
{
    valid.tbl_tree(.data)
    if (is.character(.node)) {
        .node <- .data$node[.data$label == .node]
    }
    .data[.data$parent == .node & .data$parent != .data$node, 
        ]
}
<bytecode: 0x5601e2a46ef8>
<environment: namespace:tidytree>


# 函数: ComBn_plot 
function(P,w,out_parm=opts$outdir,biomaker_num_fix_parm=biomaker_num_fix,
                       parm=learner_rf$tuning_result$learner_param_vals[[1]]){
  # P <- roc_plot
  if (length(P) == 3){
  layout <- "
  AABBGGGGG
  CCDDGGGGG
  EEFFHHHHH"
  }else if (length(P) == 2){
    layout <- "
  AABBGGGGGHH
  CCDDGGGGGHH"
  }else if (length(P) == 1){
    #layout <- "AABBGHH"
    layout <- 
      "AABBGGHH
       AABBGGHH
       AABBGGHH
       AABBGGHH"
  }
  
  Q <- P[[1]][[1]] + P[[1]][[2]]
  if (length(P) > 1){ for (i in 2:length(P)){ Q <- Q + P[[i]][[1]] + P[[i]][[2]]} }
  Q <- Q + w + plot_layout(design = layout,guides = 'collect') + 
    plot_annotation(tag_levels = c('A', '1')) + # ncol = 2, byrow = TRUE
    gridExtra::tableGrob(t(as.data.frame(parm)))
  ggsave(file.path(out_parm,paste0(biomaker_num_fix_parm,".roc.pdf")),Q,dpi = 300,device = cairo_pdf,
         width = 18,
         height = 5*length(P))
}


# 函数: extract_roc_data_from_bmr 
function(bmr_object) {# bmr_object <- bmr
  # 获取所有resample结果
  resample_results <- bmr_object$resample_results
  
  # 创建一个空列表来存储每个模型的ROC数据
  roc_data_list <- list()
  
  # 遍历每个模型的结果
  for (i in 1:nrow(resample_results)) {# i <- 1
    # 获取当前模型的信息 - 正确访问resample_result列
    current_result <- resample_results$resample_result[[i]]
    
    # 获取模型名称
    model_name <- current_result$learner$id
    model_name <- str_replace(model_name, "classif\\.(.*?)\\.tuned", "\\1")
    
    # 获取预测结果
    predictions <- current_result$predictions()
    
    # 合并所有折叠的预测结果
    all_preds <- data.frame()
    
    for (fold in seq_along(predictions)) {# fold <- 2
      pred <- predictions[[fold]]      # 获取当前折叠的预测
      truth_values <- pred$truth      # 提取真实标签和预测概率
      pos_class <- current_result$task$positive      # 获取正类名称
      prob_colnames <- colnames(pred$prob)      # 获取概率列名
      prob_values <- pred$prob[, pos_class]      
      # 安全计算truth整数值
      truth_int <- as.integer(truth_values == pos_class)

      # 创建一致长度的向量
      n_rows <- length(truth_int)
      dataset_vec <- rep("CV", n_rows)
      model_vec <- rep(model_name, n_rows)
      
      # 安全创建数据框
      fold_data <- data.frame(
        truth = truth_int,
        prob = prob_values,
        dataset = dataset_vec,
        model = model_vec,
        stringsAsFactors = FALSE
      )
      
      all_preds <- rbind(all_preds, fold_data)
    }
    
    # 将该模型的数据添加到列表中
    roc_data_list[[model_name]] <- all_preds
  }
  
  return(roc_data_list)
}
<bytecode: 0x560208834ec0>


# 函数: get_model_info 
function(model_name, current_learner, task) {
  model_info <- list(
    model_name = model_name,
    features = task$feature_names, # 获取特征名称
    n_features = length(task$feature_names),
    hyperparameters = list(),
    performance = list(),
    feature_importance = NULL
  )

  # 获取超参数
  if (inherits(current_learner, "AutoTuner")) {
    model_info$hyperparameters <- current_learner$tuning_instance$result_learner_param_vals
    model_info$tuning_results <- current_learner$tuning_instance$archive$data
  }

  # 获取特征重要性
  if (model_name == "rf") {
    imp <- current_learner$model$learner$model$variable.importance
    if (!is.null(imp)) {
      model_info$feature_importance <- data.frame(
        Feature = names(imp),
        Importance = as.numeric(imp)
      )
    }
  } else if (model_name == "xgboost") {
    model <- current_learner$model$learner$model
    if (inherits(model, "xgb.Booster")) {
      imp <- xgb.importance(model = model)
      if (!is.null(imp)) {
        model_info$feature_importance <- data.frame(
          Feature = imp$Feature,
          Importance = imp$Gain
        )
      }
    }
  } else if (model_name == "glmnet") {
    coefs <- coef(current_learner$model$learner$model)
    if (!is.null(coefs)) {
      nonzero_idx <- which(coefs[-1] != 0)
      model_info$feature_importance <- data.frame(
        Feature = rownames(coefs)[-1][nonzero_idx],
        Importance = abs(coefs[-1][nonzero_idx])
      )
    }
  } else if (model_name == "lightgbm") {
    model <- current_learner$model$learner$model
    if (inherits(model, "lgb.Booster")) {
      imp <- lightgbm::lgb.importance(model)
      if (!is.null(imp)) {
        model_info$feature_importance <- data.frame(
          Feature = imp$Feature,
          Importance = imp$Gain
        )
      }
    }
  }

  return(model_info)
}
<bytecode: 0x56022fc39d58>


# 函数: is_binary 
function(x) {
    vals <- unique(x[!is.na(x)])
    length(vals) == 2 && all(vals %in% c(0, 1)) && all(c(0, 1) %in% vals)
}
<bytecode: 0x560231795bc0>


# 函数: knee_point 
function(y){
  kk <- boxplot.stats(y)$conf[[2]]
  return(kk)
}


# 函数: mytheme 
function(){
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
<bytecode: 0x56023049e6d0>


# 函数: nodeid.tbl_tree 
function (tree, label) 
{
    tree$node[match(label, tree$label)]
}
<bytecode: 0x5601e2ad62e8>
<environment: namespace:tidytree>


# 函数: offspring.tbl_tree 
function (.data, .node, tiponly = FALSE, self_include = FALSE, 
    ...) 
{
    if (missing(.node) || is.null(.node)) {
        stop(".node is required")
    }
    if (length(.node) == 1) {
        res <- .offspring.tbl_tree_item(.data = .data, .node = .node, 
            tiponly = tiponly, self_include = self_include, ...)
    }
    else {
        res <- lapply(.node, function(node) {
            .offspring.tbl_tree_item(.data = .data, .node = node, 
                tiponly = tiponly, self_include = self_include, 
                ...)
        })
        names(res) <- .node
    }
    return(res)
}
<bytecode: 0x5601e2abdcb8>
<environment: namespace:tidytree>


# 函数: offspring.tbl_tree_item 
function (.data, .node, tiponly = FALSE, self_include = FALSE, 
    ...) 
{
    x <- child.tbl_tree(.data, .node)
    rn <- rootnode.tbl_tree(.data)$node
    x <- x[x$node != rn, ]
    if (nrow(x) == 0) {
        if (self_include) {
            x <- .data[.data$node == .node, ]
        }
        return(x)
    }
    parent <- .data$parent
    children <- .data$node
    n <- max(parent)
    kids <- vector("list", n)
    for (i in seq_along(parent)) {
        kids[[parent[i]]] <- c(kids[[parent[i]]], children[i])
    }
    id <- x$node
    i <- 1
    while (i <= length(id)) {
        id <- c(id, kids[[id[i]]])
        i <- i + 1
    }
    if (self_include) {
        id <- c(.node, id)
    }
    sp <- .data[children %in% id, ]
    if (tiponly) {
        return(sp[sp$node < rn, ])
    }
    return(sp)
}
<bytecode: 0x5601e2ab0000>
<environment: namespace:tidytree>


# 函数: parent.tbl_tree 
function (.data, .node, ...) 
{
    valid.tbl_tree(.data)
    ndata <- itself(.data, .node)
    .node <- ndata$node
    pnode <- ndata$parent
    if (pnode == .node) 
        return(.data[0, ])
    .data[.data$node == pnode, ]
}
<bytecode: 0x5601e2a38c20>
<environment: namespace:tidytree>


# 函数: plot_p_feat 
function(data_var_imp,out=opts$outdir,plot=T){
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


# 函数: roc_pict 
function(pod_list, title = "", path = "./train_roc_plot",
                     colors = NULL, labels = NULL) {
  # pod_list: 一个列表，每个元素包含一个数据框，有d和m两列
  # colors: 自定义颜色向量
  # labels: 每条曲线的标签

  # 创建一个空的数据框来存储所有ROC数据
  all_roc_data <- c() # bind_rows(pod_list)
  auc_texts <- c()

  # 如果没有提供颜色，使用默认颜色方案
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(min(9, length(pod_list)), "Set1")
    if (length(pod_list) > 9) {
      colors <- colorRampPalette(colors)(length(pod_list))
    }
  }

  # 处理每个模型的数据
  for (i in seq_along(pod_list)) { # i <- 2
    pod <- pod_list[[i]]

    # 确保数据格式正确
    if (!all(c("d", "m") %in% names(pod))) {
      pod <- data.frame(
        d = pod[, 1], # 真实标签
        m = pod[, 2] # 预测概率
      )
    }

    # 计算ROC数据
    roc_obj <- roc(pod$d, pod$m, quiet = TRUE)
    auc_value <- round(auc(roc_obj), 3)
    ci_obj <- ci.auc(roc_obj)

    # 存储ROC数据
    roc_df <- data.frame(
      specificity = roc_obj$specificities,
      sensitivity = roc_obj$sensitivities,
      model = labels[i]
    )
    all_roc_data <- rbind(all_roc_data, roc_df)

    # 创建AUC文本
    auc_texts[i] <- sprintf(
      "%s: AUC = %.3f (%.3f-%.3f)",
      labels[i], auc_value,
      round(ci_obj[1], 3), round(ci_obj[3], 3)
    )
  }
  all_roc_data$model <- factor(all_roc_data$model, levels = names(pod_list))
  # 生成ROC曲线
  p <- ggplot(all_roc_data, aes(x = 1 - specificity, y = sensitivity,color = model)) +
    geom_path(size = 1) +
    geom_abline(slope = 1, intercept = 0,linetype = "dashed", color = "grey40") +
    scale_color_manual(values = colors) +
    ggtitle(title) +
    labs(
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Models"
    ) +
    annotate("text",
      x = 0.55, y = 0.25 - 0.05 * seq_along(auc_texts),
      label = auc_texts, size = 4, hjust = 0
    ) +
    coord_equal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
    )

  # 保存图形
  ggsave(paste0(path, ".pdf"), p,
    width = 10, height = 8, units = "in", dpi = 300
  )
  all_roc_data_split <- split(all_roc_data, all_roc_data$model)
  return(list(p,all_roc_data_split))
}
<bytecode: 0x5601fbf1ecc0>


# 函数: roc_pict_single 
function( pod, title = ''){ # pod = pod_test ; path = './训练集roc曲线'
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
  #ggsave(paste0(path,'.png'),p,width = 12,height = 12,units = 'in',dpi = 600 )
  return(list(p,q))
}


# 函数: rootnode.tbl_tree 
function (.data, ...) 
{
    valid.tbl_tree(.data)
    .data[.data$parent == .data$node, ]
}
<bytecode: 0x5601e2accfd0>
<environment: namespace:tidytree>
