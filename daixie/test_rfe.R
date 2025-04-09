getOption("repos")
options(repos = structure(c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = "https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
## install.packages("remotes")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager") # ;BiocManager::install(version = "3.18")
if (!("mlr3extralearners" %in% installed.packages())) {
    remotes::install_github("mlr-org/mlr3extralearners@*release")
}
if (!("aplot" %in% installed.packages())) {
    remotes::install_github("YuLab-SMU/aplot")
}
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(
    optparse, tidyverse, openxlsx, mlr3verse, data.table, mltools, mlr3tuningspaces, future, readxl,
    treeshap, kernelshap, shapviz, mlr3extralearners, ranger, randomForest, pROC, patchwork, Boruta
)

if (TRUE) {
    option_list <- list(
        # 输入
        make_option(c("-i", "--input"), type = "character", default = "rarefac.ASV_genus.xls", help = "输入的OTU表格"),
        make_option(c("-g", "--map"), type = "character", default = "map-group.txt", help = "分组文件"),
        make_option(c("-c", "--color"), type = "character", default = "none", help = "指定颜色：color.txt"),
        make_option(c("--part"), type = "character", default = "2/3", help = "咋分的样本，2/3还是3/4，或者0.8"),
        make_option(c("--split"), type = "character", default = "none", help = "客户指定train和testsplit.map-group.txt"),
        make_option(c("--min"), type = "numeric", default = 3, help = "最小特征数"),
        make_option(c("--max"), type = "numeric", default = 20, help = "最大特征数"),
        # 添加筛选条件
        make_option(c("-u", "--unif"), type = "logical", default = T, help = "要不要归一化"),
        make_option(c("--gp"), type = "character", default = "none", help = "control-test顺序指定"),
        # make_option(c("-t", "--test"),  type="character", default="rarefac.Wilcoxon_rank_sum_unpaired.ALL.xls", help="差异"),
        make_option(c("--pv"), type = "double", default = 0.05, help = "显著性筛选"),
        make_option(c("--pj"), type = "double", default = 1, help = "pvalue.adjust.fdr显著性筛选"),
        make_option(c("--select"), type = "character", default = "none", help = "select.list"),
        make_option(c("--delete"), type = "character", default = "none", help = "delete.list"),
        # 外部验证
        make_option(c("--trainOnly"), type = "logical", default = T, help = "TRUE表示只用train排序，FALSE表示训练集和内部验证集一起排序"),
        make_option(c("--valid"), type = "character", default = "valid.rarefac.otu_genus.xls", help = "排列组合"),
        make_option(c("--map2"), type = "character", default = "map2.txt", help = "显著性标记划分阈值？"),
        make_option(c("-s", "--seed"), type = "numeric", default = "123", help = "设置种子,默认1234"),
        # 路径设置
        make_option(c("--cores"), type = "double", default = 8, help = "准备给多少线程分析啊兄弟"),
        make_option(c("-o", "--outdir"), type = "character", default = "", help = "输出文件夹默认为当前目录")
    )
    opts <- parse_args(OptionParser(option_list = option_list))
}
if (opts$outdir == "") {
    opts$outdir <- getwd()
}
future::plan(multisession, workers = ifelse(opts$cores != 0, floor(opts$cores), ceiling(availableCores() / 6)))

# 设置任务随机执行顺序，有助于处理不均衡的运行时间
options(mlr3.exec_random = TRUE)

# 设置任务块大小为1，提高任务分配粒度
options(mlr3.exec_chunk_size = 1)

set.seed(opts$seed, kind = "Mersenne-Twister")
options("endocing" = "UTF-8")
options(scipen = 5000)
options(dplyr.summarise.inform = FALSE)
nodeid.tbl_tree <- utils::getFromNamespace("nodeid.tbl_tree", "tidytree")
rootnode.tbl_tree <- utils::getFromNamespace("rootnode.tbl_tree", "tidytree")
offspring.tbl_tree <- utils::getFromNamespace("offspring.tbl_tree", "tidytree")
offspring.tbl_tree_item <- utils::getFromNamespace(".offspring.tbl_tree_item", "tidytree")
child.tbl_tree <- utils::getFromNamespace("child.tbl_tree", "tidytree")
parent.tbl_tree <- utils::getFromNamespace("parent.tbl_tree", "tidytree")

# 01.Function -------------------------------------------------------------
mytheme <- function() {
    theme_bw() +
        theme(
            plot.title = element_text(size = rel(1), hjust = 0.5),
            axis.title = element_text(size = rel(1)),
            axis.text.x = element_text(
                size = rel(1), angle = 90,
                vjust = 0.5, hjust = 0.5, color = "black"
            ),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "white"),
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.size = unit(.4, "cm")
        )
}

roc_pict <- function(pod, title = "") {
    # pod = pod_test ; path = './训练集roc曲线'
    rocobj <- roc(pod[, 2], pod[, 4], # cases=pod[,4][pod[,2]=='OR'],
        # levels=c('OR','HC'),controls=pod[,4][pod[,2]=='HC'],  # 可以设置实验组或对照组
        smooth = F
    ) # 曲线是否光滑，当光滑时，无法计算置信区间

    # 计算临界点/阈值
    cutOffPoint <- coords(rocobj, "best")
    cutOffPointText <- paste0(round(cutOffPoint[1, 1], 3), "\n(", round(cutOffPoint[1, 2], 3), ",", round(cutOffPoint[1, 3], 3), ")")

    # 计算AUC值
    auc <- auc(rocobj)[1]
    # AUC的置信区间
    auc_low <- ci(rocobj, of = "auc")[1]
    auc_high <- ci(rocobj, of = "auc")[3]

    # 计算置信区间
    ciobj <- ci.se(rocobj, specificities = seq(0, 1, 0.01))
    data_ci <- ciobj[1:101, 1:3]
    data_ci <- as.data.frame(data_ci)
    x <- as.numeric(rownames(data_ci))
    data_ci <- data.frame(x, data_ci)

    # 绘图
    p <- ggroc(rocobj, color = "black", size = 0.5, legacy.axes = F) + # FALSE时 横坐标为1-0 specificity；TRUE时 横坐标为0-1 1-specificity
        geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), # 绘制对角线
            colour = "grey", linetype = "longdash"
        ) +
        geom_ribbon(data = data_ci, aes(x = x, ymin = X2.5., ymax = X97.5.), fill = "lightblue", alpha = 0.5) + # 绘制置信区间,当legacy.axes=TRUE时， 把x=x改为x=1-x
        geom_point(aes(x = cutOffPoint[1, 2], y = cutOffPoint[1, 3]), color = "red") + # 绘制临界点/阈值
        # geom_text(aes(x = cutOffPoint[[2]],y = cutOffPoint[[3]],label=cutOffPointText,fontface = "plain"),size=4,vjust=-1)+# 添加临界点/阈值文字标
        ggtitle(title) + mytheme() + # theme_bw()+
        annotate("text",
            x = cutOffPoint[1, 2] * .9, y = cutOffPoint[1, 3] * .9,
            label = cutOffPointText, hjust = 0.5, vjust = 0, size = 4
        ) +
        annotate("text",
            x = 0.2, y = 0.1, hjust = 0.6, vjust = 0.2, size = 4,
            label = paste0("AUC: ", round(auc, 4), "\n", "95% CI: ", round(auc_low, 4), "-", round(auc_high, 4))
        )

    plot_bx <- pod[, c(2, 5)] %>% rename_all(~ c("group", "prob"))
    q <- ggplot(plot_bx) +
        stat_boxplot(aes(x = group, y = prob), geom = "errorbar", linetype = 1, width = 0.5) + # whiskers
        geom_boxplot(aes(x = group, y = prob, fill = group), show.legend = FALSE) +
        scale_fill_manual(values = sc$V2) +
        ggtitle(title) +
        mytheme()

    # ggsave(paste0(path,'.pdf'),p,width = 12.5,height = 12,units = 'in',dpi = 300)
    # ggsave(paste0(path,'.png'),p,width = 12,height = 12,units = 'in',dpi = 600 )
    return(list(p, q))
}

# importance plot
plot_p_feat <- function(data_var_imp, out = opts$outdir, plot = T) {
    p_feat <- ggplot(data_var_imp, aes(x = old_colnames_n, y = importance, fill = importance)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        ylab("Variable Importance") +
        xlab("") + # ggtitle("Information Value Summary") +
        guides(fill = "none") +
        scale_fill_gradient(low = "#327eba", high = "#e06663") +
        theme_bw() +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
    if (isTRUE(plot)) {
        ggsave(file.path(out, "00.MeanDecreaseAccuracy.pdf"), p_feat, width = 10, height = 6)
    }
    return(p_feat)
}

# 02.Read_in --------------------------------------------------------------
if (opts$outdir == "") {
    opts$outdir <- getwd()
}
if (!isTRUE(dir.exists(opts$outdir))) {
    dir.create(opts$outdir)
}
if (!isTRUE(dir.exists(file.path(opts$outdir, paste0("ROC.", opts$seed))))) {
    dir.create(file.path(opts$outdir, paste0("ROC.seed", opts$seed)))
}

mapcol <- c("#61d04f", "#df536b", "#377EB8", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF") # ,"#4DAF4A","#E41A1C" RColorBrewer::brewer.pal(n = 8,name = "Set1")
Gp <- read_tsv(opts$map) %>%
    rename_all(~ c("SampleID", "group")) %>%
    mutate(group = fct_inorder(group))

# 指定分组顺序，要求对照组在前，实验组在后，重要
if (opts$gp != "none") {
    gp <- strsplit(opts$gp, split = "-")[[1]]
    if (length(gp) == 1) stop("Length of variable gp is equal to 1. Program terminated.")
    Gp <- Gp %>%
        filter(group %in% gp) %>%
        mutate(group = factor(group, levels = gp))
    if (length(unique(Gp$group)) == 1) stop("Length of variable Gp  is equal to 1. Program terminated.")
} else {
    gp <- levels(Gp$group)
}

# 指定分组颜色，对照组为绿色，实验组为红色
if (opts$color != "none") { # opts$color = c("color.txt")
    sc <- read.table(opts$color, sep = "\t", comment.char = "", check.names = FALSE)
    sc <- sc[which(as.vector(sc[, 1]) %in% unique(unlist(Gp$group))), ]
    Gp$group <- factor(Gp$group, levels = as.vector(sc[, 1]))
    Gp <- Gp %>% arrange(group)
} else {
    sc <- cbind(levels(Gp$group), mapcol[1:nlevels(Gp$group)]) %>%
        as.data.frame(stringsAsFactors = FALSE)
}

# 列是物质名 行是样本名，最后一列是group分组，其他都可以自行选择喜欢的
Data <- read_tsv(opts$input) %>% rename_at(1, ~"Features")
if (isTRUE(opts$unif)) {
    Data <- Data %>% mutate(across(where(is.numeric), ~ . / sum(.)))
}
if (opts$select != "none") {
    ss <- read_tsv(opts$select, col_names = F) %>% rename_at(1, ~"Features")
    Data <- inner_join(Data, ss)
}
if (opts$delete != "none") {
    dd <- read_tsv(opts$delete, col_names = F) %>% rename_at(1, ~"Features")
    Data <- Data %>% filter(!Features %in% (dd %>% t() %>% as.character()))
}
Data <- Data %>%
    pivot_longer(!Features) %>%
    pivot_wider(names_from = "Features", values_from = "value") %>%
    rename_at(1, ~"SampleID") %>%
    inner_join(Gp)
Data <- as.data.frame(Data)
rownames(Data) <- Data$SampleID
Data <- Data[, -1]

## 注意！！！ 因为列名即物质名 大多都比较奇怪，现在的算法不支持这种奇怪的名字
## data external_data 都需要处理，此外data external_data的物质名顺序需一致
## 此外在特征重要性中需要物质名，所以下面是处理方式
names_values <- tibble(
    old_colnames = colnames(Data),
    new_colnames = c(paste0("feature_", seq(1, length(old_colnames) - 1)), "group")
)
colnames(Data) <- names_values$new_colnames

# 03.建模准备 ------------------------------------------------------------------
# 创建分类任务
pbp_task <- TaskClassif$new(
    id = "classify_model",
    backend = Data,
    target = "group",
    positive = gp[1]
)

# 识别分类变量和连续变量
is_binary <- function(x) {
    vals <- unique(x[!is.na(x)])
    length(vals) == 2 && all(vals %in% c(0, 1)) && all(c(0, 1) %in% vals)
}

binary_check <- sapply(Data, is_binary)
categorical_cols <- names(Data)[binary_check]
continuous_cols <- names(Data)[!binary_check]

# 定义预处理管道
pbp_prep <- po("removeconstants",ratio = 0.05) %>>% # 移除常亮特征
    po("filter",filter = flt("variance"), filter.cutoff = 1e-9) %>>% # 设置一个非常小的阈值
    po("imputehist", affect_columns = selector_name(continuous_cols)) %>>%
    po("imputemode", affect_columns = selector_name(categorical_cols)) 

# po("scale",affect_columns = selector_name(continuous_cols))
task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

if (!isTRUE(opts$trainOnly)) {
    task_train <- task_prep$clone()
} else {
    # 数据划分 - 使用预处理后的任务
    if (grepl("/", opts$part)) {
        aa <- strsplit(opts$part, "/")[[1]][1] %>% as.numeric()
        bb <- strsplit(opts$part, "/")[[1]][2] %>% as.numeric()
    } else {
        aa <- strsplit(MASS::fractions(opts$part %>% as.numeric()) %>% as.character(), "/")[[1]][1] %>% as.numeric()
        bb <- strsplit(MASS::fractions(opts$part %>% as.numeric()) %>% as.character(), "/")[[1]][2] %>% as.numeric()
    }
    zz <- aa / bb

    if (opts$split == "none") {
        split_gp <- partition(task = pbp_task, ratio = zz)

        split_gp_pre <- bind_rows(map(split_gp, as_tibble), .id = "Split") %>%
            mutate(value = rownames(Data)[value]) %>%
            rename_at(2, ~"SampleID") %>%
            left_join(Data %>% select(group) %>% rownames_to_column(var = "SampleID") %>%
                mutate(group = as.character(group)), .) %>%
            write_tsv(file.path(opts$outdir, "split.map-group.txt"))
    } else {
        split_gp_pre <- Data %>%
            select(group) %>%
            rownames_to_column(var = "SampleID") %>%
            left_join(read_tsv(opts$split) %>% rename_all(~ c("SampleID", "group", "Split")))
        split_gp_pre0 <- split_gp_pre %>%
            mutate(Split = factor(Split, levels = c("train", "test"))) %>%
            group_by(group) %>%
            nest() %>%
            mutate(
                data2 = map(data, ~ dplyr::filter(.x, Split == "train") %>% dplyr::select(SampleID)),
                data3 = map(data, ~ dplyr::filter(.x, Split == "test") %>% dplyr::select(SampleID))
            ) %>%
            mutate(
                train = map(data2, ~ factor(pull(.x, SampleID), levels = Data$SampleID) %>% as.numeric()),
                test = map(data3, ~ factor(pull(.x, SampleID), levels = Data$SampleID) %>% as.numeric())
            )
        split_gp <- list(train = unlist(split_gp_pre0$train), test = unlist(split_gp_pre0$test))
    }
    task_train <- task_prep$clone()$filter(split_gp$train)
    task_test <- task_prep$clone()$filter(split_gp$test)
}

# 定义重抽样方案
inner_resampling <- rsmp("cv", folds = 5)
outer_resampling <- rsmp("cv", folds = 5)

boruta_filter <- flt("boruta", maxRuns = 11) # 100
rfe_filter <- fs("rfe",
    n_features = to_tune(5, 20),
    feature_fraction = to_tune(0.5, 0.8)
)

model_params <- ps(
    boruta.filter.frac = p_dbl(0.5, 0.8),
    # RFE参数 - 注意前缀"rfe."
    rfe.n_features = p_int(5, 20),
    rfe.feature_fraction = p_dbl(0.5, 0.8),
    # Elastic Net参数

    # 核心正则化参数
    classif.glmnet.lambda = p_dbl(lower = 1e-4, upper = 1, logscale = TRUE), # 正则化强度
    classif.glmnet.alpha = p_dbl(lower = 0, upper = 1), # L1/L2混合比例
    # 预处理相关参数
    classif.glmnet.standardize = p_lgl(), # 是否标准化特征
    classif.glmnet.thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE), # 收敛阈值
    classif.glmnet.maxit = p_int(lower = 1e3, upper = 1e4), # 最大迭代次数

    # XGBoost参数
    classif.xgboost.eta = p_dbl(0.01, 0.3), # 学习率
    classif.xgboost.max_depth = p_int(1, 10), # 树的最大深度
    classif.xgboost.nrounds = p_int(50, 500), # 迭代次数
    classif.xgboost.min_child_weight = p_dbl(1, 10), # 最小子节点权重
    classif.xgboost.subsample = p_dbl(0.5, 1), # 样本采样比例
    classif.xgboost.colsample_bytree = p_dbl(0.5, 1), # 特征采样比例

    # 随机森林参数
    classif.ranger.num.trees = p_int(lower = 500, upper = 1000),
    classif.ranger.max.depth = p_int(lower = 3, upper = 15),
    classif.ranger.min.node.size = p_int(lower = 1, upper = 10),
    classif.ranger.mtry = p_int(1, 20), # 特征采样数量

    # SVM参数
    classif.svm.cost = p_dbl(lower = -2, upper = 2, trafo = function(x) 10^x),
    classif.svm.kernel = p_fct(c("polynomial", "radial")),
    classif.svm.degree = p_int(1, 3, depends = classif.svm.kernel == "polynomial"),
    
    # lightGBM
    classif.lightgbm.learning_rate = p_dbl(0.01, 0.3),
    classif.lightgbm.num_iterations = p_int(50, 500),
    classif.lightgbm.max_depth = p_int(3, 12),
    classif.lightgbm.min_data_in_leaf = p_int(10, 100),
    classif.lightgbm.feature_fraction = p_dbl(0.5, 1.0),
    classif.lightgbm.bagging_fraction = p_dbl(0.5, 1.0),
    classif.lightgbm.bagging_freq = p_int(0, 10),
    classif.lightgbm.min_gain_to_split = p_dbl(0.0, 0.5),
    classif.lightgbm.num_leaves = p_int(10, 255)
)

# 创建一个基础学习器用于特征重要性评估
base_learner <- lrn("classif.ranger", importance = "impurity")

learner_base_glmnet <- lrn("classif.glmnet",
    lambda = 0.1, alpha = 0.5,
    predict_type = "prob"
)
learner_base_xgb <- lrn("classif.xgboost",
    predict_type = "prob",
    nrounds = 100, eta = 0.1, max_depth = 6
)
learner_base_rf <- lrn("classif.ranger",
    predict_type = "prob",
    num.trees = 500, importance = "impurity"
)
learner_base_svm <- lrn("classif.svm",
    predict_type = "prob",
    type = "C-classification", # 必须指定分类类型
    kernel = "radial", cost = 1
)
learner_base_lgbm <- lrn("classif.lightgbm")

# 创建终止条件 ----------------------------------------------------------
terminator <- trm("combo",
    terminators = list( # 性能停滞终止
        trm("stagnation", iters = 5, threshold = 0.005), # 观察最近15次
        trm("evals", n_evals = 10), # 100评估次数终止
        trm("run_time", secs = 3600), # 1小时时间限制
        trm("perf_reached", level = 0.85) # 目标性能终止
    ) # 达到85%准确率就停止
)
# 定义调优器
tuner <- tnr("random_search", batch_size = 5) # 100

auto_tuners <- list(
    glmnet_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter, frac = 0.
            ) %>>%
                po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$glmnet,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_boruta"
    ),
    xgboost_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter,
               filter.frac = to_tune(0.5, 0.8) # 这里添加filter.frac调优
            ) %>>%
                po("select") %>>% learner_base_xgb
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$xgboost,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "xgboost_boruta"
    ),
    rf_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter,
               filter.frac = to_tune(0.5, 0.8) # 这里添加filter.frac调优
            ) %>>%
                po("select") %>>% learner_base_rf
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$rf,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "rf_boruta"
    ),
    svm_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter,
               filter.frac = to_tune(0.5, 0.8) # 这里添加filter.frac调优
            ) %>>%
                po("select") %>>% learner_base_svm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$svm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "svm_boruta"
    ),
    lgbm_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter,
               filter.frac = to_tune(0.5, 0.8) # 这里添加filter.frac调优
            ) %>>%
                po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$lgbm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "lgbm_boruta"
    ))
auto_tuners <- list(
    glmnet_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("learner_cv", base_learner) %>>%
                po("fselect", fselector = rfe_filter) %>>%
                po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$glmnet,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_rfe"
    ),
    xgboost_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("learner_cv", base_learner) %>>%
                po("fselect", fselector = rfe_filter) %>>%
                po("select") %>>% learner_base_xgb
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$xgboost,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "xgboost_rfe"
    ),
    rf_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("learner_cv", base_learner) %>>%
                po("fselect", fselector = rfe_filter) %>>%
                po("select") %>>% learner_base_rf
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$rf,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "rf_rfe"
    ),
    svm_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("learner_cv", base_learner) %>>%
                po("fselect", fselector = rfe_filter) %>>%
                po("select") %>>% learner_base_svm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$svm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "svm_rfe"
    ),
    lgbm_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("learner_cv", base_learner) %>>%
                po("fselect", fselector = rfe_filter) %>>%
                po("select") %>>% learner_base_lgbm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$lgbm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "lgbm_rfe"
    )
)

# 整合所有学习器用于基准测试
learners_for_benchmark <- list(
    auto_tuners$glmnet_boruta,  # Boruta+glmnet
    auto_tuners$xgboost_boruta, # Boruta+xgboost 
    auto_tuners$rf_boruta,      # Boruta+rf
    auto_tuners$svm_boruta,     # Boruta+svm
    auto_tuners$lgbm_boruta     # Boruta+lgbm
)
learners_for_benchmark <- list(
    auto_tuners$glmnet_rfe,     # RFE+glmnet
    auto_tuners$xgboost_rfe,    # RFE+xgboost
    auto_tuners$rf_rfe,         # RFE+rf
    auto_tuners$svm_rfe,        # RFE+svm
    auto_tuners$lgbm_rfe        # RFE+lgbm
)

auto_tuners <- list(
    glmnet_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter) %>>%
                po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_boruta"
    )
)
auto_tuners <- list(
    glmnet_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("learner_cv", base_learner) %>>%
                po("filter", filter = rfe_filter) %>>%
                po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_rfe"
    ),
)

learners_for_benchmark <- list(
    auto_tuners$glmnet_boruta#,
    #auto_tuners$glmnet_rfe   
)

# 创建benchmark网格
benchmark_grid <- benchmark_grid(
    tasks = task_train, # 使用预处理后的任务train
    learners = learners_for_benchmark,
    resamplings = outer_resampling # 外层交叉验证
)

# 执行基准测试
bmr <- benchmark(benchmark_grid, store_models = TRUE)
# saveRDS(bmr, "benchmark_results.rds")
save.image("test2.1.RData")