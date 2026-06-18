############################################### 00.环境 ##############################################################
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
.libPaths(c("/work/users/yuren/Moonlight-box/R-4.5.1/library"))


############################################### 01.安装包 ##############################################################
#R版本4.5.1
#因为本地安装过
#/work/users/yuren/Moonlight-box/R-4.5.1/bin/Rscript /work/users/yuren/Scripts/maaslin3.R
#一直运行这个命令 缺哪个包 就把哪个包拖过去 来回下载太累了


## 在 R 中执行以下命令创建 Makevars 文件
#dir.create("~/.R", showWarnings = FALSE)
#cat(paste0("CFLAGS += -std=c99\n", 
#           "CXXFLAGS += -std=c99\n",
#           "FCFLAGS += -std=c99\n"),
#    file = "~/.R/Makevars")
#
## 然后安装 rlang
#install.packages("rlang", 
#                 lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library",
#                 repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
#install.packages("glue", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
## 重新创建 Makevars 文件，添加完整的编译标志
#dir.create("~/.R", showWarnings = FALSE, recursive = TRUE)
#
#cat(paste0("CFLAGS += -std=c99 -D_POSIX_C_SOURCE=200809L\n",
#           "CXXFLAGS += -std=c++11 -D_POSIX_C_SOURCE=200809L\n",
#           "FCFLAGS += -std=c99\n"),
#    file = "~/.R/Makevars")
#
## 验证文件内容
#cat(readLines("~/.R/Makevars"), sep = "\n")
#
## 然后安装 cli
#install.packages("cli", 
#                 lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library",
#                 repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
#install.packages("lifecycle", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
## 使用清华镜像安装 farver 2.0.3
#install.packages("https://mirrors.tuna.tsinghua.edu.cn/CRAN/src/contrib/Archive/farver/farver_2.0.3.tar.gz",
#                 lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library",
#                 repos = NULL)
#install.packages("vctrs", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
#install.packages("S7", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
#install.packages("generics", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
#install.packages("magrittr", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
#
#install.packages("tibble", lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
#install.packages("dplyr", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/",lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")
i#nstall.packages("stringi", version = "1.7.6", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/",lib = "/work/users/yuren/Moonlight-box/R-4.5.1/library")

library(optparse)
library(dplyr)
library(forcats)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(reshape2)
library(pheatmap)
library(cowplot)

# 从Bioconductor安装
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("maaslin3")
library(maaslin3)


############################################### 02.设置参数 包括对照组什么的 ##############################################################
map_data <- read_tsv("map-group.txt")
# 获取 group 列的所有唯一值
all_groups <- unique(map_data$group)

# 检查是否存在 "Con", "Contral", "HC" 这三个字符串中的任意一个
target_strings <- c("Con", "Contral", "HC")

# 判断是否有任意一个目标字符串存在于分组中
if (any(target_strings %in% all_groups)) {
  # 找出实际存在于数据中的那个字符串
  control_group <- intersect(all_groups, target_strings)[1]
} else {
  # 如果都没有，取第一个
  control_group <- all_groups[1]
}

# 打印结果
print(paste("确定的对照组是:", control_group))

# 从分组中去掉对照组
experimental_groups <- setdiff(all_groups, control_group)

# 生成所需的字符串向量
group_strings <- paste0("group ", experimental_groups)


if (TRUE) {
  option_list <- list(
    make_option(c("-i", "--input"),            type = "character", default = "Community_raw/genus.xls", help = "丰度表格;rarefac.otu_genus.xls"),
    make_option(c("-s", "--select"),           type = "character", default = "none",                help = "筛选表格:select.txt挑选物种"),
    make_option(c("-m", "--map"),              type = "character", default = "map-group.txt",       help = "分组文件:map-group.txt"),
    make_option(c("-e", "--env"),              type = "character", default = "map-group.txt",      help = "生理数据:env.txt"),
    make_option(c("-f", "--unif"),             type = "logical",   default = F,                     help="要不要归一化"),
    make_option(c("-p", "--per"),              type = "double",    default = 0,                     help="丰度筛选"),
    make_option(c("-v", "--value"),            type = "double",    default = 0,                   help="核心微生物筛选"),
    make_option(c("-j", "--pvalue"),           type = "double",    default = 0.05,                  help = "pvalue"),
    
    #maaslin3内部参数
    make_option(c("-o", "--output"),           type = "character", default = "./",                  help = "输出文件夹名"),
    #make_option(c("--formula"),                type = "character", default = "~ age + gender + region + age:gender + age:region",                  help = "输出文件夹名"),
    make_option(c("--fixed_effects"),          type = "character", default = c("group"),   help = "固定效应"),
    make_option(c("--random_effects "),        type = "character", default = NULL,                  help = "随机效应"),
    #make_option(c("--reference"),              type = "character", default = c("group,V0"),            help = "对照组 组名+对照组"),
    make_option(c("--reference"),              type = "character", default = c(str_c("group,",control_group)),         help = "对照组 组名+对照组 c(group,HC)"),
    make_option(c("--min_abundance "),         type = "double",    default = 0,                     help = "最小丰度"),
    make_option(c("--min_prevalence "),        type = "double",    default = 0,                     help = "最小流行率"),
    make_option(c("--zero_threshold "),        type = "double",    default = 0,                     help = "零值阈值，小于这个就都是0"),
    make_option(c("--min_variance "),          type = "double",    default = 0,                     help = "最小方差"),
    make_option(c("--max_significance"),       type = "double",    default = 0.1,                   help = "校正后的q值小于"),
    make_option(c("--normalization"),          type = "character", default = "CLR",                 help = "标准化方法TSS（总求和标准化）, CLR（中心对数比）,NONE（不进行标准化）"),
    make_option(c("--transform"),              type = "character", default = "NONE",                help = "LOG（base 2）,PLOG,NONE"),
    make_option(c("--standardize"),            type = "logical",   default = F,                     help = "要不要标准化"),
    make_option(c("--warn_prevalence"),        type = "logical",   default = F,                     help = "仅丰度分析就是FALSE；真实流行率效应和丰度效应就是TRUE"),
    make_option(c("--evaluate_only"),          type = "character", default = NULL,                  help = "仅评估丰度（abundance）模型还是流行率（prevalence）模型 默认是NULL"),
    make_option(c("--plot_summary_plot"),      type = "logical",   default = T,                     help = "显著关联汇总图summary_plot.pdf"),
    make_option(c("--summary_plot_first_n"),   type = "double",    default = 10,                    help = "展示前几个特征"),
    #make_option(c("--coef_plot_vars"),         type = "character", default = c("group V1","group V2"),                  help = "NULL或者c('gender M','gender F')"),
    #make_option(c("--heatmap_vars"),           type = "character", default = c("group V1","group V2"),                  help = "展不展示热图NULL或者c('gender M','gender F')"),
    make_option(c("--coef_plot_vars"),         type = "character", default = group_strings,                  help = "NULL或者c('gender M','gender F')"),
    make_option(c("--heatmap_vars"),           type = "character", default = group_strings,                  help = "展不展示热图NULL或者c('gender M','gender F')"),
    make_option(c("--plot_associations"),      type = "logical",   default = T,                     help = "散点图"),
    make_option(c("--max_pngs"),               type = "double",    default = 10,                    help = "展示前几个特征的png"),
    make_option(c("--cores"),                  type = "double",    default = 2,                     help = "使用 4 个核心并行计算"),
    make_option(c("--summary_plot_balanced"),  type = "logical",   default = F,                     help = "平均分配coef_plot_vars")
    
  )
  opts <- parse_args(OptionParser(option_list = option_list))
}

############################ Read in #################
Map <- read_tsv(opts$map) %>%
  rename_at(c(1), ~"SampleID") %>%
  mutate(group = fct_inorder(group))

# 行为样本，列为菌群
#SampleID,Bacteroides,Prevotella,Lactobacillus,Roseburia,Clostridium,Streptococcus
#S1,1500,200,50,300,0,100
#S2,3000,50,150,0,400,80
#S3,1200,300,30,250,100,200
#S4,800,150,200,100,50,0

genus <- read_tsv(opts$input) %>% 
  rename_at(1,~"SampleID") %>% 
  .[,c("SampleID",Map$SampleID)]


genus_longer <- pivot_longer(genus, 
             cols = colnames(genus)[-1],  # 明确指定需要转换的列
             names_to = "Sample",       # 新列名，用于存储原始列名
             values_to = "Value") 

genus_wider <- pivot_wider(genus_longer, 
            id_cols = Sample,       # 保留的列
            names_from = SampleID,       # 从"Sample"列获取新列名
            values_from = Value)    

genus <- genus_wider %>% rename_at(1,~"SampleID")

# Uniform
if (opts$unif){
  genus[,2:ncol(genus)] <- sapply(2:ncol(genus),function(x) genus[,x]/sum(genus[,x]))
}

# 丰度筛选
if(opts$per != 0){
  genus <- genus %>% 
    mutate(SELECT = sapply(1:nrow(.),function(x){
      any(.[x,2:ncol(.)]>=opts$per)})) %>% 
    .[.$SELECT,] %>% as_tibble %>% dplyr::select(-SELECT)
}


# 核心微生物筛选
if(opts$value != 0){
  otu_coverage <- apply(genus[,2:ncol(genus)],1,function(x) 
    length(x[x>0])/(ncol(genus)-1))
  genus <- genus[otu_coverage >= opts$value,]
}

# 物中挑选
if(opts$select != "none"){
  ss <- read_tsv(opts$select,col_names = F) %>% rename_at(1,~"SampleID")
  genus <- inner_join(genus,ss)
}

if(opts$env == "map-group.txt"){
  Env <- Map
}else{
  Env <- read_tsv(opts$env) %>%
    rename_at(1, ~"SampleID") %>%
    inner_join(., Map) %>% select(-group)
}


################################################################################
# 在Maaslin3中，输入数据的格式要求是：
# 行 = 样本
# 列 = 特征（物种）

data1 <- as.matrix(genus[,-1] %>% t)
colnames(data1) <- genus$SampleID

data2 <- as.data.frame(Env[, -1])
rownames(data2) <- Env$SampleID

#data2 <- Map
#rownames(data2) <- Map$SampleID
#data1 <- data1[,colSums(data1) != 0]

####################### maaslin3 #####################
file_path <- opts$input
# 取最后一个斜杠后的部分
last_part <- basename(file_path)
last_part <- sub("\\..*$", "", last_part)

dir.create(path = str_c("Maaslin3/",last_part))

# 第一步：清洗data2hhh的列名（核心！移除[]，替换为下划线，避免特殊符号）
# 先保留原始列名（可选，用于后续溯源），再创建清洗后的列名
colnames_original <- colnames(data2)
# 清洗规则：把[和]全部替换为_，连续下划线会自动保留（不影响使用）
colnames_clean <- gsub(pattern = "\\[|\\]", replacement = "_", x = colnames_original)
# 赋值清洗后的列名到metadata
colnames(data2) <- colnames_clean

colnames(data2) <- colnames(data2) %>% str_remove_all("[()]")
colnames(data2) <- colnames(data2) %>% str_replace_all("/", "_")




set.seed(20190731)

fit_data3 <- maaslin3::maaslin3(
  input_data = data1,                   # 微生物丰度数据
  input_metadata = data2,               # 环境因子数据
  output = str_c(opts$output,"Maaslin3/",last_part),      # 输出路径
  fixed_effects = opts$fixed_effects,              # 固定效应 自变量：年龄分组（分类变量）
  random_effects = opts$random_effects,              # 随机效应 自变量：年龄分组（分类变量）
  #formula =  opts$formula ,
  #formula =  ~ age + gender + region + age:gender + age:region ,
  reference = opts$reference,                     #对照组 不设置就默认第一个
  min_abundance = opts$min_abundance   ,                 # 经过normalization参数指定的方法处理后的值
  min_prevalence = opts$min_prevalence   ,                 # 经过normalization参数指定的方法处理后的值
  zero_threshold = opts$zero_threshold   ,                 # 经过normalization参数指定的方法处理后的值
  min_variance = opts$min_variance   ,                 # 经过normalization参数指定的方法处理后的值
  #max_significance = opts$max_significance  ,               # 校正后的q值小于等于opts$pvalue 默认0.1
  max_significance = 1  ,               # 校正后的q值小于等于opts$pvalue 默认0.1
  normalization = opts$normalization,                # 标准化方法（推荐 CLR 处理组成性数据）
  transform = opts$transform,                   # 数据已转换则无需再变换
  standardize = opts$standardize,                 # 不进行标准化
  warn_prevalence = opts$warn_prevalence,              # 仅丰度分析就是FALSE；真实流行率效应和丰度效应就是TRUE
  evaluate_only = opts$evaluate_only,          # 仅评估丰度（“abundance”）模型还是流行率（“prevalence”）模型
  plot_summary_plot = opts$plot_summary_plot ,               # 显著关联汇总图summary_plot.pdf
  summary_plot_first_n = opts$summary_plot_first_n,            # 展示前几个特征
  coef_plot_vars = opts$coef_plot_vars,       # 每次手动调一下 记得中间有空格
  heatmap_vars = opts$heatmap_vars,          # 热图
  plot_associations = opts$plot_associations           ,     # 画散点图
  max_pngs = opts$max_pngs                   ,    #排名前max_pngs的会用png展示出来
  cores = opts$cores,                           # 使用 24 个核心并行计算
  summary_plot_balanced = opts$summary_plot_balanced         
  )


#原始 p 值（pval_individual）未经过多重检验校正，可能高估显著性，因此优先以校正后的qval_individual作为判断标准。
#常用0.25
#严格0.05
#宽松0.5
#然后再结合coef

#summary_plot_balanced
#展示coef_plot_vars中每个变量的前 N 个特征，其中 N 等于：ceiling (summary_plot_first_n /length (coef_plot_vars))
#当分析涉及多个变量（如同时纳入age、genderF、genderM等）时，默认情况下summary_plot_first_n（如 25）会展示所有变量中排名前 25 的特征，可能导致某一变量占据多数展示位置，其他变量被忽略。
#而summary_plot_balanced = TRUE会让每个变量 “公平分配” 展示名额
#例如，若summary_plot_first_n = 25且coef_plot_vars包含 3 个变量，则每个变量展示ceiling(25/3) = 9个特征（总展示 27 个，略多于 25），确保每个变量都有足够的特征被呈现。
#使用前提：
#必须同时设置coef_plot_vars（指定要展示的变量），否则会报错。例如：

#coef_plot_vars = c("age", "genderF", "genderM"),  # 3个变量
#summary_plot_first_n = 25,
#summary_plot_balanced = TRUE  # 每个变量展示9个特征



#适用场景：
#适合多变量分析（如同时研究年龄、性别等），希望在汇总图中均衡展示每个变量的重要关联特征，避免某一变量的特征 “垄断” 图表，更全面地呈现不同变量的关联模式。




#fit_data3$fit_data_abundance$results %>%
#  as_tibble() %>% # group_by(feature) %>% nest %>%
#  #filter(pval_individual < opts$pvalue) %>%
#  filter(qval_individual < opts$max_significance) %>%
#  pull(feature) %>%
#  unique() %>%
#  as_tibble() %>%
#  write_tsv(str_c(opts$output, "maaslin3.select.list"), col_names = FALSE)

##########################################################################################
# 找到最长参数的长度
max_len <- max(sapply(opts, length))

# 每个参数填充至相同长度
opts_padded <- lapply(opts, function(x) {
  max_len <- max(sapply(opts, length))  # 按最长参数长度对齐
  if (length(x) < max_len) c(x, rep(NA, max_len - length(x))) else x
})

write_tsv(opts_padded %>% as.data.frame() %>% t() %>% as.data.frame() %>% rownames_to_column() %>% as_tibble() %>% mutate(across(everything(), ~replace_na(., ""))),
          str_c(str_c(opts$output,"Maaslin3/",last_part,"/"),
            "Parameter",
            str_replace_all(as.character(date()), " ", "_") %>% str_replace_all(":", "_"),
            ".xls"
          ),
          col_names = FALSE
)
