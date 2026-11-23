library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(flextable)
library(cluster)
library(tibble)











set.seed(1234)
options(scipen = 50)
# part1 数据整理及基线分析===========================================================
# 1、导入文件 统一变量名----------------------------------------------------
raw_data <- read_excel("data1119.xlsx")

# 查看原始数据
dim(raw_data)
names(raw_data)
head(raw_data)

# 统一英文变量名
data <- raw_data %>%
  transmute(
    ID = `序号`,
    CDS_group = `分组`,
    hs_cTnT = as.numeric(`hs-TnT`),
    Sex = `性别`,
    Age = as.numeric(`年龄`),
    BMI = as.numeric(`BMI`),
    DBP = as.numeric(`舒张压DBP`),
    SBP = as.numeric(`收缩压SBP`),
    FPG = as.numeric(`空腹血糖FPG`),
    TG = as.numeric(`甘油三酯TG`),
    HDL_C = as.numeric(`HDL-C`),
    TC = as.numeric(`总胆固醇TChl`),
    LDL_C = as.numeric(`LDL-C`),
    WBC = as.numeric(`WBC(10^9)`),
    HGB = as.numeric(`血红蛋白Hgb`),
    SCr = as.numeric(`血清肌酐SCr`),
    eGFR = as.numeric(`肾小球滤过率EGFR`),
    UA = as.numeric(`尿酸UA`)
  )

# 设置分组及性别
data <- data %>%
  mutate(
    CDS_group = factor(
      CDS_group,
      levels = c("Control", "MetD", "MetS")
    ),
    Sex = factor(
      Sex,
      levels = c("女", "男"),
      labels = c("Female", "Male")
    ),
    
    # 主要研究结局
    hs_positive = if_else(hs_cTnT > 3, 1L, 0L),
    hs_group = factor(
      hs_positive,
      levels = c(0, 1),
      labels = c("<=3 ng/L", ">3 ng/L")
    )
  )

# 数据规模及分组
dim(data)
table(data$CDS_group)
table(data$hs_group)
table(data$CDS_group, data$hs_group)



# 2、逐变量缺失数量-----------------------------------------------------
missing_summary <- data.frame(
  Variable = names(data),
  Missing_N = colSums(is.na(data))
)

print(missing_summary)

# ID唯一性
anyDuplicated(data$ID)

# 关键变量范围
data %>%
  summarise(
    N = n(),
    Age_min = min(Age, na.rm = TRUE),
    Age_max = max(Age, na.rm = TRUE),
    BMI_min = min(BMI, na.rm = TRUE),
    BMI_max = max(BMI, na.rm = TRUE),
    FPG_max = max(FPG, na.rm = TRUE),
    TG_max = max(TG, na.rm = TRUE),
    eGFR_min = min(eGFR, na.rm = TRUE),
    hs_cTnT_max = max(hs_cTnT, na.rm = TRUE)
  )




# 3、生成table1-------------------------------------------------------------
# 按CDS三组比较基线特征

table1 <- data %>%
  select(
    CDS_group,
    Age, Sex,
    BMI, SBP, DBP, FPG, TG,
    HDL_C, TC, LDL_C,
    WBC, HGB, SCr, eGFR, UA,
    hs_group
  ) %>%
  tbl_summary(
    by = CDS_group,
    
    statistic = list(
      all_continuous() ~
        "{median} ({p25}, {p75})",
      all_categorical() ~
        "{n} ({p}%)"
    ),
    
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ 1
    ),
    
    type = list(
      hs_group ~ "dichotomous"
    ),
    
    value = list(
      hs_group ~ ">3 ng/L"
    ),
    
    label = list(
      Age ~ "Age, years",
      Sex ~ "Sex",
      BMI ~ "BMI, kg/m2",
      SBP ~ "SBP, mmHg",
      DBP ~ "DBP, mmHg",
      FPG ~ "FPG, mmol/L",
      TG ~ "TG, mmol/L",
      HDL_C ~ "HDL-C, mmol/L",
      TC ~ "TC, mmol/L",
      LDL_C ~ "LDL-C, mmol/L",
      WBC ~ "WBC, 10^9/L",
      HGB ~ "Hemoglobin, g/L",
      SCr ~ "Creatinine, umol/L",
      eGFR ~ "eGFR, mL/min/1.73m2",
      UA ~ "Uric acid, umol/L",
      hs_group ~ "hs-cTnT >3 ng/L"
    ),
    
    missing = "ifany"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = function(x) {
      gtsummary::style_pvalue(
        x,
        digits = 3
      )
    }
  ) %>%
  add_overall(last = FALSE) %>%
  bold_labels()

# 显示表格
table1




# 导出word
table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(
    path = "Table1_Baseline.docx"
  )








# part2 K-means聚类分析======================================================

# 1、聚类数量选择-----------------------------------------------------------

set.seed(1234)

# 选择6项代谢指标
cluster_vars <- c("BMI", "SBP", "DBP", "FPG", "TG", "HDL_C")

# Z-score标准化（不进行对数转换）
cluster_data <- scale(data[, cluster_vars])

# 计算不同k值的Silhouette
sil_result <- data.frame()

for (k in 2:6) {
  
  km <- kmeans(cluster_data, centers = k, nstart = 100)
  
  sil <- silhouette(km$cluster, dist(cluster_data))
  
  sil_result <- rbind(
    sil_result,
    data.frame(
      K = k,
      Silhouette = mean(sil[, 3]),
      Min_N = min(table(km$cluster))
    )
  )
}

print(sil_result)

# 绘制Silhouette图
p1 <- ggplot(sil_result, aes(x = factor(K), y = Silhouette)) +
  geom_col(fill = "#4472C4", width = 0.65) +
  geom_text(
    aes(label = sprintf("%.3f", Silhouette)),
    vjust = -0.4
  ) +
  labs(
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) +
  theme_classic(base_size = 12)

print(p1)

ggsave(
  "Figure2A_Silhouette.tiff",
  p1,
  width = 6,
  height = 4.2,
  dpi = 600,
  compression = "lzw"
)




# 2、正式K-means聚类--------------------------------------------------------

set.seed(1234)

km2 <- kmeans(
  cluster_data,
  centers = 2,
  nstart = 100
)

# 查看聚类人数
table(km2$cluster)

# 查看标准化聚类中心
round(km2$centers, 2)

# 根据代谢负担确定Cluster 1和Cluster 2
# HDL-C方向相反，因此取负值
score <- rowMeans(
  cbind(
    km2$centers[, 1:5, drop = FALSE],
    -km2$centers[, 6]
  )
)

low_cluster <- which.min(score)

data$Cluster <- factor(
  ifelse(km2$cluster == low_cluster, "Cluster 1", "Cluster 2"),
  levels = c("Cluster 1", "Cluster 2")
)

# 查看最终聚类人数
table(data$Cluster)

# CDS与Cluster交叉分布
table(data$CDS_group, data$Cluster)




# 3、Table 2：Cluster临床特征-----------------------------------------------

table2 <- data %>%
  select(
    Cluster,
    Age, Sex, BMI, SBP, DBP,
    FPG, TG, HDL_C,
    eGFR, CDS_group
  ) %>%
  tbl_summary(
    by = Cluster,
    
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ 1
    ),
    
    label = list(
      Age ~ "Age, years",
      Sex ~ "Sex",
      BMI ~ "BMI, kg/m2",
      SBP ~ "SBP, mmHg",
      DBP ~ "DBP, mmHg",
      FPG ~ "FPG, mmol/L",
      TG ~ "TG, mmol/L",
      HDL_C ~ "HDL-C, mmol/L",
      eGFR ~ "eGFR, mL/min/1.73m2",
      CDS_group ~ "CDS classification"
    )
  ) %>%
  bold_labels()

table2

# 导出Word
table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(
    path = "Table2_Cluster.docx"
  )



# 4、Figure 2B：聚类热图----------------------------------------------------

# 按Cluster排序
ord <- order(data$Cluster)

# 热图注释
annotation <- data.frame(
  Cluster = data$Cluster[ord]
)

rownames(annotation) <- as.character(data$ID[ord])

# 热图矩阵：行=受试者，列=代谢指标
heatmap_data <- cluster_data[ord, ]
rownames(heatmap_data) <- as.character(data$ID[ord])

pheatmap(
  t(heatmap_data),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  annotation_col = annotation,
  show_colnames = FALSE,
  color = colorRampPalette(
    c("#4575B4", "white", "#D73027")
  )(100),
  main = "Metabolic profiles",
  filename = "Figure2B_Heatmap.tiff",
  width = 10,
  height = 4
)




# 5、Figure 2C：代谢特征图--------------------------------------------------

profile <- as.data.frame(cluster_data) %>%
  mutate(Cluster = data$Cluster) %>%
  pivot_longer(
    cols = all_of(cluster_vars),
    names_to = "Variable",
    values_to = "Z_score"
  ) %>%
  group_by(Cluster, Variable) %>%
  summarise(
    Mean = mean(Z_score),
    SD = sd(Z_score),
    .groups = "drop"
  )

profile$Variable <- factor(
  profile$Variable,
  levels = cluster_vars
)

p2 <- ggplot(
  profile,
  aes(x = Variable, y = Mean, color = Cluster, group = Cluster)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    width = 0.12,
    alpha = 0.5
  ) +
  labs(
    x = NULL,
    y = "Standardized value (Z-score)"
  ) +
  theme_classic(base_size = 12)

print(p2)

ggsave(
  "Figure2C_Metabolic_Profile.tiff",
  p2,
  width = 7,
  height = 5,
  dpi = 600,
  compression = "lzw"
)



# 6、Figure 2D：CDS与Cluster交叉分布-----------------------------------------

cross_data <- data %>%
  count(CDS_group, Cluster) %>%
  group_by(CDS_group) %>%
  mutate(Percent = n / sum(n) * 100)

p3 <- ggplot(
  cross_data,
  aes(x = CDS_group, y = Percent, fill = Cluster)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(n, " (", sprintf("%.1f", Percent), "%)")),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(
    x = "CDS classification",
    y = "Percentage (%)",
    fill = "Metabolic phenotype"
  ) +
  theme_classic(base_size = 12)

print(p3)

ggsave(
  "Figure2D_CDS_Cluster.tiff",
  p3,
  width = 6,
  height = 5,
  dpi = 600,
  compression = "lzw"
)




##===========================================================================
# PCA二维可视化
pca <- prcomp(cluster_data, center = FALSE, scale. = FALSE)

pca_data <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  Cluster = data$Cluster
)

variance <- summary(pca)$importance[2, 1:2] * 100

p_pca <- ggplot(
  pca_data,
  aes(x = PC1, y = PC2, color = Cluster)
) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    x = paste0("PC1 (", round(variance[1], 1), "%)"),
    y = paste0("PC2 (", round(variance[2], 1), "%)")
  ) +
  theme_classic(base_size = 12)

print(p_pca)
##########====================================================================







# part3 K-means聚类稳定性验证==============================================

# 1、500次80%子抽样验证----------------------------------------------------

set.seed(1234)

B <- 500
n <- nrow(data)

# 保存结果
stability <- data.frame(
  ARI = numeric(B),
  Jaccard_1 = numeric(B),
  Jaccard_2 = numeric(B)
)

# 原始聚类标签（已按照代谢负担命名）
original_cluster <- as.integer(data$Cluster)

for (i in 1:B) {
  
  # 随机抽取80%样本
  idx <- sample(
    1:n,
    size = floor(0.8 * n),
    replace = FALSE
  )
  
  # 每次重新标准化
  x <- scale(data[idx, cluster_vars])
  
  # 重新进行K-means聚类
  km <- kmeans(
    x,
    centers = 2,
    nstart = 100
  )
  
  # 原始聚类与新聚类（仅比较共同受试者）
  old <- original_cluster[idx]
  new <- km$cluster
  
  # ARI：不受Cluster数字标签交换影响
  stability$ARI[i] <- mclust::adjustedRandIndex(
    old, new
  )
  
  # 根据两种标签排列，选择总重叠人数较大的排列
  tab <- table(
    factor(old, levels = 1:2),
    factor(new, levels = 1:2)
  )
  
  if (tab[1, 1] + tab[2, 2] <
      tab[1, 2] + tab[2, 1]) {
    new <- 3 - new
  }
  
  # Jaccard：交集 / 并集
  stability$Jaccard_1[i] <-
    sum(old == 1 & new == 1) /
    sum(old == 1 | new == 1)
  
  stability$Jaccard_2[i] <-
    sum(old == 2 & new == 2) /
    sum(old == 2 | new == 2)
}


# 2、汇总ARI和Jaccard------------------------------------------------------

stability_summary <- stability %>%
  summarise(
    Mean_ARI = mean(ARI),
    Median_ARI = median(ARI),
    ARI_P2.5 = quantile(ARI, 0.025),
    ARI_P97.5 = quantile(ARI, 0.975),
    
    Mean_Jaccard_1 = mean(Jaccard_1),
    Mean_Jaccard_2 = mean(Jaccard_2),
    
    Prop_ARI_08 = mean(ARI >= 0.8) * 100
  )

print(round(stability_summary, 3))

# 查看稳定性指标分布
summary(stability)



# 3、生成稳定性结果表------------------------------------------------------

stability_table <- data.frame(
  Metric = c(
    "Resampling iterations",
    "Sampling proportion",
    "Mean adjusted Rand index",
    "Median adjusted Rand index",
    "ARI, 2.5th–97.5th percentiles",
    "Mean Jaccard, Cluster 1",
    "Mean Jaccard, Cluster 2",
    "ARI >= 0.80, %"
  ),
  
  Result = c(
    as.character(B),
    "80%",
    sprintf("%.3f", mean(stability$ARI)),
    sprintf("%.3f", median(stability$ARI)),
    sprintf(
      "%.3f–%.3f",
      quantile(stability$ARI, 0.025),
      quantile(stability$ARI, 0.975)
    ),
    sprintf("%.3f", mean(stability$Jaccard_1)),
    sprintf("%.3f", mean(stability$Jaccard_2)),
    sprintf("%.1f", mean(stability$ARI >= 0.8) * 100)
  )
)

print(stability_table)

# 导出Word
flextable(stability_table) %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx(
    path = "Table_S2_Cluster_Stability.docx"
  )

# 保存每次抽样的原始结果
write.csv(
  stability,
  "Cluster_Stability_Raw.csv",
  row.names = FALSE
)






# part4 多变量Logistic回归=============================================

# 1、准备分析数据--------------------------------------------------------

# 设置参考组
data$CDS_group <- relevel(data$CDS_group, ref = "Control")
data$Cluster <- relevel(data$Cluster, ref = "Cluster 1")
data$Sex <- relevel(data$Sex, ref = "Female")

# 年龄和eGFR按每10单位计算，便于OR解释
data <- data %>%
  mutate(
    Age10 = Age / 10,
    eGFR10 = eGFR / 10
  )

# 核查主要结局
table(data$hs_positive)

# 核查Cluster的阳性人数
table(data$Cluster, data$hs_positive)

# 核查CDS与Cluster分布
table(data$CDS_group, data$Cluster)

# 检查关键变量是否缺失
colSums(is.na(
  data[, c(
    "hs_positive", "Age10", "Sex",
    "eGFR10", "CDS_group", "Cluster"
  )]
))


# 2、建立多变量Logistic模型-----------------------------------------------

# Model A：基础模型
model_A <- glm(
  hs_positive ~ Age10 + Sex + eGFR10,
  data = data,
  family = binomial()
)

# Model B：基础模型 + CDS分组
model_B <- glm(
  hs_positive ~ Age10 + Sex + eGFR10 + CDS_group,
  data = data,
  family = binomial()
)

# Model C：基础模型 + CDS分组 + Cluster
model_C <- glm(
  hs_positive ~ Age10 + Sex + eGFR10 + CDS_group + Cluster,
  data = data,
  family = binomial()
)

# 查看回归结果
summary(model_A)
summary(model_B)
summary(model_C)




# 3、提取OR、95%CI和P值-------------------------------------------------

get_OR <- function(model) {
  
  s <- summary(model)$coefficients
  
  result <- data.frame(
    Variable = rownames(s),
    OR = exp(s[, 1]),
    Lower = exp(s[, 1] - 1.96 * s[, 2]),
    Upper = exp(s[, 1] + 1.96 * s[, 2]),
    P = s[, 4],
    row.names = NULL
  )
  
  result <- result[result$Variable != "(Intercept)", ]
  
  result
}

# 分别提取Model B和Model C
OR_B <- get_OR(model_B)
OR_C <- get_OR(model_C)

# 整理P值格式
format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}

# 整理回归结果格式
format_OR <- function(df) {
  
  df %>%
    mutate(
      Result = paste0(
        sprintf("%.2f", OR),
        " (",
        sprintf("%.2f", Lower),
        "–",
        sprintf("%.2f", Upper),
        ")"
      ),
      P_value = format_p(P)
    ) %>%
    select(Variable, Result, P_value)
}

# 合并Model B与Model C
table3 <- full_join(
  format_OR(OR_B) %>%
    rename(
      Model_B = Result,
      P_B = P_value
    ),
  
  format_OR(OR_C) %>%
    rename(
      Model_C = Result,
      P_C = P_value
    ),
  
  by = "Variable"
)

# 统一变量名称及顺序
table3 <- table3 %>%
  mutate(
    Variable = recode(
      Variable,
      "Age10" = "Age (per 10 years)",
      "SexMale" = "Male vs Female",
      "eGFR10" = "eGFR (per 10 units)",
      "CDS_groupMetD" = "MetD vs Control",
      "CDS_groupMetS" = "MetS vs Control",
      "ClusterCluster 2" = "Cluster 2 vs Cluster 1"
    )
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "MetD vs Control",
        "MetS vs Control",
        "Cluster 2 vs Cluster 1",
        "Age (per 10 years)",
        "Male vs Female",
        "eGFR (per 10 units)"
      )
    )
  ) %>%
  arrange(Variable) %>%
  mutate(
    Variable = as.character(Variable),
    across(c(Model_B, P_B, Model_C, P_C),
           ~ replace_na(.x, "—"))
  )

print(table3)

# 导出Word
flextable(table3) %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx(
    path = "Table3_Logistic_Regression.docx"
  )




# 4、模型比较-----------------------------------------------------------

# Model A vs Model B：加入CDS后是否改善拟合
anova(model_A, model_B, test = "Chisq")

# Model B vs Model C：加入Cluster后是否改善拟合
anova(model_B, model_C, test = "Chisq")

# 比较AIC
AIC(model_A, model_B, model_C)


# # 提取似然比检验P值
# p_AB <- anova(model_A, model_B, test = "Chisq")$`Pr(>Chi)`[2]
# p_BC <- anova(model_B, model_C, test = "Chisq")$`Pr(>Chi)`[2]
# 
# model_comparison <- data.frame(
#   Comparison = c(
#     "Model A vs Model B",
#     "Model B vs Model C"
#   ),
#   Added_variable = c(
#     "CDS classification",
#     "Metabolic cluster"
#   ),
#   P_value = format_p(c(p_AB, p_BC))
# )
# 
# print(model_comparison)
# 
# # 导出Word
# flextable(model_comparison) %>%
#   theme_booktabs() %>%
#   autofit() %>%
#   save_as_docx(
#     path = "Table_Model_Comparison.docx"
#   )
# 



# Supplementary Table S2：嵌套模型似然比检验==========================

# 1. 提取模型比较结果
lrt_12 <- anova(model_A, model_B, test = "Chisq")
lrt_23 <- anova(model_B, model_C, test = "Chisq")

# 2. 整理结果
model_comparison <- data.frame(
  Comparison = c(
    "Model I vs Model II",
    "Model II vs Model III"
  ),
  
  Added_variable = c(
    "CDS classification",
    "Metabolic phenotype"
  ),
  
  LR_Chi_square = c(
    lrt_12$Deviance[2],
    lrt_23$Deviance[2]
  ),
  
  Df = c(
    lrt_12$Df[2],
    lrt_23$Df[2]
  ),
  
  P_value = c(
    lrt_12$`Pr(>Chi)`[2],
    lrt_23$`Pr(>Chi)`[2]
  )
)

# 3. 统一小数位数
model_comparison <- model_comparison %>%
  mutate(
    LR_Chi_square = sprintf("%.3f", LR_Chi_square),
    P_value = ifelse(
      P_value < 0.001,
      "<0.001",
      sprintf("%.3f", P_value)
    )
  )

# 4. 查看结果
print(model_comparison)









# 5、Figure 3：调整OR森林图（显示具体统计结果）-------------------------

forest_data <- OR_C %>%
  mutate(
    Variable = recode(
      Variable,
      "Age10" = "Age",
      "SexMale" = "Male",
      "eGFR10" = "eGFR",
      "CDS_groupMetD" = "MetD",
      "CDS_groupMetS" = "MetS",
      "ClusterCluster 2" = "Cluster 2"
    ),
    
    # 按希望展示的顺序排列
    Variable = factor(
      Variable,
      levels = rev(c(
        "MetD",
        "MetS",
        "Cluster 2",
        "Age",
        "Male",
        "eGFR"
      ))
    ),
    
    # OR及95%CI
    OR_label = sprintf(
      "%.2f (%.2f–%.2f)",
      OR, Lower, Upper
    ),
    
    # P值统一保留3位
    P_label = ifelse(
      P < 0.001,
      "<0.001",
      sprintf("%.3f", P)
    ),
    
    # 便于绘图时突出Cluster
    Group = ifelse(
      Variable == "Cluster 2 vs Cluster 1",
      "Cluster",
      "Other"
    )
  )

# 右侧文本列的位置，位于置信区间最大值右边
x_or <- 20
x_p  <- 65

p_forest <- ggplot(
  forest_data,
  aes(y = Variable)
) +
  # OR=1参考线
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey65"
  ) +
  
  # 95% CI
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      yend = Variable,
      color = Group
    ),
    linewidth = 0.8
  ) +
  geom_errorbarh(
    aes(xmin = Lower, xmax = Upper),
    height = 0.18,
    linewidth = 0.7,
    color = "#4472C4"
  ) +
  # OR点估计
  geom_point(
    aes(x = OR, color = Group),
    size = 3
  ) +
  
  # 右侧OR及95%CI文本
  geom_text(
    aes(x = x_or, label = OR_label),
    hjust = 0,
    size = 3.4,
    color = "black"
  ) +
  
  # 右侧P值
  geom_text(
    aes(x = x_p, label = P_label),
    hjust = 0,
    size = 3.4,
    color = "black"
  ) +
  
  # 添加表头
  annotate(
    "text",
    x = x_or,
    y = 6.7,
    label = "aOR (95% CI)",
    hjust = 0,
    fontface = "bold",
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = x_p,
    y = 6.7,
    label = "P value",
    hjust = 0,
    fontface = "bold",
    size = 3.5
  ) +
  
  scale_x_log10(
    breaks = c(0.1, 0.5, 1, 2, 5, 10),
    limits = c(0.1, 150)
  ) +
  
  scale_color_manual(
    values = c(
      "Cluster" = "#4472C4",
      "Other" = "#4472C4"
    )
  ) +
  
  labs(
    x = "Adjusted odds ratio (95% CI)",
    y = NULL
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "none",
    plot.margin = margin(20, 20, 10, 10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )

print(p_forest)

ggsave(
  "Figure3_Adjusted_OR.tiff",
  plot = p_forest,
  width = 11,
  height = 5,
  dpi = 600,
  compression = "lzw"
)









# CDS与Cluster关联程度
tab <- table(data$CDS_group, data$Cluster)

chi <- chisq.test(tab)

cramers_v <- sqrt(
  as.numeric(chi$statistic) /
    (sum(tab) * min(dim(tab) - 1))
)

print(cramers_v)

# 多重共线性
car::vif(model_C)










# Table 3：多变量Logistic回归==========================

# 1. 提取OR、95%CI及P值
get_OR <- function(model) {
  
  s <- summary(model)$coefficients
  
  data.frame(
    Variable = rownames(s),
    OR = exp(s[, 1]),
    Lower = exp(s[, 1] - 1.96 * s[, 2]),
    Upper = exp(s[, 1] + 1.96 * s[, 2]),
    P = s[, 4],
    row.names = NULL
  ) %>%
    filter(Variable != "(Intercept)") %>%
    mutate(
      OR_CI = sprintf(
        "%.2f (%.2f–%.2f)",
        OR, Lower, Upper
      ),
      P_value = ifelse(
        P < 0.001, "<0.001", sprintf("%.3f", P)
      )
    ) %>%
    select(Variable, OR_CI, P_value)
}

# 2. 分别提取Model II与Model III
result_II <- get_OR(model_B) %>%
  rename(
    Model_II = OR_CI,
    P_II = P_value
  )

result_III <- get_OR(model_C) %>%
  rename(
    Model_III = OR_CI,
    P_III = P_value
  )

# 3. 合并结果
table3 <- full_join(
  result_II, result_III,
  by = "Variable"
) %>%
  filter(
    Variable %in% c(
      "CDS_groupMetD",
      "CDS_groupMetS",
      "ClusterCluster 2"
    )
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      "CDS_groupMetD" = "MetD",
      "CDS_groupMetS" = "MetS",
      "ClusterCluster 2" = "Cluster 2"
    ),
    Variable = factor(
      Variable,
      levels = c("MetD", "MetS", "Cluster 2")
    )
  ) %>%
  arrange(Variable) %>%
  mutate(
    Variable = as.character(Variable),
    across(-Variable, ~ replace_na(.x, "—"))
  )

# 4. 添加参考组及分组标题
section <- function(x) {
  data.frame(
    Variable = x,
    Model_II = "", P_II = "",
    Model_III = "", P_III = ""
  )
}

reference <- function(x, in_II = FALSE, in_III = TRUE) {
  data.frame(
    Variable = x,
    Model_II = ifelse(in_II, "Reference", "—"),
    P_II = "—",
    Model_III = ifelse(in_III, "Reference", "—"),
    P_III = "—"
  )
}

table3 <- bind_rows(
  section("CDS classification"),
  reference("Control", TRUE, TRUE),
  table3 %>% filter(Variable %in% c("MetD", "MetS")),
  
  section("Metabolic phenotype"),
  reference("Cluster 1", FALSE, TRUE),
  table3 %>% filter(Variable == "Cluster 2")
)

# 5. 生成Word表格
ft <- flextable(table3) %>%
  set_header_labels(
    Variable = "Characteristics",
    Model_II = "Model II\naOR (95% CI)",
    P_II = "P-value",
    Model_III = "Model III\naOR (95% CI)",
    P_III = "P-value"
  ) %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

# 6. 突出分组标题
ft <- bold(
  ft,
  i = which(table3$Variable %in%
              c("CDS classification", "Metabolic phenotype")),
  bold = TRUE,
  part = "body"
)

# 7. 表格标题
ft <- set_caption(
  ft,
  caption = paste(
    "Table 3. Associations of conventional metabolic",
    "classification and clustering-derived phenotypes",
    "with hs-cTnT >3 ng/L"
  )
)

# 8. 添加表下注释
ft <- add_footer_lines(
  ft,
  values = paste(
    "Note: Model II was adjusted for age, sex,",
    "estimated glomerular filtration rate (eGFR),",
    "and Chinese Diabetes Society (CDS)-based metabolic classification.",
    "Model III was additionally adjusted for K-means-derived metabolic phenotype.",
    "Control and Cluster 1 served as the reference categories, respectively.",
    "Age and eGFR were included as continuous covariates,",
    "scaled per 10-unit increase.",
    "Results are presented as adjusted odds ratios (aORs)",
    "with 95% confidence intervals (CIs).",
    "A dash indicates that the variable was not included in the model."
  )
)

ft <- fontsize(ft, size = 8, part = "footer")

# 9. 查看并导出Word
ft

save_as_docx(
  ft,
  path = "Table3_Logistic_Regression2.docx"
)































# 
# # 5、Figure 3：调整OR森林图----------------------------------------------
# 
# # 整理Model C结果
# forest_data <- OR_C %>%
#   mutate(
#     Variable = recode(
#       Variable,
#       "Age10" = "Age (per 10 years)",
#       "SexMale" = "Male vs Female",
#       "eGFR10" = "eGFR (per 10 units)",
#       "CDS_groupMetD" = "MetD vs Control",
#       "CDS_groupMetS" = "MetS vs Control",
#       "ClusterCluster 2" = "Cluster 2 vs Cluster 1"
#     )
#   ) %>%
#   mutate(
#     Variable = factor(
#       Variable,
#       levels = rev(c(
#         "MetD vs Control",
#         "MetS vs Control",
#         "Cluster 2 vs Cluster 1",
#         "Age (per 10 years)",
#         "Male vs Female",
#         "eGFR (per 10 units)"
#       ))
#     ),
#     Label = paste0(
#       sprintf("%.2f", OR),
#       " (",
#       sprintf("%.2f", Lower),
#       "–",
#       sprintf("%.2f", Upper),
#       ")"
#     )
#   )
# 
# # 绘制森林图
# p_forest <- ggplot(
#   forest_data,
#   aes(x = OR, y = Variable)
# ) +
#   geom_vline(
#     xintercept = 1,
#     linetype = "dashed",
#     color = "grey60"
#   ) +
#   geom_errorbarh(
#     aes(xmin = Lower, xmax = Upper),
#     height = 0.18,
#     linewidth = 0.7,
#     color = "#4472C4"
#   ) +
#   geom_point(
#     size = 2.8,
#     color = "#4472C4"
#   ) +
#   scale_x_log10() +
#   labs(
#     x = "Adjusted odds ratio (95% CI)",
#     y = NULL
#   ) +
#   theme_classic(base_size = 12)
# 
# print(p_forest)
# 
# # 导出TIFF
# ggsave(
#   "Figure3_Adjusted_OR.tiff",
#   p_forest,
#   width = 7,
#   height = 4.5,
#   dpi = 600,
#   compression = "lzw"
# )

