setwd("~/GitHub/House Prices")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(readr)
library(data.table)
library(GGally)
library(caret)
library(gridExtra)
library(scales)
library(corrplot)
library(ggrepel)
library(plyr)


train_raw <- read_csv("train.csv")
test_raw <- read_csv("test.csv")


str(train_raw)
str(test_raw)

test_raw$SalePrice <- NA
full <- bind_rows(train_raw, test_raw)
length(unique(full$Id)) == length(full$Id) ## check no duplicate entries

test_Id <- test_raw$Id

full$Id <- NULL

summary(full)
# To check for missing values
hhh <- sapply(full, function(x) sum(is.na(x) | x ==""))

# Historam plot for the response var
ggplot(full[!is.na(full$SalePrice),],aes(x=SalePrice) ) +
    geom_histogram(bins = 50) +
    scale_x_continuous(breaks = seq( 0, max(full$SalePrice,na.rm = TRUE), 
                                     by = 100000), labels = comma)

# Separating Numeric and Char vars
num_vars <- which(sapply(full, is.numeric))
char_vars <- which(sapply(full, is.character))

# Numeric vars correlation with SalesPrice
# sort the cor matrix and then limit to cor > 0.5
num_vars_cor <- cor(full[,num_vars],use = "pairwise.complete.obs")
cor_sorted <- as.matrix(sort(num_vars_cor[,'SalePrice'], decreasing = TRUE))
high_cor <- cor_sorted[abs(cor_sorted) > 0.5, ]
high_cor_mat <- num_vars_cor[names(high_cor), names(high_cor)]
corrplot.mixed(high_cor_mat, tl.col="black", tl.pos = "lt")


# Plot cor between highest two vars vs SalePrice
ggplot(full[!is.na(full$SalePrice), ],aes(x=factor(OverallQual), 
                                          y= SalePrice))+
    geom_boxplot(col = 'blue') +
    scale_y_continuous(breaks = seq(0,800000, by = 100000), labels = comma)


ggplot(data=full[!is.na(full$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
    geom_point(col='blue') +
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_text_repel(aes(label = ifelse
                      (full$GrLivArea[!is.na(full$SalePrice)]>4500,
                          rownames(full), '')))


# Recalling which vars has missing values
missing_var <- sapply(full, function(x) sum(is.na(x) | x == ""))
missing_var <- sort(missing_var[missing_var>0], decreasing= TRUE)
missing_var

# Starting with the var of most missing values till var with the least missing 
# values
# 
# since PoolQC: Pool quality
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool 
# 
# It is obvious that the values of this var is ordinal, so it is fair to assign
# ordinal values to each of the values here. But first, I will have to replace
# each NA value with a string No_Pool
# 
full$PoolQC[is.na(full$PoolQC)] <- "No_Pool"
ordinal_val <- c("No_Pool" = 0, "Fa" = 1, "TA" = 2, "Gd" = 3,"Ex" = 4, "Po" = 5)
full$PoolQC <- as.integer(revalue(full$PoolQC, ordinal_val))

all$PoolQC[is.na(all$PoolQC)] <- 'None'
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))


TRY USING MICE TO IMPUTE MISSING DATA LOOK FOR A KERNEL THAT USED IT
# # Unique values per column
# sapply(full, function(x) length(unique(x))) 
# 
# 
# missing_values <- full %>% summarize_all(funs(sum(is.na(.))/n()))
# 
# missing_values <- gather(missing_values, key="feature", value="missing_pct")
# missing_values %>% 
#     ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
#     geom_bar(stat="identity",fill="red")+
#     coord_flip()+theme_bw()
# 
# setdiff(names(train_raw), names(test_raw)) 

#full %>% map_dbl(~sum(is.na(.)))


# checkColumn = function(df,colname){
#     
#     testData = df[[colname]]
#     numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
#     
#     
#     if (class(testData) == 'numeric' | class(testData) == 'Date' |
#         class(testData) == 'difftime' | class(testData) == 'integer'){
#         list('col' = colname,'class' = class(testData), 
#              'num' = length(testData) - numMissing, 
#              'numMissing' = numMissing, 
#              'numInfinite' = sum(is.infinite(testData)), 
#              'avgVal' = mean(testData,na.rm=TRUE), 
#              'minVal' = round(min(testData,na.rm = TRUE)), 
#              'maxVal' = round(max(testData,na.rm = TRUE)))
#     } else{
#         list('col' = colname,
#              'class' = class(testData), 
#              'num' = length(testData) - numMissing, 
#              'numMissing' = numMissing, 
#              'numInfinite' = NA,  
#              'avgVal' = NA, 
#              'minVal' = NA, 
#              'maxVal' = NA)
#     }
#     
# }
# checkAllCols = function(df){
#     resDF = data.frame()
#     for (colName in names(df)){
#         resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
#     }
#     resDF
# }
# 
# 
# #datatable(checkAllCols(train), style="bootstrap", class="table-condensed",
# # options = list(dom = 'tp',scrollX = TRUE))
# # ###
# # tbl_corr <- full %>%
# #     filter(set=="train") %>%
# #     select_if(is.numeric) %>%
# #     cor(use="complete.obs") %>%
# #     corrplot.mixed(tl.cex=0.85)
# # ###
# full %>%
#     mutate_all(as.numeric) %>%
#     select(everything()) %>%
#     ggcorr(method = c("pairwise","spearman"), label = FALSE, angle = -0, 
#            hjust = 0.2) +
#     coord_flip()
# 


### Model Fitting

full_train <- full[1:1460,]
full_test <- full[1461:2919,]
set.seed(123)
inTrain <- createDataPartition(y=full_train$SalePrice, p=0.75, list = FALSE)
train <- full_train[inTrain,]
test <- full_train[-inTrain,]

set.seed(123)
logreg <- ran