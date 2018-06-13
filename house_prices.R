setwd("~/GitHub/House Prices")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(readr)
library(data.table)
library(GGally)


train <- read_csv("train.csv")
test <- read_csv("test.csv")
full <- bind_rows(train, test)

str(full)

# To check for missing values
sapply(full, function(x) sum(is.na(x) | x ==""))

# Unique values per column
sapply(full, function(x) length(unique(x))) 


missing_values <- full %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
    ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
    geom_bar(stat="identity",fill="red")+
    coord_flip()+theme_bw()

setdiff(names(train), names(test)) 

full %>% map_dbl(~sum(is.na(.)))


checkColumn = function(df,colname){
    
    testData = df[[colname]]
    numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
    
    
    if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
        list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
    } else{
        list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
    }
    
}
checkAllCols = function(df){
    resDF = data.frame()
    for (colName in names(df)){
        resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
    }
    resDF
}


datatable(checkAllCols(train), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

tbl_corr <- full %>%
    filter(set=="train") %>%
    select_if(is.numeric) %>%
    cor(use="complete.obs") %>%
    corrplot.mixed(tl.cex=0.85)

full %>%
    mutate_all(as.numeric) %>%
    select(everything()) %>%
    ggcorr(method = c("pairwise","spearman"), label = FALSE, angle = -0, hjust = 0.2) +
    coord_flip()
