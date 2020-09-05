library(tidyverse)
library(readxl) 

DF_REVIEWS <- read.csv(paste0(file.choose())) 
DF_ITEMS <- read.csv(paste0(file.choose())) 

DF_ITEMS_SUBSET = subset(DF_ITEMS, select = c("asin", "brand", "rating"))
DF_REVIEWS_SUBSET = subset(DF_REVIEWS, select = c("asin", "rating", "verified", "body", "helpfulVotes"))


DF_ITEMS_WIDE <- aggregate(asin ~ brand, DF_ITEMS_SUBSET, c)

DF_ITEMS_WIDER <- DF_ITEMS_WIDE %>% unnest(asin) %>% 
                  group_by(brand) %>% 
                  mutate(col=seq_along(brand)) %>% #add a column indicator
                  spread(key=col, value=asin)


df2 <- data.frame(t(df[-1]))
colnames(df2) <- df[, 1]

DF_ITEMS_TRANSPOSED <- data.frame(t(DF_ITEMS_WIDER[-1]))
table(DF_ITEMS_TRANSPOSED$X1)

library(plyr)
DF_ITEMS_TRANSPOSED <- rename(DF_ITEMS_TRANSPOSED, c("X1"=" ", "X2"="Apple", "X3"="ASUS", "X4"="Google", "X5"= "HUAWEI", "X6"="Motorola",
                                                     "X7"="Nokia", "X8"="OnePlus", "X9"="Samsung", "X10"="Sony", "X11"="Xiaomi"))


# Apple Subset
DF_APPLE <- subset(DF_ITEMS_TRANSPOSED, select = c("Apple"))
DF_APPLE <- rename(DF_APPLE, c("Apple"="asin"))

DF_APPLE_REVIEWS <- merge(DF_APPLE, DF_REVIEWS_SUBSET, by = "asin", all.x = TRUE)
DF_APPLE_REVIEWS$brand <- "Apple"
table(DF_APPLE_REVIEWS$verified)



