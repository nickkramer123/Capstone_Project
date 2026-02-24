library(foreign)
library(dplyr)
library(tidyverse)
library(MASS)
library(brant)
  read.dct <- function(dct, labels.included = "yes") {
      temp <- readLines(dct)
      temp <- temp[grepl("_column", temp)]
      switch(labels.included,
             yes = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
                 classes <- c("numeric", "character", "character", "numeric", "character")
                 N <- 5
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
             },
             no = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
                 classes <- c("numeric", "character", "character", "numeric")
                 N <- 4
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
             })
      temp_metadata <- setNames(lapply(1:N, function(x) {
          out <- gsub(pattern, paste("\\", x, sep = ""), temp)
          out <- gsub("^\\s+|\\s+$", "", out)
          out <- gsub('\"', "", out, fixed = TRUE)
          class(out) <- classes[x] ; out }), NAMES)
      temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
      temp_metadata
  }

  read.dat <- function(dat, metadata_var, labels.included = "yes") {
      read.table(dat, col.names = metadata_var[["ColName"]])
  }


GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

GSS[GSS < 0] <- NA
GSS$BALLOT <- NULL

sum(is.na(GSS$ETHNIC))
sum(is.na(GSS$WRKSTAT))
sum(is.na(GSS$SATJOB))

df_no_neg <- na.omit(GSS)
df_cleaned <- GSS[!is.na(GSS$SATJOB), ]
df_cleaned <- df_cleaned[!is.na(df_cleaned$AGE),]

ggplot(data = df_cleaned, aes(x = factor(SATJOB), fill = factor(INCOME)))+
  geom_bar()
ggplot(data = df_cleaned, aes(y = factor(SATJOB), x = AGE))+
  geom_boxplot()
ggplot(data = df_cleaned, aes(x = factor(SATJOB), fill = factor(HEALTH)))+
  geom_bar()
table(df_cleaned$SATJOB)

model <- polr(factor(SATJOB) ~ AGE, data = df_cleaned, Hess = TRUE)
summary(model)
brant(model)

predicted_prob <- as.data.frame(predict(model,type = "probs"))
colnames(predicted_prob) <- c("1", "2", "3", "4")
ggplot(predicted_prob, aes(x = df_cleaned$AGE)) +
  geom_line(aes(y = `1`, color = "Category 1"), size = 1) +
  geom_line(aes(y = `2`, color = "Category 2"), size = 1) +
  geom_line(aes(y = `3`, color = "Category 3"), size = 1) +
  geom_line(aes(y = `4`, color = "Category 4"), size = 1) +
  labs(title = "Ordinal Logistic Regression",
       x = "AGE",
       y = "Predicted Probability") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
                     name = "Category") +
  theme_minimal()

table(df_cleaned$AGE)

