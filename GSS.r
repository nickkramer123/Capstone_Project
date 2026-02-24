library(foreign)
library(dplyr)
library(tidyverse)
library(MASS)
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

ggplot(data = df_cleaned, aes(x = factor(SATJOB), fill = factor(INCOME)))+
  geom_bar()
ggplot(data = df_cleaned, aes(y = factor(SATJOB), x = AGE))+
  geom_boxplot()
ggplot(data = df_cleaned, aes(x = factor(SATJOB), fill = factor(HEALTH)))+
  geom_bar()
table(df_cleaned$SATJOB)

model <- polr(factor(SATJOB) ~ AGE + INCOME, data = df_cleaned, Hess = TRUE)
summary(model)
