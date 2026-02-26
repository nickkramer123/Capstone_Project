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


GSS_metadata <- read.dct("data/GSS.dct")
GSS_ascii <- read.dat("data/GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

GSS[GSS < 0] <- NA
GSS$BALLOT <- NULL

sum(is.na(GSS$ETHNIC))
sum(is.na(GSS$WRKSTAT))
sum(is.na(GSS$SATJOB))

df_no_neg <- na.omit(GSS)
df_cleaned <- GSS[!is.na(GSS$SATJOB), ]
df_cleaned$SATJOB_BIN <- ifelse(df_cleaned$SATJOB %in% c(1, 2), 0,
                          ifelse(df_cleaned$SATJOB %in% c(3, 4), 1, NA))
df_cleaned$RACE <- factor(df_cleaned$RACE,
                          levels = c(1, 2, 3),
                          labels = c("White", "Black", "Other"))
df_cleaned$HEALTH <- factor(df_cleaned$HEALTH,
                            levels = c(1, 2, 3, 4),
                            labels = c("Excellent", "Good", "Fair", "Poor"))
df_cleaned <- df_cleaned[!is.na(df_cleaned$AGE),]

hist(df_cleaned$CHILDS,
     breaks = seq(-0.5, 8.5, by = 1),
     main = "Histogram of Number of Children",
     xlab = "Number of Children",
     ylab = "Frequency",
     xaxt = "n")
axis(1, at = 0:8)

df_cleaned$DECADE <- floor(df_cleaned$YEAR / 10) * 10
race_decade_table <- table(df_cleaned$DECADE, df_cleaned$RACE)
barplot(race_decade_table,
        beside = TRUE,
        col = c("salmon", "darkorange", "yellow", "lightgreen", "lightblue", "darkorchid"),
        legend = TRUE,
        main = "Race Distribution by Decade",
        xlab = "Decade",
        ylab = "Count")

health_counts <- table(df_cleaned$HEALTH)

barplot(health_counts,
        main = "Distribution of Self-Rated Health",
        xlab = "Health Status",
        ylab = "Count",
        col = "lightblue",
        border = "black")

df_cleaned <- df_cleaned[, !(names(df_cleaned) %in% c("HAPPY", "SATFIN", "WRKSLF"))]

model <- glm(SATJOB_BIN ~ YEAR + AGE,
             data = df_cleaned,
             family = binomial(link = "logit"))
summary(model)

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


df_cleaned <- df_cleaned |> 
  mutate(JOBSAT = recode(SATJOB,
                         `1` = 1,
                         `2` = 1,
                         `3` = 0,
                         `4` = 0))
logReg <- glm(formula = JOBSAT ~ AGE + SATFIN, family = binomial(link = "logit"), data = df_cleaned)
summary(logReg)


