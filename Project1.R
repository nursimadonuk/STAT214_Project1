# A sued B, A handled personal injury cases
# B handled only workers compensation cases
# adds resulted in more compensation cases for partner B,
# even though firm worked w personal injury cases, so A
# wants B to cover advertising expenses. 

# ---------------- Setting Up ------------------------

# Set working directory
setwd("Desktop")

# Load data
load("LEGALADV.Rdata")

# View Data
View(LEGALADV)

# Handling Missing Data 
newdata <- LEGALADV[7:48,]
View(newdata)
head(newdata)
# ------------- Installing Packages -----------------------
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

# ------------------ Summary of data -------------------------
str(newdata)
summary(newdata)

# ---------------- Scatterplots -------------------------------

# For workers compensation cases
ggplot(LEGALADV, aes(LEGALADV$ADVEXP6, LEGALADV$NEWWC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear plot of NewWC vs AdvExp6") +
  xlab("6-Month Cumulative Spending") +
  ylab("Number of New WC Cases")


# For personal injury cases
ggplot(LEGALADV, aes(LEGALADV$ADVEXP6, LEGALADV$NEWPI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear plot of NewPI vs AdvExp6") +
  xlab("6-Month Cumulative Spending") +
  ylab("Number of New PI Cases")

# ----------------- The Models ---------------------------

# NEW_PI = 7.7675 + 0.1129(ADVEXP6)
PI_lm <- lm(formula = NEWPI ~ ADVEXP6, data = newdata)
coefficients(PI_lm)

# NEW_WC = 24.5741 + 0.0098(ADVEXP6)
WC_lm <- lm(formula = NEWWC ~ ADVEXP6, data = newdata)
coefficients(WC_lm)

# ------------------- Bar plots ----------------------------

barplot(newdata$TOTADV,
        main = "Total Advertising Expenses per Month",
        xlab = "Month",
        ylab = "Expenses",
        names.arg = newdata$MONTH,
        col = "darkmagenta")


barplot(newdata$NEWPI,
        main = "New PI Cases per Month",
        xlab = "Month",
        ylab = "New PI Cases",
        names.arg = newdata$MONTH,
        col = "blue")

barplot(newdata$NEWWC,
        main = "New WC Cases per Month",
        xlab = "Month",
        ylab = "New PI Cases",
        names.arg = newdata$MONTH,
        col = "green")

barplot(newdata$ADVEXP6,
        main = "Cumulative Spending per Month",
        xlab = "Month",
        ylab = "6-Month Cumulative Spending",
        names.arg = newdata$MONTH,
        col = "purple",
        horiz = T)
