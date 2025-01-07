library(tidyverse)
library(irr)
library(knitr)
library(kableExtra)

setwd("~/work/conferences/2025/dh2025/")

janis <- read.csv("DH_2025_Anno_DEU_2nd_run.xlsx - Annotationen_2nd_run_Janis.tsv", sep = "\t")
janis$id <- 1:nrow(janis)

axel <- read.csv("DH_2025_Anno_DEU_2nd_run.xlsx - Annotationen_2nd_run_Axel.tsv", sep = "\t")
axel$id <- 1:nrow(axel)


create_iaa_table <- function(iaa_object) {
  df <- data.frame(cohen.kappa = iaa_object$value, p.value = iaa_object$p.value, raters = iaa_object$raters, subjects = iaa_object$subjects)
  df
}

iaa_processing <- function(df) {
  iaa.alle <- create_iaa_table(kappa2(df[,c("janis", "axel")]))
  iaa.alle
}

df_fokalisierung <- as.data.frame(cbind(janis$Fokalisierung, axel$Fokalisierung))
colnames(df_fokalisierung) <- c("janis", "axel")
df_fokalisierung$id <- 1:nrow(df_fokalisierung)
df_fokalisierung$Autor <- janis$Autor
df_fokalisierung$Titel <- janis$Titel
df_fokalisierung$Absatz <- janis$Absatz
iaa_fokalisierung <- iaa_processing(df_fokalisierung)

kable(x = iaa_fokalisierung, format = "simple", row.names = FALSE)

# Rows were there is no agreement
write.table(x = df_fokalisierung %>% select(-id) %>% filter(janis != axel), file = "disagreements.tsv", sep = "\t", row.names = FALSE)
