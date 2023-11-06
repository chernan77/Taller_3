#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Merit Tejeda: 202210104
# Celin Hernández: 202210067

install.packages("readxl")
install.packages("readr")
install.packages("pacman")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("writexl")
install.packages("stargazer")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("rvest")
install.packages("glmnet")
install.packages("caret")
library(ggplot2)
library(openxlsx)
library(pacman)
library(readxl)
library(dplyr)
library(writexl)
library(stargazer)
library(gridExtra)
library(tidyverse)
library(rvest)
library(readr)
library(glmnet)
library(caret)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos 
       tmaptools, # geocode_OSM()
       tidymodels,
       dplyr) #para modelos de ML

submission_template <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/submission_template.xlsx")
train_personas1 <- read.xlsx("https://github.com/chernan77/Data_Taller3/blob/main/train_personas_1.xlsx")
train_personas2 <- read.xlsx("https://github.com/chernan77/Data_Taller3/blob/main/train_personas_2.xlsx")
train_personas3 <- read.xlsx("https://github.com/chernan77/Data_Taller3/blob/main/train_personas_3.xlsx")
train_personas4 <- read.xlsx("https://github.com/chernan77/Data_Taller3/blob/main/train_personas_4.xlsx")
train_hogares <- read.csv("https://github.com/chernan77/Data_Taller3/blob/main/train_hogares.csv")
test_personas1 <- read.xlsx("https://github.com/chernan77/Data_Taller3/blob/main/test_personas_1.xlsx")
test_personas2 <- read.xlsx("https://github.com/chernan77/Data_Taller3/blob/main/test_personas_2.xlsx")
test_hogares <- read.csv("https://github.com/chernan77/Data_Taller3/blob/main/test_hogares.csv")



train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# -----------------------DUPERACIÓN DE DATOS------------------------------------# 

