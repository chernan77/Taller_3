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
p_load(rio, # Import data easily
       plotly, # Gráficos 
       tmaptools, # geocode_OSM()
       tidymodels) #para modelos de ML


sample_submission <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/sample_submission.xlsx")
train_personas1 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_1.xlsx")
train_personas2 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_2.xlsx")
train_personas3 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_3.xlsx")
train_personas4 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/train_personas_4.xlsx")
train_hogares <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/train_hogares.xlsx")
test_personas1 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_personas_1.xlsx")
test_personas2 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_personas_2.xlsx")
test_hogares <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_hogares.xlsx")



# -----------------------UNIÓN DE DATOS------------------------------------# 


train_personas <- rbind(train_personas1, train_personas2,train_personas3,train_personas4)
test_personas <- rbind(test_personas1, test_personas2)

train_personas %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

train_personas <- train_personas %>%
  mutate(Des = ifelse(Oc == 1 & is.na(Des), 0, Des))
train_personas$Oc <- ifelse(train_personas$Des == 1 & is.na(train_personas$Oc), 0, train_personas$Oc)

# Renombrar las variables para una mayor comprensión de las variables que estamos trabajando
train_personas <- train_personas %>% rename(Sexo=P6020) # 1:Hombre y 2: Mujer
train_personas <- train_personas %>% rename(Edad=P6040) # ¿Cuantos años tiene cumplido?
train_personas <- train_personas %>% rename(Parent_jh=P6050) # Parentesco con el jefe de hogar
train_personas <- train_personas %>% rename(SS=P6090) # 1:Si, 2: No y 9: No sabe
train_personas <- train_personas %>% rename(R_SS=P6100) # 1:Si, 2: No y 9: No sabe
train_personas <- train_personas %>% rename(Educ=P6210) 
train_personas <- train_personas %>% rename(G_Educ=P6210s1)
train_personas <- train_personas %>% rename(Act_Ocup=P6240)
train_personas <- train_personas %>% rename(Exp_Emp=P6426)
train_personas <- train_personas %>% rename(Cat_Ocup=P6430)
train_personas <- train_personas %>% rename(Ing_MesP=P6500)
train_personas <- train_personas %>% rename(Ing_Extras=P6510)
train_personas <- train_personas %>% rename(Hrs_Extras=P6510s1)
train_personas <- train_personas %>% rename(Primas=P6545)
train_personas <- train_personas %>% rename(Ing_Primas=P6545s1)
train_personas <- train_personas %>% rename(Bonif=P6580)
train_personas <- train_personas %>% rename(Ing_Bonif=P6580s1)
train_personas <- train_personas %>% rename(Sub_Alim=P6585s1)
train_personas <- train_personas %>% rename(Ing_Sub_Alim=P6585s1a1)
train_personas <- train_personas %>% rename(Sub_Transp=P6585s2)
train_personas <- train_personas %>% rename(Ing_Sub_Transp=P6585s2a1)
train_personas <- train_personas %>% rename(Sub_Fam=P6585s3)
train_personas <- train_personas %>% rename(Ing_Sub_Fam=P6585s3a1)
train_personas <- train_personas %>% rename(Sub_Educ=P6585s4)
train_personas <- train_personas %>% rename(Ing_Sub_Educ=P6585s4a1)
train_personas <- train_personas %>% rename(Pag_Alim=P6590)
train_personas <- train_personas %>% rename(Ing_Pag_Alim=P6590s1)
train_personas <- train_personas %>% rename(Pag_Viv=P6600)
train_personas <- train_personas %>% rename(Ing_Pag_Viv=P6600s1)
train_personas <- train_personas %>% rename(Pag_Transp=P6610)
train_personas <- train_personas %>% rename(Ing_Pag_Transp=P6610s1)
train_personas <- train_personas %>% rename(Otros_Ing=P6620)
train_personas <- train_personas %>% rename(Vl_Otros_Ing=P6620s1)
train_personas <- train_personas %>% rename(Prima_Serv=P6630s1)
train_personas <- train_personas %>% rename(Ing_Prima_Serv=P6630s1a1)
train_personas <- train_personas %>% rename(Prima_Nav=P6630s2)
train_personas <- train_personas %>% rename(Ing_Prima_Nav=P6630s2a1)
train_personas <- train_personas %>% rename(Prima_Vac=P6630s3)
train_personas <- train_personas %>% rename(Ing_Prima_Vac=P6630s3a1)
train_personas <- train_personas %>% rename(Viats=P6630s4)
train_personas <- train_personas %>% rename(Ing_Viats=P6630s4a1)
train_personas <- train_personas %>% rename(Bons_Anuales=P6630s6)
train_personas <- train_personas %>% rename(Ing_Bons_Anuales=P6630s6a1)
train_personas <- train_personas %>% rename(Gan_Neta=P6750)
train_personas <- train_personas %>% rename(Gan_Neta_Meses=P6760)
train_personas <- train_personas %>% rename(Gan_Neta_Neg=P550)
train_personas <- train_personas %>% rename(Hrs_Trab=P6800)
train_personas <- train_personas %>% rename(Num_Trab=P6870)
train_personas <- train_personas %>% rename(Fond_Pens=P6920)
train_personas <- train_personas %>% rename(Act_Sec=P7040)
train_personas <- train_personas %>% rename(Hrs_Act_Sec=P7045)
train_personas <- train_personas %>% rename(Ocup_Act_Sec=P7050)
train_personas <- train_personas %>% rename(Ing_Act_Sec=P7070)
train_personas <- train_personas %>% rename(Qtrab_Hrs=P7090)
train_personas <- train_personas %>% rename(DC_trab=P7140s1)
train_personas <- train_personas %>% rename(Mot_DC_trab=P7140s2)
train_personas <- train_personas %>% rename(Rec_Rem=P7510s2)
train_personas <- train_personas %>% rename(Ing_Rec_Rem=P7510s2a1)
train_personas <- train_personas %>% rename(Ayuda_Gub=P7510s3)
train_personas <- train_personas %>% rename(Ing_Ayuda_Gub=P7510s3a1)
train_personas <- train_personas %>% rename(Intrs=P7510s5)
train_personas <- train_personas %>% rename(Ing_Intrs=P7510s5a1)
train_personas <- train_personas %>% rename(Cesant=P7510s6)
train_personas <- train_personas %>% rename(Ing_Cesant=P7510s6a1)
train_personas <- train_personas %>% rename(Otrs_Ftes=P7510s7)
train_personas <- train_personas %>% rename(Ing_Otrs_Ftes=P7510s7a1)
train_personas <- train_personas %>% rename(Ocupac=Oc)
train_personas <- train_personas %>% rename(Ing_Total_Obs=Ingtotob)
train_personas <- train_personas %>% rename(Ing_Total=Ingtot)

# Renombrar las variables de train_hogares

train_hogares <- train_hogares %>% rename(Habit_por_Hogar=P5000)
train_hogares <- train_hogares %>% rename(Dormit=P5010)
train_hogares <- train_hogares %>% rename(Prop_Vivienda=P5090)
train_hogares <- train_hogares %>% rename(Pag_Arriendo_Est=P5130)
train_hogares <- train_hogares %>% rename(Pag_Arriendo=P5140)
train_hogares <- train_hogares %>% rename(Per_por_Hogar=Nper)
train_hogares <- train_hogares %>% rename(Per_Uni_Gasto=Npersug)
train_hogares <- train_hogares %>% rename(Linea_Indigencia=Li)
train_hogares <- train_hogares %>% rename(Linea_Pobreza=Lp)
























































