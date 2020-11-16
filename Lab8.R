
#Instalar Data explore
#install.packages("/Users/raulcastellanos/Desktop/Universidad/S6/Data Wrangling/DataExplorer_0.8.1.tar", repos = NULL, type = "source")
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(modeest)
library(DataExplorer)
library(DT)
library(gdata)
timd <- read_csv("titanic_MD.csv")
ti <- read_csv("titanic.csv")


# Inciso 1 ----------------------------------------------------------------
# Reporte Missing Data para todas las columnas

summary(timd)

#Columnas MD: Sex, Age, SibSp, Parch, Fare, Embarked

# Inciso 2 ----------------------------------------------------------------
#Para cada columna qué método utilizará
timd2 <- read_csv("titanic_MD.csv")

#Sex 
#Utilizaría la de filling values ya que en la columna de name están las abreviaviones como: Mr, Mrs, Miss, Master, Sir,
#Mme, Dr, Major, Countess
timd2$Sex <- ifelse(str_detect(timd2$Name, "Mrs|Miss|Mme|Mlle|Countess"), "female", "male")

#Age 
#Prediccion con Regresion lineal, con la relacion entre age y fare, devolvera el valor con el intercepto en Y (Fare)
#cuando Age es cero + la pendiente multiplicada por fare
cor(timd2$Age, timd2$Fare, use = "pairwise.complete.obs")
lm(Age ~ Fare, data = timd2)
intercept <- 37.98950
slope <- -0.02655

timd2$Age <- ifelse(is.na(timd2$Age), (intercept + slope*timd2$Fare), timd2$Age)

#SibSP
#Para esta columna llenare los NA con los valores de la media 
timd2$SibSp <- ifelse(is.na(timd2$SibSp), round(mean(timd2$SibSp, na.rm = TRUE),0), timd2$SibSp)

#Parch
#Para esta columna llenare los NA con los valores de la moda
timd2$Parch <- ifelse(is.na(timd2$Parch), mfv1(timd2$Parch, na_rm = TRUE), timd2$Parch)

#Fare
#Para esta columna usare el relleno de valores NA con la media
timd2$Fare <- ifelse(is.na(timd2$Fare), round(mean(timd2$Fare, na.rm = TRUE),0), timd2$Fare)

#Embarked
#Usare filling values top to down para esta columna
timd2 <- timd2 %>% fill(Embarked)

# Inciso 3 ----------------------------------------------------------------
# Reporte que filas estan completas
summary(timd)

#Columnas completads: PassengerID, Survived, Pclass, Name, Ticket, Cabin


# Inciso 4 ----------------------------------------------------------------
# Utilizar los siguientes métodos para cada columna que contiene missing values: (50%)
#a. Pairwise deletion
#b. Imputación general (media, moda y mediana)
#c. Imputación sectorizada
#d. Modelo de regresión lineal simple
#e. Eliminación de outliers: Standard deviation approach
#f. Eliminación de outliers: Percentile approach
timd4 <- timd

#Nota 1: Agregar Sex 2 como nueva columna para MedMedMo
# Columna Sex -------------------------------------------------------------
timd4$Sex2 <- ifelse(timd4$Sex == "female", 0,
                     ifelse(timd4$Sex == "male", 1, NA))

#Pairwise deletion
Pairwise_Sex <- cor(timd4$Sex2, timd4$Survived, use = "pairwise.complete.obs")

# Funcion MMM -------------------------------------------------------------
Imputacion_MMM <- function(df, columna){
  df <- df
  if(columna == 13){
    df$media <- NA
    df$mediana <- NA
    df$moda <- NA
    df$media <- ifelse(is.na(df$Sex2), round(mean(df$Sex2, na.rm = TRUE),0), df$Sex2)
    df$mediana <- ifelse(is.na(df$Sex2), round(median(df$Sex2, na.rm = TRUE),0), df$Sex2)
    df$moda <- ifelse(is.na(df$Sex2), round(mfv1(df$Sex2, na_rm = TRUE),0), df$Sex2)
    MMM <- cbind.data.frame(df$media, df$mediana, df$moda)
  }
  else if(columna == 6){
    df$media <- NA
    df$mediana <- NA
    df$moda <- NA
    df$media <- ifelse(is.na(df$Age), round(mean(df$Age, na.rm = TRUE),0), df$Age)
    df$mediana <- ifelse(is.na(df$Age), round(median(df$Age, na.rm = TRUE),0), df$Age)
    df$moda <- ifelse(is.na(df$Age), round(mfv1(df$Age, na_rm = TRUE),0), df$Age)
    MMM <- cbind.data.frame(df$media, df$mediana, df$moda)
  }
  else if(columna == 7){
    df$media <- NA
    df$mediana <- NA
    df$moda <- NA
    df$media <- ifelse(is.na(df$SibSp), round(mean(df$SibSp, na.rm = TRUE),0), df$SibSp)
    df$mediana <- ifelse(is.na(df$SibSp), round(median(df$SibSp, na.rm = TRUE),0), df$SibSp)
    df$moda <- ifelse(is.na(df$SibSp), round(mfv1(df$SibSp, na_rm = TRUE),0), df$SibSp)
    MMM <- cbind.data.frame(df$media, df$mediana, df$moda)
  }
  else if(columna == 8){
    df$media <- NA
    df$mediana <- NA
    df$moda <- NA
    df$media <- ifelse(is.na(df$Parch), round(mean(df$Parch, na.rm = TRUE),0), df$Parch)
    df$mediana <- ifelse(is.na(df$Parch), round(median(df$Parch, na.rm = TRUE),0), df$Parch)
    df$moda <- ifelse(is.na(df$Parch), round(mfv1(df$Parch, na_rm = TRUE),0), df$Parch)
    MMM <- cbind.data.frame(df$media, df$mediana, df$moda)
  }
  else if(columna == 10){
    df$media <- NA
    df$mediana <- NA
    df$moda <- NA
    df$media <- ifelse(is.na(df$Fare), round(mean(df$Fare, na.rm = TRUE),0), df$Fare)
    df$mediana <- ifelse(is.na(df$Fare), round(median(df$Fare, na.rm = TRUE),0), df$Fare)
    df$moda <- ifelse(is.na(df$Fare), round(mfv1(df$Fare, na_rm = TRUE),0), df$Fare)
    MMM <- cbind.data.frame(df$media, df$mediana, df$moda)
  }
  else if(columna == 12){
    df$media <- NA
    df$mediana <- NA
    df$moda <- NA
    df$media <- ifelse(is.na(df$Embarked), round(mean(df$Embarked, na.rm = TRUE),0), df$Embarked)
    df$mediana <- ifelse(is.na(df$Embarked), round(median(df$Embarked, na.rm = TRUE),0), df$Embarked)
    df$moda <- ifelse(is.na(df$Embarked), round(mfv1(df$Embarked, na_rm = TRUE),0), df$Embarked)
    MMM <- cbind.data.frame(df$media, df$mediana, df$moda)
  }
  return(as.data.frame(MMM))
}


Imputacion <- Imputacion_MMM(timd4, 6)

#Imputacion Sectorizada

#Regresion lineal
#No se podra realizar la regresion lineal debido a que la columna es de tipo caracter 
#el cual no tendra una correlacion real con alguna columna numerica

#Eliminación de outliers: Standard deviation approach
#No se podra realizar la regresion lineal debido a que la columna es de tipo caracter ó 0 y 1

#Eliminación de outliers: Percentile approach
#No se podra realizar la regresion lineal debido a que la columna es de tipo caracter ó 0 y 1

#Utilizaría la de filling values ya que en la columna de name están las abreviaviones como: Mr, Mrs, Miss, Master, Sir,
#Mme, Dr, Major, Countess
timd2$Sex <- ifelse(str_detect(timd2$Name, "Mrs|Miss|Mme|Mlle|Countess"), "female", "male")


# Columna Age -------------------------------------------------------------

#Pairwise deletion

Pairwise_Age <- cor(timd4$Age, timd4$Pclass, use = "pairwise.complete.obs")

#Imputación general (media, moda y mediana)

Age_Imputacion <- Imputacion_MMM(timd4, 7)

#Imputación sectorizada



#Modelo de regresión lineal simple

a <- lm(Age ~ Pclass, data = timd4)
a$coefficients[1]
a$coefficients[2]
Intercepto_Age <- 47.58
Slope_Age <- -10.38

Regresion_Age <- as.data.frame(ifelse(is.na(timd4$Age), (Intercepto_Age + Slope_Age*timd4$Pclass), timd4$Age))

#Eliminación de outliers: Standard deviation approach
  
x <- 3
Lower_Age <- mean(timd4$Age, na.rm = TRUE) - (sd(timd4$Age, na.rm = TRUE)*x)
Upper_Age <- mean(timd4$Age, na.rm = TRUE) + (sd(timd4$Age, na.rm = TRUE)*x)
  
SdApproach_Age <- timd4[(!is.na(timd4$Age)) & (timd4$Age >= Lower_Age) & (timd4$Age <= Upper_Age),]

#Eliminación de outliers: Percentile approach
PLower_Age <- quantile(timd4$Age, na.rm = TRUE, probs = 0.1)
PUpper_Age <- quantile(timd4$Age, na.rm = TRUE, probs = 0.9)

PApproach_Age <- timd4[(!is.na(timd4$Age)) & (timd4$Age >= PLower_Age) & (timd4$Age <= PUpper_Age),]

g <- timd2 %>% filter(!is.na(timd2$Age)) %>% group_by(Age) %>% summarise(freq = n(), .groups = 'drop')
max(g$freq)
timd2$Age2 <- ifelse(is.na(timd2$Age), max(g$freq), timd2$Age)


# Funcion Imp Sec ---------------------------------------------------------

Imputacion_Sectorizada <- function(df, columna){
  if(columna == 6){
    df$IS <- NA
    g <- df %>% filter(!is.na(df$Age)) %>% group_by(Age) %>% summarise(freq = n(), .groups = 'drop')
    df$IS <- ifelse(is.na(df$Age), max(g$freq), df$Age)
    ISM <- df$IS
  }
  else if(columna == 7){
    df$IS <- NA
    g <- df %>% filter(!is.na(df$SibSp)) %>% group_by(SibSp) %>% summarise(freq = n(), .groups = 'drop')
    df$IS <- ifelse(is.na(df$SibSp), max(g$freq), df$SibSp)
    ISM <- df$IS
  }
  else if(columna == 8){
    df$IS <- NA
    g <- df %>% filter(!is.na(df$Parch)) %>% group_by(Parch) %>% summarise(freq = n(), .groups = 'drop')
    df$IS <- ifelse(is.na(df$Parch), max(g$freq), df$Parch)
    ISM <- df$IS
  }
  else if(columna == 10){
    df$IS <- NA
    g <- df %>% filter(!is.na(df$Fare)) %>% group_by(Fare) %>% summarise(freq = n(), .groups = 'drop')
    df$IS <- ifelse(is.na(df$Fare), max(g$freq), df$Fare)
    ISM <- df$IS
  }
  return(as.data.frame(ISM))
}


# Funcion RL --------------------------------------------------------------

RegresionLineal <- function(df, columna, columna2){
  if(columna == 6 & columna2 == 2){
    df$RLineal <- NA
    a <- lm(Age ~ Survived, df)
    df$RLineal <- ifelse(is.na(df$Age), round((a$coefficients[1] + a$coefficients[2]*df$Survived),0), df$Age)
    RL <- df$RLineal
  }
  else if(columna == 6 & columna2 == 3){
    df$RLineal <- NA
    a <- lm(Age ~ Pclass, df)
    df$RLineal <- ifelse(is.na(df$Age), round((a$coefficients[1] + a$coefficients[2]*df$Pclass),0), df$Age)
    RL <- df$RLineal
  }
  else if(columna == 7 & columna2 == 2){
    df$RLineal <- NA
    a <- lm(SibSp ~ Survived, df)
    df$RLineal <- ifelse(is.na(df$SibSp), round((a$coefficients[1] + a$coefficients[2]*df$Survived),0), df$SibSp)
    RL <- df$RLineal
  }
  else if(columna == 7 & columna2 == 3){
    df$RLineal <- NA
    a <- lm(SibSp ~ Pclass, df)
    df$RLineal <- ifelse(is.na(df$SibSp), round((a$coefficients[1] + a$coefficients[2]*df$Pclass),0), df$SibSp)
    RL <- df$RLineal
  }
  else if(columna == 8 & columna2 == 2){
    df$RLineal <- NA
    a <- lm(Parch ~ Survived, df)
    df$RLineal <- ifelse(is.na(df$Parch), round((a$coefficients[1] + a$coefficients[2]*df$Survived),0), df$Parch)
    RL <- df$RLineal
  }
  else if(columna == 8 & columna2 == 3){
    df$RLineal <- NA
    a <- lm(Parch ~ Pclass, df)
    df$RLineal <- ifelse(is.na(df$Parch), round((a$coefficients[1] + a$coefficients[2]*df$Pclass),0), df$Parch)
    RL <- df$RLineal
  }
  else if(columna == 10 & columna2 == 2){
    df$RLineal <- NA
    a <- lm(Fare ~ Survived, df)
    df$RLineal <- ifelse(is.na(df$Fare), round((a$coefficients[1] + a$coefficients[2]*df$Survived),0), df$Fare)
    RL <- df$RLineal
  }
  else if(columna == 10 & columna2 == 3){
    df$RLineal <- NA
    x <- 3
    df$RLineal <- ifelse(is.na(df$Fare), round((a$coefficients[1] + a$coefficients[2]*df$Pclass),0), df$Fare)
    RL <- df$RLineal
  }
  return(as.data.frame(RL))
}


# Funcion Sd Approach -----------------------------------------------------

StandardDev_Approach <- function(df, columna){
  if(columna == 6){
    x <- 3
    Lower <- mean(df$Age, na.rm = TRUE) - (sd(df$Age, na.rm = TRUE)*x)
    Upper <- mean(df$Age, na.rm = TRUE) + (sd(df$Age, na.rm = TRUE)*x)
    Sd <- df[(!is.na(df$Age)) & (df$Age >= Lower) & (df$Age <= Upper),]
    SDA <- Sd$Age
  }
  else if(columna == 7){
    x <- 3
    Lower <- mean(df$SibSb, na.rm = TRUE) - (sd(df$SibSb, na.rm = TRUE)*x)
    Upper <- mean(df$SibSb, na.rm = TRUE) + (sd(df$SibSb, na.rm = TRUE)*x)
    Sd <- df[(!is.na(df$SibSb)) & (df$SibSb >= Lower) & (df$SibSb <= Upper),]
    SDA <- Sd$SibSb
  }
  else if(columna == 8){
    x <- 3
    Lower <- mean(df$Parch, na.rm = TRUE) - (sd(df$Parch, na.rm = TRUE)*x)
    Upper <- mean(df$Parch, na.rm = TRUE) + (sd(df$Parch, na.rm = TRUE)*x)
    Sd <- df[(!is.na(df$Parch)) & (df$Parch >= Lower) & (df$Parch <= Upper),]
    SDA <- Sd$Parch
  }
  else if(columna == 10){
    x <- 3
    Lower <- mean(df$Fare, na.rm = TRUE) - (sd(df$Fare, na.rm = TRUE)*x)
    Upper <- mean(df$Fare, na.rm = TRUE) + (sd(df$Fare, na.rm = TRUE)*x)
    Sd <- df[(!is.na(df$Fare)) & (df$Fare >= Lower) & (df$Fare <= Upper),]
    SDA <- Sd$Fare
  }
  return(as.data.frame(SDA))
}


# Funcion Percentile Approach ---------------------------------------------

Percentile_Approach <- function(df, columna){
  if(columna == 6){
    Lower <- quantile(df$Age, na.rm = TRUE, probs = 0.1)
    Upper <- quantile(df$Age, na.rm = TRUE, probs = 0.9)
    Pa <- df[(!is.na(df$Age)) & (df$Age >= Lower) & (df$Age <= Upper),]
    PA <- Pa$Age
  }
  else if(columna == 7){
    Lower <- quantile(df$SibSp, na.rm = TRUE, probs = 0.1)
    Upper <- quantile(df$SibSp, na.rm = TRUE, probs = 0.9)
    Pa <- df[(!is.na(df$SibSp)) & (df$SibSp >= Lower) & (df$SibSp <= Upper),]
    PA <- Pa$SibSp
  }
  else if(columna == 8){
    Lower <- quantile(df$Parch, na.rm = TRUE, probs = 0.1)
    Upper <- quantile(df$Parch, na.rm = TRUE, probs = 0.9)
    Pa <- df[(!is.na(df$Parch)) & (df$Parch >= Lower) & (df$Parch <= Upper),]
    PA <- Pa$Parch
  }
  else if(columna == 10){
    Lower <- quantile(df$Fare, na.rm = TRUE, probs = 0.1)
    Upper <- quantile(df$Fare, na.rm = TRUE, probs = 0.9)
    Pa <- df[(!is.na(df$Fare)) & (df$Fare >= Lower) & (df$Fare <= Upper),]
    PA <- Pa$Fare
  }
  return(as.data.frame(PA))
}

a <- as.numeric(timd[1,1])

embarked <- timd %>% group_by(Embarked) %>% count()


# Parte 2 -----------------------------------------------------------------


# Standarization ----------------------------------------------------------

ZValue <- function(df, columna){
 if(columna == 6){
   Z <- df %>% mutate(Z_Age = (Age - mean(Age, na.rm = TRUE))/sd(Age, na.rm = TRUE))
   Z <- Z %>% select(Z_Age)
   return(as.data.frame(Z))
 }
 
 else if(columna == 10){
   Z <- df %>% mutate(Z_Fare = (Fare - mean(Fare, na.rm = TRUE))/sd(Fare, na.rm = TRUE))
   Z <- Z %>% select(Z_Fare)
   return(as.data.frame(Z))
 }
}

MinMax <- function(df, columna){
  if(columna == 6){
    MM <- df %>% mutate(MM_Age = (Age - min(Age, na.rm = TRUE))/(max(Age, na.rm = TRUE)-min(Age, na.rm = TRUE)))
    MM <- MM %>% select(MM_Age)
    return(as.data.frame(MM))
  }
  
  else if(columna == 10){
    MM <- df %>% mutate(MM_Fare = (Fare - min(Fare, na.rm = TRUE))/(max(Fare, na.rm = TRUE)-min(Fare, na.rm = TRUE)))
    MM <- MM %>% select(MM_Fare)
    return(as.data.frame(MM))
  }
}

MaxAbs <- function(df, columna){
  if(columna == 6){
    Max <- max(df$Age, na.rm = TRUE)
    Min <- min(df$Age, na.rm = TRUE)
    MA <- df %>% mutate(MA_Age = (Age - mean(c(max(Age,na.rm = TRUE),min(Age,na.rm = TRUE)))) / (max(Age,na.rm = TRUE) - mean(c(max(Age,na.rm = TRUE),min(Age,na.rm = TRUE)))))
    MMA<- MA %>% select(MA_Age)
    return(as.data.frame(MMA))
  }
  
  if(columna == 10){
    Max <- max(df$Fare, na.rm = TRUE)
    Min <- min(df$Fare, na.rm = TRUE)
    MA <- df %>% mutate(MA_Fare = (Fare - mean(c(max(Fare,na.rm = TRUE),min(Fare,na.rm = TRUE)))) / (max(Fare,na.rm = TRUE) - mean(c(max(Fare,na.rm = TRUE),min(Fare,na.rm = TRUE)))))
    MMA<- MA %>% select(MA_Fare)
    return(as.data.frame(MMA))
  }
}

