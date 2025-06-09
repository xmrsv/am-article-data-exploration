# ============================================================================
# EDA COMPLETO - FACTORES DE ÉXITO EN PROYECTOS AGILE
# ============================================================================

# Cargar librerías necesarias
library(readxl)
library(PerformanceAnalytics) 
library(corrplot)
library(psych)
#library(VIM)
#library(e1071)


# ========================== EDA 1: EXPLORACIÓN INICIAL ==========================

# Cargar datos
agile_data <- read_excel("./Descargas/Agile_Projects_Dataset.xlsx")

# 1.1 Visualización inicial
head(agile_data, 5)
tail(agile_data, 3)

# 1.2 Dimensiones y estructura
dim(agile_data)
str(agile_data)
names(agile_data)

# 1.3 Estadísticos descriptivos completos
summary(agile_data)
describe(agile_data)  # psych package - más detallado


# ========================== EDA 2: AJUSTE DE VARIABLES ==========================

# 2.1 Verificar nombres (ya están bien en tu dataset)
names(agile_data)

# 2.2 Verificar tipos de datos
sapply(agile_data, class)

# 2.3 Convertir Project_Success a factor si es necesario
agile_data$Project_Success <- as.factor(agile_data$Project_Success)


# ========================== EDA 3: DATOS AUSENTES ==========================

# 3.1 Verificar datos ausentes
sum(is.na(agile_data))
colSums(is.na(agile_data))

# 3.2 Patrón de datos ausentes (si los hay)
# aggr(agile_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE)

# ========================== EDA 4: OUTLIERS MULTIVARIADOS ==========================

# 4.1 Seleccionar solo variables predictoras
predictoras <- agile_data[,1:5]  # Las 5 primeras columnas

# 4.2 Método básico: Boxplots (tu método)
boxplot(predictoras, 
        main="Detección de Outliers - Variables Predictoras",
        names=c("Agile", "Risk", "Management", "Supply", "Time"),
        las=2)

# 4.3 Método avanzado: Distancia de Mahalanobis (de la guía)
# Calcular distancia de Mahalanobis
maha_dist <- mahalanobis(predictoras, 
                         colMeans(predictoras), 
                         cov(predictoras))

# Punto de corte (distribución chi-cuadrado)
cutoff <- qchisq(0.975, df = ncol(predictoras))

# Identificar outliers
outliers <- which(maha_dist > cutoff)
print(paste("Outliers detectados con Mahalanobis:", length(outliers)))
print(paste("Casos outliers:", paste(outliers, collapse=", ")))

# 4.4 Visualizar outliers
plot(maha_dist, 
     main="Distancia de Mahalanobis",
     ylab="Distancia", 
     xlab="Observación")
abline(h=cutoff, col="red", lwd=2)


# ========================== EDA 5: CORRELACIONES ==========================

# 5.1 Matriz de correlación básica
cor_matrix <- cor(predictoras)
print(round(cor_matrix, 3))

# 5.2 Correlaciones con significancia
chart.Correlation(predictoras, 
                  histogram=TRUE, 
                  pch=19,
                  main="Matriz de Correlaciones - Variables Predictoras")

# 5.3 Visualización corrplot
corrplot(cor_matrix, 
         method="color",
         type="upper", 
         order="hclust",
         tl.cex=0.8, 
         tl.col="black",
         main="Correlaciones entre Factores de Éxito")

# 5.4 Correlaciones con variables de resultado
cor_with_success <- cor(predictoras, as.numeric(agile_data$Project_Success))
cor_with_cost <- cor(predictoras, agile_data$Cost_Savings)

print("Correlaciones con Éxito del Proyecto:")
print(round(cor_with_success, 3))
print("Correlaciones con Ahorro de Costos:")
print(round(cor_with_cost, 3))

