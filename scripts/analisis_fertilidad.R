# Título: Análisis de datos de fertilidad
# Autor: Ana Paula Solórzano
# Fecha: 29/06/2026

# Cargar librerías
library(readr)
library(ggplot2)
library(dplyr)

# Cargar los datos
fertilidad <- read.csv("data/fertility.csv")
head(fertilidad)


# --- Descripción de datos --- #
# Dimensiones del data frame
dim(fertilidad)

# Nombres de las columnas del dataframe
names(fertilidad)

# Tipos de variables en cada columna
str(fertilidad)


# --- Análisis de datos --- #
# ¿Existe una diferencia en la edad entre las personas con diagnóstico normal y alterado de fertilidad?
edad_fertilidad <- fertilidad %>%
  group_by(Age) %>%
  summarise(
    total = n(),
    normal = sum(Diagnosis == "Normal"),
    alterada = sum(Diagnosis == "Altered"),
    prop_infertilidad = alterada / total,
    porcentaje_normal = normal / total * 100
  ) %>%
  arrange(desc(fertilidad_normal))
edad_fertilidad

# Gráfica edad vs diagnóstico de fertilidad
ggplot(data = fertilidad,
       aes(x = Diagnosis,
           y = Age)) +
  geom_boxplot() +
  labs(title = "Relación entre edad y fertilidad",
       x = "Diagnosis de fertilidad",
       y = "Edad de los pacientes") +
  theme_minimal()



# ¿El hábito de fumar está asociado con el diagnóstico de fertilidad?
fumar_fertilidad <- fertilidad %>%
  group_by(Smoking.habit) %>%
  summarise(
    total = n(),
    normal = sum(Diagnosis == "Normal"),
    alterada = sum(Diagnosis == "Altered"),
    prop_infertilidad = alterada / total,
    porcentaje_normal = normal / total * 100
    ) %>%
  arrange(desc(porcentaje_normal))
fumar_fertilidad

# Gráfica frecuencia de fumar vs diagnóstico de fertilidad
ggplot(data = fertilidad, 
       aes(x = Smoking.habit, 
           fill = Diagnosis)) +
  geom_bar(position = "dodge") +
  labs(title = "Relación entre el hábito de fumar y fertilidad",
       x = "Hábito de fumar",
       y = "Conteo de pacientes") +
  theme_minimal()



# ¿La frecuencia de consumo de alcohol está asociado con el diagnóstico de fertilidad?
alcohol_fertilidad <- fertilidad %>%
  group_by(Frequency.of.alcohol.consumption) %>%
  summarise(
    total = n(),
    normal = sum(Diagnosis == "Normal"),
    alterada = sum(Diagnosis == "Altered"),
    prop_infertilidad = alterada / total,
    porcentaje_normal = normal / total * 100
    ) %>%
  arrange(desc(fertilidad_normal))
alcohol_fertilidad

# Gráfica frecuencia de consumo de alcohol vs diagnóstico de fertilidad
ggplot(fertilidad, 
       aes(x = Frequency.of.alcohol.consumption, 
           fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de diagnóstico según de alcohol",
       x = "Consumo de alcohol",
       y = "Proporción") +
  theme_minimal()



# ¿Las personas con antecedentes médicos (enfermedades infantiles, traumatismos o intervenciones quirúrgicas) presentan una mayor proporción de diagnósticos de infertilidad?
medica_fertilidad <- fertilidad %>%
  group_by(Childish.diseases, Accident.or.serious.trauma, Surgical.intervention) %>%
  summarise(
    total = n(),
    normal = sum(Diagnosis == "Normal"),
    infertilidad = sum(Diagnosis == "Altered" ),
    prop_infertilidad = infertilidad / total,
    porcentaje_normal = normal / total * 100)
medica_fertilidad

# Crear una nueva columna combinando los antescedentes médicos
fertilidad2 <- fertilidad %>%
  mutate(grupo_antecedentes = paste(
      Childish.diseases,
      Accident.or.serious.trauma,
      Surgical.intervention,
      sep = " | "), 
      Season = NULL, 
      Age = NULL, 
      High.fevers.in.the.last.year = NULL, 
      Frequency.of.alcohol.consumption = NULL, 
      Smoking.habit = NULL, 
      Number.of.hours.spent.sitting.per.day = NULL)
head(fertilidad2)

# Gráfica del efecto de los antescedentes médicos en la fertilidad
ggplot(fertilidad2, 
       aes(x = grupo_antecedentes,
           fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(x = "Antecedentes médicos (combinación)",
       y = "Proporción",
       fill = "Diagnóstico",
       title = "Relación entre antecedentes médicos y diagnóstico de fertilidad") +
  theme_minimal()



# ¿El tiempo sedentario diario cambia según estación y se asocia con el diagnóstico?
sedentario_fertilidad <- fertilidad %>%
  group_by(Season, Diagnosis) %>%
  summarise(
    total = n(),
    media_h_setadas = mean(Number.of.hours.spent.sitting.per.day, na.rm = TRUE),
    mediana_h_sentadas = median(Number.of.hours.spent.sitting.per.day, na.rm = TRUE))

# Gráfica estación del año vs horas de permanecer sentado por día
ggplot(fertilidad, 
       aes(x = Season,
           y = Number.of.hours.spent.sitting.per.day,
           fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Estación de año",
       y = "Horas de permanecer sentado por día",
       title = "Relación entre el tiempo de sedentarismo por estación y fertilidad") +
  theme_minimal() + 
  ylim(0, 20) # se puso un límite de 20 h para una mejor visualización de la gráfica
