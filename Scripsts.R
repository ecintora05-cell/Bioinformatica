#Descargar datos directamente de internet
#Llamar a las librerias
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
#Se copian los URL de las tablas que se encuentran en internet
url_tinto  <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
url_blanco <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

vino_tinto  <- read_delim(url_tinto,  delim = ";", show_col_types = FALSE)
vino_blanco <- read_delim(url_blanco, delim = ";", show_col_types = FALSE)

# Añadimos columna "tipo" para identificarlos al unirlos
vino_tinto  <- mutate(vino_tinto,  tipo = "tinto")
vino_blanco <- mutate(vino_blanco, tipo = "blanco")

# bind_rows() apila las dos tablas (como pegar una encima de la otra)
vinos <- bind_rows(vino_tinto, vino_blanco)

glimpse(vinos)
#Leer los datos crudos de una computadora
salud <- read_csv("01_RawData/dataset_categorical_NA.csv", show_col_types = FALSE)

glimpse(salud)
# %>% #Control shift M para tener la pipa 
vinos |>
  filter(tipo == "tinto") |>
  filter(quality >= 7) |>
  select(tipo, quality, alcohol) |>
  arrange(desc(alcohol)) |>
  head(8)
salud |>
  filter(SmokingStatus == "Fuma") |>
  select(ID, Age, BMI,
         Cholesterol) |>
  head(8)
vinos %>% filter(quality<= 9)
#Para poner dos condiciones se cumplen a la vez se utiliza &
salud %>% filter(BMI>20&BMI<30)
#Para poner que una condiciión u otra condición se cumpla se pone |
#Fumadores, colesterol mayor 100
View(salud)
salud %>% filter(SmokingStatus=="Fuma",MaritalStatus=="Divorciada")
#Para seleccionar columnas de interes se le pone select
salud %>% filter(SmokingStatus=="Fuma"|MaritalStatus=="Casada") %>% 
  select(Glucose,Cholesterol,Age) %>% head(8)
# Vinos de calidad 8 o 9
filter(vinos, quality %in% c(8, 9)) |>
  select(tipo, quality, alcohol) |>
  head(8)
#Si quieres rangos especificos se puede usar between, se incorpora los extremos 
filter(vinos, between(pH, 2.0, 3.8)) |>
  select(tipo, pH, quality) |>
  head(6)
#Manejar los NA con filtros, para saber si algo es NA se usa is.na
filter(salud,is.na(Age)|Age < 30) %>% select(ID,Age,SmokingStatus)
# Participantes urbanos, con empleo de tiempo completo y estrés alto
salud |>
  filter(ResidenceType == "Urbano") |>
  filter(EmploymentStatus == "Tiempo completo") |>
  filter(StressLevel > 40) |>
  select(ID, Age, BMI, SmokingStatus, StressLevel) |>
  head(10)
salud %>% filter(is.na(SmokingStatus)) %>% count()
#Ejercicios
#Ejercicio 1 ¿Cuántos vinos blancos tienen una calidad de 5 o menos?
vinos %>% filter(tipo=="Blanco",quality<=5) %>% count() 
#Ejercicio 2 Filtra los vinos tintos que tengan alcohol mayor al promedio de todos los vinos.
#Pista: puedes usar mean(vinos$alcohol) como valor de referencia dentro del filter.
vinos %>% filter(alcohol<mean(vinos$alcohol))
#Ejercciio 3 ¿Cuántos participantes del dataset salud son ex-fumadores y residen en zona urbano
salud %>% filter(SmokingStatus=="Ex-fumadora",ResidenceType=="Urbano") %>% count()
#Ejercicio 4 Filtra los participantes con nivel de estrés mayor a 80. ¿Qué estatus de tabaquismo predomina entre ellos?
salud %>% filter(StressLevel>80) %>% count(SmokingStatus,sort = T)
#La función contains
#Ejercicios
#Ejercicio 1 Del dataset vinos, selecciona solo las columnas de texto (tipo character).
vinos %>% select(where(is.character)) %>% head(8)
#Ejercicio 2  Del dataset vinos, selecciona todas las columnas cuyo nombre contenga la palabra "acid".
vinos %>% select(contains("acid"))
#Ejercicio 3 Del dataset salud, selecciona ID, Age y todas las columnas que contienen medidas de circunferencia. Renombra WaistCircumference como cintura.
salud %>% select(ID,Age,contains("Circum"),cintura=WaistCircumference) %>% head(4)
#Ejercicio 4 Filtra solo los participantes del dataset salud que residan en zona rural y luego selecciona únicamente ID, Age, BMI y SmokingStatus.
salud %>% filter(ResidenceType=="Rural") %>% select(ID,Age,BMI,SmokingStatus) %>% head(5)
#Ejercicios
#Ejercicio 1 1. ¿Cuáles son los 10 vinos con mayor pH? Muestra también su tipo y calidad.
View(vinos)
vinos %>% select(pH,quality,tipo) %>% arrange(desc(pH)) %>% head(10)
#Ejercicio 2  ¿Qué participante del dataset salud tiene el nivel de glucosa más alto? Muestra el top 5 con su ID, edad y estatus de tabaquismo
salud %>% arrange(desc(Glucose)) %>% select(ID,Age,SmokingStatus) %>% head(5)
#Ejercicio 3 Ordena los vinos blancos primero por calidad descendente y luego por acidez volátil (volatile acidity) ascendente. Muestra las primeras 10 filas.
vinos %>% filter(tipo=="blanco") %>% arrange(desc(quality),`volatile acidity`) %>% head(5)
#Ejercicios sección 6
#Ejercicio 1. Crea una nueva columna en vinos que se llame nivel_alcohol que clasifique así: - Menos de 10%: "Bajo" · Entre 10% y 12%: "Medio" · Más de 12%: "Alto"
#Pista: usa case_when().
vinos %>% mutate(nivel_alcohol = case_when(alcohol<10~"Bajo",alcohol<=12~"Medio",alcohol>12~"Alto")) %>%
  count(nivel_alcohol)
#Ejercicio 2. En salud, crea una columna grupo_edad que clasifique a los participantes en:
# - Menos de 30: "Joven" · Entre 30–50: "Adulto" · Más de 50: "Mayor"
#Recuerda manejar los NA como primera condición
salud %>% mutate(grupo_edad = case_when(is.na(Age)~"Sin datos",Age<30~"Joven",Age<=50~"Adulto",Age>50~"Mayor")) %>% 
  count(grupo_edad)
#Ejercicio 3. Crea una columna fumador_activo en salud que sea TRUE si el participante fuma actualmente (SmokingStatus == "Fuma"), FALSE en caso contrario.
salud %>% mutate(fumador_activo=case_when(SmokingStatus=="Fuma"~T,T~F)) %>% count(fumador_activo)
#Ejercicios sección 8
#Ejercicio 1. Calcula el promedio de alcohol, pH y calidad para cada combinación de tipo y categoria de vino.
vinos %>% group_by(tipo) %>% summarise(
  n = n(),
  promedio_alcohol = round(mean(alcohol,na.rm=T),1),
  promedio_calidad = round(mean(quality,na.rm=T),1),
  promedio_pH = round(mean(pH,na.rm=T),1),
  .groups = "drop"
) 
#Ejercicio 2. ¿Qué estado civil del dataset salud tiene el nivel de estrés más alto en promedio? Muestra el top 3.
salud %>%filter(!is.na(MaritalStatus)) %>% group_by(MaritalStatus) %>% 
  summarise(
    n=n(),
    estres_promedio = round(mean(StressLevel,na.rm=T),1),
    .groups = "drop"
  ) %>% slice_max(estres_promedio,n=3)
#Ejercicio 3. Calcula el promedio de glucosa y colesterol por estatus de tabaquismo en salud.
salud %>% filter(!is.na(SmokingStatus)) %>% 
group_by(SmokingStatus) %>% summarise(
  promedio_glucosa = round(mean(Glucose,na.rm=T),1),
  promedio_colesterol = round(mean(Cholesterol,na.rm=T),1),
  .groups = "drop"
)
#Ejercicio 4. Desafío: ¿En qué combinación de EducationLevel y ResidenceType se observa el mayor BMI promedio en salud? Muestra el top 5.
salud %>% filter(!is.na(EducationLevel),!is.na(ResidenceType)) %>% group_by(EducationLevel,ResidenceType) %>% 
  summarise(
  promedio_BMI = round(mean(BMI,na.rm=T),1),
  .groups = "drop"
  ) %>% slice_max(promedio_BMI,n=5)
#Ejercicios sección 9
#Ejercicio 1. Construye una tabla resumen_educacion con el BMI promedio y nivel de estrés promedio por EducationLevel. Luego usa left_join() para agregarla al dataset salud. ¿Cuántas filas tiene el resultado?
resumen_educacion<-salud %>% filter(!is.na(EducationLevel)) %>% group_by(EducationLevel) %>% summarise(
  BMI_promedio = round(mean(BMI,na.rm=T),1),
  estres_promedio = round(mean(StressLevel,na.rm=T),1),
  .groups = "drop"
) 
resul_join<-salud %>% left_join(resumen_educacion,by = "EducationLevel")
nrow(resul_join)
#Ejercicio 2. ¿Existen niveles educativos en resumen_educacion que no aparezcan en salud? Usa anti_join() para verificarlo.
anti_join(salud,resumen_educacion,by="EducationLevel")
#Ejercicio 3. Usando left_join(), combina salud con resumen_zona y luego muestra el BMI promedio por ResidenceType, comparándolo con el imc_promedio_zona calculado en el resumen.
resumen_zona<-salud %>% group_by(ResidenceType) %>% summarise(
  IMC_promedio_zona = round(mean(BMI,na.rm=T),1),
  colesterol_promedio_zona = round(mean(Cholesterol,na.rm=T),1),
  n_zona = n(),
  .groups = "drop"
)
salud %>% left_join(resumen_zona, by = "ResidenceType") %>% group_by(ResidenceType) %>% 
  summarise(
    media_IMC_real = round(mean(BMI,na.rm=T),1),
    IMC_zona = first(IMC_promedio_zona),
    .groups = "drop"
  )
#Ejercicios de la sección 13
#Usando los dos datasets (vinos y salud), responde las siguientes preguntas:
  
#Ejercicio A) ¿Cuál es el promedio de calidad de los vinos blancos de categoría “Alta” o “Excepcional”, agrupado por nivel de alcohol (nivel_alcohol)?
#Pista: primero crea la columna nivel_alcohol con mutate() y case_when().
vinos<-vinos %>% mutate(
  categoria = case_when(
    quality<=4 ~ "Baja",
    quality<=6 ~ "Media",
    quality<=8 ~ "Alta",
    T ~ "Excepcional")
)
vinos %>% mutate(nivel_alcohol = case_when(alcohol<10~"Bajo",alcohol<=12~"Medio",T~"Alto")) %>%
group_by(nivel_alcohol) %>% filter(categoria == "Alta"|categoria=="Excepcional") %>% 
  summarise(
  n = n(),
  promedio_calidad = round(mean(quality,na.rm=T),2),
  .groups = "drop"
)
#Ejercicio B) En el dataset salud, ¿qué 5 participantes tienen la mayor diferencia entre su BMI y el BMI promedio de su grupo educativo? 
#Muestra su ID, nivel educativo, BMI individual y la diferencia.
