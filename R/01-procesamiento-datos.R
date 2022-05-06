# Code 1:Preparacion ------------------------------------------------------
## Valentina Andrade

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(tidyverse, sjPlot, sjmisc)
# 2. Cargar datos ---------------------------------------------------------
ene2021 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2021/stata/ene-2021-11-ond.dta?sfvrsn=fe87a57d_8&download=true")              
ene2019 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2019/stata/ene-2019-11-ond.dta?sfvrsn=3d70644f_18&download=true")
# 3. Explorar -------------------------------------------------------------
find_var(ene2021, "principal")
find_var(ene2019, "actividad")

## Por cada variable
descr(ene2021$edad)

# 4. Subset datos: filtrar y seleccionar ------------------------------------------------
## Explorar
ene2021 %>%
  filter(edad>= 15) %>%  ## Personas no pertenecientes a PEA (menores de 15 años)
  select(cae_especifico, activ, starts_with("c2_1"), sexo, cine,edad)

ene2019 %>% 
  filter(edad>= 15) %>% 
  select(cae_especifico, activ, starts_with("c2_1"), sexo, cine,edad)

## Guardar objeto
ene2021 <- ene2021 %>%
  filter(edad>= 15) %>%  ## Personas no pertenecientes a PEA (menores de 15 años)
  select(cae_especifico, activ, starts_with("c2_1"), sexo, cine,edad)

ene2019 <- ene2019 %>% 
  filter(edad>= 15) %>% 
  select(cae_especifico, activ, starts_with("c2_1"), sexo, cine,edad)

## Recomendacion: explorar datos para ver como quedan despues de crear objeto

# 5. Transformar datos -------------------------------------------------------

# A. Recodificar sociodemograficas -------------------------------------------
## Explorar
frq(ene2021$sexo) # Es como la solicitada
frq(ene2019$sexo) 

frq(ene2021$cine) # Hay que colapsar (y que los 999 queden como NA)
frq(ene2019$cine)  

## ENE2021
ene2021 %>% 
  mutate(edad_tramo = case_when(edad >= 15 & edad <=39 ~ "15 a 39 años",
                                edad >= 40 & edad <=  64 ~ "40 a 64 años",
                                edad >= 65 ~ "65+",
                                TRUE ~ NA_character_),
         educ_tramo = ifelse(cine %in% c(1,2,3), "Basica o menos",
         ifelse(cine %in% c(4,5,6), "Media y basica",
         ifelse(cine %in% c(7,8,9), "Superior", NA_character_))))  %>% # Con esto los 999 se van a NA
  select(edad, edad_tramo,cine, educ_tramo) # Con esto exploro

## ENE2019
### Mismo procedimiento pues vimos con frq que eran similares
ene2019 %>% 
  mutate(edad_tramo = case_when(edad >= 15 & edad <=39 ~ "15 a 39 años",
                                edad >= 40 & edad <=  64 ~ "40 a 64 años",
                                edad >= 65 ~ "65+",
                                TRUE ~ NA_character_),
         educ_tramo = ifelse(cine %in% c(1,2,3), "Basica o menos",
                             ifelse(cine %in% c(4,5,6), "Media y basica",
                                    ifelse(cine %in% c(7,8,9), "Superior", NA_character_))))  %>% # Con esto los 999 se van a NA
  select(edad, edad_tramo, cine, educ_tramo) # Con esto exploro

### Guardar objetos cuandoe estoy lista
ene2021 <-
  ene2021 %>% 
  mutate(edad_tramo = case_when(edad >= 15 & edad <=39 ~ "15 a 39 años",
                                edad >= 40 & edad <=  64 ~ "40 a 64 años",
                                edad >= 65 ~ "65+",
                                TRUE ~ NA_character_),
         educ_tramo = ifelse(cine %in% c(1,2,3), "Basica o menos",
                             ifelse(cine %in% c(4,5,6), "Media y basica",
                                    ifelse(cine %in% c(7,8,9), "Superior", NA_character_)))) # Con esto los 999 se van a NA
ene2019 <- ene2019 %>% 
  mutate(edad_tramo = case_when(edad >= 15 & edad <=39 ~ "15 a 39 años",
                                edad >= 40 & edad <=  64 ~ "40 a 64 años",
                                edad >= 65 ~ "65+",
                                TRUE ~ NA_character_),
         educ_tramo = ifelse(cine %in% c(1,2,3), "Basica o menos",
                             ifelse(cine %in% c(4,5,6), "Media y basica",
                                    ifelse(cine %in% c(7,8,9), "Superior", NA_character_)))) # Con esto los 999 se van a NA

## Tengo dos variables nuevas: edad_tramo y educ_tramo

## B. Recodificar sustantivas -----------------------------------------------

# CAE ---------------------------------------------------------------------
# Mirar manual metodológico para ver como colapsan
# Notar que 0 (menores de 15) no son parte de la PET (entonces no se calculan como parte de la PEA)


# ENE2021-CAE ------------------------------------------------------------
## Explorar
frq(ene2021$cae_especifico)
frq(ene2021$activ)

ene2021 %>%
  mutate(cae_corregido = ifelse(cae_especifico %in% c(10:28), "Fuera de la fuerza de trabajo",
                                ifelse(cae_especifico %in% c(1:7), "Ocupado",
                                       ifelse(cae_especifico %in% c(8:9), "Desocupado", NA_character_)))) %>%  
  select(activ, cae_corregido, cae_especifico) # Hago esto para ver como queda

# Check: está bien, creo objeto pero sin select
ene2021 <-
  ene2021 %>%
  mutate(cae_corregido = ifelse(cae_especifico %in% c(10:28), "Fuera de la fuerza de trabajo",
                                ifelse(cae_especifico %in% c(1:7), "Ocupado",
                                       ifelse(cae_especifico %in% c(8:9), "Desocupado", NA_character_))))

# ENE2019-CAE ------------------------------------------------------------
## Explorar
frq(ene2019$cae_especifico)
frq(ene2019$activ)

ene2019 %>%
  mutate(cae_corregido = ifelse(cae_especifico %in% c(10:28), "Fuera de la fuerza de trabajo",
                                ifelse(cae_especifico %in% c(1:7), "Ocupado",
                                       ifelse(cae_especifico %in% c(8:9), "Desocupado", NA_character_)))) %>%  
  select(activ, cae_corregido, cae_especifico) # Hago esto para ver como queda

# Check: está bien, creo objeto pero sin select
ene2019 <-
  ene2019 %>%
  mutate(cae_corregido = ifelse(cae_especifico %in% c(10:28), "Fuera de la fuerza de trabajo",
                                ifelse(cae_especifico %in% c(1:7), "Ocupado",
                                       ifelse(cae_especifico %in% c(8:9), "Desocupado", NA_character_))))

# Nuevas variables cae_corregido

# Horas trabajo -----------------------------------------------------------

# ENE2021-horastrabajo ----------------------------------------------------
descr(ene2021$c2_1_1)
frq(ene2021$c2_1_1) # No sabe (888) y No responde (999)

##Hay que hacer dos cosas: recodificar NA y hacer índice
## 1.Recodificar NA
ene2021 %>%
  mutate_at(vars(starts_with("c2")), funs(car::recode(., recodes = c("c(888,999)=NA")))) %>% 
    frq(.$c2_1_1) # Para comprobar puedo ir cambiando los c2
  
## 2. Indice sumativo
ene2021 %>%
  mutate_at(vars(starts_with("c2")), funs(car::recode(., recodes = c("c(888,999)=NA")))) %>%
  rowwise() %>% 
  mutate(indice_c2_1 = sum(c2_1_1, c2_1_2, c2_1_3, na.rm = T)) %>%
  select(contains("c2_1")) #para comprobar como quedí

## Guardar
ene2021 <- ene2021 %>%
  mutate_at(vars(starts_with("c2")), funs(car::recode(., recodes = c("c(888,999)=NA")))) %>%
  rowwise() %>% 
  mutate(indice_c2_1 = sum(c2_1_1, c2_1_2, c2_1_3, na.rm = T)) %>% 
  ungroup() #desagrupar
 
# Ojo: hay que evitar que cuando sume los na cancelen las sumas de horas  

# ENE2019-horastrabajo ----------------------------------------------------
descr(ene2019$c2_1_1)
frq(ene2019$c2_1_1) # No sabe (999) (distinto a datos anteriores)

##Hay que hacer dos cosas: recodificar NA y hacer índice
## 1.Recodificar NA
ene2019 %>%
  mutate_at(vars(starts_with("c2")), funs(car::recode(., recodes = c("999=NA")))) %>% 
  frq(.$c2_1_1) # Para comprobar puedo ir cambiando los c2

## 2. Indice sumativo
ene2019 %>%
  mutate_at(vars(starts_with("c2")), funs(car::recode(., recodes = c("999=NA")))) %>%
  rowwise() %>% 
  mutate(indice_c2_1 = sum(c2_1_1, c2_1_2, c2_1_3, na.rm = T)) %>%
  select(contains("c2_1")) #para comprobar como quedó

## Guardar
ene2019 <- ene2019 %>%
  mutate_at(vars(starts_with("c2")), funs(car::recode(., recodes = c("999=NA")))) %>%
  rowwise() %>% 
  mutate(indice_c2_1 = sum(c2_1_1, c2_1_2, c2_1_3, na.rm = T)) %>% 
  ungroup() 


# 6. Unir datos -----------------------------------------------------------
## Para distinguir las bases crearé una variable año que será mi key
ene2019 <- mutate(ene2019, ano = 2019)
ene2021 <- mutate(ene2021, ano = 2021)

# Explorar union
merge(ene2019, ene2021, all = T)

# Guardar
ene_unida <- merge(ene2019, ene2021, all = T)

# También se puede hacer con row_bind 

# Explorar datos finales (ver dimensiones y clases)
# 7. Guardar --------------------------------------------------------------
save(ene2019, ene2021, ene_unida, file = "output/data/datos_proc.RData")



