# Code 1:Preparacion ------------------------------------------------------
## Valentina Andrade

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(tidyverse, sjPlot, sjmisc)
# 2. Cargar datos ---------------------------------------------------------
ene2021 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2021/stata/ene-2021-03-fma.dta?sfvrsn=d2acd9d6_11&amp;download=true")
ene2019 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2019/formato-stata/ene-2019-03.dta?sfvrsn=936ca6e9_8&amp;download=true")
# 3. Explorar -------------------------------------------------------------
find_var(ene2021, "principal")
find_var(ene2019, "actividad")

## Por cada variable
descr(ene2021$edad)

# 4. Subset datos: filtrar y seleccionar ------------------------------------------------
ene2021 %>%
  filter(edad>= 15) %>%  ## Personas no pertenecientes a PEA (menores de 15 años)
  select(cae_especifico, activ, starts_with("c2_1"), sexo, cine,edad)

ene2019 %>% 
  filter(edad>= 15) %>% 
  select(cae_especifico, activ, starts_with("c2_1"), sexo, cine,edad)

# 5. Transformar datos -------------------------------------------------------
## Recodificar sociodemográficos
frq(ene2021$sexo)
frq(ene2021$cine)
frq(ene2019$sexo)
frq(ene2019$cine)

### ENE2021
ene2021 %>% 
  mutate(edad_tramo = case_when(edad >= 18 & edad <=39 ~ "18 a 39 años",
                                edad >= 40 & edad <=  64 ~ "40 a 64 años",
                                edad >= 65 ~ "65+",
                                TRUE ~ NA_character_),
         educ_tramo = ifelse(cine %in% c(1,2,3), "Basica o menos",
         ifelse(cine %in% c(4,5,6), "Media y basica",
         ifelse(cine %in% c(7,8,9), "Superior", NA_character_)))) # Con esto los 999 se van a NA


ene2019 %>% 
  mutate(edad_tramo = case_when(edad >= 18 & edad <=39 ~ "18 a 39 años",
                                edad >= 40 & edad <=  64 ~ "40 a 64 años",
                                edad >= 65 ~ "65+",
                                TRUE ~ NA_character_),
         educ_tramo = ifelse(cine %in% c(1,2,3), "Basica o menos",
                             ifelse(cine %in% c(4,5,6), "Media y basica",
                                    ifelse(cine %in% c(7,8,9), "Superior", NA_character_)))) # Con esto los 999 se van a NA

## Recodificar sustantivas
frq(ene2021$cae_especifico)
frq(ene2021$activ)
descr(ene2021$c2_1_1)

ene2021 %>%
  mutate(cae_corregido = ifelse(cae_especifico %in% c(0, 10:28), "Fuera de la fuerza de trabajo",
                                ifelse(cae_especifico %in% c(1:7), "Ocupado",
                                       ifelse(cae_especifico %in% c(8:9), "Desocupado", NA_character_)))) %>%  #Hay un par de mal clasificados en activ
  select(cae_corregido,activ,  cae_general, cae_especifico)
  mutate_at(starts_with("c2"), funs(sum))

# 6. Unir datos -----------------------------------------------------------


# 7. Guardar --------------------------------------------------------------




