

####Cargar Paquetes-------------------------------------------------------------------------------------------
pacman::p_load(sjPlot, 
               tidyverse, 
               srvyr,
               survey,
               haven)

####Cargar datos ----------------------------------------------------------------------------------------------

Base_de_datos_Full_VI_EME_extracto_ <- read_dta("input/Base de datos Full VI EME (extracto).dta")

datos_proc <- select(Base_de_datos_Full_VI_EME_extracto_,
                     Identificador_único_de_personas = Enc_rph,
                     Ganancias_mensual_de_los_microemprendedores_as = ganancia_final_mensual,
                     Registro_contable = conta_completa,
                     Registro_en_impuestos_internos = registro_SII, 
                     Clasificación_Internacional_de_la_situación_en_el_empleo  = CISE,
                     región_de_residencia_de_la_persona = region,
                     Factor_de_expansión_de_personas = Factor_EME)

####Recodificar------------------------------------------------------------------------------------------------
#conta_completa
datos_proc <- mutate(datos_proc,
                              Registro_contable = 
                                case_when(Registro_contable == 1~"Sí",
                                          Registro_contable == 2~"No"))
#registro_sii
datos_proc <-  mutate(datos_proc, Registro_en_impuestos_internos = 
                                       case_when(Registro_en_impuestos_internos == 1~"Sí",
                                                 Registro_en_impuestos_internos == 2~"No"))
#CISE
datos_proc <-  mutate(datos_proc, Clasificación_Internacional_de_la_situación_en_el_empleo =
         case_when(Clasificación_Internacional_de_la_situación_en_el_empleo == 0~"Cuenta Propia",
                   Clasificación_Internacional_de_la_situación_en_el_empleo == 1~"Empleador"))

#Región
datos_proc <- mutate(datos_proc, región_de_residencia_de_la_persona = 
                   case_when(región_de_residencia_de_la_persona == 1~"Región de Tarapacá",
                             región_de_residencia_de_la_persona == 2~"Región de Antofagasta",
                             región_de_residencia_de_la_persona == 3~"Región de Atacama",
                             región_de_residencia_de_la_persona == 4~"Región de Coquimbo",
                             región_de_residencia_de_la_persona == 5~"Región de Valparaiso",
                             región_de_residencia_de_la_persona == 6~"Región del Libertador Bernardo O´higgins",
                             región_de_residencia_de_la_persona == 7~"Región del Maule",
                             región_de_residencia_de_la_persona == 8~"Región del Biobío",
                             región_de_residencia_de_la_persona == 9~"Región de la Aracunía",
                             región_de_residencia_de_la_persona == 10~"Región de los Lagos",
                             región_de_residencia_de_la_persona == 11~"Región de Aysén del General Carlos Ibañez del Campo",
                             región_de_residencia_de_la_persona == 12~"Región de Magallanes y Antártica Chilena",
                             región_de_residencia_de_la_persona == 13~"Región Metropolitana de Santiago",
                             región_de_residencia_de_la_persona == 14~"Región de los Ríos",
                             región_de_residencia_de_la_persona == 15~"Región de Arica y Parinacota",
                             región_de_residencia_de_la_persona == 16~"Región de Ñuble"))

####Nueva Variable Macrozona-----------------------------------------------------------------------------------
#Tramo 1
datos_proc <- mutate(datos_proc, Macrozona =
         case_when(región_de_residencia_de_la_persona == "Región de Arica y Parinacota"~"Tramo 1",
                   región_de_residencia_de_la_persona == "Región de Tarapacá"~"Tramo 1",
                   región_de_residencia_de_la_persona == "Región de Antofagasta"~"Tramo 1",
                   región_de_residencia_de_la_persona == "Región de Atacama"~"Tramo 1",
                   región_de_residencia_de_la_persona == "Región de Coquimbo"~"Tramo 2",
                   región_de_residencia_de_la_persona == "Región de Valparaiso"~"Tramo 2",
                   región_de_residencia_de_la_persona == "Región del Libertador Bernardo O´higgins"~"Tramo 2",
                   región_de_residencia_de_la_persona == "Región del Maule"~"Tramo 2",
                   región_de_residencia_de_la_persona == "Región de Ñuble"~"Tramo 2",
                   región_de_residencia_de_la_persona == "Región del Biobío"~"Tramo 2",
                   región_de_residencia_de_la_persona == "Región de la Aracunía"~"Tramo 3",
                   región_de_residencia_de_la_persona == "Región de los Ríos"~"Tramo 3",
                   región_de_residencia_de_la_persona == "Región de los Lagos"~"Tramo 3",
                   región_de_residencia_de_la_persona == "Región de Aysén del General Carlos Ibañez del Campo"~"Tramo 4",
                   región_de_residencia_de_la_persona == "Región de Magallanes y Antártica Chilena"~"Tramo 4",
                   TRUE ~ NA_character_))

####Objeto encuesta--------------------------------------------------------------------------------------------
objeto_enc <- as_survey_design(datos_proc,
                 id= Identificador_único_de_personas,
                 weights = Factor_de_expansión_de_personas)

####Tabla hasta las 23.59------------------------------------------------------------------------------------------------------
#Este fue mi avance hasta las 23.59
Tabla_por_Macrozona <- objeto_enc %>% 
  group_by(Macrozona) %>%
  summarise(Clasificación_Internacional_de_la_situación_en_el_empleo = survey_prop(vartype = "ci", na.rm = T))
            
####Tabla completa despues de tiempo-------------------------------------------------------------------------------------------

as_factor()

#####

Tabla_por_Macrozona <- objeto_enc %>% 
  group_by(Macrozona) %>%
  summarise(Clasificación_Internacional_de_la_situación_en_el_empleo = sum(Clasificación_Internacional_de_la_situación_en_el_empleo/n(),
                                                                           survey_prop(Registro_en_impuestos_internos))
            (vartype = "ci",
                                                                                    na.rm = T,
                                                                                    deff = F))


######
Tabla_por_Macrozona <- objeto_enc %>%
  group_by(Macrozona) %>%
  summarise(Clasificación_Internacional_de_la_situación_en_el_empleo = survey_total(vartype = "cv",na.rm = T),
            Registro_contable = survey_prop(vartype = "ci", na.rm = T),
            Ganancias_mensual_de_los_microemprendedores_as = survey_total(vartype = "ci", na.rm = T),
            Clasificación_Internacional_de_la_situación_en_el_empleo = survey_prop(vartype = "ci", na.rm = T)) %>% 
  mutate(Clasificación_Internacional_de_la_situación_en_el_empleo, case_when())

#####No supe como agrupar las variables, se me ocurrio hacer mutate para crear variables adicionales dentro del objeto encuesta y poder
### crear las variables que pide la tabla pero, dada la hora que es ya no alcanzo a hacer más

          
            Registro_en_impuestos_internos,
           Ganancias_mensual_de_los_microemprendedores_as,
           Ganancias_mensual_de_los_microemprendedores_as) %>% 
  
  select(Clasificación_Internacional_de_la_situación_en_el_empleo,
        Registro_en_impuestos_internos,
         Ganancias_mensual_de_los_microemprendedores_as,
         Ganancias_mensual_de_los_microemprendedores_as)
