

####Cargar Paquetes---------------------
pacman::p_load(sjPlot, 
               tidyverse, 
               srvyr,
               survey,
               haven)

####Cargar datos --------------------------

Base_de_datos_Full_VI_EME_extracto_ <- read_dta("input/Base de datos Full VI EME (extracto).dta")

datos_proc <- select(Base_de_datos_Full_VI_EME_extracto_,
                     Identificador_único_de_personas = Enc_rph,
                     Ganancias_mensual_de_los_microemprendedores_as = ganancia_final_mensual,
                     Registro_contable = conta_completa,
                     Registro_en_impuestos_internos = registro_SII, 
                     Clasificación_Internacional_de_la_situación_en_el_empleo  = CISE,
                     región_de_residencia_de_la_persona = region,
                     Factor_de_expansión_de_personas = Factor_EME)

######Eliminar los N.A----------
datos_proc2019 <- na.omit(datos_proc2019)
sum(is.)


####Recodificar---------------
#conta_completa
datos_proc <- datos_proc %>%  mutate(datos_proc,
                              Registro_contable = 
                                case_when(Registro_contable == 1~"Sí",
                                          Registro_contable == 2~"No"))
#registro_sii
datos_proc <- datos_proc %>%  mutate(datos_proc,
                                     Registro_contable = 
                                       case_when(Registro_contable == 1~"Sí",
                                                 Registro_contable == 2~"No"))

#Región
datos_proc <- datos_proc %>%  mutate(datos_proc,
                                     Registro_contable = 
                                       case_when(Registro_contable == 1~"Sí",
                                                 Registro_contable == 2~"No"))

#CISE
datos_proc <- datos_proc %>%  mutate(datos_proc,
                                     Registro_contable = 
                                       case_when(Registro_contable == 1~"Sí",
                                                 Registro_contable == 2~"No"))

#Objeto encuesta
objeto_enc <- as_survey_design(datos_proc,
                 id= Identificador_único_de_personas,
                 weights = Factor_de_expansión_de_personas)



