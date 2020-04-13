setwd("O:\\R_docentes_grande_area-sandra")
library(openxlsx)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
source("funcoes.R")


sintese_plan <- read.xlsx(xlsxFile = "docentes_editados/Tabela_sintese_edit.xlsx",
                   colNames = TRUE, startRow = 1, rowNames = FALSE)

UFABC_plan <- read.xlsx(xlsxFile = "docentes_editados/Docentes_UFABC_por_area_e_grande_area.xlsx",
                          colNames = TRUE, startRow = 1, rowNames = FALSE)

UFSCar_plan <- read.xlsx(xlsxFile = "docentes_editados/Docentes_UFSCar_unidade_regime_trabalho.xlsx",
                         sheet = 1, colNames = TRUE, startRow = 1, rowNames = FALSE)

UNESP_plan <- read.xlsx(xlsxFile = "docentes_editados/Docentes_UNESP_por_unidade_regime_trabalho_teste.xlsx",
                             colNames = TRUE, startRow = 1, rowNames = FALSE)

UNIFESP_plan <- read.xlsx(xlsxFile = "docentes_editados/Docentes_UNIFESP_por_unidade_regime_trabalho.xlsx",
                            sheet = 2 ,colNames = TRUE, startRow = 1, rowNames = FALSE)

UNICAMP_plan <- read.xlsx(xlsxFile = "docentes_editados/Docentes_Unicamp_tabelas_referencia_uso_tempo_GrandeArea_20200303.xlsx",
                            sheet = 4 ,colNames = TRUE, startRow = 1, rowNames = FALSE)

USP_plan <- read.xlsx(xlsxFile = "docentes_editados/Docentes_USP_por_unidade_regime_trabalho.xlsx",
                              colNames = TRUE, startRow = 1, rowNames = FALSE)

#UNIFESP_plan$Grande.Área <- factor(UNIFESP_plan$Grande.Área) 

# retira o ponto(.) e substitui por underline(_)
names(sintese_plan) <- names(sintese_plan) %>% ajustar_nomes() %>% rm_accent()
# UFABC ainda não tem horas de dedicação - deixa-la 
names(UFABC_plan) <- names(UFABC_plan) %>% ajustar_nomes() %>% rm_accent()
names(UFSCar_plan) <- names(UFSCar_plan) %>% ajustar_nomes() %>% rm_accent()
names(UNESP_plan) <- names(UNESP_plan) %>% ajustar_nomes() %>% rm_accent()
names(UNIFESP_plan) <- names(UNIFESP_plan) %>% ajustar_nomes() %>% rm_accent()
names(UNICAMP_plan) <- names(UNICAMP_plan) %>% ajustar_nomes() %>% rm_accent()
names(USP_plan) <- names(USP_plan) %>% ajustar_nomes() %>% rm_accent()

# -----------------------UFSCAR -------------------------------------------
# rename a column  
names(UFSCar_plan)[1] <- "departamento"
names(UFSCar_plan)[2] <- "grande_area"

ufscar_dedic_exc <- UFSCar_plan %>% 
  group_by(grande_area) %>% 
  summarise(soma_dedic=sum(dedicacao_exclusiva, na.rm=TRUE)) 

ufscar_dedic_exc$grande_area <- ufscar_dedic_exc$grande_area %>%
  replace_na("outros")
  

ufscar_dedic_par <- UFSCar_plan %>% 
  group_by(grande_area)  %>% 
  summarise(soma_parc=sum(dedicacao_parcial, na.rm=TRUE))

ufscar_dedic_par$grande_area <- ufscar_dedic_par$grande_area %>%
  replace_na("outros")

# ---------------------- UNESP --------------------------------------------

unesp_dedic_exc <- UNESP_plan %>% 
  group_by(grande_area) %>% 
  summarise(soma_dedic=sum(rdidp, na.rm=TRUE))

unesp_dedic_exc$grande_area <- unesp_dedic_exc$grande_area %>%
  replace_na("outros")
  

unesp_dedic_par <- UNESP_plan %>% 
  group_by(grande_area)  %>% 
  summarise(soma_parc=sum(rtc, na.rm=TRUE))

#unesp_dedic_par <- na.omit(unesp_dedic_par)
unesp_dedic_par$grande_area <- unesp_dedic_par$grande_area %>%
  replace_na("outros")

# ------------------------ unifesp --------------------------------------
# normalizando os nomes na variavel tipo
# col_tipo <- subset(UNIFESP_plan, select = c("tipo"))
# inx <- grepl("PROFESSOR ADJUNTO DEDICAÇÃO EXCLUSIVA", toupper(col_tipo))
# str <- col_tipo[which(inx), ]
# subs <- str_replace(str,"PROFESSOR ADJUNTO DEDICAÇÃO EXCLUSIVA", "PROFESSOR 40hs")
# UNIFESP_plan$tipo <- factor(UNIFESP_plan$tipo)
# valores <-levels(UNIFESP_plan$tipo)
# dado <- UNIFESP_plan[UNIFESP_plan$tipo %like% "40", ]  
# subst <- str_replace(dado, "")
# gsub(as.character(dado$tipo), "DEDICAÇÃO EXCLUSIVA", dado$tipo)
# teste <- dado %>% str_remove_all(dado$tipo, "EXCLUSIVO")

unifesp_dedic_exc <- UNIFESP_plan %>%
  group_by(grande_area) %>%
  filter(tipo == "PROFESSOR DEDICACAO EXCLUSIVA") %>%
  count()
# renomear a coluna
unifesp_dedic_exc <- unifesp_dedic_exc %>% rename(soma_dedic = n)

unifesp_dedic_par <- UNIFESP_plan %>% 
  group_by(grande_area)  %>% 
  filter(tipo == "PROFESSOR DEDICACAO PARCIAL") %>%
  count()

unifesp_dedic_par <- unifesp_dedic_par %>% rename(soma_parc = n)

#------------------------- UNICAMP --------------------------------

# unicamp_dedic_exc <- UNICAMP_plan[25, -1]
# row.names(UNICAMP_plan)

unicamp_dedic_exc <- UNICAMP_plan %>%
                    filter(instituto == "Total") %>%
                    select(. , -1)
# renomear uma coluna específica
unicamp_dedic_exc <- unicamp_dedic_exc %>% rename(outros = x11)

unicamp_dedic_exc <- unicamp_dedic_exc %>% 
  rownames_to_column() %>% 
  gather(grande_area, value, -rowname) %>% 
  spread(rowname, value)

colnames(unicamp_dedic_exc) <- c("grande_area", "soma_dedic")

# ----------------------- USP --------------------------------------

usp_dedic_exc <- USP_plan %>%
  group_by(grande_area, regime_de_trabalho) %>%
  filter(regime_de_trabalho == "RDIDP") %>%
  summarise(soma_dedic=sum(total, na.rm=TRUE))

usp_dedic_exc$grande_area <- usp_dedic_exc$grande_area %>%
  replace_na("outros")

usp_dedic_par <- USP_plan %>% 
  group_by(grande_area, regime_de_trabalho) %>%
  filter(regime_de_trabalho == "RTC") %>%
  summarise(soma_parc=sum(total, na.rm=TRUE))

usp_dedic_par$grande_area <- usp_dedic_par$grande_area %>%
  replace_na("outros")

# -------------------------  montagem da tabela -----------------------

# merge UFSCAR
df_ufscar <- merge(ufscar_dedic_exc, ufscar_dedic_par, by.x="grande_area", by.y="grande_area")
#merge unesp
df_unesp <- merge(unesp_dedic_exc, unesp_dedic_par, by.x="grande_area", by.y="grande_area")
# unicamp
df_unicamp <- unicamp_dedic_exc
df_unicamp['soma_parc'] <- 0
#merge unifesp
df_unifesp <- merge(unifesp_dedic_exc, unifesp_dedic_par, by = "grande_area", all = TRUE)
df_unifesp$soma_parc <- df_unifesp$soma_parc %>%
  replace_na(0)
#merge usp
usp_select_exc <- subset(usp_dedic_exc, select = c("grande_area", "soma_dedic"))
usp_select_par <- subset(usp_dedic_par, select = c("grande_area", "soma_parc"))
df_usp <- merge(usp_select_exc, usp_select_par, by.x="grande_area", all = TRUE)
df_usp$soma_parc <- df_usp$soma_parc %>%
  replace_na(0)

# acrescentando a variável instituicao nos dfs
df_ufscar['instituicao_de_ensino_superior'] <- "UFSCAR"
df_unicamp['instituicao_de_ensino_superior'] <- "UNICAMP"
df_unifesp['instituicao_de_ensino_superior'] <- "UNIFESP"
df_unesp['instituicao_de_ensino_superior'] <- "UNESP"
df_usp['instituicao_de_ensino_superior'] <- "USP"

# renomear as colunas 
df_ufscar <- df_ufscar %>% rename(docente_dedicacao_exclusiva = soma_dedic)
df_ufscar <- df_ufscar %>% rename(docente_dedicacao_parcial = soma_parc)
df_unicamp <- df_unicamp %>% rename(docente_dedicacao_exclusiva = soma_dedic)
df_unicamp <- df_unicamp %>% rename(docente_dedicacao_parcial = soma_parc)
df_unifesp <- df_unifesp %>% rename(docente_dedicacao_exclusiva = soma_dedic)
df_unifesp <- df_unifesp %>% rename(docente_dedicacao_parcial = soma_parc)
df_unesp <- df_unesp %>% rename(docente_dedicacao_exclusiva = soma_dedic)
df_unesp <- df_unesp %>% rename(docente_dedicacao_parcial = soma_parc)
df_usp <- df_usp %>% rename(docente_dedicacao_exclusiva = soma_dedic)
df_usp <- df_usp %>% rename(docente_dedicacao_parcial = soma_parc)


cols_df <- C("instituicao_de_ensino_superior", "grande_area", 
             "docente_dedicacao_exclusiva", "docente_dedicacao_parcial")


df <- rbind(df_ufscar, df_unesp, df_unicamp, df_unifesp, df_usp)
colnames(df) <- c("instituicao_de_ensino_superior", "grande_area", "soma_dedic", "soma_parc" )

# ordenando as colunas
df2 <- df[, c(4, 1, 2, 3)]

write.xlsx(df2, "planilha_final.xlsx", sheetName = "total")

# ----------------------- arquivos para usar depois ---------------

# exemplo

Dados <-
  "PRODUTO;TAG1;TAMANHO;Jan/2015;Fev/2015;Mar/2015
AAA;YYY;1000;10;11;12
AAA;III;500;12;14;15
AAA;YYY;100;8;6;7
BBB;III;1000;5;4;3
BBB;YYY;500;10;11;12
BBB;III;100;6;6;7"

Originais <- read.table(header = TRUE, text=Dados, sep=";") 

Organizados <- Originais %>% gather("DATA","VAL",4:6)

teste <- Organizados %>%
  group_by(PRODUTO, TAG1) %>%
  filter(DATA=="Fev.2015", TAG1=="YYY") %>%
  summarise(soma=sum(VAL))




# para selecionar as colunas das planilhas para a soma
apply(df_que_se_quer[, 1:3], 2, sum)

# para contar em colunas e para contar em linhas
colSums()
rowSums()

# # retira o ponto(.) e o espaço vazio e substitui por underline(_) 
stringr::str_replace_all("[.' ']","_")

# para substituir caracteres especiais e acentos no R
iconv(c("Santa CecÃ­lia", "AnhangÃ¼era", "Freguesia Do Ã“"), to = "ASCII//TRANSLIT")
