setwd("O:\\R_docentes_grande_area-sandra")
library(openxlsx)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
source("funcoes.R")

arquivo <- "Docentes_IES_agregada.xlsx"

UFABC_plan <- read.xlsx(xlsxFile = arquivo, sheet = 1, colNames = TRUE, startRow = 1, rowNames = FALSE)
UFSCar_plan <- read.xlsx(xlsxFile = arquivo, sheet = 2, colNames = TRUE, startRow = 1, rowNames = FALSE)
UNESP_plan <- read.xlsx(xlsxFile = arquivo, sheet = 3, colNames = TRUE, startRow = 1, rowNames = FALSE)
UNIFESP_plan <- read.xlsx(xlsxFile = arquivo, sheet = 4, colNames = TRUE, startRow = 1, rowNames = FALSE)
UNICAMP_plan <- read.xlsx(xlsxFile = arquivo, sheet = 5, colNames = TRUE, startRow = 1, rowNames = FALSE)
USP_plan <- read.xlsx(xlsxFile = arquivo, sheet = 6, colNames = TRUE, startRow = 1, rowNames = FALSE)


# retira o ponto(.) e substitui por underline(_)
names(UFABC_plan) <- names(UFABC_plan) %>% ajustar_nomes() %>% rm_accent()
names(UFSCar_plan) <- names(UFSCar_plan) %>% ajustar_nomes() %>% rm_accent()
names(UNESP_plan) <- names(UNESP_plan) %>% ajustar_nomes() %>% rm_accent()
names(UNIFESP_plan) <- names(UNIFESP_plan) %>% ajustar_nomes() %>% rm_accent()
names(UNICAMP_plan) <- names(UNICAMP_plan) %>% ajustar_nomes() %>% rm_accent()
names(USP_plan) <- names(USP_plan) %>% ajustar_nomes() %>% rm_accent()

#------------------- GRANDES AREAS ---------------------------------------

plan <- UFABC_plan %>% add_row(grande_area = "Linguística, Letras e Artes")
fac_ufabc <- factor(plan$grande_area)
todas_grandes_areas <- levels(fac_ufabc)
todas_grandes_areas <- todas_grandes_areas %>% ajustar_nomes() %>% rm_accent()

# ----------------------- UFABC -------------------------------------------

ufabc_dedic_exc <- UFABC_plan %>% 
  group_by(grande_area) %>% 
  count(grande_area)

ufabc_dedic_exc <- ufabc_dedic_exc %>% rename(soma_dedic = n)

# agrupa por grande area, cria a variavel soma_parc com mutate e soma os valores omitindo os NA se tiverem
ufabc_dedic_par <- UFABC_plan %>%
  group_by(grande_area) %>% 
  mutate(soma_parc = 0) %>%
  summarise(soma_parc=sum(soma_parc, na.rm=TRUE))

# UFABC_plan %>% add_row(grande_area = "Linguística, Letras e Artes", soma_dedic = 0 )
# UFABC_plan %>% add_row(grande_area = "Linguística, Letras e Artes")

# -----------------------UFSCAR -------------------------------------------
# rename a column  
#names(UFSCar_plan)[1] <- "departamento"
#names(UFSCar_plan)[2] <- "grande_area"

UFSCar_plan <- UFSCar_plan %>%
  rename(dedicacao_parcial = as.character("20h"))

ufscar_dedic_exc <- UFSCar_plan %>% 
  group_by(grande_area) %>% 
  summarise(soma_dedic=sum(dedicacao_exclusiva, na.rm=TRUE)) 

ufscar_dedic_par <- UFSCar_plan %>% 
  group_by(grande_area)  %>% 
  summarise(soma_parc=sum(dedicacao_parcial, na.rm=TRUE))


# ---------------------- UNESP --------------------------------------------

unesp_dedic_exc <- UNESP_plan %>% 
  group_by(grande_area) %>% 
  summarise(soma_dedic=sum(rdidp, na.rm=TRUE))

unesp_dedic_par <- UNESP_plan %>% 
  group_by(grande_area)  %>% 
  summarise(soma_parc=sum(rtc, na.rm=TRUE))


# ------------------------ unifesp --------------------------------------

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
  filter(departamento == "Total") %>%
  select(. , -1)
# renomear uma coluna específica
unicamp_dedic_exc <- unicamp_dedic_exc %>% rename(outros = x11)

# transforma colunas em linhas
unicamp_dedic_exc <- unicamp_dedic_exc %>% 
  rownames_to_column() %>% 
  gather(grande_area, value, -rowname) %>% 
  spread(rowname, value)

colnames(unicamp_dedic_exc) <- c("grande_area", "soma_dedic")

# substituindo o valor de outros por 0
unicamp_dedic_exc$soma_dedic[unicamp_dedic_exc$grande_area == "outros"] <- 0

unicamp_dedic_par <- unicamp_dedic_exc
colnames(unicamp_dedic_par) <- c("grande_area", "soma_parc")
unicamp_dedic_par$soma_parc <- 0

# ----------------------- USP --------------------------------------

usp_dedic_exc <- USP_plan %>%
  group_by(grande_area) %>%
  filter(regime_de_trabalho == "RDIDP") %>%
  summarise(soma_dedic=sum(total, na.rm=TRUE))

usp_dedic_par <- USP_plan %>% 
  group_by(grande_area) %>%
  filter(regime_de_trabalho == "RTC") %>%
  summarise(soma_parc=sum(total, na.rm=TRUE))


# --------------------------- Normalizando o texto das grandes areas  -------------------

lista_para_normalizar <- list(ufabc_dedic_exc, ufabc_dedic_par, ufscar_dedic_exc, ufscar_dedic_par,
                              unesp_dedic_exc, unesp_dedic_par, unifesp_dedic_exc, unifesp_dedic_par,
                              unicamp_dedic_exc, unicamp_dedic_par, usp_dedic_exc, usp_dedic_par)


lista_de_dados_normalizados <- suppressWarnings(normalizar_variavel(lista_para_normalizar))

# -------------------  desempacotando a lista e atribuindo os dataframes às variáveis ----

ufabc_total <- lista_de_dados_normalizados[[1]]
ufabc_parc <- lista_de_dados_normalizados[[2]]

ufscar_total <- lista_de_dados_normalizados[[3]]
ufscar_parc <-  lista_de_dados_normalizados[[4]]

unesp_total <-  lista_de_dados_normalizados[[5]]
unesp_parc <-  lista_de_dados_normalizados[[6]]

unifesp_total <- lista_de_dados_normalizados[[7]]
unifesp_parc <- lista_de_dados_normalizados[[8]]

unicamp_total <- lista_de_dados_normalizados[[9]]
unicamp_parc <- lista_de_dados_normalizados[[10]]

#excluindo o NA de grande area - verificar pq estão vazios
usp_total <- lista_de_dados_normalizados[[11]] %>% drop_na(grande_area) 
usp_parc <-  lista_de_dados_normalizados[[12]] %>% drop_na(grande_area) 

# --------------------------- EQUIPARANDO OS CAMPOS DE GRANDE AREA -------------------

# ufabc falta linguistica letras e artes
falta_na_ufabc_total <- diferenca(ufabc_total)
ufabc_total <- ufabc_total %>% add_row(grande_area = falta_na_ufabc_total, .before = 9)
ufabc_total$soma_dedic[ufabc_total$grande_area == falta_na_ufabc_total] <- 0


falta_na_ufabc_parc <- diferenca(ufabc_parc)
ufabc_parc <- ufabc_parc %>% add_row(grande_area = falta_na_ufabc_parc, .before = 9)
ufabc_parc$soma_parc[ufabc_parc$grande_area == falta_na_ufabc_parc] <- 0

# ------- UFSCAR ------------------
falta_na_ufcar_total <- diferenca(ufscar_total)
ufscar_total <- ufscar_total %>% add_row(grande_area = falta_na_ufcar_total, .after = 9)
ufscar_total$soma_dedic[ufscar_total$grande_area == falta_na_ufcar_total] <- 0

falta_na_ufabc_parc <- diferenca(ufscar_parc)
ufscar_parc <- ufscar_parc %>% add_row(grande_area = falta_na_ufabc_parc, .after = 9)
ufscar_parc$soma_parc[ufscar_parc$grande_area == falta_na_ufabc_parc] <- 0

# ------ UNESP ---------------------
# ok

# ------ UNIFESP ---------------
# falta ciencias agrárias
falta_na_unifesp_total <- diferenca(unifesp_total)
unifesp_total <- unifesp_total %>% add_row(grande_area = falta_na_unifesp_total, .before = 1)
unifesp_total$soma_dedic[unifesp_total$grande_area == falta_na_unifesp_total] <- 0

# devido ao fato de unifesp-parc ter muitos faltantes o melhor caminho foi fazer o merge e tratá-los aqui
df_merg_unifesp <- merge(unifesp_total, unifesp_parc, by.x = "grande_area", all = TRUE )

temp <- sapply(todas_grandes_areas, function(x,df_merg_unifesp){which(df_merg_unifesp$grande_area == x)}, df_merg_unifesp=df_merg_unifesp)
df_unifesp_merged <- df_merg_unifesp[temp,]
df_unifesp_merged$soma_parc <- df_unifesp_merged$soma_parc  %>%
  replace_na(0)

# df_merg <- merge(unifesp_total, unifesp_parc, by.x = "grande_area", all.x = TRUE )
# new_order <- sapply(todas_grandes_areas, function(x,df_merg){which(df_merg$grande_area == x)}, df_merg=df_merg)
# df_merg <- df_merg[new_order,]
# ------ modelo------ 
# new_order <- sapply(target, function(x,df){which(df$name == x)}, df=df)
# df        <- df[new_order,]

# ---- UNICAMP -------------------
# OK

# ------------- USP ---------------
# OK

# -------------------------  montagem da tabela -----------------------

# merge UFABC
df_ufabc <- merge(ufabc_total, ufabc_parc, by.x="grande_area", by.y="grande_area")#
#merge UFSCAR
df_ufscar <- merge(ufscar_total, ufscar_parc, by.x="grande_area", by.y="grande_area")
#merge unesp
df_unesp <- merge(unesp_total, unesp_parc, by.x="grande_area", by.y="grande_area")
# merge unifesp já feito acima, aqui, só atribuição da variável
df_unifesp <- df_unifesp_merged
# unicamp
df_unicamp <- merge(unicamp_total, unicamp_parc, by.x="grande_area", by.y="grande_area")
#merge usp
df_usp <- merge(usp_total, usp_parc, by.x="grande_area", all = TRUE)


# acrescentando a variável instituicao nos dfs
df_ufabc['instituicao_de_ensino_superior'] <- "UFABC"
df_ufscar['instituicao_de_ensino_superior'] <- "UFSCAR"
df_unicamp['instituicao_de_ensino_superior'] <- "UNICAMP"
df_unifesp['instituicao_de_ensino_superior'] <- "UNIFESP"
df_unesp['instituicao_de_ensino_superior'] <- "UNESP"
df_usp['instituicao_de_ensino_superior'] <- "USP"

# renomear as colunas 
df_ufabc <- df_ufabc %>% rename(docente_dedicacao_exclusiva = soma_dedic)
df_ufabc <- df_ufabc %>% rename(docente_dedicacao_parcial = soma_parc)
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


df <- rbind(df_ufabc, df_ufscar, df_unesp, df_unifesp, df_unicamp, df_usp)

# ordenando as colunas
df2 <- df[, c(4, 1, 2, 3)]

write.xlsx(df2, "planilha_contagem_docentes.xlsx", sheetName = "total")
