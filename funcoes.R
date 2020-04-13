library(stringr)
rm_accent <- function(str,pattern="all") {
  # Rotinas e fun��es �teis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Fun��o que tira todos os acentos e pontua��es de um vetor de strings.
  # Par�metros:
  # str - vetor de strings que ter�o seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos dever�o ser retirados.
  #            Para indicar quais acentos dever�o ser retirados, um vetor com os s�mbolos dever�o ser passados.
  #            Exemplo: pattern = c("�", "^") retirar� os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que s�o "�", "`", "^", "~", "�", "�")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="�"))
    pattern[pattern=="�"] <- "�"
  
  symbols <- c(
    acute = "������������",
    grave = "����������",
    circunflex = "����������",
    tilde = "������",
    umlaut = "�����������",
    cedil = "��"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("�","`","^","~","�","�")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

#nomes=c('Anivers�rio', 'F�SICO', 'Situa��o', 'Ra�a', 'IMC', 'Tipo f�sico', 'tabaco por dia (cig/dia)')
#rm_accent(nomes)

#------------ fucao para remover espa�os, caracter especial e converte para minusculo --------------

ajustar_nomes=function(x){
  x%>%
    stringr::str_trim() %>%                        #Remove espaços em branco sobrando
    stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
    rm_accent() %>%                                #Remove os acentos com a funcao criada acima
    stringr::str_replace_all("[/' '.,()]", "_") %>% #Substitui os caracteres especiais por "_"
    stringr::str_replace_all("_+", "_") %>%        #Substitui os caracteres especiais por "_"   
    stringr::str_replace("_$", "")                 #Substitui o caracter especiais por " "
}

# -------------------------- func�o para normalizar as palavras da grande area ---------
# retorna um dataframe com a grande area e seus valores

funcoes_norm_text <- function(val) {
  text_norm <- val$grande_area %>% ajustar_nomes() %>% rm_accent()
  
  if ("soma_dedic" %in% colnames(val)) {
    valores <- val$soma_dedic
    df <- data.frame(grande_area = text_norm, soma_dedic = val$soma_dedic)
    
  } else {
    valores <- val$soma_dedic
    df <- data.frame(grande_area = text_norm, soma_parc = val$soma_parc)
    
  }
  
  return(df)
}

# --------- fun��o que recebe um alista de dataframes e passa cada um deles a funcao - funcoes_norm_text

normalizar_variavel <- function(lista = list()) {
  #print(lista)
  lista_normalizada <- lapply(lista, funcoes_norm_text )
  #print(lista_normalizada)
  return(lista_normalizada)
}


# --------------------    funcao para verificar quais linhas est�o faltando em comparacao 
# -------------------- com a quantidade totla de grandes areas ----------------------------------

diferenca <- function(x) {
  ga <- todas_grandes_areas
  ga_rest <- setdiff(ga, levels(factor(x$grande_area)))
  #setdiff(levels(factor1), levels(factor2))
  return(ga_rest)
}

#-------------------------------------- fim -------------------------------------------------------