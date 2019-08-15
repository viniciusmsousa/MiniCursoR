
# Selecionar Paises com dados Balanceados ---------------------------------

filtro <- function(df){
  # OBS: 
  # Seleciona os paises que tem dados balanceados
  # É necessário que a coluna com os paises tenha o nome de "Pais"
  
  lista_paises <- as.character(unique(df$Pais))
  paises_completos <- vector(mode = "character",length = length(lista_paises))
  for(i in seq_along(lista_paises)){
    pais_atual <- df %>% 
      filter(Pais == lista_paises[i])
    
    if(any(is.na(pais_atual))==F){
      paises_completos[i] <- lista_paises[i]
    } else {
      paises_completos[i] <- NA
    }
  }
  dados <- df %>% 
    filter(Pais %in% paises_completos)
  return(dados)
}