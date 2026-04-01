#=============================================================================
# FUNÇÃO: SANITIZAÇÃO DE DADOS
#=============================================================================

sanitizar_texto <- function(texto) {
  if (is.null(texto) || is.na(texto)) return("")
  
  # Converter para string
  texto <- as.character(texto)
  
  # Remover caracteres de controle (usando padrão compatível)
  texto <- gsub("[[:cntrl:]]", " ", texto)
  
  # Remover espaços múltiplos
  texto <- gsub("\\s+", " ", texto)
  
  # Remover espaços nas extremidades
  texto <- trimws(texto)
  
  return(texto)
}

# Teste
cat("Testando sanitizar_texto:\n")
texto_teste <- "  Olá   mundo  \n  com  quebras  "
cat("Antes:", nchar(texto_teste), "caracteres\n")
resultado <- sanitizar_texto(texto_teste)
cat("Depois:", nchar(resultado), "caracteres\n")
cat("Resultado: [", resultado, "]\n", sep="")
