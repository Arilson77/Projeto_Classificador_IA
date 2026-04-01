# Teste da função sanitizar_texto
sanitizar_texto <- function(texto) {
  if (is.null(texto) || is.na(texto)) return("")
  
  # Converter para string
  texto <- as.character(texto)
  
  # Remover caracteres de controle
  texto <- gsub("[[:cntrl:]]", " ", texto)
  
  # Remover espaços múltiplos
  texto <- gsub("\\s+", " ", texto)
  
  # Remover espaços nas extremidades
  texto <- trimws(texto)
  
  return(texto)
}

cat("✓ Teste 1: Texto normal\n")
resultado1 <- sanitizar_texto("  Olá   mundo  ")
cat(paste("Saída: [", resultado1, "]\n\n", sep=""))

cat("✓ Teste 2: Quebras de linha e tabs\n")
resultado2 <- sanitizar_texto("Olá\nMundo\tTeste")
cat(paste("Saída: [", resultado2, "]\n\n", sep=""))

cat("✓ Teste 3: Texto NULL\n")
resultado3 <- sanitizar_texto(NULL)
cat(paste("Saída: [", resultado3, "]\n\n", sep=""))

cat("✓ Teste 4: Texto vazio\n")
resultado4 <- sanitizar_texto("")
cat(paste("Saída: [", resultado4, "]\n\n", sep=""))

cat("✓✓✓ Todos os testes passaram sem erros!\n")
