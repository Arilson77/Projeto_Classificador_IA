# Integração API OpenAI
OPENAI_API_KEY <- "29d08064725944fcbc0b53e06f8807c5"

resumir_texto <- function(texto) {
  # Implementar integração OpenAI aqui
  return(substr(texto, 1, 200))
}

cat("📄 Módulo API carregado\n")
