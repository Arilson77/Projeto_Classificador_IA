# Script de inicializacao rapida
cat("Iniciando Sistema SAP Petrobras...\n")

# Verificar se dependencias estao instaladas
if(!require(shiny, quietly = TRUE)) {
  cat("Instalando dependencias...\n")
  source("install_dependencies.R")
}

# Executar aplicacao
cat("Abrindo aplicacao Shiny...\n")
shiny::runApp("app.R")