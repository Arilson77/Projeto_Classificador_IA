# ═══════════════════════════════════════════════════════════════════════════
# COMANDOS DE INSTALAÇÃO - COPIE E COLE NO CONSOLE R
# ═══════════════════════════════════════════════════════════════════════════

# PASSO 1: Configure o repositório correto
options(repos = c(CRAN = "https://cran.posit.co/"))

# PASSO 2: Instale randomForest (pacote crítico)
install.packages("randomForest")

# PASSO 3: Instale os demais pacotes de ML
install.packages(c("tm", "SnowballC", "e1071"))

# PASSO 4: Verifique se funcionou
library(randomForest)
library(tm)
cat("✅ Pacotes instalados com sucesso!\n")

# PASSO 5 (OPCIONAL): Instale todos os demais pacotes
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinycssloaders",
  "dplyr", "tidyr", "data.table", "stringr", "stringi",
  "ggplot2", "plotly", "DT",
  "readxl", "openxlsx", "readr",
  "httr", "httr2", "jsonlite", "xml2",
  "lubridate", "glue", "cli", "rlang"
))

# PASSO 6: Execute a aplicação
shiny::runApp("CLASSIFICADOR_VERSAO14.2.R")

# ═══════════════════════════════════════════════════════════════════════════
# ALTERNATIVA: Se tiver problemas de conectividade
# ═══════════════════════════════════════════════════════════════════════════

# Tente outro mirror
options(repos = c(CRAN = "https://mirror.rcg.usm.my/web/packages/"))
install.packages("randomForest")

# Ou outro mirror
options(repos = c(CRAN = "http://cran.us.r-project.org/"))
install.packages("randomForest")
