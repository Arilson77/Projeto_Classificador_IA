# ═══════════════════════════════════════════════════════════════════════════
# INSTALADOR DE DEPENDÊNCIAS - SISTEMA SAP PETROBRAS v14.2
# ═══════════════════════════════════════════════════════════════════════════

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════════╗\n")
cat("║          INSTALANDO DEPENDÊNCIAS - SISTEMA SAP PETROBRAS v14.2            ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════╝\n\n")

# Configurar mirrors alternativos (CRAN primário + fallback)
mirrors_disponiveis <- c(
  "https://cran.posit.co/",
  "https://mirror.rcg.usm.my/web/packages/",
  "https://cran.r-project.org/"
)

# Tentar configurar um mirror que funcione
mirror_ativo <- NULL
for(mirror in mirrors_disponiveis) {
  cat("🔍 Testando mirror:", mirror, "...\n")
  test_url <- paste0(mirror, "src/contrib/PACKAGES.gz")
  test_result <- tryCatch({
    readLines(test_url, n = 1, warn = FALSE)
    mirror
  }, error = function(e) NULL, warning = function(w) NULL)
  
  if(!is.null(test_result)) {
    mirror_ativo <- mirror
    cat("   ✅ Mirror ativo!\n")
    break
  }
}

if(is.null(mirror_ativo)) {
  cat("⚠️  Nenhum mirror acessível. Usando mirror padrão do Posit...\n")
  mirror_ativo <- "https://cran.posit.co/"
}

# Usar o mirror ativo
options(repos = mirror_ativo)
cat("✅ Mirror configurado:", mirror_ativo, "\n\n")

# ═══════════════════════════════════════════════════════════════════════════
# LISTA COMPLETA DE PACOTES (incluindo ML)
# ═══════════════════════════════════════════════════════════════════════════

pacotes <- c(
  # Core Shiny
  "shiny",
  "shinydashboard", 
  "shinyjs",
  "shinycssloaders",
  "shinyalert",
  
  # Data manipulation
  "dplyr",
  "tidyr",
  "data.table",
  "stringr",
  "stringi",
  
  # Visualization
  "ggplot2",
  "plotly",
  "DT",
  
  # File handling
  "readxl",
  "openxlsx",
  "readr",
  
  # API & HTTP
  "httr",
  "httr2",
  "jsonlite",
  "xml2",
  
  # Machine Learning
  "randomForest",
  "tm",
  "SnowballC",
  "e1071",
  
  # Utilities
  "lubridate",
  "format",
  "glue",
  "cli",
  "rlang"
)

# ═══════════════════════════════════════════════════════════════════════════
# INSTALAÇÃO COM RETRY
# ═══════════════════════════════════════════════════════════════════════════

cat("📦 Verificando pacotes...\n\n")

instalados <- installed.packages()[,"Package"]
pacotes_faltando <- pacotes[!pacotes %in% instalados]
pacotes_ok <- pacotes[pacotes %in% instalados]

if(length(pacotes_ok) > 0) {
  cat("✅ Já instalados (", length(pacotes_ok), "):\n")
  cat("   ", paste(pacotes_ok, collapse = ", "), "\n\n")
}

if(length(pacotes_faltando) > 0) {
  cat("📥 Faltando (", length(pacotes_faltando), "):\n")
  cat("   ", paste(pacotes_faltando, collapse = ", "), "\n\n")
  
  cat("⏳ Iniciando instalação (máx 3 tentativas por pacote)...\n\n")
  
  instalados_com_sucesso <- c()
  falhados <- c()
  
  for(pkg in pacotes_faltando) {
    sucesso <- FALSE
    for(tentativa in 1:3) {
      cat(sprintf("[%s] Tentativa %d/3: %s\n", Sys.time(), tentativa, pkg))
      
      resultado <- tryCatch({
        install.packages(pkg, dependencies = TRUE, quiet = FALSE)
        cat("   ✅ Instalado com sucesso!\n")
        TRUE
      }, error = function(e) {
        cat("   ❌ Erro:", as.character(e), "\n")
        FALSE
      }, warning = function(w) {
        cat("   ⚠️  Aviso:", as.character(w), "\n")
        FALSE
      })
      
      if(resultado) {
        sucesso <- TRUE
        break
      }
    }
    
    if(sucesso) {
      instalados_com_sucesso <- c(instalados_com_sucesso, pkg)
    } else {
      falhados <- c(falhados, pkg)
    }
    cat("\n")
  }
  
  cat("\n═══════════════════════════════════════════════════════════════════════════\n")
  cat("📊 RESUMO DA INSTALAÇÃO:\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("✅ Sucesso:  ", length(instalados_com_sucesso), "pacotes\n")
  if(length(instalados_com_sucesso) > 0) {
    cat("   ", paste(instalados_com_sucesso, collapse = ", "), "\n")
  }
  
  if(length(falhados) > 0) {
    cat("❌ Falhados: ", length(falhados), "pacotes\n")
    cat("   ", paste(falhados, collapse = ", "), "\n")
    cat("\n⚠️  AÇÃO MANUAL NECESSÁRIA:\n")
    for(pkg in falhados) {
      cat("   install.packages('", pkg, "')\n", sep = "")
    }
  }
} else {
  cat("🎉 Todos os pacotes já estão instalados!\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# TESTE DE CARREGAMENTO
# ═══════════════════════════════════════════════════════════════════════════

cat("\n\n📋 Testando carregamento de pacotes críticos:\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

criticos <- c("shiny", "dplyr", "randomForest", "tm", "httr", "DT")
status_final <- c()

for(pkg in criticos) {
  resultado <- tryCatch({
    library(pkg, character.only = TRUE)
    "✅ OK"
  }, error = function(e) {
    "❌ ERRO"
  })
  cat(sprintf("%-15s: %s\n", pkg, resultado))
  status_final <- c(status_final, resultado)
}

cat("\n")
if(all(grepl("✅", status_final))) {
  cat("🎉 Sistema pronto para uso!\n\n")
  cat("Execute: shiny::runApp('CLASSIFICADOR_VERSAO14.2.R')\n")
} else {
  cat("⚠️  Alguns pacotes falharam. Verifique os erros acima.\n\n")
}