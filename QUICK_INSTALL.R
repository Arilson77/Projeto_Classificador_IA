#!/usr/bin/env Rscript
# ═══════════════════════════════════════════════════════════════════════════
# INSTALAÇÃO RÁPIDA - randomForest e dependências críticas
# ═══════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════╗\n")
cat("║          INSTALAÇÃO RÁPIDA - DEPENDÊNCIAS CRÍTICAS                        ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════╝\n\n")

# Configurar repositório
options(repos = c(CRAN = "https://cran.posit.co/"))
cat("✅ Repositório configurado: https://cran.posit.co/\n\n")

# Pacotes críticos
criticos <- c(
  "randomForest",
  "tm",
  "SnowballC",
  "dplyr",
  "shiny"
)

cat("📦 Instalando pacotes críticos...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

instalados <- 0
falhados <- 0

for(pkg in criticos) {
  cat("⏳", pkg, "...")
  
  if(pkg %in% installed.packages()[,"Package"]) {
    cat(" [JÁ INSTALADO]\n")
    instalados <- instalados + 1
  } else {
    resultado <- tryCatch({
      install.packages(pkg, dependencies = TRUE)
      cat(" ✅\n")
      instalados <- instalados + 1
    }, error = function(e) {
      cat(" ❌ Erro:", substr(as.character(e), 1, 40), "...\n")
      FALSE
    })
  }
}

cat("\n═══════════════════════════════════════════════════════════════════════════\n")
cat("✅ Status final: ", instalados, "/", length(criticos), " pacotes OK\n\n")

# Teste rápido
cat("🧪 Testando carregamento...\n")
teste_ok <- TRUE
for(pkg in criticos) {
  resultado <- try(library(pkg, character.only = TRUE, quietly = TRUE), silent = TRUE)
  if(inherits(resultado, "try-error")) {
    cat("   ❌", pkg, "\n")
    teste_ok <- FALSE
  } else {
    cat("   ✅", pkg, "\n")
  }
}

if(teste_ok) {
  cat("\n🎉 Sistema pronto! Execute:\n")
  cat("   shiny::runApp('CLASSIFICADOR_VERSAO14.2.R')\n\n")
} else {
  cat("\n⚠️  Alguns pacotes falharam. Tente instalar manualmente:\n\n")
  cat("   install.packages('randomForest')\n")
  cat("   install.packages('tm')\n")
  cat("   install.packages('SnowballC')\n\n")
}
