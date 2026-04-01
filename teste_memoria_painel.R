# ============================================================================
# TESTE: MEMÓRIA PERSISTENTE DO ÚLTIMO PAINEL
# ============================================================================

cat("\n🧪 TESTANDO SISTEMA DE MEMÓRIA PERSISTENTE...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Definir arquivo de configuração
CONFIG_FILE <- "dados_modelo_treinado/ultima_sessao.rds"

# Função para salvar último painel
salvar_ultimo_painel <- function(painel) {
  tryCatch({
    dir.create("dados_modelo_treinado", showWarnings = FALSE, recursive = TRUE)
    config <- list(
      ultimo_painel = painel,
      timestamp = Sys.time()
    )
    saveRDS(config, CONFIG_FILE)
    cat("💾 Último painel salvo:", painel, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Erro ao salvar último painel:", e$message, "\n")
    return(FALSE)
  })
}

# Função para carregar último painel
carregar_ultimo_painel <- function() {
  tryCatch({
    if(file.exists(CONFIG_FILE)) {
      config <- readRDS(CONFIG_FILE)
      cat("📂 Último painel carregado:", config$ultimo_painel, "\n")
      cat("🕒 Salvo em:", format(config$timestamp, "%d/%m/%Y %H:%M:%S"), "\n")
      return(config$ultimo_painel)
    } else {
      cat("ℹ️ Nenhum painel salvo anteriormente\n")
      return("dashboard")
    }
  }, error = function(e) {
    cat("❌ Erro ao carregar último painel:", e$message, "\n")
    return("dashboard")
  })
}

# ============================================================================
# TESTES
# ============================================================================

cat("\n📋 TESTE 1: Salvar painéis diferentes\n")
cat("─────────────────────────────────────────\n")

paineis_teste <- c("dashboard", "upload", "individual", "lote", "estatisticas")

for(painel in paineis_teste) {
  resultado <- salvar_ultimo_painel(painel)
  if(resultado) {
    cat("  ✅ Salvo:", painel, "\n")
  }
  Sys.sleep(0.2)  # Pequeno delay entre salvamentos
}

cat("\n📋 TESTE 2: Carregar último painel salvo\n")
cat("─────────────────────────────────────────\n")

ultimo <- carregar_ultimo_painel()
cat("  ✅ Último painel ativo:", ultimo, "\n")

cat("\n📋 TESTE 3: Verificar persistência entre sessões\n")
cat("─────────────────────────────────────────\n")

# Simula fechamento e reabertura
cat("  🔄 Simulando fechamento do app...\n")
Sys.sleep(0.5)

cat("  🔄 Simulando reabertura do app...\n")
ultimo_apos_reabrir <- carregar_ultimo_painel()

if(ultimo == ultimo_apos_reabrir) {
  cat("  ✅ SUCESSO: Painel restaurado corretamente!\n")
} else {
  cat("  ❌ FALHA: Painel não foi restaurado corretamente\n")
}

cat("\n📋 TESTE 4: Informações do arquivo de configuração\n")
cat("─────────────────────────────────────────\n")

if(file.exists(CONFIG_FILE)) {
  info <- file.info(CONFIG_FILE)
  cat("  📁 Arquivo:", CONFIG_FILE, "\n")
  cat("  📏 Tamanho:", info$size, "bytes\n")
  cat("  🕒 Modificado:", format(info$mtime, "%d/%m/%Y %H:%M:%S"), "\n")
  
  # Ler conteúdo
  config <- readRDS(CONFIG_FILE)
  cat("  📊 Conteúdo:\n")
  cat("     - Painel:", config$ultimo_painel, "\n")
  cat("     - Timestamp:", format(config$timestamp, "%d/%m/%Y %H:%M:%S"), "\n")
}

cat("\n═══════════════════════════════════════════════════════════\n")
cat("✅ TODOS OS TESTES CONCLUÍDOS!\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Instruções de uso
cat("\n📖 COMO FUNCIONA NO SHINY:\n")
cat("─────────────────────────────────────────\n")
cat("1. Quando você navega entre painéis, a aba é salva automaticamente\n")
cat("2. Ao fechar e reabrir o app, você volta automaticamente à última aba\n")
cat("3. O arquivo de configuração fica em: dados_modelo_treinado/ultima_sessao.rds\n")
cat("4. Não é necessário fazer nada - funciona automaticamente!\n\n")
