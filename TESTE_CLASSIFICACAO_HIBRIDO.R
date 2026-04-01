# ============================================================================
# TESTE: FUNÇÃO CLASSIFICAR_HIBRIDO (Dict + API)
# ============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("🧪 TESTE: FUNÇÃO classificar_hibrido (Dicionário + API)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Carregar o arquivo
source('CLASSIFICADOR_VERSAO14 copy.R')

# Teste 1: Texto que deve dar CRITICA (Sistema parado)
cat("\n✅ TESTE 1: Classificação de Sistema Crítico\n")
cat("─────────────────────────────────────────\n")
texto1 <- "Sistema parado por falha crítica no compressor principal"
cat("Texto:", texto1, "\n")
resultado1 <- classificar_hibrido(texto1, CONFIG_USUARIO())
cat("Resultado:\n")
cat("  Tipo:", resultado1$tipo, "\n")
cat("  Criticidade:", resultado1$criticidade, "\n")
cat("  Confiança:", resultado1$confianca, "%\n")
cat("  Método:", resultado1$metodo, "\n")
cat("  Resumo:", resultado1$resumo, "\n")

# Teste 2: Texto que deve dar BAIXA (Limpeza)
cat("\n✅ TESTE 2: Classificação de Limpeza\n")
cat("─────────────────────────────────────────\n")
texto2 <- "Necessário fazer limpeza da válvula principal"
cat("Texto:", texto2, "\n")
resultado2 <- classificar_hibrido(texto2, CONFIG_USUARIO())
cat("Resultado:\n")
cat("  Tipo:", resultado2$tipo, "\n")
cat("  Criticidade:", resultado2$criticidade, "\n")
cat("  Confiança:", resultado2$confianca, "%\n")
cat("  Método:", resultado2$metodo, "\n")
cat("  Resumo:", resultado2$resumo, "\n")

# Teste 3: Texto que deve dar MEDIA (Inspeção planejada)
cat("\n✅ TESTE 3: Classificação de Inspeção Planejada\n")
cat("─────────────────────────────────────────\n")
texto3 <- "Realizar inspeção planejada no motor de transmissão"
cat("Texto:", texto3, "\n")
resultado3 <- classificar_hibrido(texto3, CONFIG_USUARIO())
cat("Resultado:\n")
cat("  Tipo:", resultado3$tipo, "\n")
cat("  Criticidade:", resultado3$criticidade, "\n")
cat("  Confiança:", resultado3$confianca, "%\n")
cat("  Método:", resultado3$metodo, "\n")
cat("  Resumo:", resultado3$resumo, "\n")

# Teste 4: Validar que função existe e pode ser chamada
cat("\n✅ TESTE 4: Verificação de Existência da Função\n")
cat("─────────────────────────────────────────\n")
if(exists("classificar_hibrido", where = -1, inherits = TRUE)) {
  cat("✅ Função classificar_hibrido EXISTE\n")
  cat("✅ Tipo:", typeof(get("classificar_hibrido")), "\n")
  cat("✅ É função?", is.function(get("classificar_hibrido")), "\n")
} else {
  cat("❌ Função classificar_hibrido NÃO ENCONTRADA\n")
}

# Teste 5: Comparar com classificar_hibrido_com_modelo
cat("\n✅ TESTE 5: Roteamento classificar_hibrido_com_modelo\n")
cat("─────────────────────────────────────────\n")
resultado_router <- classificar_hibrido_com_modelo(texto1, CONFIG_USUARIO())
cat("Resultado via roteador:\n")
cat("  Tipo:", resultado_router$tipo, "\n")
cat("  Método:", resultado_router$metodo, "\n")

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("✅ TESTE COMPLETO\n")
cat("═══════════════════════════════════════════════════════════════\n")
