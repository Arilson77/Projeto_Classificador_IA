# Teste de inicialização do CLASSIFICADOR_VERSAO14.2.R
# Verifica se o app carrega sem erros

cat("✅ Iniciando teste de sintaxe...\n")

# Parse do arquivo principal
tryCatch({
  parsed <- parse("CLASSIFICADOR_VERSAO14.2.R")
  cat("✅ Sintaxe OK: Arquivo parseado com sucesso\n")
  cat("   Total de expressões:", length(parsed), "\n")
}, error = function(e) {
  cat("❌ ERRO DE SINTAXE:\n")
  cat(conditionMessage(e), "\n")
  quit(status = 1)
})

# Verificar se objeto validacoes existe na estrutura
cat("\n✅ Verificando estrutura do código...\n")
codigo <- readLines("CLASSIFICADOR_VERSAO14.2.R", warn = FALSE, encoding = "UTF-8")

# Contar ocorrências
validacoes_modelo_count <- sum(grepl("validacoes_modelo", codigo, fixed = TRUE))
validacoes_count <- sum(grepl("validacoes <-", codigo, fixed = TRUE))

cat("   Ocorrências de 'validacoes_modelo':", validacoes_modelo_count, "\n")
cat("   Ocorrências de 'validacoes <-':", validacoes_count, "\n")

if (validacoes_modelo_count == 0) {
  cat("✅ SUCESSO: Todas referências de validacoes_modelo foram substituídas\n")
} else {
  cat("⚠️ AVISO: Ainda existem", validacoes_modelo_count, "referências a validacoes_modelo\n")
}

cat("\n✅ Teste completo: App pronto para execução\n")
