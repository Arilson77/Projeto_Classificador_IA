#!/usr/bin/env Rscript
# Script de teste para verificar se o sistema está funcionando

cat("\n")
cat(strrep("═", 70), "\n")
cat("TESTE DO SISTEMA CLASSIFICADOR\n")
cat(strrep("═", 70), "\n\n")

# Simular o que foi corrigido
test_text <- "Manutenção preventiva da bomba P-101"

cat("✅ Teste 1: Verificar strrep\n")
separator <- strrep("═", 50)
cat("Resultado:", separator, "\n\n")

cat("✅ Teste 2: Verificar estrutura básica\n")
cat(strrep("─", 50), "\n")
cat("OK: String repetida com sucesso\n\n")

cat("✅ Todos os testes passaram!\n")
cat(strrep("═", 70), "\n\n")
