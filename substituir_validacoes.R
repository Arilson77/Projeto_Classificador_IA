# Script para substituir validacoes_modelo por validacoes de forma segura
cat("✅ Iniciando substituição segura...\n")

# Ler arquivo com encoding UTF-8
linhas <- readLines("CLASSIFICADOR_VERSAO14.2.R", warn = FALSE, encoding = "UTF-8")
cat("   Linhas lidas:", length(linhas), "\n")

# Contar antes
antes <- sum(grepl("validacoes_modelo", linhas, fixed = TRUE))
cat("   Ocorrências de 'validacoes_modelo' antes:", antes, "\n")

# Substituir
linhas_corrigidas <- gsub("validacoes_modelo", "validacoes", linhas, fixed = TRUE)

# Contar depois
depois <- sum(grepl("validacoes_modelo", linhas_corrigidas, fixed = TRUE))
cat("   Ocorrências de 'validacoes_modelo' depois:", depois, "\n")

# Salvar
writeLines(linhas_corrigidas, "CLASSIFICADOR_VERSAO14.2.R", useBytes = TRUE)
cat("✅ Arquivo salvo com sucesso\n")

# Verificar
linhas_verificacao <- readLines("CLASSIFICADOR_VERSAO14.2.R", warn = FALSE, encoding = "UTF-8")
verificacao <- sum(grepl("validacoes_modelo", linhas_verificacao, fixed = TRUE))
cat("   Verificação final: ", verificacao, "ocorrências restantes\n")

if (verificacao == 0) {
  cat("✅ SUCESSO: Todas substituições concluídas!\n")
} else {
  cat("⚠️ AVISO: Ainda restam", verificacao, "ocorrências\n")
}
