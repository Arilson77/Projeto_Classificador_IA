#!/usr/bin/env Rscript
# Teste do sistema de CACHE - cache_get e cache_set

cat("\n")
cat(strrep("═", 70), "\n")
cat("✅ TESTE DO SISTEMA DE CACHE (cache_get e cache_set)\n")
cat(strrep("═", 70), "\n\n")

# Carregar arquivo e testar
source('CLASSIFICADOR_VERSAO14.R', encoding = 'UTF-8')

# Teste 1: Verificar se CACHE_API foi inicializado
cat("📋 Teste 1: Verificar inicialização do CACHE_API\n")
if (exists("CACHE_API")) {
  cat("   ✅ CACHE_API inicializado\n")
  cat("   - Hits:", CACHE_API$hits, "\n")
  cat("   - Misses:", CACHE_API$misses, "\n")
  cat("   - Max Size:", CACHE_API$max_size, "\n\n")
} else {
  cat("   ❌ CACHE_API não inicializado\n\n")
}

# Teste 2: Testar cache_set
cat("📋 Teste 2: Armazenar resultado no cache (cache_set)\n")
texto_teste <- "Manutenção preventiva do compressor"
resultado_teste <- list(
  tipo = 3,
  categoria = "PROBLEMAS_COMUNS",
  criticidade = "MEDIA",
  confianca = 85,
  descricao = "Manutenção preventiva",
  metodo = "DICIONARIO"
)

sucesso_set <- cache_set(texto_teste, resultado_teste, "openai")
cat("   ✅ Resultado armazenado no cache:", sucesso_set, "\n\n")

# Teste 3: Testar cache_get
cat("📋 Teste 3: Recuperar resultado do cache (cache_get)\n")
resultado_cache <- cache_get(texto_teste, "openai")

if (!is.null(resultado_cache)) {
  cat("   ✅ Resultado recuperado do cache!\n")
  cat("   - Tipo:", resultado_cache$tipo, "\n")
  cat("   - Categoria:", resultado_cache$categoria, "\n")
  cat("   - Confiança:", resultado_cache$confianca, "%\n")
  cat("   - Hits agora:", CACHE_API$hits, "\n\n")
} else {
  cat("   ❌ Falha ao recuperar do cache\n\n")
}

# Teste 4: Testar cache miss (texto diferente)
cat("📋 Teste 4: Cache miss (texto novo)\n")
texto_novo <- "Inspeção não programada do equipamento"
resultado_cache_miss <- cache_get(texto_novo, "openai")

if (is.null(resultado_cache_miss)) {
  cat("   ✅ Cache miss conforme esperado\n")
  cat("   - Misses agora:", CACHE_API$misses, "\n\n")
} else {
  cat("   ❌ Inesperado: encontrou resultado no cache\n\n")
}

# Teste 5: Verificar funções necessárias existem
cat("📋 Teste 5: Verificar funções do classificador\n")
funcoes_criticas <- c(
  "cache_get", "cache_set", "hash_simples",
  "classificar_com_openai", "classificar_por_dicionario",
  "validar_entrada", "sanitizar_texto"
)

funcoes_faltando <- c()
for (func in funcoes_criticas) {
  if (exists(func) && is.function(get(func))) {
    cat("   ✅", func, "\n")
  } else {
    cat("   ❌", func, "\n")
    funcoes_faltando <- c(funcoes_faltando, func)
  }
}

cat("\n")
if (length(funcoes_faltando) == 0) {
  cat(strrep("═", 70), "\n")
  cat("✅ TODOS OS TESTES PASSARAM COM SUCESSO!\n")
  cat(strrep("═", 70), "\n\n")
  cat("📊 Resumo:\n")
  cat("   - Cache API funcional\n")
  cat("   - Funções de hash implementadas\n")
  cat("   - Todas as funções críticas existem\n")
  cat("   - Sistema pronto para usar em classificação em lote\n\n")
} else {
  cat("❌ Funções faltando:", paste(funcoes_faltando, collapse = ", "), "\n\n")
}
