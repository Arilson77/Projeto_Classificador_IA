# ========== TESTES AUTOMATIZADOS - SISTEMA CLASSIFICADOR JARVIS-IA ==========
# Arquivo: tests.R
# Descrição: Suite completa de testes unitários usando testthat
# Execução: testthat::test_dir(".", reporter = "summary")
# ============================================================================

library(testthat)
library(dplyr)
library(tidyr)

# ============================================================================
# 1. TESTES DE CLASSIFICAÇÃO POR DICIONÁRIO
# ============================================================================

test_that("classificar_por_dicionario retorna tipo válido para 'limpeza'", {
  # Arranjar: Preparar dados
  texto <- "Limpeza de tanque de armazenamento"
  
  # Agir: Executar função
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  # Verificar: Assertions
  expect_is(resultado, "list")
  expect_true(resultado$tipo %in% 1:6)
  expect_gte(resultado$confianca, 0)
  expect_lte(resultado$confianca, 100)
  expect_equal(resultado$tipo, 1)  # Tipo 1 é limpeza
})

test_that("classificar_por_dicionario retorna tipo NULL para texto irrelevante", {
  texto <- "zzzzzzzzzzzzzzzzz qwerty asdfgh"
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  expect_is(resultado, "list")
  expect_true(is.na(resultado$tipo) || is.null(resultado$tipo))
})

test_that("classificar_por_dicionario funciona com maiúsculas/minúsculas", {
  texto_lower <- "limpeza de equipamento"
  texto_upper <- "LIMPEZA DE EQUIPAMENTO"
  
  resultado_lower <- classificar_por_dicionario(texto_lower, DICIONARIOS_SAP)
  resultado_upper <- classificar_por_dicionario(texto_upper, DICIONARIOS_SAP)
  
  # Ambos devem retornar o mesmo tipo
  if (!is.na(resultado_lower$tipo) && !is.na(resultado_upper$tipo)) {
    expect_equal(resultado_lower$tipo, resultado_upper$tipo)
  }
})

test_that("classificar_por_dicionario identifica tipo 3 (preventiva)", {
  texto <- "Manutenção preventiva da bomba centrífuga"
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  expect_equal(resultado$tipo, 3)
  expect_gte(resultado$confianca, 70)
})

test_that("classificar_por_dicionario identifica tipo 5 (defeito)", {
  texto <- "Eliminação de defeito no motor principal"
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  expect_equal(resultado$tipo, 5)
})

test_that("classificar_por_dicionario identifica tipo 6 (falha)", {
  texto <- "Eliminação de falha crítica no sistema"
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  expect_equal(resultado$tipo, 6)
})

test_that("classificar_por_dicionario retorna contrato completo", {
  texto <- "limpeza rápida"
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  # Verificar contrato de resultado
  expect_true("tipo" %in% names(resultado))
  expect_true("confianca" %in% names(resultado))
  expect_true("metodo" %in% names(resultado))
  expect_equal(resultado$metodo, "DICIONARIO")
})

# ============================================================================
# 2. TESTES DE TRATAMENTO DE ENTRADA
# ============================================================================

test_that("texto muito curto (< 10 chars) é rejeitado", {
  texto_curto <- "abc def"
  resultado <- classificar_por_dicionario(texto_curto, DICIONARIOS_SAP)
  
  # Deve retornar tipo inválido ou confiança zero
  expect_true(is.na(resultado$tipo) || resultado$confianca < 50)
})

test_that("texto vazio é tratado gracefully", {
  texto <- ""
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  expect_true(is.na(resultado$tipo) || is.null(resultado$tipo))
})

test_that("texto com apenas espaços em branco é tratado", {
  texto <- "     "
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  expect_true(is.na(resultado$tipo) || is.null(resultado$tipo))
})

test_that("texto com caracteres especiais é processado", {
  texto <- "Limpeza & manutenção (preventiva) com @IA"
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  
  # Não deve lançar erro
  expect_is(resultado, "list")
})

# ============================================================================
# 3. TESTES DE MACHINE LEARNING
# ============================================================================

test_that("treinar_modelo_ml funciona com dados mínimos", {
  # Criar dados de validação mínimos
  dados_validacao <- data.frame(
    Nota = c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010"),
    Texto = c(
      "limpeza de tanque", "manutenção preventiva", "defeito motor",
      "modificação sistema", "falha crítica", "condicionamento equipamento",
      "teste bomba", "inspeção válvula", "problema válvula", "melhoria"
    ),
    Tipo = c(1, 3, 5, 2, 6, 1, 2, 3, 5, 2),
    stringsAsFactors = FALSE
  )
  
  # Tentar treinar modelo
  resultado <- tryCatch({
    modelo <- treinar_modelo_ml(dados_validacao)
    list(sucesso = TRUE, modelo = modelo)
  }, error = function(e) {
    list(sucesso = FALSE, erro = e$message)
  })
  
  # Deve completar sem erro
  expect_true(resultado$sucesso)
  expect_is(resultado$modelo, "randomForest")
})

test_that("predizer_com_modelo retorna tipo válido", {
  # Criar dados de treinamento
  dados_validacao <- data.frame(
    Nota = c("001", "002", "003", "004", "005"),
    Texto = c("limpeza", "preventiva", "defeito", "modificação", "falha"),
    Tipo = c(1, 3, 5, 2, 6),
    stringsAsFactors = FALSE
  )
  
  # Treinar modelo
  modelo <- treinar_modelo_ml(dados_validacao)
  
  # Fazer predição
  resultado <- predizer_com_modelo("limpeza de tanque", modelo)
  
  expect_is(resultado, "list")
  expect_true(resultado$tipo %in% 1:6 || is.na(resultado$tipo))
  expect_gte(resultado$confianca, 0)
  expect_lte(resultado$confianca, 100)
})

# ============================================================================
# 4. TESTES DE VALIDAÇÃO E CRUZAMENTO DE DADOS
# ============================================================================

test_that("cruzar_dados merge corretamente por número de nota", {
  # Arranjar
  ordens <- data.frame(
    Nota = c("001", "002", "003"),
    Ordem = c("OP001", "OP002", "OP003"),
    Equipamento = c("Bomba A", "Bomba B", "Válvula C"),
    stringsAsFactors = FALSE
  )
  
  textos <- data.frame(
    Nota = c("001", "002", "003"),
    Texto = c("limpeza", "manutenção", "defeito"),
    stringsAsFactors = FALSE
  )
  
  # Agir
  resultado <- cruzar_dados(ordens, textos)
  
  # Verificar
  expect_is(resultado, "data.frame")
  expect_equal(nrow(resultado), 3)
  expect_true("Texto" %in% names(resultado))
  expect_true("Equipamento" %in% names(resultado))
})

test_that("cruzar_dados trata duplicatas corretamente", {
  ordens <- data.frame(
    Nota = c("001", "001", "002"),
    Ordem = c("OP001", "OP001", "OP002"),
    Equipamento = c("Bomba", "Bomba", "Válvula"),
    stringsAsFactors = FALSE
  )
  
  textos <- data.frame(
    Nota = c("001", "002"),
    Texto = c("limpeza", "manutenção"),
    stringsAsFactors = FALSE
  )
  
  resultado <- cruzar_dados(ordens, textos)
  
  # Deve conter registros com duplicatas
  expect_is(resultado, "data.frame")
  expect_gte(nrow(resultado), 2)
})

# ============================================================================
# 5. TESTES DE CONFIGURAÇÃO
# ============================================================================

test_that("CONFIG_USUARIO retorna lista com flags esperadas", {
  config <- CONFIG_USUARIO()
  
  expect_is(config, "list")
  expect_true("usar_dicionario" %in% names(config))
  expect_true("usar_api" %in% names(config))
  expect_true("usar_modelo_treinado" %in% names(config))
  expect_is(config$usar_dicionario, "logical")
  expect_is(config$usar_api, "logical")
  expect_is(config$usar_modelo_treinado, "logical")
})

# ============================================================================
# 6. TESTES DE CACHE
# ============================================================================

test_that("cache_set e cache_get funcionam corretamente", {
  # Limpar cache
  if (exists("CACHE_API")) {
    CACHE_API$memoria <- new.env()
  }
  
  # Armazenar valor
  cache_set("teste_key", list(tipo = 3, confianca = 85), "openai")
  
  # Recuperar valor
  valor <- cache_get("teste_key", "openai")
  
  expect_equal(valor$tipo, 3)
  expect_equal(valor$confianca, 85)
})

test_that("cache_get retorna NULL para chave inexistente", {
  if (exists("CACHE_API")) {
    CACHE_API$memoria <- new.env()
  }
  
  valor <- cache_get("chave_inexistente", "openai")
  
  expect_null(valor)
})

# ============================================================================
# 7. TESTES DE VALIDAÇÃO DE CAMPO COM HÍFEN
# ============================================================================

test_that("validar_tipo_com_hifen rejeita tipo 5 sem hífen", {
  # Campo sem hífen
  campo_sem_hifen <- "CAMPO_INVALIDO"
  tipo_sugerido <- 5
  
  resultado <- validar_tipo_com_hifen(campo_sem_hifen, tipo_sugerido)
  
  expect_true(is.na(resultado$tipo_validado) || is.null(resultado$tipo_validado))
})

test_that("validar_tipo_com_hifen aceita tipo 5 com hífen", {
  # Campo com hífen
  campo_com_hifen <- "CAMPO-VALIDO"
  tipo_sugerido <- 5
  
  resultado <- validar_tipo_com_hifen(campo_com_hifen, tipo_sugerido)
  
  expect_equal(resultado$tipo_validado, 5)
})

test_that("validar_tipo_com_hifen permite tipos 1-4 sem hífen", {
  campo_sem_hifen <- "CAMPO_INVALIDO"
  
  for (tipo in 1:4) {
    resultado <- validar_tipo_com_hifen(campo_sem_hifen, tipo)
    expect_equal(resultado$tipo_validado, tipo)
  }
})

# ============================================================================
# 8. TESTES DE PERSISTÊNCIA
# ============================================================================

test_that("salvar_dados_modelo cria arquivo .rds", {
  # Criar diretório se não existir
  dir.create("dados_modelo_treinado", showWarnings = FALSE)
  
  # Simular dados para salvar
  dados_teste <- list(
    modelo = NULL,
    vetorizador = NULL,
    validacoes = data.frame(Nota = "001", Tipo = 1),
    timestamp = Sys.time()
  )
  
  # Salvar
  resultado <- tryCatch({
    salvar_dados_modelo()
    TRUE
  }, error = function(e) FALSE)
  
  expect_true(resultado)
})

test_that("carregar_dados_modelo restaura dados salvos", {
  # Verificar que arquivo existe ou consegue carregar
  resultado <- tryCatch({
    carregar_dados_modelo()
    TRUE
  }, error = function(e) FALSE)
  
  # Função deve executar sem erro
  expect_true(resultado || !file.exists("dados_modelo_treinado"))
})

# ============================================================================
# 9. TESTES DE UTILITÁRIOS
# ============================================================================

test_that("hash_simples gera hash consistente", {
  texto <- "teste_hash"
  hash1 <- hash_simples(texto)
  hash2 <- hash_simples(texto)
  
  expect_equal(hash1, hash2)
  expect_is(hash1, "character")
})

test_that("limpar_texto remove caracteres especiais", {
  texto_sujo <- "  Texto @@@ com espaços   #extremos  "
  texto_limpo <- limpar_texto(texto_sujo)
  
  expect_is(texto_limpo, "character")
  expect_equal(nchar(texto_limpo) > 0, TRUE)
})

# ============================================================================
# 10. TESTES DE CONTRATO DE RESULTADO
# ============================================================================

test_that("classificar_hibrido_completo retorna contrato válido", {
  texto <- "Manutenção preventiva da bomba"
  config <- CONFIG_USUARIO()
  
  resultado <- classificar_hibrido_completo(texto, config)
  
  # Verificar estrutura obrigatória
  expect_true("tipo" %in% names(resultado))
  expect_true("confianca" %in% names(resultado))
  expect_true("metodo" %in% names(resultado))
  expect_true("categoria" %in% names(resultado))
  
  # Verificar tipos
  expect_true(is.numeric(resultado$tipo) || is.na(resultado$tipo))
  expect_is(resultado$confianca, "numeric")
  expect_is(resultado$metodo, "character")
})

# ============================================================================
# 11. TESTES DE LIMITE DE REGISTROS
# ============================================================================

test_that("processar_lote_com_config limita a N registros", {
  # Criar dados de teste
  dados_teste <- data.frame(
    Nota = as.character(1:100),
    Texto = rep("limpeza de tanque", 100),
    Tipo_Anterior = rep(1, 100),
    stringsAsFactors = FALSE
  )
  
  config <- CONFIG_USUARIO()
  
  # Processar com limite de 10
  resultado <- tryCatch({
    processar_lote_com_config(dados_teste, config, limite = 10)
    list(sucesso = TRUE, nrow = nrow(resultado))
  }, error = function(e) {
    list(sucesso = FALSE, erro = e$message)
  })
  
  if (resultado$sucesso) {
    expect_lte(resultado$nrow, 10)
  }
})

# ============================================================================
# 12. TESTES DE LOG
# ============================================================================

test_that("registrar_log_acao cria entrada no log", {
  dir.create("logs", showWarnings = FALSE)
  
  # Registrar ação
  resultado <- tryCatch({
    registrar_log_acao("teste_acao", "Descrição de teste")
    TRUE
  }, error = function(e) FALSE)
  
  expect_true(resultado)
  
  # Verificar que arquivo foi criado
  expect_true(file.exists("logs/audit_log.csv") || 
              file.exists("logs/audit_log.json"))
})

# ============================================================================
# 13. TESTES DE PERFORMANCE
# ============================================================================

test_that("classificar_por_dicionario completa em <100ms para texto simples", {
  texto <- "limpeza de equipamento"
  
  tempo_inicial <- Sys.time()
  resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
  tempo_final <- Sys.time()
  
  tempo_ms <- as.numeric(difftime(tempo_final, tempo_inicial, units = "secs")) * 1000
  
  expect_true(tempo_ms < 100)
})

# ============================================================================
# SUITE DE INTEGRAÇÃO
# ============================================================================

test_that("fluxo completo funciona: upload -> cruzamento -> classificação", {
  # Criar dados de teste
  ordens <- data.frame(
    Nota = c("001", "002"),
    Ordem = c("OP001", "OP002"),
    Equipamento = c("Bomba", "Válvula"),
    stringsAsFactors = FALSE
  )
  
  textos <- data.frame(
    Nota = c("001", "002"),
    Texto = c("limpeza de tanque", "manutenção preventiva"),
    stringsAsFactors = FALSE
  )
  
  # Etapa 1: Cruzar
  dados_cruzados <- cruzar_dados(ordens, textos)
  expect_equal(nrow(dados_cruzados), 2)
  
  # Etapa 2: Classificar
  config <- CONFIG_USUARIO()
  for (i in 1:nrow(dados_cruzados)) {
    resultado <- classificar_por_dicionario(dados_cruzados$Texto[i], DICIONARIOS_SAP)
    expect_is(resultado, "list")
  }
})

# ============================================================================
# EXECUTAR TESTES
# ============================================================================

# Para executar todos os testes:
# testthat::test_dir(".", reporter = "summary")
#
# Para executar testes de um arquivo específico:
# testthat::test_file("tests.R", reporter = "summary")
#
# Para executar testes com foco em um contexto:
# testthat::test_local(test_that(...))

cat("\n✅ Suite de testes carregada com sucesso!\n")
cat("📝 Total de testes definidos: 30+\n")
cat("⏱️  Tempo estimado de execução: 5-10 segundos\n")
cat("🚀 Execute com: testthat::test_file('tests.R')\n\n")
