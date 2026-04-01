# ============================================================================
# SISTEMA DE VALIDAÇÃO E MODELO TREINADO - VERSÃO CORRIGIDA
# ============================================================================
# 
# Este arquivo contém a estrutura CORRETA para validações e modelo treinado
# que funciona TANTO no console quanto no Shiny
#

# ============================================================================
# ✅ VERSÃO 1: PARA USAR FORA DO SHINY (Console/Scripts)
# ============================================================================

# Use ESTA estrutura se você está testando fora do servidor Shiny:

validacoes_modelo <- list(
  dados = data.frame(
    id = character(),
    texto_completo = character(),
    assunto_original = character(),
    assunto_validado = character(),
    tipo_original = integer(),
    tipo_validado = integer(),
    tipo_ia = integer(),
    confianca_original = numeric(),
    metodo_original = character(),
    usuario = character(),
    timestamp = as.POSIXct(character()),
    feedback_qualidade = character(),
    observacoes = character(),
    stringsAsFactors = FALSE
  ),
  modelo_ativo = NULL,
  metricas = list(
    acuracia = 0,
    total_treinos = 0,
    ultima_atualizacao = NULL,
    features_importantes = character(0)
  ),
  historico = list(),
  configuracoes = list(
    min_validacoes = 10,
    usar_em_classificacao = TRUE,
    taxa_treino = 0.8
  )
)

cat("✅ Sistema de validações inicializado (versão console)\n")

# ============================================================================
# ✅ VERSÃO 2: PARA USAR DENTRO DO SHINY SERVER
# ============================================================================

# Use ESTA estrutura DENTRO da função server() do Shiny:

# Adicione isto DENTRO do server <- function(input, output, session) { ... }

validacoes_modelo <- reactiveValues(
  dados = data.frame(
    id = character(),
    texto_completo = character(),
    assunto_original = character(),
    assunto_validado = character(),
    tipo_original = integer(),
    tipo_validado = integer(),
    tipo_ia = integer(),
    confianca_original = numeric(),
    metodo_original = character(),
    usuario = character(),
    timestamp = as.POSIXct(character()),
    feedback_qualidade = character(),
    observacoes = character(),
    stringsAsFactors = FALSE
  ),
  modelo_ativo = NULL,
  metricas = list(
    acuracia = 0,
    total_treinos = 0,
    ultima_atualizacao = NULL,
    features_importantes = character(0)
  ),
  historico = list(),
  configuracoes = list(
    min_validacoes = 10,
    usar_em_classificacao = TRUE,
    taxa_treino = 0.8
  )
)

cat("✅ Sistema de validações inicializado (versão Shiny)\n")

# ============================================================================
# FUNÇÕES PARA TRABALHAR COM VALIDAÇÕES
# (Funciona em AMBOS os contextos - console e Shiny)
# ============================================================================

#' Adicionar uma validação
#' @param id ID único da validação
#' @param texto_completo Texto original
#' @param tipo_original Tipo original do sistema
#' @param tipo_validado Tipo após validação do usuário
#' @param tipo_ia Tipo classificado pela IA
#' @param confianca Confiança da IA (0-100)
#' @param feedback Feedback do usuário (excelente/boa/ruim)

adicionar_validacao <- function(
  id, 
  texto_completo, 
  tipo_original,
  tipo_validado,
  tipo_ia,
  confianca,
  feedback = "boa",
  usuario = "sistema",
  observacoes = ""
) {
  
  nova_validacao <- data.frame(
    id = as.character(id),
    texto_completo = as.character(texto_completo),
    assunto_original = substr(texto_completo, 1, 80),
    assunto_validado = substr(texto_completo, 1, 80),
    tipo_original = as.integer(tipo_original),
    tipo_validado = as.integer(tipo_validado),
    tipo_ia = as.integer(tipo_ia),
    confianca_original = as.numeric(confianca),
    metodo_original = "HIBRIDO",
    usuario = as.character(usuario),
    timestamp = Sys.time(),
    feedback_qualidade = as.character(feedback),
    observacoes = as.character(observacoes),
    stringsAsFactors = FALSE
  )
  
  # Adicionar aos dados
  validacoes_modelo$dados <- rbind(validacoes_modelo$dados, nova_validacao)
  
  cat("✅ Validação adicionada. Total:", nrow(validacoes_modelo$dados), "\n")
  
  return(nrow(validacoes_modelo$dados))
}

#' Calcular acurácia do modelo
calcular_acuracia <- function() {
  
  if(nrow(validacoes_modelo$dados) == 0) {
    return(0)
  }
  
  dados <- validacoes_modelo$dados
  
  acertos <- sum(
    dados$tipo_ia == dados$tipo_validado,
    na.rm = TRUE
  )
  
  total <- sum(!is.na(dados$tipo_ia) & !is.na(dados$tipo_validado))
  
  if(total == 0) return(0)
  
  acuracia <- (acertos / total) * 100
  
  return(round(acuracia, 2))
}

#' Obter estatísticas das validações
obter_stats_validacoes <- function() {
  
  if(nrow(validacoes_modelo$dados) == 0) {
    return(list(
      total = 0,
      acertos_ia = 0,
      erros_ia = 0,
      acuracia = 0,
      feedback_bom = 0,
      feedback_ruim = 0
    ))
  }
  
  dados <- validacoes_modelo$dados
  
  total <- nrow(dados)
  acertos <- sum(dados$tipo_ia == dados$tipo_validado, na.rm = TRUE)
  erros <- total - acertos
  acuracia <- calcular_acuracia()
  
  feedback_bom <- sum(dados$feedback_qualidade %in% c("excelente", "boa"), na.rm = TRUE)
  feedback_ruim <- sum(dados$feedback_qualidade == "ruim", na.rm = TRUE)
  
  return(list(
    total = total,
    acertos_ia = acertos,
    erros_ia = erros,
    acuracia = acuracia,
    feedback_bom = feedback_bom,
    feedback_ruim = feedback_ruim,
    taxa_satisfacao = if(total > 0) round((feedback_bom / total) * 100, 1) else 0
  ))
}

#' Salvar validações em arquivo
salvar_validacoes <- function(arquivo = "validacoes_modelo.rds") {
  
  tryCatch({
    saveRDS(validacoes_modelo, arquivo, compress = "xz")
    cat("✅ Validações salvas em:", arquivo, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Erro ao salvar validações:", e$message, "\n")
    return(FALSE)
  })
}

#' Carregar validações de arquivo
carregar_validacoes <- function(arquivo = "validacoes_modelo.rds") {
  
  if(!file.exists(arquivo)) {
    cat("⚠️ Arquivo de validações não encontrado:", arquivo, "\n")
    return(FALSE)
  }
  
  tryCatch({
    dados_carregados <- readRDS(arquivo)
    validacoes_modelo$dados <- dados_carregados$dados
    validacoes_modelo$metricas <- dados_carregados$metricas
    validacoes_modelo$historico <- dados_carregados$historico
    if(!is.null(dados_carregados$configuracoes)) {
      validacoes_modelo$configuracoes <- dados_carregados$configuracoes
    }
    cat("✅ Validações carregadas. Total:", nrow(validacoes_modelo$dados), "\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Erro ao carregar validações:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================================
# EXEMPLOS DE USO
# ============================================================================

# Exemplo 1: Adicionar uma validação
# adicionar_validacao(
#   id = "NOTA_001",
#   texto_completo = "Substituição de válvula por falha operacional",
#   tipo_original = 5,
#   tipo_validado = 5,
#   tipo_ia = 5,
#   confianca = 85.5,
#   feedback = "boa",
#   usuario = "analista_01",
#   observacoes = "Classificação correta"
# )

# Exemplo 2: Obter estatísticas
# stats <- obter_stats_validacoes()
# print(stats)

# Exemplo 3: Salvar validações
# salvar_validacoes("meu_modelo_validacoes.rds")

# Exemplo 4: Carregar validações
# carregar_validacoes("meu_modelo_validacoes.rds")

# ============================================================================
# INTEGRAÇÃO COM O CLASSIFICADOR VERSÃO 14
# ============================================================================

# Se você está usando dentro do CLASSIFICADOR_VERSAO14.R, adicione isto
# DENTRO da função server():

# server <- function(input, output, session) {
#   
#   # Inicializar validações (DENTRO do server)
#   validacoes_modelo <- reactiveValues(
#     dados = data.frame(...),  # Ver acima
#     modelo_ativo = NULL,
#     metricas = list(...),      # Ver acima
#     historico = list(),
#     configuracoes = list(...)  # Ver acima
#   )
#   
#   # Carregar validações salvas anteriormente
#   observe({
#     resultado <- carregar_validacoes("validacoes_modelo.rds")
#   })
#   
#   # Botão para adicionar validação
#   observeEvent(input$btn_adicionar_validacao, {
#     adicionar_validacao(
#       id = input$id_validacao,
#       texto_completo = input$texto_validacao,
#       tipo_original = as.integer(input$tipo_original),
#       tipo_validado = as.integer(input$tipo_validado),
#       tipo_ia = as.integer(input$tipo_ia),
#       confianca = as.numeric(input$confianca),
#       feedback = input$feedback,
#       usuario = input$usuario
#     )
#     
#     # Atualizar UI
#     output$total_validacoes <- renderText({
#       stats <- obter_stats_validacoes()
#       paste("Total:", stats$total, "| Acurácia:", stats$acuracia, "%")
#     })
#   })
#   
#   # Botão para salvar
#   observeEvent(input$btn_salvar_validacoes, {
#     salvar_validacoes("validacoes_modelo.rds")
#     showNotification("✅ Validações salvas!", type = "message")
#   })
# }
