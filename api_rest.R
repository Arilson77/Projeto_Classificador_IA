# ========== API REST - JARVIS-IA ==========
# Arquivo: api_rest.R
# Descrição: API REST para integração com outros sistemas
# Execução: plumber::plumb_run("api_rest.R", port = 8000)
# =============================================

#' @apiTitle JARVIS-IA API
#' @apiDescription API REST para sistema de classificação SAP
#' @apiVersion 1.0.0
#' @apiContact support@petrobras.com.br

# =============================================
# 1. MIDDLEWARE E AUTENTICAÇÃO
# =============================================

#' @filter authentication
function(req, res) {
  # Verificar token de autenticação
  auth_header <- req$HTTP_AUTHORIZATION
  
  if (is.null(auth_header)) {
    # Rotas públicas que não precisam de autenticação
    public_routes <- c("/health", "/version")
    
    if (!any(sapply(public_routes, function(x) grepl(x, req$PATH_INFO)))) {
      res$status <- 401
      return(list(error = "Não autorizado. Token de autenticação obrigatório."))
    }
  } else {
    # Validar token
    token <- gsub("Bearer ", "", auth_header)
    if (!validar_token(token)) {
      res$status <- 401
      return(list(error = "Token inválido ou expirado"))
    }
  }
  
  plumber::forward()
}

#' @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#' @filter logging
function(req, res) {
  start_time <- Sys.time()
  
  # Executar request
  result <- plumber::forward()
  
  # Logar
  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  log_api_call(
    endpoint = req$PATH_INFO,
    method = req$REQUEST_METHOD,
    status_code = res$status,
    response_time_ms = duration * 1000
  )
  
  return(result)
}

# =============================================
# 2. ENDPOINTS BÁSICOS
# =============================================

#' Health Check
#' 
#' Verifica se o serviço está funcionando
#'
#' @get /health
#' @response 200 Serviço está funcionando
function() {
  return(list(
    status = "ok",
    timestamp = Sys.time(),
    version = "1.0.0"
  ))
}

#' Informações da Versão
#' 
#' Retorna informações sobre a versão do sistema
#'
#' @get /version
function() {
  return(list(
    sistema = "JARVIS-IA",
    versao = "14.2",
    data_lancamento = "2025-01-15",
    ambiente = Sys.getenv("ENVIRONMENT", "production")
  ))
}

# =============================================
# 3. ENDPOINTS DE CLASSIFICAÇÃO
# =============================================

#' Classificar Texto Único
#' 
#' Classifica um texto usando o sistema híbrido
#'
#' @post /api/v1/classificar
#' @param texto:string O texto a ser classificado
#' @param usar_cache:logical Usar cache da API (padrão: TRUE)
#' @response 200 Classificação realizada com sucesso
#' @response 400 Parâmetros inválidos
#' @response 500 Erro ao classificar
#'
function(texto, usar_cache = TRUE) {
  tryCatch({
    # Validação
    if (missing(texto) || nchar(trimws(texto)) < 10) {
      return(list(
        erro = "Parâmetro 'texto' inválido (mínimo 10 caracteres)",
        sucesso = FALSE
      ))
    }
    
    # Obter configuração
    config <- CONFIG_USUARIO()
    
    # Classificar
    resultado <- classificar_hibrido_completo(texto, config)
    
    # Enriquecer resultado
    resultado$timestamp <- Sys.time()
    resultado$texto_hash <- hash_simples(texto)
    resultado$versao_modelo <- "14.2"
    
    return(list(
      sucesso = TRUE,
      dados = resultado
    ))
    
  }, error = function(e) {
    log_error("Erro ao classificar via API", "api", e)
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

#' Classificar em Lote
#' 
#' Classifica múltiplos textos
#'
#' @post /api/v1/classificar-lote
#' @param textos:string Vetor JSON de textos
#' @param limite:integer Máximo de textos a processar (padrão: 100)
#' @response 200 Classificação realizada
#' @response 400 Parâmetros inválidos
#' @response 413 Lote muito grande
#'
function(textos, limite = 100) {
  tryCatch({
    # Parsear JSON se necessário
    if (is.character(textos)) {
      textos_parsed <- jsonlite::fromJSON(textos)
    } else {
      textos_parsed <- textos
    }
    
    # Validação
    if (!is.character(textos_parsed) && !is.list(textos_parsed)) {
      return(list(erro = "Formato de 'textos' inválido", sucesso = FALSE))
    }
    
    if (length(textos_parsed) > limite) {
      return(list(
        erro = paste("Lote excede limite de", limite, "registros"),
        sucesso = FALSE
      ))
    }
    
    # Processar
    config <- CONFIG_USUARIO()
    resultados <- lapply(textos_parsed, function(texto) {
      tryCatch({
        classificar_hibrido_completo(texto, config)
      }, error = function(e) {
        list(tipo = NA, confianca = 0, erro = e$message)
      })
    })
    
    return(list(
      sucesso = TRUE,
      total_processados = length(resultados),
      dados = resultados,
      timestamp = Sys.time()
    ))
    
  }, error = function(e) {
    log_error("Erro ao classificar lote via API", "api", e)
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

# =============================================
# 4. ENDPOINTS DE MODELO ML
# =============================================

#' Obter Informações do Modelo
#' 
#' Retorna status e métricas do modelo ML
#'
#' @get /api/v1/modelo/info
#' @response 200 Informações do modelo
#'
function() {
  tryCatch({
    garantir_modelo_carregado()
    
    info <- list(
      modelo_presente = !is.null(values$modelo_treinado),
      total_validacoes = nrow(values$dados_cruzados),
      timestamp_ultimo_treinamento = attr(values$modelo_treinado, "timestamp"),
      acuracia_estimada = attr(values$modelo_treinado, "acuracia")
    )
    
    return(list(
      sucesso = TRUE,
      modelo = info
    ))
    
  }, error = function(e) {
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

#' Treinar Modelo Incrementalmente
#' 
#' Treina o modelo com novos dados
#'
#' @post /api/v1/modelo/treinar
#' @response 200 Treinamento iniciado
#' @response 400 Dados insuficientes
#'
function() {
  tryCatch({
    # Validar dados
    if (is.null(values$dados_cruzados) || nrow(values$dados_cruzados) < 10) {
      return(list(
        sucesso = FALSE,
        erro = "Mínimo 10 validações necessárias"
      ))
    }
    
    # Treinar
    modelo_novo <- treinar_modelo_ml(values$dados_cruzados)
    values$modelo_treinado <<- modelo_novo
    salvar_dados_modelo()
    
    return(list(
      sucesso = TRUE,
      mensagem = "Modelo treinado com sucesso"
    ))
    
  }, error = function(e) {
    log_error("Erro ao treinar modelo via API", "api", e)
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

# =============================================
# 5. ENDPOINTS DE DADOS
# =============================================

#' Obter Últimas Classificações
#' 
#' Retorna as N últimas classificações
#'
#' @get /api/v1/dados/ultimas
#' @param limite:integer Número de registros (padrão: 10, máx: 100)
#' @response 200 Classificações encontradas
#'
function(limite = 10) {
  tryCatch({
    limite <- min(as.integer(limite), 100)
    
    if (is.null(values$dados_cruzados) || nrow(values$dados_cruzados) == 0) {
      return(list(
        sucesso = FALSE,
        erro = "Sem dados disponíveis"
      ))
    }
    
    dados <- tail(values$dados_cruzados, limite)
    
    return(list(
      sucesso = TRUE,
      total = nrow(dados),
      dados = jsonlite::toJSON(dados, auto_unbox = TRUE)
    ))
    
  }, error = function(e) {
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

#' Obter Estatísticas
#' 
#' Retorna estatísticas gerais do sistema
#'
#' @get /api/v1/dados/estatisticas
#' @response 200 Estatísticas disponíveis
#'
function() {
  tryCatch({
    if (is.null(values$dados_cruzados)) {
      return(list(
        sucesso = FALSE,
        erro = "Sem dados para calcular estatísticas"
      ))
    }
    
    stats <- list(
      total_registros = nrow(values$dados_cruzados),
      tipos_distribuicao = table(values$dados_cruzados$Tipo_Sugerido),
      confianca_media = mean(values$dados_cruzados$Confianca, na.rm = TRUE),
      conformidade = sum(values$dados_cruzados$Tipo_Anterior == values$dados_cruzados$Tipo_Sugerido) / 
                     nrow(values$dados_cruzados)
    )
    
    return(list(
      sucesso = TRUE,
      estatisticas = stats
    ))
    
  }, error = function(e) {
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

# =============================================
# 6. ENDPOINTS DE VALIDAÇÃO
# =============================================

#' Registrar Validação
#' 
#' Registra validação de um texto classificado
#'
#' @post /api/v1/validacao/registrar
#' @param nota:string Número da nota
#' @param tipo_validado:integer Tipo validado (1-6)
#' @response 201 Validação registrada
#' @response 400 Parâmetros inválidos
#'
function(nota, tipo_validado) {
  tryCatch({
    # Validação
    if (missing(nota) || missing(tipo_validado)) {
      return(list(
        sucesso = FALSE,
        erro = "Parâmetros obrigatórios: nota, tipo_validado"
      ))
    }
    
    tipo_validado <- as.integer(tipo_validado)
    
    if (!(tipo_validado %in% 1:6)) {
      return(list(
        sucesso = FALSE,
        erro = "tipo_validado deve estar entre 1 e 6"
      ))
    }
    
    # Registrar validação
    salvar_validacao_ml(nota, tipo_validado)
    
    # Treinar se atingiu 5 validações
    if (nrow(values$dados_cruzados) %% 5 == 0) {
      treinar_modelo_ml_incremental()
    }
    
    log_audit(
      action = "VALIDACAO_REGISTRADA",
      resource_type = "NOTA",
      resource_id = nota,
      new_value = tipo_validado,
      status = "SUCCESS"
    )
    
    return(list(
      sucesso = TRUE,
      mensagem = "Validação registrada com sucesso"
    ))
    
  }, error = function(e) {
    log_error("Erro ao registrar validação via API", "api", e)
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

# =============================================
# 7. ENDPOINTS DE DICIONÁRIO
# =============================================

#' Obter Dicionários Disponíveis
#' 
#' Retorna os 6 tipos de classificação
#'
#' @get /api/v1/dicionarios
#' @response 200 Dicionários carregados
#'
function() {
  tryCatch({
    tipos <- lapply(names(DICIONARIOS_SAP), function(tipo_name) {
      tipo <- DICIONARIOS_SAP[[tipo_name]]
      list(
        tipo_id = sub("tipo_", "", tipo_name),
        descricao = tipo$descricao,
        categoria = tipo$categoria_principal,
        criticidade = tipo$criticidade,
        palavras_chave = tipo$palavras_chave
      )
    })
    
    return(list(
      sucesso = TRUE,
      total_tipos = length(tipos),
      tipos = tipos
    ))
    
  }, error = function(e) {
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

# =============================================
# 8. ENDPOINTS DE STATUS E ADMIN
# =============================================

#' Status do Sistema
#' 
#' Retorna informações de status geral
#'
#' @get /api/v1/admin/status
#' @response 200 Status disponível
#'
function() {
  tryCatch({
    status_data <- list(
      uptime = Sys.time(),
      memoria_usada_mb = as.numeric(object.size(values)) / (1024^2),
      cache_hits = if (exists("CACHE_API")) CACHE_API$hits else 0,
      cache_misses = if (exists("CACHE_API")) CACHE_API$misses else 0,
      modelo_treinado = !is.null(values$modelo_treinado),
      total_registros = if (!is.null(values$dados_cruzados)) nrow(values$dados_cruzados) else 0
    )
    
    return(list(
      sucesso = TRUE,
      status = status_data
    ))
    
  }, error = function(e) {
    return(list(
      sucesso = FALSE,
      erro = e$message
    ))
  })
}

# =============================================
# 9. FUNÇÕES UTILITÁRIAS
# =============================================

validar_token <- function(token) {
  # Validação simples (implementar com JWT/OAuth em produção)
  token_valido <- Sys.getenv("API_TOKEN")
  if (is.null(token_valido) || !nzchar(trimws(token_valido))) {
    return(FALSE)
  }

  if (is.null(token) || !nzchar(trimws(token))) {
    return(FALSE)
  }

  return(identical(trimws(token), trimws(token_valido)))
}

hash_simples <- function(texto) {
  digest::digest(texto)
}

# =============================================
# 10. DOCUMENTAÇÃO
# =============================================

#' Documentação da API
#' 
#' Página de documentação da API
#'
#' @get /docs
#' @html
function() {
  html_content <- "
  <!DOCTYPE html>
  <html>
  <head>
    <title>JARVIS-IA API Documentation</title>
    <style>
      body { font-family: Arial, sans-serif; margin: 20px; }
      h1 { color: #667eea; }
      code { background: #f0f0f0; padding: 2px 5px; }
      .endpoint { margin: 20px 0; padding: 10px; border-left: 3px solid #667eea; }
    </style>
  </head>
  <body>
    <h1>📚 JARVIS-IA API Documentation</h1>
    
    <h2>Endpoints Principais</h2>
    
    <div class='endpoint'>
      <h3>POST /api/v1/classificar</h3>
      <p>Classifica um texto único</p>
      <p><strong>Parâmetros:</strong> texto (obrigatório), usar_cache (opcional)</p>
    </div>
    
    <div class='endpoint'>
      <h3>POST /api/v1/classificar-lote</h3>
      <p>Classifica múltiplos textos</p>
      <p><strong>Parâmetros:</strong> textos (JSON array), limite (opcional)</p>
    </div>
    
    <div class='endpoint'>
      <h3>GET /api/v1/modelo/info</h3>
      <p>Informações do modelo ML</p>
    </div>
    
    <div class='endpoint'>
      <h3>GET /api/v1/dados/estatisticas</h3>
      <p>Estatísticas gerais do sistema</p>
    </div>
    
    <div class='endpoint'>
      <h3>POST /api/v1/validacao/registrar</h3>
      <p>Registra validação de classificação</p>
      <p><strong>Parâmetros:</strong> nota, tipo_validado (1-6)</p>
    </div>
    
    <h2>Autenticação</h2>
    <p>Use header: <code>Authorization: Bearer YOUR_TOKEN</code></p>
    
    <h2>Exemplo de Uso</h2>
    <pre>
curl -X POST http://localhost:8000/api/v1/classificar \\
  -H \"Authorization: Bearer seu_token\" \\
  -d \"texto=Manutenção preventiva da bomba\"
    </pre>
  </body>
  </html>
  "
  return(html_content)
}

cat("\n✅ API REST carregada com sucesso!\n")
cat("📋 Documentação em: http://localhost:8000/docs\n")
cat("🚀 Para executar: plumber::plumb_run('api_rest.R', port = 8000)\n\n")
