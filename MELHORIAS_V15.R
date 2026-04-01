# ============================================================================
# ADICIONAR ESSAS LINHAS APÓS A LINHA 164 DO CLASSIFICADOR_VERSAO14.R
# ============================================================================

# Adicionar libraries (após library(tidyr)):
library(future)
library(promises)
library(parallel)

# Configurar processamento paralelo
n_cores <- parallel::detectCores() - 1
n_cores <- max(2, min(n_cores, 8))
tryCatch({
  plan(multisession, workers = n_cores)
  cat("⚡ Processamento paralelo:", n_cores, "workers\n")
}, error = function(e) {
  cat("⚠️ Processamento sequencial ativado\n")
  plan(sequential)
})

# ============================================================================
# SISTEMA DE MONITORAMENTO EM TEMPO REAL
# Adicionar após o bloco CACHE_API (linha ~240)
# ============================================================================

if (!exists("MONITOR_LOTE")) {
  MONITOR_LOTE <- reactiveValues(
    ativo = FALSE,
    total = 0,
    processados = 0,
    erros = 0,
    tempo_inicio = NULL,
    tempo_atual = 0,
    tempo_estimado = 0,
    velocidade = 0,
    historico_velocidade = numeric(),
    pode_cancelar = FALSE,
    cancelado = FALSE,
    resultados_parciais = NULL,
    arquivo_backup = NULL
  )
  cat("✅ Sistema de monitoramento em tempo real inicializado\n")
}

# ============================================================================
# SISTEMA DE EXPORTAÇÃO INCREMENTAL
# Adicionar após funções de sanitização (linha ~360)
# ============================================================================

exportar_incremental <- function(resultados, numero_lote, diretorio = "dados_processados") {
  
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, showWarnings = FALSE, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  arquivo_csv <- file.path(diretorio, paste0("lote_", numero_lote, "_", timestamp, ".csv"))
  arquivo_rds <- file.path(diretorio, paste0("lote_", numero_lote, "_", timestamp, ".rds"))
  
  tryCatch({
    write.csv(resultados, arquivo_csv, row.names = FALSE, fileEncoding = "UTF-8")
    saveRDS(resultados, arquivo_rds, compress = "xz")
    
    cat("💾 Lote", numero_lote, "exportado:", arquivo_csv, "\n")
    
    return(list(
      sucesso = TRUE,
      arquivo_csv = arquivo_csv,
      arquivo_rds = arquivo_rds,
      tempo = Sys.time()
    ))
  }, error = function(e) {
    cat("❌ Erro ao exportar lote", numero_lote, ":", e$message, "\n")
    return(list(sucesso = FALSE, erro = e$message))
  })
}

criar_arquivo_recuperacao <- function(estado, diretorio = "dados_processados") {
  
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, showWarnings = FALSE, recursive = TRUE)
  }
  
  arquivo_recuperacao <- file.path(diretorio, "recuperacao_atual.rds")
  
  tryCatch({
    saveRDS(estado, arquivo_recuperacao, compress = "xz")
    return(arquivo_recuperacao)
  }, error = function(e) {
    cat("⚠️ Aviso: Não foi possível criar arquivo de recuperação\n")
    return(NULL)
  })
}

recuperar_processamento <- function(diretorio = "dados_processados") {
  
  arquivo_recuperacao <- file.path(diretorio, "recuperacao_atual.rds")
  
  if (!file.exists(arquivo_recuperacao)) {
    return(NULL)
  }
  
  tryCatch({
    estado <- readRDS(arquivo_recuperacao)
    cat("🔄 Processamento anterior encontrado!\n")
    cat("   Registros processados:", nrow(estado$resultados_parciais), "\n")
    return(estado)
  }, error = function(e) {
    cat("⚠️ Erro ao recuperar processamento anterior\n")
    return(NULL)
  })
}

# ============================================================================
# SUBSTITUIR O LOOP DE PROCESSAMENTO EM LOTE
# Procure por: observeEvent(input$classificar_lote, { (por volta da linha 10800)
# E SUBSTITUA O TRECHO withProgress ... } pelo código abaixo:
# ============================================================================

# Inicializar monitoramento
MONITOR_LOTE$ativo <- TRUE
MONITOR_LOTE$total <- total
MONITOR_LOTE$processados <- 0
MONITOR_LOTE$tempo_inicio <- Sys.time()
MONITOR_LOTE$cancelado <- FALSE

# Determinar se usar paralelo (>50 registros)
usar_paralelo <- total > 50
tamanho_chunk <- if(usar_paralelo) max(10, total %/% n_cores) else total
chunks <- split(1:total, ceiling(seq_along(1:total) / tamanho_chunk))

if(usar_paralelo) {
  cat("⚡ Processamento paralelo ativado com", length(chunks), "chunks\n")
}

withProgress(message = 'Classificando em lote...', value = 0, {
  
  for(chunk_idx in seq_along(chunks)) {
    indices <- chunks[[chunk_idx]]
    
    # Verificar cancelamento
    if (isTRUE(MONITOR_LOTE$cancelado)) {
      cat("⏹ Processamento cancelado pelo usuário\n")
      break
    }
    
    if(usar_paralelo) {
      # PROCESSAMENTO PARALELO
      resultados_chunk <- future_lapply(indices, function(i) {
        
        texto <- sanitizar_texto(resultados$texto_completo[i])
        validacao <- validar_entrada(texto)
        
        if (!validacao$valido) {
          return(list(
            assunto = "", tipo = NA, categoria = "ERRO", criticidade = NA,
            confianca = 0, descricao = paste(validacao$erros, collapse = ", "),
            resumo = "", metodo = "VALIDACAO_FALHOU", status = "SEM_TEXTO"
          ))
        }
        
        # Verificar cache
        resultado_cache <- cache_get(texto, input$metodo_classificacao)
        if (!is.null(resultado_cache)) {
          resultado_cache$metodo <- paste0(resultado_cache$metodo, "_CACHE")
          return(resultado_cache)
        }
        
        classificacao <- switch(
          input$metodo_classificacao,
          "DICIONARIO" = classificar_por_dicionario(texto, DICIONARIOS_SAP),
          "API" = classificar_com_openai(texto),
          "ML" = if (!is.null(validacoes_modelo$modelo_ativo)) {
            resultado_ml <- predizer_com_modelo(texto)
            if(isTRUE(resultado_ml$sucesso)) resultado_ml 
            else classificar_por_dicionario(texto, DICIONARIOS_SAP)
          } else classificar_por_dicionario(texto, DICIONARIOS_SAP),
          "HIBRIDO_2" = classificar_hibrido_completo(texto, CONFIG_USUARIO()),
          "HIBRIDO_3" = classificar_hibrido_completo(texto, CONFIG_USUARIO()),
          list(tipo = NA, categoria = "ERRO", criticidade = NA, confianca = 0)
        )
        
        # Salvar no cache
        cache_set(texto, classificacao, input$metodo_classificacao)
        
        assunto <- if (isTRUE(input$extrair_assunto)) {
          tryCatch(extrair_assunto_principal(texto), 
                   error = function(e) substr(trimws(texto), 1, 80))
        } else {
          substr(trimws(texto), 1, 80)
        }
        
        tipo_antigo <- resultados$tipo_intervencao_antigo[i]
        status_conf <- if (!is.na(tipo_antigo) && !is.na(classificacao$tipo)) {
          ifelse(tipo_antigo == classificacao$tipo, "CONFORME", "DIVERGENTE")
        } else {
          "SEM_REFERENCIA"
        }
        
        list(
          assunto = assunto,
          tipo = classificacao$tipo,
          categoria = classificacao$categoria,
          criticidade = classificacao$criticidade,
          confianca = classificacao$confianca,
          descricao = classificacao$descricao,
          resumo = classificacao$resumo,
          metodo = classificacao$metodo,
          status = status_conf
        )
      }, future.seed = TRUE)
      
    } else {
      # PROCESSAMENTO SEQUENCIAL (padrão)
      resultados_chunk <- lapply(indices, function(i) {
        
        texto <- sanitizar_texto(resultados$texto_completo[i])
        validacao <- validar_entrada(texto)
        
        if (!validacao$valido) {
          return(list(
            assunto = "", tipo = NA, categoria = "ERRO", criticidade = NA,
            confianca = 0, descricao = paste(validacao$erros, collapse = ", "),
            resumo = "", metodo = "VALIDACAO_FALHOU", status = "SEM_TEXTO"
          ))
        }
        
        resultado_cache <- cache_get(texto, input$metodo_classificacao)
        if (!is.null(resultado_cache)) {
          resultado_cache$metodo <- paste0(resultado_cache$metodo, "_CACHE")
          return(resultado_cache)
        }
        
        classificacao <- switch(
          input$metodo_classificacao,
          "DICIONARIO" = classificar_por_dicionario(texto, DICIONARIOS_SAP),
          "API" = classificar_com_openai(texto),
          "ML" = if (!is.null(validacoes_modelo$modelo_ativo)) {
            resultado_ml <- predizer_com_modelo(texto)
            if(isTRUE(resultado_ml$sucesso)) resultado_ml 
            else classificar_por_dicionario(texto, DICIONARIOS_SAP)
          } else classificar_por_dicionario(texto, DICIONARIOS_SAP),
          "HIBRIDO_2" = classificar_hibrido_completo(texto, CONFIG_USUARIO()),
          "HIBRIDO_3" = classificar_hibrido_completo(texto, CONFIG_USUARIO()),
          list(tipo = NA, categoria = "ERRO", criticidade = NA, confianca = 0)
        )
        
        cache_set(texto, classificacao, input$metodo_classificacao)
        
        assunto <- if (isTRUE(input$extrair_assunto)) {
          tryCatch(extrair_assunto_principal(texto), 
                   error = function(e) substr(trimws(texto), 1, 80))
        } else {
          substr(trimws(texto), 1, 80)
        }
        
        tipo_antigo <- resultados$tipo_intervencao_antigo[i]
        status_conf <- if (!is.na(tipo_antigo) && !is.na(classificacao$tipo)) {
          ifelse(tipo_antigo == classificacao$tipo, "CONFORME", "DIVERGENTE")
        } else {
          "SEM_REFERENCIA"
        }
        
        list(
          assunto = assunto,
          tipo = classificacao$tipo,
          categoria = classificacao$categoria,
          criticidade = classificacao$criticidade,
          confianca = classificacao$confianca,
          descricao = classificacao$descricao,
          resumo = classificacao$resumo,
          metodo = classificacao$metodo,
          status = status_conf
        )
      })
    }
    
    # Atualizar resultados
    for(j in seq_along(indices)) {
      i <- indices[j]
      r <- resultados_chunk[[j]]
      resultados$assunto_principal[i] <- r$assunto
      resultados$tipo_novo[i] <- r$tipo
      resultados$categoria[i] <- r$categoria
      resultados$criticidade[i] <- r$criticidade
      resultados$confianca[i] <- r$confianca
      resultados$descricao[i] <- r$descricao
      resultados$resumo[i] <- r$resumo
      resultados$metodo[i] <- r$metodo
      resultados$status_conformidade[i] <- r$status
    }
    
    # Atualizar monitoramento
    MONITOR_LOTE$processados <- MONITOR_LOTE$processados + length(indices)
    MONITOR_LOTE$tempo_atual <- as.numeric(difftime(Sys.time(), MONITOR_LOTE$tempo_inicio, units = "secs"))
    MONITOR_LOTE$velocidade <- MONITOR_LOTE$processados / max(1, MONITOR_LOTE$tempo_atual)
    MONITOR_LOTE$historico_velocidade <- c(MONITOR_LOTE$historico_velocidade, MONITOR_LOTE$velocidade)
    
    if (MONITOR_LOTE$velocidade > 0) {
      registros_restantes <- total - MONITOR_LOTE$processados
      MONITOR_LOTE$tempo_estimado <- registros_restantes / MONITOR_LOTE$velocidade
    }
    
    # EXPORTAÇÃO INCREMENTAL a cada chunk
    if (chunk_idx %% max(1, length(chunks) %/% 5) == 0 || chunk_idx == length(chunks)) {
      export_result <- exportar_incremental(resultados, chunk_idx, "dados_processados")
      if (export_result$sucesso) {
        MONITOR_LOTE$arquivo_backup <- export_result$arquivo_rds
      }
    }
    
    # Atualizar progresso na UI
    progresso <- MONITOR_LOTE$processados / total
    tempo_min <- round(MONITOR_LOTE$tempo_estimado / 60, 1)
    velocidade_rpm <- round(MONITOR_LOTE$velocidade * 60, 1)
    
    incProgress(
      length(indices) / total, 
      detail = paste0(
        "Processado: ", MONITOR_LOTE$processados, "/", total, 
        " | Velocidade: ", velocidade_rpm, " reg/min",
        " | ETA: ", tempo_min, " min"
      )
    )
    
    if(input$metodo_classificacao %in% c("API", "HIBRIDO_2", "HIBRIDO_3")) {
      Sys.sleep(0.2)
    }
  }
})

MONITOR_LOTE$ativo <- FALSE

# ============================================================================
# ADICIONAR BOTÃO "PARAR" NA UI
# Procure pela seção de botões (por volta da linha 4300)
# E ADICIONE ISTO APÓS O BOTÃO "classificar_lote":
# ============================================================================

# Na UI (em tabPanel de "Classificação em Lote"), adicione:

actionButton(
  "parar_processamento",
  label = "⏹ PARAR",
  class = "btn btn-danger",
  style = "width: 100%; padding: 10px; font-size: 14px; font-weight: bold;
           margin-top: 10px; display: none;",
  id = "btn_parar"
)

# ============================================================================
# ADICIONAR OBSERVERS PARA O BOTÃO PARAR
# Adicione no server (próximo aos outros observeEvent):
# ============================================================================

observeEvent(input$classificar_lote, {
  # Mostrar botão parar
  shinyjs::show("btn_parar")
})

observeEvent(input$parar_processamento, {
  cat("⏹ Cancelamento solicitado pelo usuário\n")
  MONITOR_LOTE$cancelado <- TRUE
  
  # Criar arquivo de recuperação
  estado_recuperacao <- list(
    resultados_parciais = values$resultados_lote[1:MONITOR_LOTE$processados,],
    processados = MONITOR_LOTE$processados,
    total = MONITOR_LOTE$total,
    timestamp = Sys.time()
  )
  arquivo_backup <- criar_arquivo_recuperacao(estado_recuperacao)
  
  showNotification(
    paste("⏹ Processamento interrompido.", 
          "Dados salvos em:", arquivo_backup),
    type = "warning", duration = 10
  )
  
  shinyjs::hide("btn_parar")
})

# ============================================================================
# ADICIONAR OUTPUT DE MONITORAMENTO EM TEMPO REAL
# Adicione na UI em uma nova aba "Monitoramento":
# ============================================================================

output$monitor_tempo_real <- renderUI({
  # Atualizar a cada 500ms
  invalidateLater(500)
  
  if (!isTRUE(MONITOR_LOTE$ativo)) {
    return(NULL)
  }
  
  percentual <- if (MONITOR_LOTE$total > 0) 
    round((MONITOR_LOTE$processados / MONITOR_LOTE$total) * 100, 1) 
  else 0
  
  tempo_decorrido <- sprintf("%02d:%02d", 
    as.integer(MONITOR_LOTE$tempo_atual %/% 60),
    as.integer(MONITOR_LOTE$tempo_atual %% 60)
  )
  
  tempo_estimado <- sprintf("%02d:%02d",
    as.integer(MONITOR_LOTE$tempo_estimado %/% 60),
    as.integer(MONITOR_LOTE$tempo_estimado %% 60)
  )
  
  div(
    style = "background: white; padding: 20px; border-radius: 10px; 
             box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
    
    # Barra de progresso
    div(
      style = "margin-bottom: 20px;",
      h4("Progresso", style = "margin-top: 0;"),
      div(
        style = "background: #ecf0f1; height: 30px; border-radius: 15px; 
                 overflow: hidden; position: relative;",
        div(
          style = sprintf(
            "background: linear-gradient(90deg, #667eea, #764ba2); height: 100%%; 
             width: %f%%; transition: width 0.3s ease; display: flex;
             align-items: center; justify-content: center; color: white; 
             font-weight: bold;", percentual
          ),
          paste0(percentual, "%")
        )
      ),
      p(
        sprintf("%d / %d registros", MONITOR_LOTE$processados, MONITOR_LOTE$total),
        style = "text-align: center; margin-top: 10px; color: #666; font-size: 13px;"
      )
    ),
    
    # Métricas
    div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
      
      # Velocidade
      div(
        style = "background: #ecf0f1; padding: 15px; border-radius: 8px;",
        p("Velocidade", style = "margin: 0; color: #666; font-size: 12px;"),
        h4(
          sprintf("%.1f", MONITOR_LOTE$velocidade * 60),
          style = "margin: 5px 0; color: #667eea;",
          "reg/min"
        )
      ),
      
      # Tempo decorrido
      div(
        style = "background: #ecf0f1; padding: 15px; border-radius: 8px;",
        p("Tempo", style = "margin: 0; color: #666; font-size: 12px;"),
        h4(tempo_decorrido, style = "margin: 5px 0; color: #667eea;")
      ),
      
      # ETA
      div(
        style = "background: #ecf0f1; padding: 15px; border-radius: 8px;",
        p("ETA", style = "margin: 0; color: #666; font-size: 12px;"),
        h4(tempo_estimado, style = "margin: 5px 0; color: #667eea;")
      ),
      
      # Erros
      div(
        style = "background: #ecf0f1; padding: 15px; border-radius: 8px;",
        p("Erros", style = "margin: 0; color: #666; font-size: 12px;"),
        h4(MONITOR_LOTE$erros, style = "margin: 5px 0; color: #dc3545;")
      )
    )
  )
})

# ============================================================================
# ADICIONAR GRÁFICO DE VELOCIDADE EM TEMPO REAL
# ============================================================================

output$grafico_velocidade_tempo_real <- renderPlot({
  invalidateLater(1000)
  
  if (length(MONITOR_LOTE$historico_velocidade) < 2) {
    return(NULL)
  }
  
  dados <- data.frame(
    chunk = seq_along(MONITOR_LOTE$historico_velocidade),
    velocidade = MONITOR_LOTE$historico_velocidade * 60  # converter para reg/min
  )
  
  ggplot(dados, aes(x = chunk, y = velocidade)) +
    geom_line(color = "#667eea", size = 1) +
    geom_point(color = "#667eea", size = 3) +
    theme_minimal() +
    labs(
      title = "Velocidade de Processamento em Tempo Real",
      x = "Chunk", 
      y = "Registros/minuto",
      subtitle = paste("Média:", round(mean(dados$velocidade), 1), "reg/min")
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12, color = "#666")
    )
})
