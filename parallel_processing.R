# ========== PROCESSAMENTO PARALELO - JARVIS-IA ==========
# Arquivo: parallel_processing.R
# Descrição: Otimização com processamento paralelo para grandes volumes
# ===========================================================

# ===========================================================
# 1. CONFIGURAÇÃO DE PROCESSAMENTO PARALELO
# ===========================================================

PARALLEL_CONFIG <- list(
  enabled = FALSE,                    # Ativar/desativar processamento paralelo
  workers = parallel::detectCores() - 1,  # Número de workers (N-1 para não travarem)
  min_records_for_parallel = 500,     # Mínimo de registros para usar paralelo
  backend = "multisession",           # multisession, multicore, ou sequential
  timeout_seconds = 300,              # Timeout para jobs paralelos
  batch_size = 50,                    # Tamanho de batch por worker
  progress_bar = TRUE,                # Mostrar barra de progresso
  auto_switch = TRUE                  # Auto-ativar paralelo se > min_records
)

# ===========================================================
# 2. VERIFICAÇÃO E INICIALIZAÇÃO
# ===========================================================

check_parallel_available <- function() {
  # Verificar se future e furrr estão disponíveis
  if (!requireNamespace("future", quietly = TRUE)) {
    cat("⚠️  Pacote 'future' não disponível. Processamento sequencial será usado.\n")
    return(FALSE)
  }
  
  if (!requireNamespace("furrr", quietly = TRUE)) {
    cat("ℹ️  Pacote 'furrr' não disponível. Use: install.packages('furrr')\n")
    return(FALSE)
  }
  
  return(TRUE)
}

initialize_parallel_backend <- function(backend = PARALLEL_CONFIG$backend) {
  tryCatch({
    if (!check_parallel_available()) {
      PARALLEL_CONFIG$enabled <<- FALSE
      return(FALSE)
    }
    
    # Configurar backend
    if (backend == "multisession") {
      future::plan(future::multisession, workers = PARALLEL_CONFIG$workers)
    } else if (backend == "multicore" && .Platform$OS.type == "unix") {
      future::plan(future::multicore, workers = PARALLEL_CONFIG$workers)
    } else {
      future::plan(future::sequential)
      cat("⚠️  Backend paralelo não disponível nesta plataforma\n")
      return(FALSE)
    }
    
    PARALLEL_CONFIG$enabled <<- TRUE
    
    cat("✅ Backend paralelo inicializado\n")
    cat("   Workers: ", PARALLEL_CONFIG$workers, "\n")
    cat("   Backend: ", backend, "\n\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Erro ao inicializar paralelo:", e$message, "\n")
    return(FALSE)
  })
}

# ===========================================================
# 3. CLASSIFICAÇÃO PARALELA (CORE)
# ===========================================================

classificar_lote_paralelo <- function(
  dados,
  config,
  progressHandler = NULL
) {
  tryCatch({
    n_records <- nrow(dados)
    
    # Decidir se usar paralelo
    usar_paralelo <- PARALLEL_CONFIG$auto_switch && 
                     n_records >= PARALLEL_CONFIG$min_records_for_parallel &&
                     PARALLEL_CONFIG$enabled
    
    if (!usar_paralelo) {
      cat("ℹ️  Usando processamento sequencial (", n_records, " registros)\n")
      return(classificar_lote_sequencial(dados, config, progressHandler))
    }
    
    cat("⚡ Usando processamento paralelo com ", 
        PARALLEL_CONFIG$workers, " workers\n")
    
    tempo_inicio <- Sys.time()
    
    # Preparar função para paralelo
    classificar_item_wrapper <- function(idx, dados, config) {
      # Importar configurações (necessário em paralelo)
      DICIONARIOS_SAP <- .GlobalEnv$DICIONARIOS_SAP
      OPENAI_CONFIG <- .GlobalEnv$OPENAI_CONFIG
      
      tryCatch({
        texto <- dados$Texto[idx]
        resultado <- classificar_hibrido_completo(texto, config)
        
        return(list(
          indice = idx,
          tipo = resultado$tipo,
          confianca = resultado$confianca,
          metodo = resultado$metodo,
          categoria = resultado$categoria,
          criticidade = resultado$criticidade,
          sucesso = TRUE,
          erro = NULL
        ))
      }, error = function(e) {
        return(list(
          indice = idx,
          sucesso = FALSE,
          erro = e$message
        ))
      })
    }
    
    # Usar furrr para paralelismo
    resultados <- furrr::future_map(
      1:n_records,
      ~classificar_item_wrapper(., dados, config),
      .progress = PARALLEL_CONFIG$progress_bar
    )
    
    # Processar resultados
    resultados_df <- processar_resultados_paralelos(resultados, dados)
    
    tempo_fim <- Sys.time()
    tempo_total <- as.numeric(difftime(tempo_fim, tempo_inicio, units = "secs"))
    
    cat("✅ Classificação paralela concluída em", 
        sprintf("%.1f segundos\n", tempo_total))
    cat("   Velocidade: ", 
        sprintf("%.0f registros/segundo\n", n_records / tempo_total))
    
    return(resultados_df)
    
  }, error = function(e) {
    cat("❌ Erro em classificação paralela:", e$message, "\n")
    cat("ℹ️  Tentando modo sequencial...\n")
    return(classificar_lote_sequencial(dados, config, progressHandler))
  })
}

# ===========================================================
# 4. CLASSIFICAÇÃO SEQUENCIAL (FALLBACK)
# ===========================================================

classificar_lote_sequencial <- function(
  dados,
  config,
  progressHandler = NULL
) {
  n_records <- nrow(dados)
  resultados <- list()
  
  cat("ℹ️  Processando ", n_records, " registros sequencialmente...\n\n")
  
  for (i in 1:n_records) {
    if (!is.null(progressHandler)) {
      progressHandler(i / n_records)
    }
    
    texto <- dados$Texto[i]
    resultado <- tryCatch({
      classificar_hibrido_completo(texto, config)
    }, error = function(e) {
      list(
        tipo = NA,
        confianca = 0,
        metodo = "ERRO",
        categoria = "DESCONHECIDO",
        criticidade = "DESCONHECIDA",
        descricao = e$message
      )
    })
    
    resultados[[i]] <- resultado
    
    # Log a cada 100 registros
    if (i %% 100 == 0) {
      cat("  ✅ Processados", i, "de", n_records, "registros\n")
    }
  }
  
  # Converter para data.frame
  resultados_df <- do.call(rbind, lapply(resultados, function(x) {
    data.frame(
      Tipo = x$tipo,
      Confianca = x$confianca,
      Metodo = x$metodo,
      Categoria = x$categoria,
      Criticidade = x$criticidade,
      stringsAsFactors = FALSE
    )
  }))
  
  # Combinar com dados originais
  resultado_final <- cbind(dados, resultados_df)
  
  return(resultado_final)
}

# ===========================================================
# 5. PROCESSAR RESULTADOS PARALELOS
# ===========================================================

processar_resultados_paralelos <- function(resultados, dados_originais) {
  # Converter lista de resultados em data.frame
  resultado_df <- do.call(rbind, lapply(resultados, function(x) {
    if (x$sucesso) {
      data.frame(
        Indice = x$indice,
        Tipo = x$tipo,
        Confianca = x$confianca,
        Metodo = x$metodo,
        Categoria = x$categoria,
        Criticidade = x$criticidade,
        Sucesso = TRUE,
        Erro = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Indice = x$indice,
        Tipo = NA_integer_,
        Confianca = 0,
        Metodo = "ERRO",
        Categoria = NA_character_,
        Criticidade = NA_character_,
        Sucesso = FALSE,
        Erro = x$erro,
        stringsAsFactors = FALSE
      )
    }
  }))
  
  # Ordenar por índice
  resultado_df <- resultado_df[order(resultado_df$Indice), ]
  
  # Combinar com dados originais
  resultado_final <- cbind(dados_originais, resultado_df[, -1])
  
  return(resultado_final)
}

# ===========================================================
# 6. CLASSIFICAÇÃO ML EM PARALELO
# ===========================================================

predizer_lote_paralelo <- function(
  textos,
  modelo,
  vetorizador
) {
  tryCatch({
    n_textos <- length(textos)
    
    usar_paralelo <- PARALLEL_CONFIG$auto_switch &&
                     n_textos >= PARALLEL_CONFIG$min_records_for_parallel &&
                     PARALLEL_CONFIG$enabled
    
    if (!usar_paralelo) {
      return(predizer_lote_sequencial(textos, modelo, vetorizador))
    }
    
    cat("⚡ Predição paralela com ", PARALLEL_CONFIG$workers, " workers\n")
    
    tempo_inicio <- Sys.time()
    
    predizer_item <- function(texto, modelo, vetorizador) {
      tryCatch({
        resultado <- predizer_com_modelo(texto, modelo, vetorizador)
        return(resultado)
      }, error = function(e) {
        return(list(tipo = NA, confianca = 0))
      })
    }
    
    predicoes <- furrr::future_map(
      textos,
      ~predizer_item(., modelo, vetorizador),
      .progress = PARALLEL_CONFIG$progress_bar
    )
    
    tempo_fim <- Sys.time()
    cat("✅ Predição paralela em", 
        sprintf("%.1f segundos\n", as.numeric(difftime(tempo_fim, tempo_inicio))))
    
    return(predicoes)
    
  }, error = function(e) {
    cat("⚠️  Erro em predição paralela, usando sequencial\n")
    return(predizer_lote_sequencial(textos, modelo, vetorizador))
  })
}

predizer_lote_sequencial <- function(textos, modelo, vetorizador) {
  predicoes <- lapply(textos, function(texto) {
    tryCatch({
      predizer_com_modelo(texto, modelo, vetorizador)
    }, error = function(e) {
      list(tipo = NA, confianca = 0)
    })
  })
  return(predicoes)
}

# ===========================================================
# 7. PROCESSAMENTO DE API EM PARALELO
# ===========================================================

chamar_api_paralelo <- function(
  textos,
  api_config,
  batch_size = 5
) {
  n_textos <- length(textos)
  
  usar_paralelo <- PARALLEL_CONFIG$auto_switch &&
                   n_textos >= PARALLEL_CONFIG$min_records_for_parallel &&
                   PARALLEL_CONFIG$enabled
  
  if (!usar_paralelo) {
    cat("ℹ️  Processamento de API sequencial\n")
    return(chamar_api_sequencial(textos, api_config, batch_size))
  }
  
  cat("⚡ Processamento de API paralelo\n")
  
  # Dividir em batches
  n_batches <- ceiling(n_textos / batch_size)
  batches <- split(textos, 
                   rep(1:n_batches, length.out = n_textos))
  
  tempo_inicio <- Sys.time()
  
  processar_batch <- function(batch) {
    resultados <- lapply(batch, function(texto) {
      tryCatch({
        classificar_via_api_openai(texto, api_config)
      }, error = function(e) {
        list(tipo = NA, confianca = 0)
      })
    })
    return(resultados)
  }
  
  # Processar batches em paralelo
  resultados_por_batch <- furrr::future_map(
    batches,
    ~processar_batch(.),
    .progress = PARALLEL_CONFIG$progress_bar
  )
  
  # Flatten resultados
  resultados <- unlist(resultados_por_batch, recursive = FALSE)
  
  tempo_fim <- Sys.time()
  cat("✅ API paralela em", 
      sprintf("%.1f segundos\n", as.numeric(difftime(tempo_fim, tempo_inicio))))
  
  return(resultados)
}

chamar_api_sequencial <- function(textos, api_config, batch_size = 5) {
  resultados <- lapply(textos, function(texto) {
    tryCatch({
      classificar_via_api_openai(texto, api_config)
    }, error = function(e) {
      list(tipo = NA, confianca = 0)
    })
  })
  return(resultados)
}

# ===========================================================
# 8. MONITORAMENTO E ESTATÍSTICAS
# ===========================================================

get_parallel_stats <- function() {
  stats <- list(
    enabled = PARALLEL_CONFIG$enabled,
    workers = PARALLEL_CONFIG$workers,
    backend = PARALLEL_CONFIG$backend,
    min_records = PARALLEL_CONFIG$min_records_for_parallel,
    batch_size = PARALLEL_CONFIG$batch_size
  )
  return(stats)
}

print_parallel_config <- function() {
  cat("\n⚙️  CONFIGURAÇÃO DE PROCESSAMENTO PARALELO\n")
  cat("══════════════════════════════════════════\n")
  cat("  Habilitado:", ifelse(PARALLEL_CONFIG$enabled, "✅ SIM", "❌ NÃO"), "\n")
  cat("  Workers:", PARALLEL_CONFIG$workers, "\n")
  cat("  Backend:", PARALLEL_CONFIG$backend, "\n")
  cat("  Min de registros para paralelo:", PARALLEL_CONFIG$min_records_for_parallel, "\n")
  cat("  Tamanho de batch:", PARALLEL_CONFIG$batch_size, "\n")
  cat("  Auto-ativar:", ifelse(PARALLEL_CONFIG$auto_switch, "✅ SIM", "❌ NÃO"), "\n")
  cat("══════════════════════════════════════════\n\n")
}

# ===========================================================
# 9. INICIALIZAÇÃO
# ===========================================================

# Tentar inicializar paralelo
if (check_parallel_available()) {
  initialize_parallel_backend()
} else {
  PARALLEL_CONFIG$enabled <<- FALSE
  cat("⚠️  Processamento paralelo desabilitado\n\n")
}

cat("\n✅ Módulo de processamento paralelo carregado\n")
cat("📊 Use: print_parallel_config() para ver configuração\n")
cat("ℹ️  Para habilitar: PARALLEL_CONFIG$enabled <<- TRUE\n\n")
