# ========== SISTEMA DE LOG CENTRALIZADO - JARVIS-IA ==========
# Arquivo: logger_system.R
# Descrição: Sistema completo de logging, monitoramento e auditoria
# ============================================================

# ============================================================
# 1. CONFIGURAÇÃO GLOBAL DE LOGGING
# ============================================================

LOGGER_CONFIG <- list(
  base_dir = "logs",
  max_file_size_mb = 50,
  retention_days = 30,
  log_level = "INFO",  # DEBUG, INFO, WARN, ERROR
  export_format = c("csv", "json"),  # Formatos de exportação
  realtime_buffer = TRUE,
  buffer_size = 1000
)

# Inicializar sistema de logging
initialize_logger_system <- function() {
  tryCatch({
    # Criar diretório de logs se não existir
    dir.create(LOGGER_CONFIG$base_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Criar subdiretorios
    dir.create(file.path(LOGGER_CONFIG$base_dir, "errors"), showWarnings = FALSE)
    dir.create(file.path(LOGGER_CONFIG$base_dir, "audit"), showWarnings = FALSE)
    dir.create(file.path(LOGGER_CONFIG$base_dir, "performance"), showWarnings = FALSE)
    dir.create(file.path(LOGGER_CONFIG$base_dir, "api"), showWarnings = FALSE)
    
    # Inicializar ambiente de buffer
    if (!exists("LOG_BUFFER")) {
      LOG_BUFFER <<- new.env()
      LOG_BUFFER$errors <- list()
      LOG_BUFFER$audit <- list()
      LOG_BUFFER$performance <- list()
      LOG_BUFFER$api <- list()
      LOG_BUFFER$count <- list(errors = 0, audit = 0, performance = 0, api = 0)
    }
    
    cat("✅ Sistema de logging inicializado\n")
    cat("📁 Diretório: ", LOGGER_CONFIG$base_dir, "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("❌ Erro ao inicializar logger:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================
# 2. FUNÇÃO PRINCIPAL DE LOG COM TIPO
# ============================================================

log_event <- function(
  message,
  type = "INFO",          # INFO, DEBUG, WARN, ERROR
  category = "general",   # general, classification, ml, api, validation
  details = NULL,         # Lista de detalhes adicionais
  stack_trace = FALSE,    # Incluir stack trace em caso de erro
  export = TRUE
) {
  tryCatch({
    # Criar timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    timestamp_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")
    
    # Preparar estrutura de log
    log_entry <- list(
      timestamp = timestamp,
      timestamp_iso = timestamp_iso,
      type = type,
      category = category,
      message = message,
      details = details,
      session_id = get_session_id(),
      user = Sys.getenv("USERNAME"),
      pid = Sys.getpid(),
      level = match(type, c("DEBUG", "INFO", "WARN", "ERROR"))
    )
    
    # Adicionar stack trace se solicitado
    if (stack_trace) {
      log_entry$stack_trace <- deparse(sys.calls())
    }
    
    # Determinar arquivo de destino
    base_filename <- switch(type,
      "ERROR" = "errors",
      "DEBUG" = "debug",
      "WARN" = "warnings",
      "INFO" = "info"
    )
    
    if (is.null(base_filename)) base_filename <- "general"
    
    # Salvar em buffer
    buffer_key <- tolower(base_filename)
    if (buffer_key %in% names(LOG_BUFFER)) {
      LOG_BUFFER[[buffer_key]] <<- c(LOG_BUFFER[[buffer_key]], list(log_entry))
      LOG_BUFFER$count[[buffer_key]] <<- LOG_BUFFER$count[[buffer_key]] + 1
    }
    
    # Salvar em arquivo se buffer cheio
    if (LOG_BUFFER$count[[buffer_key]] >= LOGGER_CONFIG$buffer_size / 4) {
      flush_log_buffer(buffer_key)
    }
    
    # Log no console com cores
    print_log_console(log_entry)
    
    # Exportar se solicitado
    if (export && type == "ERROR") {
      export_log_entry(log_entry)
    }
    
    invisible(log_entry)
    
  }, error = function(e) {
    cat(sprintf("[%s] ❌ ERRO NO LOGGER: %s\n",
                format(Sys.time(), "%H:%M:%S"),
                e$message))
  })
}

# ============================================================
# 3. FUNÇÕES ESPECÍFICAS DE LOG
# ============================================================

log_error <- function(message, category = "general", error_obj = NULL, details = NULL) {
  stack_trace <- !is.null(error_obj)
  
  if (!is.null(error_obj)) {
    full_message <- sprintf("%s | Erro: %s", message, conditionMessage(error_obj))
    details$error_class <- class(error_obj)
  } else {
    full_message <- message
  }
  
  log_event(
    message = full_message,
    type = "ERROR",
    category = category,
    details = details,
    stack_trace = stack_trace,
    export = TRUE
  )
}

log_warning <- function(message, category = "general", details = NULL) {
  log_event(
    message = message,
    type = "WARN",
    category = category,
    details = details,
    export = FALSE
  )
}

log_info <- function(message, category = "general", details = NULL) {
  log_event(
    message = message,
    type = "INFO",
    category = category,
    details = details,
    export = FALSE
  )
}

log_debug <- function(message, category = "general", details = NULL) {
  if (LOGGER_CONFIG$log_level %in% c("DEBUG")) {
    log_event(
      message = message,
      type = "DEBUG",
      category = category,
      details = details,
      export = FALSE
    )
  }
}

# ============================================================
# 4. LOG DE PERFORMANCE
# ============================================================

log_performance <- function(
  operation,
  duration_ms,
  success = TRUE,
  records_processed = NULL,
  details = NULL
) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  perf_entry <- list(
    timestamp = timestamp,
    operation = operation,
    duration_ms = duration_ms,
    success = success,
    records_processed = records_processed,
    throughput = if (!is.null(records_processed)) records_processed / (duration_ms / 1000) else NA,
    details = details,
    session_id = get_session_id()
  )
  
  # Adicionar ao buffer
  LOG_BUFFER$performance <<- c(LOG_BUFFER$performance, list(perf_entry))
  LOG_BUFFER$count$performance <<- LOG_BUFFER$count$performance + 1
  
  # Log em console
  cat(sprintf("⏱️  %s: %d ms (%.0f registros/s)\n",
              operation,
              duration_ms,
              perf_entry$throughput %||% 0))
}

# ============================================================
# 5. LOG DE AUDITORIA (Ações do usuário)
# ============================================================

log_audit <- function(
  action,
  resource_type,
  resource_id = NULL,
  old_value = NULL,
  new_value = NULL,
  status = "SUCCESS",
  ip_address = NULL
) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  audit_entry <- list(
    timestamp = timestamp,
    user = Sys.getenv("USERNAME"),
    session_id = get_session_id(),
    action = action,
    resource_type = resource_type,
    resource_id = resource_id,
    old_value = old_value,
    new_value = new_value,
    status = status,
    ip_address = ip_address %||% get_ip_address()
  )
  
  # Adicionar ao buffer
  LOG_BUFFER$audit <<- c(LOG_BUFFER$audit, list(audit_entry))
  LOG_BUFFER$count$audit <<- LOG_BUFFER$count$audit + 1
  
  # Log em console
  cat(sprintf("🔍 [AUDIT] %s | %s:%s | Status: %s\n",
              action,
              resource_type,
              resource_id %||% "N/A",
              status))
}

# ============================================================
# 6. LOG DE API
# ============================================================

log_api_call <- function(
  endpoint,
  method = "GET",
  status_code = NULL,
  response_time_ms = NULL,
  request_size_kb = NULL,
  response_size_kb = NULL,
  error_message = NULL
) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  api_entry <- list(
    timestamp = timestamp,
    endpoint = endpoint,
    method = method,
    status_code = status_code,
    response_time_ms = response_time_ms,
    request_size_kb = request_size_kb,
    response_size_kb = response_size_kb,
    error_message = error_message,
    session_id = get_session_id()
  )
  
  # Adicionar ao buffer
  LOG_BUFFER$api <<- c(LOG_BUFFER$api, list(api_entry))
  LOG_BUFFER$count$api <<- LOG_BUFFER$count$api + 1
  
  # Log em console com status
  status_symbol <- if (!is.null(status_code) && status_code < 400) "✅" else "❌"
  cat(sprintf("%s [API] %s %s | Status: %s | %.0f ms\n",
              status_symbol,
              method,
              endpoint,
              status_code %||% "N/A",
              response_time_ms %||% 0))
}

# ============================================================
# 7. FLUSH DO BUFFER
# ============================================================

flush_log_buffer <- function(buffer_key = NULL) {
  tryCatch({
    if (is.null(buffer_key)) {
      # Flush todos
      buffer_keys <- names(LOG_BUFFER$count)
      for (key in buffer_keys) {
        flush_log_buffer(key)
      }
    } else {
      # Flush específico
      if (LOG_BUFFER$count[[buffer_key]] > 0) {
        entries <- LOG_BUFFER[[buffer_key]]
        
        # Salvar em CSV
        csv_file <- file.path(
          LOGGER_CONFIG$base_dir,
          buffer_key,
          paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        )
        
        # Converter para data.frame
        df <- do.call(rbind, lapply(entries, function(x) {
          as.data.frame(x, stringsAsFactors = FALSE)
        }))
        
        write.csv(df, csv_file, row.names = FALSE)
        
        # Salvar em JSON
        json_file <- file.path(
          LOGGER_CONFIG$base_dir,
          buffer_key,
          paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
        )
        
        json_data <- jsonlite::toJSON(entries, pretty = TRUE, auto_unbox = TRUE)
        writeLines(json_data, json_file)
        
        # Limpar buffer
        LOG_BUFFER[[buffer_key]] <<- list()
        LOG_BUFFER$count[[buffer_key]] <<- 0
        
        cat(sprintf("💾 Buffer %s flushed para %s\n", buffer_key, csv_file))
      }
    }
  }, error = function(e) {
    cat("❌ Erro ao fazer flush do buffer:", e$message, "\n")
  })
}

# ============================================================
# 8. LIMPEZA AUTOMÁTICA DE LOGS ANTIGOS
# ============================================================

cleanup_old_logs <- function(retention_days = LOGGER_CONFIG$retention_days) {
  tryCatch({
    cutoff_date <- Sys.time() - (retention_days * 24 * 3600)
    
    log_files <- list.files(
      LOGGER_CONFIG$base_dir,
      pattern = "\\.csv$|\\.json$",
      recursive = TRUE,
      full.names = TRUE
    )
    
    removed_count <- 0
    for (file in log_files) {
      file_info <- file.info(file)
      if (file_info$mtime < cutoff_date) {
        file.remove(file)
        removed_count <- removed_count + 1
      }
    }
    
    if (removed_count > 0) {
      log_info(
        paste("Limpeza automática: removidos", removed_count, "arquivos de log antigos"),
        category = "maintenance"
      )
    }
    
    return(removed_count)
  }, error = function(e) {
    log_error("Erro ao limpar logs antigos", "maintenance", e)
    return(0)
  })
}

# ============================================================
# 9. EXPORTAÇÃO DE LOGS
# ============================================================

export_log_entry <- function(log_entry) {
  tryCatch({
    # Preparar arquivo de erro exportado
    export_file <- file.path(
      LOGGER_CONFIG$base_dir,
      "errors",
      paste0("error_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    )
    
    # Incluir contexto adicional
    export_data <- list(
      log_entry = log_entry,
      system_info = list(
        r_version = R.version$version.string,
        platform = R.version$platform,
        os = .Platform$OS.type
      ),
      session_info = sessionInfo()
    )
    
    # Salvar JSON
    json_str <- jsonlite::toJSON(export_data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_str, export_file)
    
    return(export_file)
  }, error = function(e) {
    cat("⚠️ Erro ao exportar entrada de log:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================
# 10. DASHBOARD DE LOGS EM TEMPO REAL
# ============================================================

get_log_summary <- function(time_window_minutes = 60) {
  cutoff_time <- Sys.time() - (time_window_minutes * 60)
  
  # Contar entradas por tipo
  error_count <- sum(sapply(LOG_BUFFER$errors, function(x) {
    as.POSIXct(x$timestamp) > cutoff_time
  }))
  
  warning_count <- sum(sapply(LOG_BUFFER$performance, function(x) {
    as.POSIXct(x$timestamp) > cutoff_time
  }))
  
  summary_data <- list(
    total_errors = error_count,
    total_warnings = warning_count,
    buffer_status = list(
      errors = length(LOG_BUFFER$errors),
      audit = length(LOG_BUFFER$audit),
      performance = length(LOG_BUFFER$performance),
      api = length(LOG_BUFFER$api)
    ),
    timestamp = Sys.time()
  )
  
  return(summary_data)
}

# ============================================================
# 11. UTILITÁRIOS
# ============================================================

get_session_id <- function() {
  if (!exists("SESSION_ID", envir = .GlobalEnv)) {
    SESSION_ID <<- paste0(
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_",
      substr(digest::digest(runif(1)), 1, 8)
    )
  }
  return(SESSION_ID)
}

get_ip_address <- function() {
  tryCatch({
    # Tentar obter IP do sistema
    if (.Platform$OS.type == "windows") {
      ip <- system("ipconfig | findstr /I \"IPv4\"", intern = TRUE)
      ip <- gsub("[^0-9.]", "", ip)
    } else {
      ip <- system("hostname -I", intern = TRUE)
    }
    return(trimws(ip))
  }, error = function(e) {
    return("127.0.0.1")
  })
}

print_log_console <- function(log_entry) {
  # Cores ANSI para console
  colors <- list(
    INFO = "\033[36m",   # Cyan
    WARN = "\033[33m",   # Yellow
    ERROR = "\033[31m",  # Red
    DEBUG = "\033[35m",  # Magenta
    RESET = "\033[0m"
  )
  
  color <- colors[[log_entry$type]] %||% colors$INFO
  
  cat(sprintf(
    "%s[%s] [%s] %s%s\n",
    color,
    log_entry$timestamp,
    log_entry$type,
    log_entry$message,
    colors$RESET
  ))
}

# Operador %||% para valores NULL
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ============================================================
# 12. INICIALIZAÇÃO AUTOMÁTICA
# ============================================================

# Inicializar ao carregar o arquivo
initialize_logger_system()

cat("\n✅ Sistema de logging centralizado carregado com sucesso!\n")
cat("📊 Use: log_error(), log_warning(), log_info(), log_audit(), log_performance()\n")
cat("💾 Use: flush_log_buffer() para salvar logs em disco\n\n")
