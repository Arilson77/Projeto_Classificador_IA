# ========== SISTEMA DE BACKUP AUTOMÁTICO AGENDADO - JARVIS-IA ==========
# Arquivo: backup_system.R
# Descrição: Backup automático com agendamento, retenção e compressão
# =========================================================================

library(zip)  # Para compressão

# =========================================================================
# 1. CONFIGURAÇÃO DO BACKUP
# =========================================================================

BACKUP_CONFIG <- list(
  base_dir = "backups",
  data_dir = "dados_modelo_treinado",
  logs_dir = "logs",
  schedule_interval_minutes = 30,  # A cada 30 minutos
  retention_days = 7,               # Manter 7 dias de backup
  max_backups = 20,                 # Máximo 20 backups
  compress = TRUE,                  # Comprimir backups
  auto_start = TRUE,                # Iniciar agendador automaticamente
  backup_types = c("data", "logs", "config"),  # Tipos de backup
  notification_email = NULL         # Email para notificações (opcional)
)

# =========================================================================
# 2. ESTRUTURA DE CONTROLE DO BACKUP
# =========================================================================

BACKUP_STATE <- new.env()
BACKUP_STATE$scheduler_active <- FALSE
BACKUP_STATE$last_backup_time <- NULL
BACKUP_STATE$backup_history <- data.frame(
  timestamp = character(0),
  backup_id = character(0),
  size_mb = numeric(0),
  status = character(0),
  file_path = character(0),
  stringsAsFactors = FALSE
)
BACKUP_STATE$backup_in_progress <- FALSE

# =========================================================================
# 3. INICIALIZAÇÃO
# =========================================================================

initialize_backup_system <- function() {
  tryCatch({
    # Criar diretório de backup
    dir.create(BACKUP_CONFIG$base_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(BACKUP_CONFIG$base_dir, "data"), showWarnings = FALSE)
    dir.create(file.path(BACKUP_CONFIG$base_dir, "logs"), showWarnings = FALSE)
    dir.create(file.path(BACKUP_CONFIG$base_dir, "config"), showWarnings = FALSE)
    
    # Carregar histórico se existir
    history_file <- file.path(BACKUP_CONFIG$base_dir, "backup_history.rds")
    if (file.exists(history_file)) {
      BACKUP_STATE$backup_history <<- readRDS(history_file)
    }
    
    cat("✅ Sistema de backup inicializado\n")
    cat("📁 Diretório: ", BACKUP_CONFIG$base_dir, "\n")
    cat("⏱️  Intervalo de agendamento: ", BACKUP_CONFIG$schedule_interval_minutes, " minutos\n")
    
    if (BACKUP_CONFIG$auto_start) {
      start_backup_scheduler()
    }
    
    return(TRUE)
  }, error = function(e) {
    cat("❌ Erro ao inicializar backup:", e$message, "\n")
    return(FALSE)
  })
}

# =========================================================================
# 4. EXECUTAR BACKUP COMPLETO
# =========================================================================

execute_backup <- function(backup_type = "full", description = NULL) {
  tryCatch({
    if (BACKUP_STATE$backup_in_progress) {
      cat("⚠️  Backup já em progresso, abortando...\n")
      return(FALSE)
    }
    
    BACKUP_STATE$backup_in_progress <<- TRUE
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_id <- paste0("backup_", timestamp)
    
    cat("🔄 Iniciando backup:", backup_id, "\n")
    
    backup_start_time <- Sys.time()
    backup_files <- list()
    total_size <- 0
    
    # ETAPA 1: Backup de dados
    if ("data" %in% BACKUP_CONFIG$backup_types && backup_type %in% c("full", "data")) {
      cat("  📊 Backup de dados...\n")
      
      data_backup_result <- backup_data_directory(
        source_dir = BACKUP_CONFIG$data_dir,
        dest_dir = file.path(BACKUP_CONFIG$base_dir, "data"),
        backup_id = backup_id
      )
      
      if (!is.null(data_backup_result)) {
        backup_files <- c(backup_files, data_backup_result$file_path)
        total_size <- total_size + data_backup_result$size_mb
        cat("     ✅ Dados: ", data_backup_result$size_mb, "MB\n")
      }
    }
    
    # ETAPA 2: Backup de logs
    if ("logs" %in% BACKUP_CONFIG$backup_types && backup_type %in% c("full", "logs")) {
      cat("  📝 Backup de logs...\n")
      
      logs_backup_result <- backup_logs_directory(
        source_dir = BACKUP_CONFIG$logs_dir,
        dest_dir = file.path(BACKUP_CONFIG$base_dir, "logs"),
        backup_id = backup_id
      )
      
      if (!is.null(logs_backup_result)) {
        backup_files <- c(backup_files, logs_backup_result$file_path)
        total_size <- total_size + logs_backup_result$size_mb
        cat("     ✅ Logs: ", logs_backup_result$size_mb, "MB\n")
      }
    }
    
    # ETAPA 3: Backup de configuração
    if ("config" %in% BACKUP_CONFIG$backup_types && backup_type %in% c("full", "config")) {
      cat("  ⚙️  Backup de configuração...\n")
      
      config_backup_result <- backup_config_files(
        dest_dir = file.path(BACKUP_CONFIG$base_dir, "config"),
        backup_id = backup_id
      )
      
      if (!is.null(config_backup_result)) {
        backup_files <- c(backup_files, config_backup_result$file_path)
        total_size <- total_size + config_backup_result$size_mb
        cat("     ✅ Config: ", config_backup_result$size_mb, "MB\n")
      }
    }
    
    # ETAPA 4: Compressão (opcional)
    if (BACKUP_CONFIG$compress && length(backup_files) > 0) {
      cat("  📦 Comprimindo arquivos...\n")
      
      zip_result <- compress_backup(
        files = backup_files,
        backup_id = backup_id,
        dest_dir = BACKUP_CONFIG$base_dir
      )
      
      if (!is.null(zip_result)) {
        # Remover arquivos originais após compressão
        file.remove(backup_files)
        backup_files <- zip_result$file_path
        total_size <- zip_result$size_mb
        cat("     ✅ Arquivo comprimido: ", total_size, "MB\n")
      }
    }
    
    # ETAPA 5: Registrar no histórico
    backup_duration <- as.numeric(difftime(Sys.time(), backup_start_time, units = "secs"))
    
    new_entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      backup_id = backup_id,
      size_mb = round(total_size, 2),
      status = "SUCCESS",
      file_path = paste(backup_files, collapse = "; "),
      duration_seconds = round(backup_duration, 2),
      type = backup_type,
      description = description %||% "",
      stringsAsFactors = FALSE
    )
    
    BACKUP_STATE$backup_history <<- rbind(BACKUP_STATE$backup_history, new_entry)
    BACKUP_STATE$last_backup_time <<- Sys.time()
    
    # Salvar histórico
    save_backup_history()
    
    # Limpeza de backups antigos
    cleanup_old_backups()
    
    BACKUP_STATE$backup_in_progress <<- FALSE
    
    cat("✅ Backup concluído em", sprintf("%.1f", backup_duration), "segundos\n\n")
    
    return(TRUE)
    
  }, error = function(e) {
    BACKUP_STATE$backup_in_progress <<- FALSE
    
    cat("❌ Erro durante backup:", e$message, "\n")
    
    # Registrar erro no histórico
    new_entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      backup_id = paste0("backup_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      size_mb = 0,
      status = "FAILED",
      file_path = "",
      duration_seconds = 0,
      type = backup_type,
      description = e$message,
      stringsAsFactors = FALSE
    )
    
    BACKUP_STATE$backup_history <<- rbind(BACKUP_STATE$backup_history, new_entry)
    save_backup_history()
    
    return(FALSE)
  })
}

# =========================================================================
# 5. BACKUP DE DIRETÓRIOS ESPECÍFICOS
# =========================================================================

backup_data_directory <- function(source_dir, dest_dir, backup_id) {
  tryCatch({
    if (!dir.exists(source_dir)) {
      cat("     ⚠️  Diretório de dados não existe: ", source_dir, "\n")
      return(NULL)
    }
    
    # Copiar arquivos
    files <- list.files(source_dir, full.names = TRUE, recursive = TRUE)
    
    if (length(files) == 0) {
      return(NULL)
    }
    
    dest_file <- file.path(dest_dir, paste0(backup_id, "_data.tar"))
    
    # Criar arquivo tar
    tar(dest_file, files = source_dir)
    
    size_mb <- file.size(dest_file) / (1024^2)
    
    return(list(file_path = dest_file, size_mb = size_mb))
    
  }, error = function(e) {
    cat("     ❌ Erro ao fazer backup de dados:", e$message, "\n")
    return(NULL)
  })
}

backup_logs_directory <- function(source_dir, dest_dir, backup_id) {
  tryCatch({
    if (!dir.exists(source_dir)) {
      cat("     ⚠️  Diretório de logs não existe: ", source_dir, "\n")
      return(NULL)
    }
    
    files <- list.files(source_dir, full.names = TRUE, recursive = TRUE)
    
    if (length(files) == 0) {
      return(NULL)
    }
    
    dest_file <- file.path(dest_dir, paste0(backup_id, "_logs.tar"))
    tar(dest_file, files = source_dir)
    
    size_mb <- file.size(dest_file) / (1024^2)
    
    return(list(file_path = dest_file, size_mb = size_mb))
    
  }, error = function(e) {
    cat("     ❌ Erro ao fazer backup de logs:", e$message, "\n")
    return(NULL)
  })
}

backup_config_files <- function(dest_dir, backup_id) {
  tryCatch({
    # Arquivos de configuração para backup
    config_files <- c(
      "CLASSIFICADOR_VERSAO14.2.R",
      "app.R",
      "logger_system.R",
      "backup_system.R"
    )
    
    existing_files <- config_files[file.exists(config_files)]
    
    if (length(existing_files) == 0) {
      return(NULL)
    }
    
    dest_file <- file.path(dest_dir, paste0(backup_id, "_config.tar"))
    tar(dest_file, files = existing_files)
    
    size_mb <- file.size(dest_file) / (1024^2)
    
    return(list(file_path = dest_file, size_mb = size_mb))
    
  }, error = function(e) {
    cat("     ❌ Erro ao fazer backup de config:", e$message, "\n")
    return(NULL)
  })
}

# =========================================================================
# 6. COMPRESSÃO
# =========================================================================

compress_backup <- function(files, backup_id, dest_dir) {
  tryCatch({
    zip_file <- file.path(dest_dir, paste0(backup_id, ".zip"))
    
    zip(zip_file, files = files, recursedir = TRUE)
    
    size_mb <- file.size(zip_file) / (1024^2)
    
    return(list(file_path = zip_file, size_mb = size_mb))
    
  }, error = function(e) {
    cat("     ❌ Erro ao comprimir backup:", e$message, "\n")
    return(NULL)
  })
}

# =========================================================================
# 7. LIMPEZA DE BACKUPS ANTIGOS
# =========================================================================

cleanup_old_backups <- function() {
  tryCatch({
    cutoff_date <- Sys.time() - (BACKUP_CONFIG$retention_days * 24 * 3600)
    
    # Listar backups
    backup_files <- list.files(
      BACKUP_CONFIG$base_dir,
      pattern = "\\.zip$|\\.tar$",
      recursive = TRUE,
      full.names = TRUE
    )
    
    removed_count <- 0
    
    for (file in backup_files) {
      file_info <- file.info(file)
      
      if (file_info$mtime < cutoff_date) {
        file.remove(file)
        removed_count <- removed_count + 1
      }
    }
    
    # Manter apenas os últimos N backups
    if (nrow(BACKUP_STATE$backup_history) > BACKUP_CONFIG$max_backups) {
      old_entries <- head(nrow(BACKUP_STATE$backup_history) - BACKUP_CONFIG$max_backups)
      
      for (i in old_entries) {
        old_file <- BACKUP_STATE$backup_history$file_path[i]
        if (file.exists(old_file)) {
          file.remove(old_file)
          removed_count <- removed_count + 1
        }
      }
      
      # Remover do histórico
      BACKUP_STATE$backup_history <<- tail(
        BACKUP_STATE$backup_history,
        BACKUP_CONFIG$max_backups
      )
    }
    
    if (removed_count > 0) {
      cat("🧹 Limpeza: removidos", removed_count, "backups antigos\n")
      save_backup_history()
    }
    
    return(removed_count)
    
  }, error = function(e) {
    cat("⚠️  Erro ao limpar backups antigos:", e$message, "\n")
    return(0)
  })
}

# =========================================================================
# 8. SALVAR HISTÓRICO
# =========================================================================

save_backup_history <- function() {
  tryCatch({
    history_file <- file.path(BACKUP_CONFIG$base_dir, "backup_history.rds")
    saveRDS(BACKUP_STATE$backup_history, history_file)
    
    # Também salvar em CSV para fácil visualização
    csv_file <- file.path(BACKUP_CONFIG$base_dir, "backup_history.csv")
    write.csv(BACKUP_STATE$backup_history, csv_file, row.names = FALSE)
    
    return(TRUE)
  }, error = function(e) {
    cat("⚠️  Erro ao salvar histórico:", e$message, "\n")
    return(FALSE)
  })
}

# =========================================================================
# 9. RESTAURAR BACKUP
# =========================================================================

restore_backup <- function(backup_id = NULL, backup_file = NULL) {
  tryCatch({
    cat("🔄 Iniciando restauração de backup...\n")
    
    # Determinar arquivo a restaurar
    if (!is.null(backup_file)) {
      restore_file <- backup_file
    } else if (!is.null(backup_id)) {
      # Procurar arquivo correspondente ao ID
      matching_files <- grep(backup_id, BACKUP_STATE$backup_history$file_path)
      if (length(matching_files) == 0) {
        stop("Backup não encontrado: ", backup_id)
      }
      restore_file <- BACKUP_STATE$backup_history$file_path[matching_files[1]]
    } else {
      # Usar o backup mais recente
      if (nrow(BACKUP_STATE$backup_history) == 0) {
        stop("Nenhum backup disponível")
      }
      restore_file <- tail(BACKUP_STATE$backup_history, 1)$file_path
    }
    
    if (!file.exists(restore_file)) {
      stop("Arquivo de backup não encontrado: ", restore_file)
    }
    
    # Determinar tipo de arquivo e descomprimir
    if (grepl("\\.zip$", restore_file)) {
      cat("  📦 Descompactando arquivo zip...\n")
      unzip(restore_file, exdir = BACKUP_CONFIG$base_dir)
    } else if (grepl("\\.tar$", restore_file)) {
      cat("  📦 Extraindo arquivo tar...\n")
      untar(restore_file, exdir = BACKUP_CONFIG$base_dir)
    }
    
    cat("✅ Backup restaurado com sucesso!\n")
    cat("ℹ️  Arquivo: ", restore_file, "\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Erro ao restaurar backup:", e$message, "\n")
    return(FALSE)
  })
}

# =========================================================================
# 10. AGENDADOR DE BACKUP
# =========================================================================

start_backup_scheduler <- function() {
  tryCatch({
    if (BACKUP_STATE$scheduler_active) {
      cat("⚠️  Agendador de backup já está ativo\n")
      return(FALSE)
    }
    
    BACKUP_STATE$scheduler_active <<- TRUE
    
    cat("⏱️  Agendador de backup iniciado\n")
    cat("    Intervalo: ", BACKUP_CONFIG$schedule_interval_minutes, " minutos\n\n")
    
    # Criar timer de backup
    if (requireNamespace("later", quietly = TRUE)) {
      schedule_backup_with_later()
    } else {
      cat("ℹ️  Pacote 'later' não disponível. Use execute_backup() manualmente.\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Erro ao iniciar agendador:", e$message, "\n")
    return(FALSE)
  })
}

schedule_backup_with_later <- function() {
  later::later(function() {
    execute_backup()
    # Reagendar para próximo intervalo
    schedule_backup_with_later()
  }, delay = BACKUP_CONFIG$schedule_interval_minutes * 60)
}

stop_backup_scheduler <- function() {
  BACKUP_STATE$scheduler_active <<- FALSE
  cat("⏹️  Agendador de backup parado\n")
  return(TRUE)
}

# =========================================================================
# 11. DASHBOARD DE STATUS
# =========================================================================

get_backup_status <- function() {
  summary_data <- list(
    scheduler_active = BACKUP_STATE$scheduler_active,
    backup_in_progress = BACKUP_STATE$backup_in_progress,
    last_backup_time = BACKUP_STATE$last_backup_time,
    last_backup_status = if (nrow(BACKUP_STATE$backup_history) > 0) {
      tail(BACKUP_STATE$backup_history, 1)$status
    } else {
      "NUNCA EXECUTADO"
    },
    total_backups = nrow(BACKUP_STATE$backup_history),
    total_size_mb = sum(BACKUP_STATE$backup_history$size_mb, na.rm = TRUE),
    history = tail(BACKUP_STATE$backup_history, 10)  # Últimos 10
  )
  
  return(summary_data)
}

print_backup_status <- function() {
  status <- get_backup_status()
  
  cat("\n📊 STATUS DO BACKUP\n")
  cat("═════════════════════════════════════════════════\n")
  cat("  Agendador ativo:", ifelse(status$scheduler_active, "✅ SIM", "❌ NÃO"), "\n")
  cat("  Backup em progresso:", ifelse(status$backup_in_progress, "✅ SIM", "❌ NÃO"), "\n")
  cat("  Último backup:", status$last_backup_time %||% "Nunca", "\n")
  cat("  Status:", status$last_backup_status, "\n")
  cat("  Total de backups:", status$total_backups, "\n")
  cat("  Tamanho total:", round(status$total_size_mb, 2), "MB\n")
  cat("═════════════════════════════════════════════════\n\n")
}

# =========================================================================
# 12. INICIALIZAÇÃO AUTOMÁTICA
# =========================================================================

initialize_backup_system()

cat("\n✅ Sistema de backup automático carregado com sucesso!\n")
cat("📋 Use: execute_backup() para fazer backup manual\n")
cat("📊 Use: print_backup_status() para ver status\n")
cat("🔄 Use: restore_backup() para restaurar um backup\n\n")

# Operador %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
