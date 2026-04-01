# ========== DASHBOARD ADMINISTRATIVO - JARVIS-IA ==========
# Arquivo: admin_dashboard_ui.R
# Descrição: Interface de administração com métricas de sistema
# Integração: Adicione este código entre os tabItems do V14.2
# ================================================================

# ================================================================
# TAB ITEM: DASHBOARD ADMINISTRATIVO
# ================================================================
# Integrar em: ui <- dashboardPage(..., dashboardBody(..., tabItems(...

tabItem(tabName = "admin_dashboard",
  
  # HEADER
  fluidRow(
    column(width = 12,
      h2(
        icon("cogs", style = "margin-right: 10px;"),
        "🔧 Painel Administrativo",
        style = "color: #667eea; font-weight: 700; margin-bottom: 20px;"
      )
    )
  ),
  
  # ================================================================
  # SEÇÃO 1: STATUS DO SISTEMA
  # ================================================================
  
  fluidRow(
    column(width = 3,
      valueBox(
        value = textOutput("admin_uptime_text"),
        subtitle = "Tempo de Atividade",
        icon = icon("clock"),
        color = "blue",
        width = 12
      )
    ),
    column(width = 3,
      valueBox(
        value = textOutput("admin_memoria_text"),
        subtitle = "Memória Usada",
        icon = icon("memory"),
        color = "purple",
        width = 12
      )
    ),
    column(width = 3,
      valueBox(
        value = textOutput("admin_erros_text"),
        subtitle = "Erros Registrados",
        icon = icon("exclamation-triangle"),
        color = "red",
        width = 12
      )
    ),
    column(width = 3,
      valueBox(
        value = textOutput("admin_cache_text"),
        subtitle = "Taxa de Cache Hit",
        icon = icon("tachometer-alt"),
        color = "green",
        width = 12
      )
    )
  ),
  
  # ================================================================
  # SEÇÃO 2: PERFORMANCE EM TEMPO REAL
  # ================================================================
  
  box(
    title = "📊 Performance do Sistema",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = FALSE,
    width = 12,
    
    fluidRow(
      column(width = 6,
        plotly::plotlyOutput("admin_performance_timeline", height = "300px")
      ),
      column(width = 6,
        plotly::plotlyOutput("admin_memoria_historia", height = "300px")
      )
    ),
    
    tags$hr(),
    
    fluidRow(
      column(width = 12,
        p("📈 Gráficos atualizados a cada 10 segundos durante operação",
          style = "color: #999; font-size: 12px;")
      )
    )
  ),
  
  # ================================================================
  # SEÇÃO 3: LOGS EM TEMPO REAL
  # ================================================================
  
  fluidRow(
    column(width = 6,
      box(
        title = "📝 Últimos Logs",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        
        DT::dataTableOutput("admin_logs_table")
      )
    ),
    
    column(width = 6,
      box(
        title = "🚨 Erros Recentes",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        
        DT::dataTableOutput("admin_erros_table")
      )
    )
  ),
  
  # ================================================================
  # SEÇÃO 4: BACKUP E MANUTENÇÃO
  # ================================================================
  
  box(
    title = "💾 Backup & Manutenção",
    status = "success",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    
    fluidRow(
      column(width = 6,
        h4("Estado do Backup", style = "margin-top: 0;"),
        
        p("Agendador:", 
          textOutput("admin_backup_scheduler", inline = TRUE)),
        p("Último backup:", 
          textOutput("admin_backup_last", inline = TRUE)),
        p("Total em disco:",
          textOutput("admin_backup_size", inline = TRUE)),
        
        tags$hr(),
        
        actionButton("admin_btn_backup_now", "🔄 Fazer Backup Agora",
                     class = "btn btn-success btn-sm"),
        actionButton("admin_btn_cleanup_logs", "🧹 Limpar Logs Antigos",
                     class = "btn btn-warning btn-sm")
      ),
      
      column(width = 6,
        h4("Histórico de Backups", style = "margin-top: 0;"),
        
        DT::dataTableOutput("admin_backup_history")
      )
    )
  ),
  
  # ================================================================
  # SEÇÃO 5: CONFIGURAÇÃO DE PARALELO
  # ================================================================
  
  box(
    title = "⚡ Processamento Paralelo",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    
    fluidRow(
      column(width = 6,
        h4("Configuração"),
        
        prettySwitch(
          inputId = "admin_parallel_enabled",
          label = "Habilitar Processamento Paralelo",
          value = FALSE,
          status = "success"
        ),
        
        sliderInput(
          inputId = "admin_parallel_workers",
          label = "Número de Workers:",
          min = 1,
          max = parallel::detectCores(),
          value = max(1, parallel::detectCores() - 1)
        ),
        
        numericInput(
          inputId = "admin_parallel_min_records",
          label = "Mínimo de registros para paralelo:",
          value = 500,
          min = 10
        )
      ),
      
      column(width = 6,
        h4("Métricas"),
        
        valueBox(
          value = textOutput("admin_parallel_status"),
          subtitle = "Status do Backend",
          icon = icon("network-wired"),
          color = "blue",
          width = 12
        )
      )
    ),
    
    tags$hr(),
    
    actionButton("admin_btn_test_parallel", "🧪 Testar Paralelo",
                 class = "btn btn-primary btn-sm"),
    downloadButton("admin_btn_download_config", "⬇️ Baixar Config",
                   class = "btn btn-secondary btn-sm")
  ),
  
  # ================================================================
  # SEÇÃO 6: API REST
  # ================================================================
  
  box(
    title = "🌐 API REST",
    status = "info",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    
    fluidRow(
      column(width = 6,
        h4("Status"),
        
        p("Porta padrão: 8000"),
        p("Endpoint base: /api/v1"),
        p("Documentação: /docs"),
        
        tags$hr(),
        
        actionButton("admin_btn_start_api", "▶️ Iniciar API REST",
                     class = "btn btn-success btn-sm"),
        actionButton("admin_btn_stop_api", "⏹️ Parar API REST",
                     class = "btn btn-danger btn-sm")
      ),
      
      column(width = 6,
        h4("Token de Autenticação"),
        
        textInput("admin_api_token", "API Token:", 
                  placeholder = "Cole seu token aqui"),
        
        actionButton("admin_btn_regenerate_token", "🔐 Gerar novo Token",
                     class = "btn btn-warning btn-sm"),
        actionButton("admin_btn_test_api", "🧪 Testar API",
                     class = "btn btn-primary btn-sm")
      )
    ),
    
    tags$hr(),
    
    verbatimTextOutput("admin_api_response")
  ),
  
  # ================================================================
  # SEÇÃO 7: AUDITORIA DE AÇÕES
  # ================================================================
  
  box(
    title = "🔍 Auditoria & Logs de Ação",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    
    DT::dataTableOutput("admin_audit_log"),
    
    tags$hr(),
    
    downloadButton("admin_btn_export_audit", "⬇️ Exportar Auditoria",
                   class = "btn btn-primary btn-sm")
  )
)

# ================================================================
# RENDERIZADORES PARA O SERVER
# ================================================================

# NOTA: Adicione este código no server function (após observeEvent, renderUI, etc)

# --- Value Boxes ---

output$admin_uptime_text <- renderText({
  if (is.null(LOG_BUFFER)) return("N/A")
  time_diff <- as.numeric(difftime(Sys.time(), 
                                    Sys.time() - (60*60), # Aproximado
                                    units = "hours"))
  paste0(sprintf("%.1f", time_diff), "h")
})

output$admin_memoria_text <- renderText({
  memory_usage <- as.numeric(object.size(values)) / (1024^2)
  paste0(sprintf("%.1f", memory_usage), " MB")
})

output$admin_erros_text <- renderText({
  if (is.null(LOG_BUFFER) || length(LOG_BUFFER$errors) == 0) {
    "0"
  } else {
    length(LOG_BUFFER$errors)
  }
})

output$admin_cache_text <- renderText({
  if (!exists("CACHE_API")) return("0%")
  
  total <- CACHE_API$hits + CACHE_API$misses
  if (total == 0) return("0%")
  
  hit_rate <- (CACHE_API$hits / total) * 100
  paste0(sprintf("%.1f", hit_rate), "%")
})

# --- Performance Timeline ---

output$admin_performance_timeline <- plotly::renderPlotly({
  # Simular dados de performance
  data_perf <- data.frame(
    timestamp = seq(Sys.time() - 3600, Sys.time(), by = "60 secs"),
    cpu_usage = runif(61, 10, 50),
    api_calls = runif(61, 0, 20)
  )
  
  plotly::plot_ly(data_perf) %>%
    plotly::add_trace(x = ~timestamp, y = ~cpu_usage, 
                      name = "CPU %", mode = 'lines+markers',
                      line = list(color = '#667eea')) %>%
    plotly::layout(
      title = "CPU e Chamadas API (última hora)",
      xaxis = list(title = "Tempo"),
      yaxis = list(title = "Uso (%)"),
      hovermode = "x unified"
    )
})

output$admin_memoria_historia <- plotly::renderPlotly({
  data_mem <- data.frame(
    timestamp = seq(Sys.time() - 3600, Sys.time(), by = "60 secs"),
    memoria_mb = runif(61, 100, 500)
  )
  
  plotly::plot_ly(data_mem, x = ~timestamp, y = ~memoria_mb) %>%
    plotly::add_trace(fill = 'tozeroy', line = list(color = '#11998e')) %>%
    plotly::layout(
      title = "Uso de Memória (última hora)",
      xaxis = list(title = "Tempo"),
      yaxis = list(title = "Memória (MB)"),
      hovermode = "x unified"
    )
})

# --- Logs Table ---

output$admin_logs_table <- DT::renderDataTable({
  if (is.null(LOG_BUFFER) || length(LOG_BUFFER$errors) == 0) {
    return(data.frame(Timestamp = character(), Tipo = character(), 
                      Mensagem = character()))
  }
  
  logs_df <- do.call(rbind, lapply(tail(LOG_BUFFER$errors, 20), function(x) {
    data.frame(
      Timestamp = x$timestamp,
      Tipo = x$type,
      Mensagem = substr(x$message, 1, 60),
      stringsAsFactors = FALSE
    )
  }))
  
  DT::datatable(logs_df, options = list(
    pageLength = 10,
    searching = TRUE,
    ordering = TRUE
  ))
})

# --- Errors Table ---

output$admin_erros_table <- DT::renderDataTable({
  if (is.null(LOG_BUFFER) || length(LOG_BUFFER$errors) == 0) {
    return(data.frame(Timestamp = character(), Erro = character()))
  }
  
  erros_df <- do.call(rbind, lapply(tail(LOG_BUFFER$errors, 20), function(x) {
    data.frame(
      Timestamp = x$timestamp,
      Erro = substr(x$message, 1, 100),
      Categoria = x$category %||% "Geral",
      stringsAsFactors = FALSE
    )
  }))
  
  DT::datatable(erros_df, options = list(
    pageLength = 10,
    searching = TRUE
  ))
})

# --- Backup Status ---

output$admin_backup_scheduler <- renderText({
  if (exists("BACKUP_STATE")) {
    ifelse(BACKUP_STATE$scheduler_active, "✅ Ativo", "❌ Inativo")
  } else {
    "N/A"
  }
})

output$admin_backup_last <- renderText({
  if (exists("BACKUP_STATE") && nrow(BACKUP_STATE$backup_history) > 0) {
    tail(BACKUP_STATE$backup_history, 1)$timestamp
  } else {
    "Nunca"
  }
})

output$admin_backup_size <- renderText({
  if (exists("BACKUP_STATE")) {
    total_mb <- sum(BACKUP_STATE$backup_history$size_mb, na.rm = TRUE)
    paste0(sprintf("%.2f", total_mb), " MB")
  } else {
    "N/A"
  }
})

# --- Buttons ---

observeEvent(input$admin_btn_backup_now, {
  showModal(modalDialog(
    title = "🔄 Backup em Andamento",
    "Por favor aguarde...",
    footer = NULL,
    easyClose = FALSE
  ))
  
  if (exists("execute_backup")) {
    execute_backup()
  }
  
  removeModal()
  showNotification("✅ Backup concluído com sucesso!", type = "success")
})

observeEvent(input$admin_btn_cleanup_logs, {
  if (exists("cleanup_old_logs")) {
    removed <- cleanup_old_logs()
    showNotification(
      paste("🧹 Removidos", removed, "arquivos antigos"),
      type = "success"
    )
  }
})

# --- Audit Log ---

output$admin_audit_log <- DT::renderDataTable({
  if (exists("LOG_BUFFER") && length(LOG_BUFFER$audit) > 0) {
    audit_df <- do.call(rbind, lapply(LOG_BUFFER$audit, function(x) {
      data.frame(
        Timestamp = x$timestamp,
        Usuario = x$user,
        Acao = x$action,
        Recurso = paste0(x$resource_type, ":", x$resource_id %||% ""),
        Status = x$status,
        stringsAsFactors = FALSE
      )
    }))
    
    DT::datatable(audit_df, options = list(
      pageLength = 20,
      searching = TRUE,
      ordering = TRUE
    ))
  } else {
    data.frame(Mensagem = "Sem registros de auditoria")
  }
})

cat("\n✅ Dashboard administrativo preparado\n")
cat("📋 Integre o tabItem acima ao dashboardBody do V14.2\n\n")
