# ============================================================================
# TESTE RÁPIDO - Sistema de Validação Funcionando
# ============================================================================
# Execute isto no console R para testar!

# 1. Carregar biblioteca Shiny
library(shiny)

# 2. Estrutura global (FORA do Shiny) - pode rodar no console
SISTEMA_VALIDACOES <- list(
  dados = data.frame(
    id = character(),
    texto = character(),
    tipo = integer(),
    confianca = numeric(),
    stringsAsFactors = FALSE
  ),
  metricas = list(total = 0, acertos = 0)
)

# 3. Função para adicionar (funciona em ambos contextos)
adicionar_validacao <- function(texto, tipo, confianca) {
  nova_linha <- data.frame(
    id = paste0("V_", nrow(SISTEMA_VALIDACOES$dados) + 1),
    texto = texto,
    tipo = tipo,
    confianca = confianca,
    stringsAsFactors = FALSE
  )
  SISTEMA_VALIDACOES$dados <<- rbind(SISTEMA_VALIDACOES$dados, nova_linha)
  cat("✅ Adicionado! Total:", nrow(SISTEMA_VALIDACOES$dados), "\n")
}

# ============================================================================
# TESTE NO CONSOLE (sem Shiny)
# ============================================================================
# Descomente e execute isto:

# adicionar_validacao("Troca de válvula", 1, 85.5)
# adicionar_validacao("Limpeza de tubulação", 2, 92.3)
# print(SISTEMA_VALIDACOES$dados)

# ============================================================================
# AGORA DENTRO DO SHINY
# ============================================================================

ui <- fluidPage(
  titlePanel("✅ Sistema de Validação Funcionando"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("txt_texto", "Texto de Manutenção:"),
      numericInput("num_tipo", "Tipo (1-6):", 1, min = 1, max = 6),
      numericInput("num_conf", "Confiança (0-100):", 80),
      actionButton("btn_adicionar", "Adicionar Validação", 
                   class = "btn-success"),
      hr(),
      actionButton("btn_limpar", "Limpar Tudo", 
                   class = "btn-danger")
    ),
    
    mainPanel(
      h3("Validações Registradas"),
      tableOutput("tbl_validacoes"),
      hr(),
      h4("Estatísticas"),
      textOutput("txt_stats")
    )
  )
)

server <- function(input, output, session) {
  
  # ✅ AQUI DENTRO DO SERVER - reactiveValues funciona!
  db <- reactiveValues(
    dados = data.frame(
      id = character(),
      texto = character(),
      tipo = integer(),
      confianca = numeric(),
      stringsAsFactors = FALSE
    ),
    total = 0
  )
  
  # Adicionar validação
  observeEvent(input$btn_adicionar, {
    
    if(input$txt_texto == "") {
      showNotification("⚠️ Digite um texto!", type = "warning")
      return()
    }
    
    nova_linha <- data.frame(
      id = paste0("V_", db$total + 1),
      texto = input$txt_texto,
      tipo = input$num_tipo,
      confianca = input$num_conf,
      stringsAsFactors = FALSE
    )
    
    db$dados <- rbind(db$dados, nova_linha)
    db$total <- db$total + 1
    
    # Limpar inputs
    updateTextInput(session, "txt_texto", value = "")
    updateNumericInput(session, "num_conf", value = 80)
    
    showNotification("✅ Validação adicionada!", type = "message")
  })
  
  # Limpar tudo
  observeEvent(input$btn_limpar, {
    db$dados <- data.frame(
      id = character(),
      texto = character(),
      tipo = integer(),
      confianca = numeric(),
      stringsAsFactors = FALSE
    )
    db$total <- 0
    showNotification("✅ Base limpa!", type = "message")
  })
  
  # Mostrar tabela
  output$tbl_validacoes <- renderTable({
    db$dados
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Mostrar estatísticas
  output$txt_stats <- renderText({
    if(db$total == 0) {
      return("Nenhuma validação registrada")
    }
    
    media_conf <- mean(db$dados$confianca)
    tipo_mais_comum <- names(sort(table(db$dados$tipo), decreasing = TRUE))[1]
    
    paste0(
      "Total: ", db$total, " | ",
      "Confiança média: ", round(media_conf, 1), "% | ",
      "Tipo mais comum: ", tipo_mais_comum
    )
  })
}

# EXECUTAR - Descomente isto:
# shinyApp(ui, server)
