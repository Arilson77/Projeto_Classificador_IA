# ============================================================================
# ✅ FUNÇÃO CORRIGIDA: classificar_hibrido_completo
# ============================================================================
# INSTRUÇÕES DE INTEGRAÇÃO:
# 1. Localize a função classificar_hibrido_completo no CLASSIFICADOR_VERSAO14 copy.R (linha ~1357)
# 2. SUBSTITUA COMPLETAMENTE pela função abaixo
# 3. Execute: source('CLASSIFICADOR_VERSAO14 copy.R')
# 4. Teste: shinyApp(ui, server)
#
# PRINCIPAIS CORREÇÕES:
# ✓ Validação ANTES de usar cada resultado
# ✓ Tratamento de NULL e NA
# ✓ Índices válidos (1-6) para votos
# ✓ Logging detalhado para diagnóstico
# ✓ Fallback automático com garantia de resultado
# ============================================================================

classificar_hibrido_completo <- function(texto, config) {
  
  cat("\n🔧 CLASSIFICANDO VIA MÉTODO HÍBRIDO (Dicionário + API + ML)...\n")
  cat("────────────────────────────────────────────────────────────\n")
  
  # ========================================================================
  # ETAPA 1: Validação inicial do texto
  # ========================================================================
  if(is.null(texto) || is.na(texto) || nchar(trimws(texto)) == 0) {
    cat("❌ Texto inválido ou vazio\n")
    return(list(
      tipo = 3,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "MEDIA",
      confianca = 50,
      descricao = "Manutenção preventiva (padrão)",
      resumo = "Texto vazio - classificação padrão aplicada",
      metodo = "FALLBACK_TEXTO_VAZIO",
      detalhes_hibrido = list(
        erro = "Texto vazio ou inválido",
        metodos_utilizados = c()
      )
    ))
  }
  
  # ========================================================================
  # ETAPA 2: Inicializar estrutura de resultados
  # ========================================================================
  resultados <- list(
    dicionario = NULL,
    api = NULL,
    ml = NULL
  )
  
  metodos_sucesso <- c()
  diagnostico <- list()
  
  # ========================================================================
  # ETAPA 3: Dicionário (sempre disponível como fallback)
  # ========================================================================
  cat("\n📚 Etapa 1/3: Classificação por Dicionário...\n")
  
  tryCatch({
    resultado_dict <- classificar_por_dicionario(texto, config$dicionarios)
    
    # Validar estrutura do resultado
    if(!is.null(resultado_dict) && 
       !is.na(resultado_dict$tipo) && 
       resultado_dict$tipo >= 1 && 
       resultado_dict$tipo <= 6) {
      
      resultados$dicionario <- resultado_dict
      metodos_sucesso <- c(metodos_sucesso, "Dicionário")
      
      cat("   ✅ Dicionário: Tipo", resultado_dict$tipo, 
          "com", resultado_dict$matches, "correspondência(s)")
      cat(" (confiança:", round(resultado_dict$confianca, 1), "%)\n")
      
      diagnostico$dicionario <- "OK"
    } else {
      cat("   ⚠️ Dicionário retornou resultado inválido\n")
      diagnostico$dicionario <- "INVÁLIDO"
    }
  }, error = function(e) {
    cat("   ❌ Erro no dicionário:", as.character(e), "\n")
    diagnostico$dicionario <<- paste("ERRO:", as.character(e))
  })
  
  # Se dicionário falhou completamente, usar fallback
  if(is.null(resultados$dicionario)) {
    cat("   ⚠️ Usando classificação padrão de fallback...\n")
    resultados$dicionario <- list(
      tipo = 3,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "MEDIA",
      confianca = 50,
      descricao = "Manutenção preventiva (fallback)",
      resumo = "Não foi possível classificar - classificação padrão",
      metodo = "FALLBACK_DICIONARIO",
      matches = 0
    )
  }
  
  # ========================================================================
  # ETAPA 4: API (opcional)
  # ========================================================================
  if(isTRUE(config$usar_api)) {
    cat("\n🌐 Etapa 2/3: Classificação via API OpenAI...\n")
    
    tryCatch({
      resultado_api <- classificar_com_openai(texto)
      
      # Validar API
      if(!is.null(resultado_api) && 
         !isTRUE(resultado_api$erro) &&
         !is.na(resultado_api$tipo) && 
         resultado_api$tipo >= 1 && 
         resultado_api$tipo <= 6) {
        
        resultados$api <- resultado_api
        metodos_sucesso <- c(metodos_sucesso, "API")
        
        cat("   ✅ API: Tipo", resultado_api$tipo, 
            "(confiança:", round(resultado_api$confianca, 1), "%)\n")
        
        diagnostico$api <- "OK"
      } else {
        cat("   ⚠️ API retornou resultado inválido ou com erro\n")
        diagnostico$api <- "INVÁLIDO"
        resultados$api <- NULL
      }
    }, error = function(e) {
      cat("   ⚠️ API indisponível:", as.character(e), "\n")
      diagnostico$api <<- paste("ERRO:", as.character(e))
      resultados$api <<- NULL
    })
  } else {
    cat("\n⏭️  Etapa 2/3: API desativada\n")
    diagnostico$api <- "DESATIVADA"
  }
  
  # ========================================================================
  # ETAPA 5: Modelo ML (opcional)
  # ========================================================================
  if(isTRUE(config$usar_modelo_treinado)) {
    cat("\n🤖 Etapa 3/3: Classificação via Modelo ML...\n")
    
    if(is.null(validacoes_modelo$modelo_ativo)) {
      cat("   ⏸️  Modelo ML não treinado\n")
      diagnostico$ml <- "NAO_TREINADO"
    } else {
      tryCatch({
        resultado_ml <- predizer_com_modelo(texto)
        
        # Validar ML
        if(!is.null(resultado_ml) && 
           isTRUE(resultado_ml$sucesso) &&
           !is.na(resultado_ml$tipo) && 
           resultado_ml$tipo >= 1 && 
           resultado_ml$tipo <= 6) {
          
          resultados$ml <- resultado_ml
          metodos_sucesso <- c(metodos_sucesso, "ML")
          
          cat("   ✅ ML: Tipo", resultado_ml$tipo, 
              "(confiança:", round(resultado_ml$confianca, 1), "%)\n")
          
          diagnostico$ml <- "OK"
        } else {
          cat("   ⚠️ ML retornou resultado inválido\n")
          diagnostico$ml <- "INVÁLIDO"
          resultados$ml <- NULL
        }
      }, error = function(e) {
        cat("   ⚠️ Erro no ML:", as.character(e), "\n")
        diagnostico$ml <<- paste("ERRO:", as.character(e))
        resultados$ml <<- NULL
      })
    }
  } else {
    cat("\n⏭️  Etapa 3/3: Modelo ML desativado\n")
    diagnostico$ml <- "DESATIVADO"
  }
  
  # ========================================================================
  # ETAPA 6: VOTAÇÃO PONDERADA
  # ========================================================================
  cat("\n🗳️  VOTAÇÃO PONDERADA:\n")
  cat("────────────────────────────────────────────────────────────\n")
  
  votos <- numeric(6)
  pesos <- numeric(6)
  
  # Votar Dicionário (sempre presente)
  if(!is.null(resultados$dicionario)) {
    tipo_dict <- resultados$dicionario$tipo
    if(!is.na(tipo_dict) && tipo_dict >= 1 && tipo_dict <= 6) {
      votos[tipo_dict] <- votos[tipo_dict] + 1
      conf_dict <- min(100, max(0, resultados$dicionario$confianca))
      pesos[tipo_dict] <- pesos[tipo_dict] + (conf_dict / 100)
      cat("  📚 Dicionário vota no Tipo", tipo_dict, 
          "(peso:", round(conf_dict/100, 2), ")\n")
    }
  }
  
  # Votar API (se disponível e válido)
  if(!is.null(resultados$api)) {
    tipo_api <- resultados$api$tipo
    if(!is.na(tipo_api) && tipo_api >= 1 && tipo_api <= 6) {
      votos[tipo_api] <- votos[tipo_api] + 1
      conf_api <- min(100, max(0, resultados$api$confianca))
      pesos[tipo_api] <- pesos[tipo_api] + (conf_api / 100)
      cat("  🌐 API vota no Tipo", tipo_api, 
          "(peso:", round(conf_api/100, 2), ")\n")
    }
  }
  
  # Votar ML (se disponível e válido)
  if(!is.null(resultados$ml)) {
    tipo_ml <- resultados$ml$tipo
    if(!is.na(tipo_ml) && tipo_ml >= 1 && tipo_ml <= 6) {
      votos[tipo_ml] <- votos[tipo_ml] + 1
      conf_ml <- min(100, max(0, resultados$ml$confianca))
      pesos[tipo_ml] <- pesos[tipo_ml] + (conf_ml / 100)
      cat("  🤖 ML vota no Tipo", tipo_ml, 
          "(peso:", round(conf_ml/100, 2), ")\n")
    }
  }
  
  # ========================================================================
  # ETAPA 7: DETERMINAR VENCEDOR
  # ========================================================================
  cat("\n📊 Resultado da Votação:\n")
  
  # Verificar se houve algum voto válido
  if(sum(votos) == 0) {
    cat("  ⚠️ Nenhum voto válido! Usando Dicionário como resultado final.\n")
    resultado_final <- resultados$dicionario
    resultado_final$metodo <- "HIBRIDO_FALLBACK_DICIONARIO"
  } else {
    # Encontrar tipos votados
    tipos_votados <- which(votos > 0)
    
    # Análise de concordância
    if(length(tipos_votados) == 1) {
      # Todos concordam!
      tipo_final <- tipos_votados[1]
      metodo <- "HIBRIDO_CONCORDANCIA_TOTAL"
      confianca_final <- min(100, 70 + (sum(votos) * 10))
      
      cat("  ✅ CONCORDÂNCIA TOTAL! Todos os métodos votaram no Tipo", tipo_final, "\n")
      cat("     Métodos:", paste(metodos_sucesso, collapse=" + "), "\n")
      cat("     Confiança:", round(confianca_final, 1), "%\n")
      
    } else {
      # Divergência - usar maior peso
      tipo_final <- which.max(pesos)
      
      # Calcular contribuição de cada método
      contribuicoes <- numeric(3)
      names(contribuicoes) <- c("Dicionário", "API", "ML")
      
      if(!is.null(resultados$dicionario) && 
         resultados$dicionario$tipo == tipo_final) {
        contribuicoes["Dicionário"] <- resultados$dicionario$confianca
      }
      
      if(!is.null(resultados$api) && 
         resultados$api$tipo == tipo_final) {
        contribuicoes["API"] <- resultados$api$confianca
      }
      
      if(!is.null(resultados$ml) && 
         resultados$ml$tipo == tipo_final) {
        contribuicoes["ML"] <- resultados$ml$confianca
      }
      
      metodo_vencedor <- names(which.max(contribuicoes[contribuicoes > 0]))
      metodo <- paste0("HIBRIDO_", toupper(gsub(" ", "_", metodo_vencedor)))
      confianca_final <- max(contribuicoes, na.rm = TRUE)
      
      cat("  ⚠️ DIVERGÊNCIA! Métodos votaram em tipos diferentes.\n")
      cat("     Tipo vencedor:", tipo_final, 
          "(sugerido por", metodo_vencedor, ")\n")
      cat("     Confiança:", round(confianca_final, 1), "%\n")
    }
    
    # Montar resultado final
    resultado_final <- resultados$dicionario  # Base do dicionário
    resultado_final$tipo <- tipo_final
    resultado_final$confianca <- confianca_final
    resultado_final$metodo <- metodo
  }
  
  # ========================================================================
  # ETAPA 8: ADICIONAR METADADOS DE DIAGNÓSTICO
  # ========================================================================
  resultado_final$detalhes_hibrido <- list(
    metodos_sucesso = metodos_sucesso,
    num_metodos = length(metodos_sucesso),
    votos = votos,
    pesos = pesos,
    diagnostico = diagnostico,
    resultados_por_metodo = list(
      dicionario = if(!is.null(resultados$dicionario)) 
        list(tipo = resultados$dicionario$tipo, conf = resultados$dicionario$confianca) 
        else NULL,
      api = if(!is.null(resultados$api)) 
        list(tipo = resultados$api$tipo, conf = resultados$api$confianca) 
        else NULL,
      ml = if(!is.null(resultados$ml)) 
        list(tipo = resultados$ml$tipo, conf = resultados$ml$confianca) 
        else NULL
    )
  )
  
  # ========================================================================
  # ETAPA 9: LOGGING FINAL
  # ========================================================================
  cat("\n✅ CLASSIFICAÇÃO HÍBRIDA CONCLUÍDA:\n")
  cat("────────────────────────────────────────────────────────────\n")
  cat("   Tipo Final:", resultado_final$tipo, "\n")
  cat("   Categoria:", resultado_final$categoria, "\n")
  cat("   Criticidade:", resultado_final$criticidade, "\n")
  cat("   Confiança:", round(resultado_final$confianca, 1), "%\n")
  cat("   Método:", resultado_final$metodo, "\n")
  cat("   Métodos utilizados:", paste(metodos_sucesso, collapse=", "), "\n")
  cat("════════════════════════════════════════════════════════════════\n\n")
  
  return(resultado_final)
}

# ============================================================================
# FUNÇÕES AUXILIARES NECESSÁRIAS
# ============================================================================

# Certifique-se de que estas funções existem:
# - classificar_por_dicionario()
# - classificar_com_openai()
# - predizer_com_modelo()
# - DICIONARIOS_SAP (estrutura global)
# - validacoes_modelo (estrutura reativa/global)
