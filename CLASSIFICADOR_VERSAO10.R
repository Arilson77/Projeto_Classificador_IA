#=============================================================================
# SISTEMA SAP PETROBRAS - CLASSIFICAÇÃO IA COM DICIONÁRIOS E EXTRAÇÃO DE ASSUNTO
# Versão Completa Otimizada
#=============================================================================

Sys.setlocale("LC_ALL", "Portuguese_Brazil.UTF-8")
options(encoding = "UTF-8")
# Limpar ambiente
rm(list = ls())
gc()
#=============================================================================
# BIBLIOTECAS
#=============================================================================
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)
library(readxl)
library(tidyr)
#=============================================================================
# CONFIGURAÇÕES OPENAI PETROBRAS
#=============================================================================
OPENAI_CONFIG <- list(
  base_url = "https://apit.petrobras.com.br/ia/openai/v1/openai-azure/openai",
  api_key = "29d08064725944fcbc0b53e06f8807c5",
  model = "gpt-4o-petrobras",
  api_version = "2024-06-01"
)



#=============================================================================
# DICIONÁRIOS DE CLASSIFICAÇÃO SAP
#=============================================================================
DICIONARIOS_SAP <- list(
  
  tipo_1 = list(
    categoria_principal = "PROBLEMAS_COMUNS",
    descricao = "Condicionamento, limpeza, arrumação, preservação, pintura ou desinstalação",
    quando_utilizar = "Apoio operacional e demais serviços",
    palavras_chave = c(
      "limpeza", "limpar", "limpo", "limpando",
      "pintura", "pintar", "pintado", "pintando",
      "condicionamento", "condicionar",
      "arrumação", "arrumar", "arrumado",
      "preservação", "preservar", "preservado",
      "desinstalação", "desinstalar", "desinstalado",
      "higienização", "higienizar",
      "lavagem", "lavar", "lavado"
    ),
    criticidade = "BAIXA"
  ),
  
  tipo_2 = list(
    categoria_principal = "PROBLEMAS_COMUNS",
    descricao = "Melhorias, modificações, testes, colocação em operação, instalação ou regulagem",
    quando_utilizar = "Melhorias e testes",
    palavras_chave = c(
      "melhoria", "melhorar", "melhorado",
      "modificação", "modificar", "modificado",
      "teste", "testar", "testado", "testando",
      "instalação", "instalar", "instalado",
      "regulagem", "regular", "regulado",
      "ajuste", "ajustar", "ajustado",
      "upgrade", "atualização", "atualizar",
      "implantação", "implantar",
      "modernização", "modernizar"
    ),
    criticidade = "BAIXA"
  ),
  
  tipo_3 = list(
    categoria_principal = "PROBLEMAS_COMUNS",
    descricao = "Manutenção preventiva, manutenção preditiva ou inspeção planejada",
    quando_utilizar = "Atividades preventivas não oriundas de plano de manutenção/inspeção",
    palavras_chave = c(
      "preventiva", "preventivo", "prevenção",
      "preditiva", "preditivo",
      "inspeção", "inspecionar", "inspecionado",
      "planejada", "planejado", "planejamento",
      "programada", "programado", "programação",
      "cronograma", "cronogramada",
      "rotina", "rotineira",
      "periódica", "periódico",
      "verificação", "verificar",
      "checagem", "checar"
    ),
    criticidade = "MEDIA"
  ),
  
  tipo_4 = list(
    categoria_principal = "PROBLEMAS_COMUNS",
    descricao = "Manutenção por oportunidade ou inspeção não programada",
    quando_utilizar = "Equipamento ou Sistema disponível sem restrição e com oportunidade",
    palavras_chave = c(
      "oportunidade", "oportuna", "oportuno",
      "não programada", "nao programada",
      "não planejada", "nao planejada",
      "eventual", "eventualmente",
      "disponível", "disponivel", "disponibilidade",
      "parada", "parado",
      "sem restrição", "sem restricao",
      "aproveitar", "aproveitando"
    ),
    criticidade = "MEDIA"
  ),
  
  tipo_5 = list(
    categoria_principal = "IAZF",
    descricao = "Intervenção para eliminação de defeito",
    quando_utilizar = "Equipamento ou Sistema disponível com restrição",
    palavras_chave = c(
      "defeito", "defeitos", "defeituoso",
      "problema", "problemas", "problemático",
      "anomalia", "anomalias", "anormal",
      "restrição", "restricao", "restrito",
      "limitação", "limitacao", "limitado",
      "degradação", "degradacao", "degradado",
      "comprometimento", "comprometido",
      "parcial", "parcialmente",
      "reduzida", "reduzido"
    ),
    criticidade = "ALTA"
  ),
  
  tipo_6 = list(
    categoria_principal = "IAZF",
    descricao = "Intervenção para eliminação de falha",
    quando_utilizar = "Sistema indisponível",
    palavras_chave = c(
      "falha", "falhas", "falhando",
      "quebra", "quebrado", "quebrando",
      "pane", "parado",
      "emergência", "emergencia", "emergencial",
      "crítica", "critica", "crítico",
      "parada total", "totalmente parado",
      "indisponível", "indisponivel",
      "inoperante", "inoperável",
      "avaria", "avariado",
      "colapso", "colapsado"
    ),
    criticidade = "CRITICA"
  )
)

# Função otimizada para batch
extrair_assuntos_batch <- function(textos, api_key, api_url, model, batch_size = 5) {
  
  assuntos <- character(length(textos))
  
  for(i in seq(1, length(textos), by = batch_size)) {
    
    fim <- min(i + batch_size - 1, length(textos))
    batch <- textos[i:fim]
    
    # Processar cada texto do batch
    for(j in seq_along(batch)) {
      idx <- i + j - 1
      assuntos[idx] <- extrair_assunto_principal_api(
        batch[j], api_key, api_url, model
      )
    }
    
    Sys.sleep(0.5) # Pausa entre batches
  }
  
  return(assuntos)
}

#=============================================================================
# FUNÇÃO: EXTRAIR ASSUNTO (FALLBACK SEM API)
#=============================================================================
extrair_assunto_fallback <- function(texto) {
  
  texto <- gsub("\\*", "", texto)
  texto <- gsub("SAP:.*?\\)", "", texto)
  texto <- gsub("\\d{2}\\.\\d{2}\\.\\d{4}.*?\\)", "", texto)
  texto <- trimws(texto)
  
  frases <- strsplit(texto, "\\.|/")[[1]]
  frases <- trimws(frases)
  frases <- frases[nchar(frases) > 20]
  
  if(length(frases) > 0) {
    assunto <- frases[1]
    
    if(nchar(assunto) > 80) {
      assunto <- paste0(substr(assunto, 1, 77), "...")
    }
    
    return(assunto)
  }
  
  return("Assunto não identificado")
}
#=============================================================================
# FUNÇÃO: CLASSIFICAÇÃO POR DICIONÁRIO
#=============================================================================
classificar_por_dicionario <- function(texto, dicionarios = DICIONARIOS_SAP) {
  
  if(is.null(texto) || nchar(trimws(texto)) == 0) {
    return(list(
      tipo = NA,
      categoria = NA,
      criticidade = NA,
      confianca = 0,
      descricao = "Texto vazio",
      resumo = "",
      metodo = "DICIONARIO",
      matches = 0
    ))
  }
  
  texto_lower <- tolower(texto)
  texto_lower <- iconv(texto_lower, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  scores <- list()
  
  for(tipo_num in 1:6) {
    tipo_key <- paste0("tipo_", tipo_num)
    dicionario <- dicionarios[[tipo_key]]
    
    matches <- sum(sapply(dicionario$palavras_chave, function(palavra) {
      grepl(palavra, texto_lower, fixed = FALSE)
    }))
    
    scores[[tipo_key]] <- list(
      tipo = tipo_num,
      matches = matches,
      categoria = dicionario$categoria_principal,
      criticidade = dicionario$criticidade,
      descricao = dicionario$descricao,
      quando_utilizar = dicionario$quando_utilizar
    )
  }
  
  scores_df <- do.call(rbind, lapply(scores, function(x) {
    data.frame(
      tipo = x$tipo,
      matches = x$matches,
      categoria = x$categoria,
      criticidade = x$criticidade,
      descricao = x$descricao,
      quando_utilizar = x$quando_utilizar,
      stringsAsFactors = FALSE
    )
  }))
  
  scores_df <- scores_df[order(-scores_df$matches), ]
  melhor <- scores_df[1, ]
  
  confianca <- min(95, 50 + (melhor$matches * 10))
  
  if(melhor$matches == 0) {
    melhor$tipo <- 3
    melhor$categoria <- "PROBLEMAS_COMUNS"
    melhor$criticidade <- "MEDIA"
    melhor$descricao <- "Manutenção preventiva (classificação padrão)"
    melhor$quando_utilizar <- "Nenhuma palavra-chave identificada"
    confianca <- 50
  }
  
  return(list(
    tipo = melhor$tipo,
    categoria = melhor$categoria,
    criticidade = melhor$criticidade,
    confianca = confianca,
    descricao = melhor$descricao,
    resumo = paste0("Classificado como Tipo ", melhor$tipo, " com base em ", 
                    melhor$matches, " correspondência(s) no dicionário. ",
                    melhor$quando_utilizar),
    metodo = "DICIONARIO",
    matches = melhor$matches,
    quando_utilizar = melhor$quando_utilizar
  ))
}
#=============================================================================
# FUNÇÃO: CLASSIFICAÇÃO POR PALAVRAS-CHAVE (FALLBACK)
#=============================================================================
classificar_por_palavras_chave <- function(texto) {
  
  texto_lower <- tolower(texto)
  texto_lower <- iconv(texto_lower, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  if(grepl("falha|quebra|pane|emergencia|critica|parada.total|indisponivel", texto_lower)) {
    return(list(
      tipo = 6,
      categoria = "IAZF",
      criticidade = "CRITICA",
      confianca = 85,
      descricao = "Intervenção para eliminação de falha",
      resumo = "Falha crítica identificada que requer intervenção imediata."
    ))
  } else if(grepl("defeito|problema|anomalia|restricao|limitacao", texto_lower)) {
    return(list(
      tipo = 5,
      categoria = "IAZF",
      criticidade = "ALTA",
      confianca = 80,
      descricao = "Intervenção para eliminação de defeito",
      resumo = "Defeito detectado que necessita correção para evitar falha."
    ))
  } else if(grepl("preventiva|programada|inspecao|planejada|cronograma", texto_lower)) {
    return(list(
      tipo = 3,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "MEDIA",
      confianca = 85,
      descricao = "Manutenção preventiva, preditiva ou inspeção planejada",
      resumo = "Manutenção preventiva programada conforme cronograma."
    ))
  } else if(grepl("oportunidade|nao.programada|eventual|parada|disponivel", texto_lower)) {
    return(list(
      tipo = 4,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "MEDIA",
      confianca = 75,
      descricao = "Manutenção por oportunidade ou inspeção não programada",
      resumo = "Manutenção aproveitando oportunidade de parada do equipamento."
    ))
  } else if(grepl("melhoria|modificacao|teste|instalacao|regulagem|upgrade", texto_lower)) {
    return(list(
      tipo = 2,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "BAIXA",
      confianca = 80,
      descricao = "Melhorias, modificações, testes, instalação ou regulagem",
      resumo = "Melhoria ou modificação para otimização do equipamento."
    ))
  } else if(grepl("limpeza|pintura|condicionamento|arrumacao|preservacao", texto_lower)) {
    return(list(
      tipo = 1,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "BAIXA",
      confianca = 85,
      descricao = "Condicionamento, limpeza, arrumação, preservação ou pintura",
      resumo = "Atividade de limpeza e condicionamento do equipamento."
    ))
  } else {
    return(list(
      tipo = 3,
      categoria = "PROBLEMAS_COMUNS",
      criticidade = "MEDIA",
      confianca = 70,
      descricao = "Manutenção preventiva (classificação padrão)",
      resumo = "Classificação padrão aplicada - revisar manualmente se necessário."
    ))
  }
}
#=============================================================================
# FUNÇÃO CORRIGIDA: CLASSIFICAÇÃO COM OPENAI (FORMATO CORRETO)
#=============================================================================

classificar_com_openai <- function(texto) {
  
  if(is.null(texto) || nchar(trimws(texto)) == 0) {
    return(list(
      tipo = NA,
      categoria = NA,
      criticidade = NA,
      confianca = 0,
      descricao = "Texto vazio",
      resumo = "",
      erro = TRUE
    ))
  }
  
  prompt <- paste0(
    "Você é um especialista em classificação SAP de manutenção da Petrobras.\n\n",
    "Analise o seguinte texto de manutenção e classifique conforme os critérios SAP:\n\n",
    "TEXTO: ", texto, "\n\n",
    "TIPOS SAP:\n",
    "1. Condicionamento, limpeza, arrumação, preservação ou pintura\n",
    "2. Melhorias, modificações, testes, instalação ou regulagem\n",
    "3. Manutenção preventiva, preditiva ou inspeção planejada\n",
    "4. Manutenção por oportunidade ou inspeção não programada\n",
    "5. Intervenção para eliminação de defeito\n",
    "6. Intervenção para eliminação de falha\n\n",
    "HIERARQUIAS:\n",
    "- PROBLEMAS_COMUNS: Tipos 1, 2, 3, 4\n",
    "- IAZF (Incidente de Ativos Zero Falha): Tipos 5, 6\n\n",
    "CRITICIDADES:\n",
    "- BAIXA: Tipo 1, 2\n",
    "- MEDIA: Tipo 3, 4\n",
    "- ALTA: Tipo 5\n",
    "- CRITICA: Tipo 6\n\n",
    "Responda APENAS no formato JSON:\n",
    "{\n",
    '  "tipo": [número de 1 a 6],\n',
    '  "categoria": "PROBLEMAS_COMUNS" ou "IAZF",\n',
    '  "criticidade": "BAIXA", "MEDIA", "ALTA" ou "CRITICA",\n',
    '  "confianca": [número de 0 a 100],\n',
    '  "descricao": "descrição breve do tipo SAP",\n',
    '  "resumo": "resumo executivo da análise em 1-2 frases"\n',
    "}"
  )
  
  tryCatch({
    
    # URL CORRETA com deployments
    url <- paste0(
      OPENAI_CONFIG$base_url,
      "/deployments/",
      OPENAI_CONFIG$model,
      "/chat/completions?api-version=",
      OPENAI_CONFIG$api_version
    )
    
    cat("🔗 URL da API:", url, "\n")
    
    body <- list(
      messages = list(
        list(
          role = "system",
          content = "Você é um especialista em classificação SAP de manutenção. Responda sempre em JSON válido."
        ),
        list(
          role = "user",
          content = prompt
        )
      ),
      temperature = 0.3,
      max_tokens = 500
    )
    
    response <- POST(
      url = url,
      add_headers(
        `api-key` = OPENAI_CONFIG$api_key,
        `Content-Type` = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      timeout(30)
    )
    
    if(status_code(response) == 200) {
      
      result <- content(response, "parsed")
      message_content <- result$choices[[1]]$message$content
      
      cat("✅ Resposta da API recebida\n")
      
      # Extrair JSON da resposta
      json_match <- regmatches(message_content, regexpr("\\{[^}]+\\}", message_content))
      
      if(length(json_match) > 0) {
        classificacao <- fromJSON(json_match[1])
        
        return(list(
          tipo = as.integer(classificacao$tipo),
          categoria = classificacao$categoria,
          criticidade = classificacao$criticidade,
          confianca = as.numeric(classificacao$confianca),
          descricao = classificacao$descricao,
          resumo = classificacao$resumo,
          erro = FALSE
        ))
      } else {
        cat("⚠️ Não foi possível extrair JSON da resposta\n")
        cat("Resposta:", message_content, "\n")
      }
    } else {
      cat("❌ API retornou status:", status_code(response), "\n")
      cat("Resposta:", content(response, "text"), "\n")
    }
    
    # Fallback
    cat("⚠️ Usando classificação por palavras-chave (fallback)\n")
    resultado <- classificar_por_palavras_chave(texto)
    resultado$erro <- TRUE
    return(resultado)
    
  }, error = function(e) {
    cat("❌ Erro na API OpenAI:", e$message, "\n")
    resultado <- classificar_por_palavras_chave(texto)
    resultado$erro <- TRUE
    return(resultado)
  })
}

# Função para extrair assunto principal via API
extrair_assunto_principal_api <- function(texto, api_key, api_url, model) {
  
  if(is.null(texto) || is.na(texto) || texto == "") {
    return("Texto vazio")
  }
  
  prompt <- paste0(
    "Analise o texto abaixo e extraia o assunto principal em no máximo 10 palavras.\n",
    "Seja objetivo e direto. Retorne apenas o assunto, sem explicações.\n\n",
    "Texto: ", texto
  )
  
  tryCatch({
    response <- POST(
      url = api_url,
      add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = model,
        messages = list(
          list(role = "system", content = "Você é um assistente que extrai assuntos principais de textos de forma concisa."),
          list(role = "user", content = prompt)
        ),
        temperature = 0.3,
        max_tokens = 50
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    if(status_code(response) == 200) {
      result <- content(response, "parsed")
      assunto <- trimws(result$choices[[1]]$message$content)
      return(assunto)
    } else {
      return(paste("Erro API:", status_code(response)))
    }
    
  }, error = function(e) {
    return(paste("Erro:", substr(as.character(e), 1, 50)))
  })
}


#=============================================================================
# FUNÇÃO CORRIGIDA: EXTRAIR ASSUNTO PRINCIPAL
#=============================================================================

extrair_assunto_principal <- function(texto) {
  
  if(is.null(texto) || nchar(trimws(texto)) == 0) {
    return("Texto vazio")
  }
  
  texto_limitado <- substr(texto, 1, 1000)
  
  prompt <- paste0(
    "Você é um especialista em análise de textos de manutenção industrial.\n\n",
    "Analise o texto abaixo e extraia o REAL problema apresentado em uma frase curta e objetiva (máximo 80 caracteres).\n\n",
    "TEXTO:\n", texto_limitado, "\n\n",
    "REGRAS:\n",
    "- Seja extremamente conciso e objetivo\n",
    "- Use no máximo 80 caracteres\n",
    "- Não use pontuação final\n",
    "- Foque no equipamento e na ação principal\n",
    "- Exemplos de respostas adequadas:\n",
    "  * 'Instalação de pontos de ar comprimido'\n",
    "  * 'Manutenção preventiva da bomba P-101'\n",
    "  * 'Reparo de vazamento no trocador de calor'\n",
    "  * 'Substituição de válvulas de segurança'\n\n",
    "Responda APENAS com o assunto, sem explicações adicionais."
  )
  
  tryCatch({
    
    # URL CORRETA com deployments
    url <- paste0(
      OPENAI_CONFIG$base_url,
      "/deployments/",
      OPENAI_CONFIG$model,
      "/chat/completions?api-version=",
      OPENAI_CONFIG$api_version
    )
    
    body <- list(
      messages = list(
        list(
          role = "system",
          content = "Você é um especialista em resumir textos de manutenção de forma extremamente concisa."
        ),
        list(
          role = "user",
          content = prompt
        )
      ),
      temperature = 0.3,
      max_tokens = 50
    )
    
    response <- POST(
      url = url,
      add_headers(
        `api-key` = OPENAI_CONFIG$api_key,
        `Content-Type` = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      timeout(30)
    )
    
    if(status_code(response) == 200) {
      
      result <- content(response, "parsed")
      assunto <- result$choices[[1]]$message$content
      
      # Limpar assunto
      assunto <- trimws(assunto)
      assunto <- gsub("\\.$", "", assunto)
      assunto <- gsub('"', '', assunto)
      
      # Garantir tamanho máximo
      if(nchar(assunto) > 80) {
        assunto <- paste0(substr(assunto, 1, 77), "...")
      }
      
      return(assunto)
    }
    
    return(extrair_assunto_fallback(texto))
    
  }, error = function(e) {
    cat("Erro ao extrair assunto:", e$message, "\n")
    return(extrair_assunto_fallback(texto))
  })
}

#=============================================================================
# FUNÇÃO: CLASSIFICAÇÃO HÍBRIDA
#=============================================================================
classificar_hibrido <- function(texto, config) {
  
  resultado_final <- list()
  
  if(config$usar_dicionario) {
    resultado_dicionario <- classificar_por_dicionario(texto, config$dicionarios)
  } else {
    resultado_dicionario <- NULL
  }
  
  if(config$usar_api) {
    resultado_api <- classificar_com_openai(texto)
  } else {
    resultado_api <- NULL
  }
  
  if(config$prioridade == "DICIONARIO" && !is.null(resultado_dicionario)) {
    resultado_final <- resultado_dicionario
    resultado_final$metodo <- "DICIONARIO"
    
  } else if(config$prioridade == "API" && !is.null(resultado_api)) {
    resultado_final <- resultado_api
    resultado_final$metodo <- "API"
    
  } else if(config$prioridade == "HIBRIDO") {
    
    if(!is.null(resultado_dicionario) && !is.null(resultado_api)) {
      
      if(resultado_dicionario$tipo == resultado_api$tipo) {
        resultado_final <- resultado_api
        resultado_final$confianca <- min(100, resultado_api$confianca + 5)
        resultado_final$metodo <- "HIBRIDO_CONCORDANTE"
        resultado_final$resumo <- paste0(
          "✅ Dicionário e API concordam. ",
          resultado_api$resumo
        )
        
      } else {
        if(resultado_dicionario$confianca > resultado_api$confianca) {
          resultado_final <- resultado_dicionario
          resultado_final$metodo <- "HIBRIDO_DICIONARIO"
          resultado_final$resumo <- paste0(
            "⚠️ Divergência detectada (API sugeriu tipo ", resultado_api$tipo, "). ",
            "Usando dicionário por maior confiança. ",
            resultado_dicionario$resumo
          )
        } else {
          resultado_final <- resultado_api
          resultado_final$metodo <- "HIBRIDO_API"
          resultado_final$resumo <- paste0(
            "⚠️ Divergência detectada (Dicionário sugeriu tipo ", resultado_dicionario$tipo, "). ",
            "Usando API por maior confiança. ",
            resultado_api$resumo
          )
        }
      }
      
      resultado_final$comparacao <- list(
        tipo_dicionario = resultado_dicionario$tipo,
        tipo_api = resultado_api$tipo,
        concordam = resultado_dicionario$tipo == resultado_api$tipo,
        confianca_dicionario = resultado_dicionario$confianca,
        confianca_api = resultado_api$confianca
      )
      
    } else if(!is.null(resultado_dicionario)) {
      resultado_final <- resultado_dicionario
      resultado_final$metodo <- "DICIONARIO"
      
    } else if(!is.null(resultado_api)) {
      resultado_final <- resultado_api
      resultado_final$metodo <- "API"
    }
  }
  
  return(resultado_final)
}

#==============================================================================
# FUNÇÃO AUXILIAR CORRIGIDA: CRIAR CARD MODERNO PARA CADA TIPO
#==============================================================================

criar_card_tipo_dicionario <- function(tipo_num, cor, criticidade) {
  
  tipo_key <- paste0("tipo_", tipo_num)
  dicionario <- DICIONARIOS_SAP[[tipo_key]]
  
  # Retornar div() simples
  div(
    style = "margin-bottom: 20px;",
    
    # Header do Card
    div(
      class = paste0("header-tipo-", tipo_num),
      
      h3(
        style = "margin: 0; color: white;",
        icon("clipboard-list"),
        paste(" Tipo", tipo_num, "-", dicionario$categoria_principal)
      ),
      
      p(
        style = "color: white; margin-top: 10px; opacity: 0.95;",
        dicionario$descricao
      ),
      
      div(
        style = "text-align: right; margin-top: -50px;",
        tags$span(
          style = "background: rgba(255,255,255,0.3); padding: 8px 15px; border-radius: 20px; color: white; font-weight: bold;",
          criticidade
        )
      )
    ),
    
    # Corpo do Card
    div(
      style = "padding: 20px; background: white; border-radius: 0 0 12px 12px;",
      
      fluidRow(
        column(
          width = 6,
          textAreaInput(
            paste0("desc_tipo_", tipo_num),
            label = h4(icon("file-alt"), " Descrição"),
            value = dicionario$descricao,
            rows = 4
          ),
          textAreaInput(
            paste0("quando_tipo_", tipo_num),
            label = h4(icon("lightbulb"), " Quando Utilizar"),
            value = dicionario$quando_utilizar,
            rows = 5
          )
        ),
        column(
          width = 6,
          textAreaInput(
            paste0("palavras_tipo_", tipo_num),
            label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
            value = paste(dicionario$palavras_chave, collapse = "\n"),
            rows = 12
          )
        )
      ),
      
      hr(),
      
      div(
        style = "text-align: center;",
        actionButton(
          paste0("salvar_tipo_", tipo_num),
          label = tagList(icon("save"), paste("Salvar Tipo", tipo_num)),
          class = paste0("btn-lg btn-tipo-", tipo_num)
        )
      )
    )
  )
}

#=============================================================================
# FUNÇÃO: CRUZAMENTO DE DADOS (SEM ALTERAÇÕES)
#=============================================================================

cruzar_dados <- function(df_ordens, df_textos) {
  
  cat("\n=== INICIANDO CRUZAMENTO DE DADOS ===\n\n")
  
  nomes_ordens <- names(df_ordens)
  nomes_textos <- names(df_textos)
  
  cat("📋 Arquivo de Ordens - Colunas:\n")
  print(nomes_ordens)
  cat("\n")
  
  cat("📋 Arquivo de Textos - Colunas:\n")
  print(nomes_textos)
  cat("\n")
  
  col_nota_ordens <- NULL
  if("Nota" %in% nomes_ordens) {
    col_nota_ordens <- "Nota"
  } else {
    for(col in nomes_ordens) {
      if(grepl("^nota$|^n[oº].*nota", tolower(col))) {
        col_nota_ordens <- col
        break
      }
    }
  }
  
  if(is.null(col_nota_ordens)) {
    return(list(
      sucesso = FALSE,
      erro = "Não foi possível identificar a coluna 'Nota' no arquivo de Ordens."
    ))
  }
  
  cat("✅ Coluna de Nota identificada (Arquivo Ordens):", col_nota_ordens, "\n\n")
  
  col_nota_textos <- NULL
  opcoes_nota <- c("Nº da nota", "N° da nota", "Numero da nota", "Número da nota", "No da nota", "Nota")
  
  for(opcao in opcoes_nota) {
    if(opcao %in% nomes_textos) {
      col_nota_textos <- opcao
      break
    }
  }
  
  if(is.null(col_nota_textos)) {
    for(col in nomes_textos) {
      if(grepl("n[oº°].*nota|nota", tolower(col))) {
        col_nota_textos <- col
        break
      }
    }
  }
  
  if(is.null(col_nota_textos)) {
    return(list(
      sucesso = FALSE,
      erro = paste0(
        "Não foi possível identificar a coluna 'Nº da nota' no arquivo de Textos.\n",
        "Colunas disponíveis: ", paste(nomes_textos, collapse = ", ")
      )
    ))
  }
  
  cat("✅ Coluna de Nota identificada (Arquivo Textos):", col_nota_textos, "\n\n")
  
  padronizar_nota <- function(x) {
    x <- as.character(trimws(x))
    x <- gsub("^0+", "", x)
    x[x == ""] <- "0"
    return(x)
  }
  
  df_ordens_prep <- df_ordens %>%
    filter(!is.na(!!sym(col_nota_ordens))) %>%
    mutate(
      nota_original = as.character(!!sym(col_nota_ordens)),
      nota_key = padronizar_nota(!!sym(col_nota_ordens))
    )
  
  df_textos_prep <- df_textos %>%
    filter(!is.na(!!sym(col_nota_textos))) %>%
    mutate(
      nota_original = as.character(!!sym(col_nota_textos)),
      nota_key = padronizar_nota(!!sym(col_nota_textos))
    )
  
  cat("📊 Registros antes do merge:\n")
  cat("  - Ordens:", nrow(df_ordens_prep), "\n")
  cat("  - Textos:", nrow(df_textos_prep), "\n\n")
  
  notas_ordens <- unique(df_ordens_prep$nota_key)
  notas_textos <- unique(df_textos_prep$nota_key)
  correspondencias <- intersect(notas_ordens, notas_textos)
  
  cat("🔍 ANÁLISE DE CORRESPONDÊNCIAS:\n")
  cat("  - Notas únicas em Ordens:", length(notas_ordens), "\n")
  cat("  - Notas únicas em Textos:", length(notas_textos), "\n")
  cat("  - Correspondências encontradas:", length(correspondencias), "\n\n")
  
  if(length(correspondencias) == 0) {
    return(list(
      sucesso = FALSE,
      erro = "Nenhuma correspondência encontrada entre os arquivos após padronização."
    ))
  }
  
  dup_ordens <- df_ordens_prep %>% count(nota_key) %>% filter(n > 1)
  dup_textos <- df_textos_prep %>% count(nota_key) %>% filter(n > 1)
  
  if(nrow(dup_ordens) > 0) {
    cat("⚠️ Removendo", nrow(dup_ordens), "duplicatas do arquivo de Ordens\n\n")
    df_ordens_prep <- df_ordens_prep %>%
      group_by(nota_key) %>%
      slice(1) %>%
      ungroup()
  }
  
  if(nrow(dup_textos) > 0) {
    cat("⚠️ Consolidando", nrow(dup_textos), "duplicatas do arquivo de Textos\n\n")
    df_textos_prep <- df_textos_prep %>%
      group_by(nota_key) %>%
      slice(1) %>%
      ungroup()
  }
  
  cat("🔗 Realizando merge...\n\n")
  
  df_cruzado <- df_ordens_prep %>%
    left_join(df_textos_prep, by = "nota_key", suffix = c("_ordem", "_texto"))
  
  col_tip_intervencao <- NULL
  for(col in names(df_cruzado)) {
    if(grepl("tip.*interven", tolower(col))) {
      col_tip_intervencao <- col
      break
    }
  }
  
  col_texto_breve <- NULL
  col_texto_longo <- NULL
  
  for(col in names(df_cruzado)) {
    if(grepl("texto.*breve", tolower(col))) col_texto_breve <- col
    if(grepl("texto.*longo", tolower(col))) col_texto_longo <- col
  }
  
  if(!is.null(col_texto_breve) && !is.null(col_texto_longo)) {
    df_cruzado <- df_cruzado %>%
      mutate(
        texto_completo = paste(
          ifelse(is.na(.data[[col_texto_breve]]), "", .data[[col_texto_breve]]),
          ifelse(is.na(.data[[col_texto_longo]]), "", .data[[col_texto_longo]]),
          sep = " | "
        )
      ) %>%
      mutate(
        texto_completo = gsub("^\\s*\\|\\s*|\\s*\\|\\s*$", "", texto_completo),
        texto_completo = trimws(texto_completo)
      )
  } else {
    df_cruzado <- df_cruzado %>%
      mutate(texto_completo = "")
  }
  
  total_cruzado <- nrow(df_cruzado)
  com_texto <- sum(nchar(df_cruzado$texto_completo) > 0)
  
  cat("📊 ESTATÍSTICAS DO CRUZAMENTO:\n")
  cat("  ✅ Total após merge:", total_cruzado, "\n")
  cat("  ✅ Com texto:", com_texto, "\n")
  cat("  📊 Taxa de sucesso:", round((com_texto/total_cruzado)*100, 1), "%\n\n")
  
  cat("=== CRUZAMENTO CONCLUÍDO ===\n\n")
  
  return(list(
    sucesso = TRUE,
    dados = df_cruzado,
    col_nota = "nota_key",
    col_tip_intervencao = col_tip_intervencao,
    col_texto_completo = "texto_completo",
    estatisticas = list(
      total = total_cruzado,
      com_texto = com_texto,
      correspondencias = length(correspondencias)
    )
  ))
}
cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("       🤖 SISTEMA SAP PETROBRAS - CARREGANDO INTERFACE ELEGANTE...         \n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("\n")

#=============================================================================
# INTERFACE DO USUÁRIO (UI COMPLETO) - VERSÃO ULTRA ELEGANTE
#=============================================================================

ui <- dashboardPage(
  
  skin = "purple",
  
  #===========================================================================
  # HEADER ELEGANTE
  #===========================================================================
  
  dashboardHeader(
    title = span(
      icon("industry", style = "margin-right: 10px; font-size: 24px;"),
      "SAP Petrobras IA",
      style = "font-weight: 600; letter-spacing: 1px;"
    ),
    titleWidth = 320
  ),
  
  #===========================================================================
  # SIDEBAR ULTRA ELEGANTE
  #===========================================================================
  
  dashboardSidebar(
    width = 320,
    
    sidebarMenu(
      id = "sidebar_menu",
      
      # Header do menu com gradiente
      tags$li(
        class = "header",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                 color: white; font-weight: bold; padding: 20px; 
                 text-align: center; font-size: 14px; letter-spacing: 2px;
                 box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        "MENU PRINCIPAL"
      ),
      
      menuItem(
        "📊 Dashboard",
        tabName = "dashboard",
        icon = icon("tachometer-alt"),
        badgeLabel = "novo",
        badgeColor = "green"
      ),
      
      menuItem(
        "📁 Upload & Cruzamento",
        tabName = "upload",
        icon = icon("file-upload")
      ),
      
      menuItem(
        "🤖 Classificação IA",
        tabName = "individual",
        icon = icon("robot")
      ),
      
      menuItem(
        "📦 Processamento Lote",
        tabName = "lote",
        icon = icon("list-ul")
      ),
      
      menuItem(
        "📚 Dicionários SAP",
        tabName = "dicionarios",
        icon = icon("book")
      ),
      
      menuItem(
        "📈 Estatísticas",
        tabName = "estatisticas",
        icon = icon("chart-line")
      ),
      
      tags$li(
        class = "header",
        style = "color: #b8c7ce; font-weight: bold; padding: 15px; 
                 font-size: 12px; letter-spacing: 1px;",
        "CONFIGURAÇÕES"
      ),
      
      menuItem(
        "⚙️ API OpenAI",
        tabName = "configuracoes",
        icon = icon("cog")
      ),
      
      menuItem(
        "📖 Documentação",
        tabName = "documentacao",
        icon = icon("book-open")
      ),
      
      menuItem(
        "📜 Histórico",
        tabName = "historico",
        icon = icon("history"),
        badgeLabel = "beta",
        badgeColor = "yellow"
      )
    )
  ),
  
  #===========================================================================
  # BODY - CSS ULTRA ELEGANTE
  #===========================================================================
  
  dashboardBody(
    
    # CSS GLOBAL PREMIUM
    tags$head(
      tags$style(HTML("
        /* ============================================
           ESTILOS GLOBAIS PREMIUM
           ============================================ */
        
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
        
        body {
          font-family: 'Inter', 'Segoe UI', sans-serif;
          font-weight: 400;
        }
        
        .content-wrapper {
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          min-height: 100vh;
        }
        
        /* ============================================
           SIDEBAR PREMIUM
           ============================================ */
        
        .main-sidebar {
          background: linear-gradient(180deg, #1a1f36 0%, #2d3561 100%);
          box-shadow: 4px 0 20px rgba(0,0,0,0.15);
        }
        
        .sidebar-menu > li > a {
          padding: 16px 25px;
          border-left: 4px solid transparent;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          font-size: 15px;
          font-weight: 500;
          letter-spacing: 0.5px;
        }
        
        .sidebar-menu > li:hover > a {
          background: rgba(255,255,255,0.08) !important;
          border-left-color: #667eea !important;
          padding-left: 30px;
          transform: translateX(5px);
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, rgba(102,126,234,0.2) 0%, rgba(118,75,162,0.1) 100%) !important;
          border-left-color: #667eea !important;
          box-shadow: inset 0 0 20px rgba(102,126,234,0.3);
        }
        
        .sidebar-menu > li > a > .fa,
        .sidebar-menu > li > a > .glyphicon {
          width: 30px;
          font-size: 18px;
          margin-right: 12px;
        }
        
        /* ============================================
           BOXES PREMIUM COM GLASSMORPHISM
           ============================================ */
        
        .box {
          border-radius: 20px;
          box-shadow: 0 8px 32px rgba(0,0,0,0.12);
          border-top: none;
          animation: fadeInUp 0.6s cubic-bezier(0.4, 0, 0.2, 1);
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          backdrop-filter: blur(10px);
          background: rgba(255,255,255,0.95);
        }
        
        .box:hover {
          transform: translateY(-8px);
          box-shadow: 0 16px 48px rgba(0,0,0,0.18);
        }
        
        .box-header {
          border-radius: 20px 20px 0 0;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 25px;
          border-bottom: none;
        }
        
        .box-title {
          font-weight: 700;
          font-size: 19px;
          letter-spacing: 0.5px;
        }
        
        .box-body {
          padding: 30px;
        }
        
        /* ============================================
           VALUE BOXES PREMIUM
           ============================================ */
        
        .small-box {
          border-radius: 20px;
          box-shadow: 0 8px 24px rgba(0,0,0,0.12);
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          overflow: hidden;
          position: relative;
        }
        
        .small-box::before {
          content: '';
          position: absolute;
          top: -50%;
          right: -50%;
          width: 200%;
          height: 200%;
          background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
          transition: all 0.6s ease;
        }
        
        .small-box:hover {
          transform: translateY(-12px) scale(1.03);
          box-shadow: 0 20px 40px rgba(102,126,234,0.4);
        }
        
        .small-box:hover::before {
          top: -30%;
          right: -30%;
        }
        
        .small-box > .inner {
          padding: 25px;
          position: relative;
          z-index: 1;
        }
        
        .small-box h3 {
          font-size: 48px;
          font-weight: 800;
          margin: 0 0 12px 0;
          text-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        
        .small-box .icon {
          font-size: 90px;
          opacity: 0.25;
          transition: all 0.4s ease;
        }
        
        .small-box:hover .icon {
          opacity: 0.4;
          transform: scale(1.1) rotate(5deg);
        }
        
        /* ============================================
           BOTÕES PREMIUM
           ============================================ */
        
        .btn {
          border-radius: 30px;
          padding: 14px 35px;
          font-weight: 700;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          border: none;
          text-transform: uppercase;
          letter-spacing: 1.2px;
          font-size: 13px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          position: relative;
          overflow: hidden;
        }
        
        .btn::before {
          content: '';
          position: absolute;
          top: 50%;
          left: 50%;
          width: 0;
          height: 0;
          border-radius: 50%;
          background: rgba(255,255,255,0.3);
          transform: translate(-50%, -50%);
          transition: width 0.6s, height 0.6s;
        }
        
        .btn:hover::before {
          width: 300px;
          height: 300px;
        }
        
        .btn:hover {
          transform: translateY(-4px);
          box-shadow: 0 12px 32px rgba(0,0,0,0.25);
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        
        .btn-success {
          background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        }
        
        .btn-info {
          background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
        }
        
        .btn-green {
          background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
          color: white;
        }
        
        .btn-lg {
          padding: 18px 45px;
          font-size: 15px;
        }
        
        /* Botões dos tipos com efeito neon */
        .btn-tipo-1 { 
          background: linear-gradient(135deg, #87CEEB 0%, #4682B4 100%); 
          color: white; border: none; padding: 14px 40px; border-radius: 30px; 
          font-weight: bold; box-shadow: 0 4px 15px rgba(135, 206, 235, 0.4);
        }
        .btn-tipo-1:hover { box-shadow: 0 6px 25px rgba(135, 206, 235, 0.6); }
        
        .btn-tipo-2 { 
          background: linear-gradient(135deg, #90EE90 0%, #32CD32 100%); 
          color: white; border: none; padding: 14px 40px; border-radius: 30px; 
          font-weight: bold; box-shadow: 0 4px 15px rgba(144, 238, 144, 0.4);
        }
        .btn-tipo-2:hover { box-shadow: 0 6px 25px rgba(144, 238, 144, 0.6); }
        
        .btn-tipo-3 { 
          background: linear-gradient(135deg, #FFD700 0%, #FFA500 100%); 
          color: white; border: none; padding: 14px 40px; border-radius: 30px; 
          font-weight: bold; box-shadow: 0 4px 15px rgba(255, 215, 0, 0.4);
        }
        .btn-tipo-3:hover { box-shadow: 0 6px 25px rgba(255, 215, 0, 0.6); }
        
        .btn-tipo-4 { 
          background: linear-gradient(135deg, #FFA500 0%, #FF8C00 100%); 
          color: white; border: none; padding: 14px 40px; border-radius: 30px; 
          font-weight: bold; box-shadow: 0 4px 15px rgba(255, 165, 0, 0.4);
        }
        .btn-tipo-4:hover { box-shadow: 0 6px 25px rgba(255, 165, 0, 0.6); }
        
        .btn-tipo-5 { 
          background: linear-gradient(135deg, #FF6347 0%, #DC143C 100%); 
          color: white; border: none; padding: 14px 40px; border-radius: 30px; 
          font-weight: bold; box-shadow: 0 4px 15px rgba(255, 99, 71, 0.4);
        }
        .btn-tipo-5:hover { box-shadow: 0 6px 25px rgba(255, 99, 71, 0.6); }
        
        .btn-tipo-6 { 
          background: linear-gradient(135deg, #DC143C 0%, #8B0000 100%); 
          color: white; border: none; padding: 14px 40px; border-radius: 30px; 
          font-weight: bold; box-shadow: 0 4px 15px rgba(220, 20, 60, 0.4);
        }
        .btn-tipo-6:hover { box-shadow: 0 6px 25px rgba(220, 20, 60, 0.6); }
        
        /* ============================================
           INPUTS PREMIUM
           ============================================ */
        
        .form-control {
          border-radius: 12px;
          border: 2px solid #e9ecef;
          padding: 14px 22px;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          font-size: 14px;
          background: rgba(255,255,255,0.9);
        }
        
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 0.3rem rgba(102, 126, 234, 0.25), 0 4px 12px rgba(102, 126, 234, 0.15);
          transform: scale(1.02);
          background: white;
        }
        
        textarea.form-control {
          min-height: 140px;
        }
        
        /* ============================================
           TABS PILLS PREMIUM
           ============================================ */
        
        .nav-pills > li > a {
          border-radius: 35px;
          margin: 0 10px;
          padding: 14px 30px;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          font-weight: 700;
          background: rgba(248,249,250,0.8);
          backdrop-filter: blur(10px);
          letter-spacing: 0.5px;
        }
        
        .nav-pills > li.active > a,
        .nav-pills > li.active > a:hover,
        .nav-pills > li.active > a:focus {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
          transform: translateY(-3px);
        }
        
        /* ============================================
           PROGRESS BAR PREMIUM
           ============================================ */
        
        .progress {
          height: 35px;
          border-radius: 20px;
          background: #e9ecef;
          box-shadow: inset 0 2px 8px rgba(0,0,0,0.1);
          overflow: hidden;
        }
        
        .progress-bar {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
          border-radius: 20px;
          line-height: 35px;
          font-weight: bold;
          box-shadow: 0 2px 8px rgba(102, 126, 234, 0.3);
          animation: progressShine 2s infinite;
        }
        
        @keyframes progressShine {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.8; }
        }
        
        /* ============================================
           DATATABLES PREMIUM
           ============================================ */
        
        .dataTables_wrapper {
          padding: 25px;
        }
        
        table.dataTable {
          border-radius: 12px;
          overflow: hidden;
        }
        
        table.dataTable thead th {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          font-weight: 700;
          padding: 18px;
          border: none;
          text-transform: uppercase;
          letter-spacing: 1px;
          font-size: 12px;
        }
        
        table.dataTable tbody tr {
          transition: all 0.3s ease;
        }
        
        table.dataTable tbody tr:hover {
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          transform: scale(1.01);
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        }
        
        /* ============================================
           HEADERS DOS TIPOS PREMIUM
           ============================================ */
        
        .header-tipo-1 {
          background: linear-gradient(135deg, #87CEEB 0%, rgba(135, 206, 235, 0.8) 100%);
          padding: 35px;
          border-radius: 20px 20px 0 0;
          color: white;
          box-shadow: 0 8px 24px rgba(135, 206, 235, 0.3);
        }
        
        .header-tipo-2 {
          background: linear-gradient(135deg, #90EE90 0%, rgba(144, 238, 144, 0.8) 100%);
          padding: 35px;
          border-radius: 20px 20px 0 0;
          color: white;
          box-shadow: 0 8px 24px rgba(144, 238, 144, 0.3);
        }
        
        .header-tipo-3 {
          background: linear-gradient(135deg, #FFD700 0%, rgba(255, 215, 0, 0.8) 100%);
          padding: 35px;
          border-radius: 20px 20px 0 0;
          color: white;
          box-shadow: 0 8px 24px rgba(255, 215, 0, 0.3);
        }
        
        .header-tipo-4 {
          background: linear-gradient(135deg, #FFA500 0%, rgba(255, 165, 0, 0.8) 100%);
          padding: 35px;
          border-radius: 20px 20px 0 0;
          color: white;
          box-shadow: 0 8px 24px rgba(255, 165, 0, 0.3);
        }
        
        .header-tipo-5 {
          background: linear-gradient(135deg, #FF6347 0%, rgba(255, 99, 71, 0.8) 100%);
          padding: 35px;
          border-radius: 20px 20px 0 0;
          color: white;
          box-shadow: 0 8px 24px rgba(255, 99, 71, 0.3);
        }
        
        .header-tipo-6 {
          background: linear-gradient(135deg, #DC143C 0%, rgba(220, 20, 60, 0.8) 100%);
          padding: 35px;
          border-radius: 20px 20px 0 0;
          color: white;
          box-shadow: 0 8px 24px rgba(220, 20, 60, 0.3);
        }
        
        /* ============================================
           SCROLLBAR PREMIUM
           ============================================ */
        
        ::-webkit-scrollbar {
          width: 14px;
          height: 14px;
        }
        
        ::-webkit-scrollbar-track {
          background: linear-gradient(180deg, #f1f1f1 0%, #e0e0e0 100%);
          border-radius: 10px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(180deg, #667eea 0%, #764ba2 100%);
          border-radius: 10px;
          border: 2px solid #f1f1f1;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(180deg, #764ba2 0%, #667eea 100%);
          box-shadow: 0 0 10px rgba(102, 126, 234, 0.5);
        }
        
        /* ============================================
           ANIMAÇÕES PREMIUM
           ============================================ */
        
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(40px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        @keyframes pulse {
          0%, 100% {
            transform: scale(1);
          }
          50% {
            transform: scale(1.08);
          }
        }
        
        @keyframes shimmer {
          0% {
            background-position: -1000px 0;
          }
          100% {
            background-position: 1000px 0;
          }
        }
        
        .pulse {
          animation: pulse 2.5s cubic-bezier(0.4, 0, 0.2, 1) infinite;
        }
        
        /* ============================================
           BADGES PREMIUM
           ============================================ */
        
        .label {
          border-radius: 20px;
          padding: 6px 15px;
          font-weight: 700;
          font-size: 11px;
          letter-spacing: 0.5px;
          text-transform: uppercase;
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        }
        
        /* ============================================
           CARDS TIMELINE PREMIUM (HISTÓRICO)
           ============================================ */
        
        .timeline-card {
          background: white;
          border-radius: 16px;
          padding: 25px;
          margin-bottom: 20px;
          box-shadow: 0 4px 16px rgba(0,0,0,0.08);
          transition: all 0.3s ease;
          border-left: 5px solid #667eea;
        }
        
        .timeline-card:hover {
          transform: translateX(10px);
          box-shadow: 0 8px 24px rgba(102, 126, 234, 0.2);
        }
        
        .timeline-badge {
          width: 50px;
          height: 50px;
          border-radius: 50%;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          display: flex;
          align-items: center;
          justify-content: center;
          color: white;
          font-size: 20px;
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
        }
      "))
    ),
    
    
      tabItems(
        
        #===========================================================================
        # ABA 1: DASHBOARD PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "dashboard",
          
          # Header Hero do Dashboard
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 40px; border-radius: 20px; margin-bottom: 30px; 
                   box-shadow: 0 10px 40px rgba(102, 126, 234, 0.3);",
                div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  div(
                    h1(style = "color: white; margin: 0; font-weight: 700; font-size: 32px;",
                       icon("chart-line", style = "margin-right: 15px;"), 
                       "Dashboard Analytics"),
                    p(style = "color: rgba(255,255,255,0.9); margin: 10px 0 0 0; font-size: 16px;",
                      "Visão geral em tempo real das classificações SAP")
                  ),
                  div(
                    style = "text-align: right;",
                    h2(style = "color: white; margin: 0; font-weight: 800; font-size: 48px;",
                       textOutput("dashboard_total_inline", inline = TRUE)),
                    p(style = "color: rgba(255,255,255,0.9); margin: 5px 0 0 0;", "Registros Processados")
                  )
                )
              )
            )
          ),
          
          # Value Boxes Premium
          fluidRow(
            valueBoxOutput("total_textos", width = 3),
            valueBoxOutput("textos_iazf", width = 3),
            valueBoxOutput("precisao", width = 3),
            valueBoxOutput("taxa_conformidade", width = 3)
          ),
          
          # Gráficos Principais - Linha 1
          fluidRow(
            column(
              width = 6,
              box(
                title = div(
                  icon("balance-scale", style = "margin-right: 10px;"),
                  "Comparação: Tipo Anterior vs Novo"
                ),
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                height = 450,
                
                div(
                  style = "padding: 15px;",
                  plotOutput("grafico_comparacao_antes_depois", height = "350px")
                )
              )
            ),
            
            column(
              width = 6,
              box(
                title = div(
                  icon("chart-bar", style = "margin-right: 10px;"),
                  "Distribuição por Tipos SAP"
                ),
                status = "warning",
                solidHeader = TRUE,
                width = NULL,
                height = 450,
                
                div(
                  style = "padding: 15px;",
                  plotOutput("grafico_distribuicao_tipos", height = "350px")
                )
              )
            )
          ),
          
          # Gráficos Principais - Linha 2
          fluidRow(
            column(
              width = 6,
              box(
                title = div(
                  icon("layer-group", style = "margin-right: 10px;"),
                  "Distribuição por Hierarquia"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                height = 450,
                
                div(
                  style = "padding: 15px;",
                  plotOutput("grafico_hierarquia", height = "350px")
                )
              )
            ),
            
            column(
              width = 6,
              box(
                title = div(
                  icon("check-circle", style = "margin-right: 10px;"),
                  "Status de Conformidade"
                ),
                status = "success",
                solidHeader = TRUE,
                width = NULL,
                height = 450,
                
                div(
                  style = "padding: 15px;",
                  plotOutput("grafico_conformidade", height = "350px")
                )
              )
            )
          ),
          
          # Tabela de Últimas Classificações
          fluidRow(
            box(
              title = div(
                icon("history", style = "margin-right: 10px;"),
                "Últimas Classificações Processadas"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 20px;",
                DT::dataTableOutput("tabela_recentes")
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 2: UPLOAD E CRUZAMENTO PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "upload",
          
          # Header da Aba
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
                   padding: 35px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(17, 153, 142, 0.3);",
                h2(style = "color: white; margin: 0; font-weight: 700;",
                   icon("cloud-upload-alt", style = "margin-right: 15px;"), 
                   "Upload & Cruzamento de Dados"),
                p(style = "color: rgba(255,255,255,0.9); margin: 10px 0 0 0; font-size: 15px;",
                  "Faça upload dos arquivos de Ordens e Textos para iniciar o processamento")
              )
            )
          ),
          
          # Cards de Upload
          fluidRow(
            column(
              width = 6,
              box(
                title = div(
                  icon("file-excel", style = "margin-right: 10px; color: #11998e;"),
                  "Arquivo de Ordens"
                ),
                status = "success",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                       padding: 30px; border-radius: 15px; text-align: center; 
                       margin-bottom: 25px; border: 3px dashed #11998e;",
                    icon("database", style = "font-size: 64px; color: #11998e; margin-bottom: 15px;"),
                    h4(style = "color: #2E7D32; margin: 0 0 10px 0;", "Arquivo 1: Ordens SAP"),
                    p(style = "color: #666; font-size: 13px; margin: 0;", 
                      "Arquivo contendo as ordens de manutenção")
                  ),
                  
                  fileInput(
                    "arquivo_ordens",
                    label = NULL,
                    accept = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Escolher Arquivo",
                    placeholder = "Nenhum arquivo selecionado"
                  ),
                  
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 10px; margin-top: 15px;",
                    tags$strong(icon("info-circle"), " Formatos aceitos:", style = "color: #11998e;"),
                    tags$p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                           "CSV, Excel (.xlsx, .xls)")
                  )
                )
              )
            ),
            
            column(
              width = 6,
              box(
                title = div(
                  icon("file-alt", style = "margin-right: 10px; color: #4facfe;"),
                  "Arquivo de Textos"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                       padding: 30px; border-radius: 15px; text-align: center; 
                       margin-bottom: 25px; border: 3px dashed #4facfe;",
                    icon("align-left", style = "font-size: 64px; color: #4facfe; margin-bottom: 15px;"),
                    h4(style = "color: #1565C0; margin: 0 0 10px 0;", "Arquivo 2: Textos"),
                    p(style = "color: #666; font-size: 13px; margin: 0;", 
                      "Arquivo contendo os textos das notas")
                  ),
                  
                  fileInput(
                    "arquivo_textos",
                    label = NULL,
                    accept = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Escolher Arquivo",
                    placeholder = "Nenhum arquivo selecionado"
                  ),
                  
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 10px; margin-top: 15px;",
                    tags$strong(icon("info-circle"), " Formatos aceitos:", style = "color: #4facfe;"),
                    tags$p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                           "CSV, Excel (.xlsx, .xls)")
                  )
                )
              )
            )
          ),
          
          # Botão de Cruzamento e Status
          fluidRow(
            column(
              width = 8,
              box(
                title = div(
                  icon("project-diagram", style = "margin-right: 10px;"),
                  "Executar Cruzamento"
                ),
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 30px; text-align: center;",
                  
                  actionButton(
                    "cruzar",
                    label = div(
                      icon("link", style = "margin-right: 10px; font-size: 20px;"),
                      "CRUZAR DADOS",
                      style = "font-size: 16px; font-weight: 700; letter-spacing: 1px;"
                    ),
                    class = "btn-primary btn-lg",
                    style = "padding: 20px 60px; border-radius: 50px; 
                       box-shadow: 0 8px 24px rgba(102, 126, 234, 0.4);"
                  ),
                  
                  p(style = "margin-top: 20px; color: #666; font-size: 13px;",
                    "O sistema irá cruzar os dados pelos números das notas")
                )
              )
            ),
            
            column(
              width = 4,
              box(
                title = div(
                  icon("chart-pie", style = "margin-right: 10px;"),
                  "Status do Processo"
                ),
                status = "warning",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  htmlOutput("status_cruzamento"),
                  
                  conditionalPanel(
                    condition = "output.cruzamento_concluido",
                    div(
                      style = "margin-top: 20px;",
                      downloadButton(
                        "download_cruzado",
                        label = div(
                          icon("download"), " Download Dados"
                        ),
                        class = "btn-green btn-block",
                        style = "padding: 15px; border-radius: 30px; font-weight: 700;"
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # Preview dos Dados Cruzados
          fluidRow(
            box(
              title = div(
                icon("eye", style = "margin-right: 10px;"),
                "Preview dos Dados Cruzados"
              ),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 25px;",
                
                htmlOutput("estatisticas_cruzados"),
                
                conditionalPanel(
                  condition = "output.cruzamento_concluido",
                  div(
                    style = "margin: 20px 0;",
                    actionButton(
                      "processar_assuntos_preview",
                      label = div(
                        icon("magic"), " Extrair Assuntos (Preview)"
                      ),
                      class = "btn-info",
                      style = "padding: 12px 30px; border-radius: 30px; font-weight: 700;"
                    )
                  )
                ),
                
                htmlOutput("info_preview"),
                
                div(
                  style = "margin-top: 25px;",
                  DT::dataTableOutput("preview_cruzado")
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 3: CLASSIFICAÇÃO INDIVIDUAL PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "individual",
          
          # Header da Aba
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                   padding: 35px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(240, 147, 251, 0.3);",
                h2(style = "color: white; margin: 0; font-weight: 700;",
                   icon("robot", style = "margin-right: 15px;"), 
                   "Classificação Individual com IA"),
                p(style = "color: rgba(255,255,255,0.9); margin: 10px 0 0 0; font-size: 15px;",
                  "Classifique textos individuais e compare com o tipo anterior")
              )
            )
          ),
          
          fluidRow(
            # Card Principal de Classificação
            column(
              width = 8,
              box(
                title = div(
                  icon("brain", style = "margin-right: 10px;"),
                  "Análise de Texto"
                ),
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  # Área de Texto
                  div(
                    style = "margin-bottom: 25px;",
                    tags$label(
                      style = "font-weight: 700; font-size: 15px; color: #333; margin-bottom: 10px; display: block;",
                      icon("edit"), " Digite ou cole o texto para análise:"
                    ),
                    textAreaInput(
                      "texto_individual",
                      label = NULL,
                      height = "180px",
                      placeholder = "Exemplo: Executar manutenção preventiva da bomba P-101 devido a vibração anormal detectada durante inspeção de rotina..."
                    )
                  ),
                  
                  # Opções de Comparação
                  div(
                    style = "background: #f8f9fa; padding: 20px; border-radius: 12px; margin-bottom: 25px;",
                    h5(style = "margin: 0 0 15px 0; color: #667eea; font-weight: 700;",
                       icon("sliders-h"), " Opções de Comparação"),
                    
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "tipo_anterior",
                          label = div(icon("history"), " Tipo Anterior (opcional):"),
                          value = NA,
                          min = 1,
                          max = 6,
                          step = 1
                        )
                      ),
                      column(
                        6,
                        selectInput(
                          "nota_referencia",
                          label = div(icon("search"), " Ou selecione uma Nota:"),
                          choices = NULL
                        )
                      )
                    )
                  ),
                  
                  # Botões de Ação
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; margin-bottom: 25px;",
                    
                    actionButton(
                      "classificar_individual",
                      label = div(
                        icon("rocket", style = "margin-right: 8px;"), 
                        "CLASSIFICAR"
                      ),
                      class = "btn-primary btn-lg",
                      style = "padding: 18px; border-radius: 30px; font-weight: 700; width: 100%;"
                    ),
                    
                    actionButton(
                      "extrair_assunto_individual",
                      label = div(
                        icon("lightbulb", style = "margin-right: 8px;"), 
                        "EXTRAIR ASSUNTO"
                      ),
                      class = "btn-info btn-lg",
                      style = "padding: 18px; border-radius: 30px; font-weight: 700; width: 100%;"
                    ),
                    
                    actionButton(
                      "limpar_individual",
                      label = div(
                        icon("eraser", style = "margin-right: 8px;"), 
                        "LIMPAR"
                      ),
                      class = "btn-secondary btn-lg",
                      style = "padding: 18px; border-radius: 30px; font-weight: 700; width: 100%;"
                    )
                  ),
                  
                  # Resultados
                  div(
                    htmlOutput("assunto_extraido"),
                    htmlOutput("resultado_individual")
                  )
                )
              )
            ),
            
            # Card Lateral de Informações
            column(
              width = 4,
              box(
                title = div(
                  icon("info-circle", style = "margin-right: 10px;"),
                  "Guia Rápido SAP"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 20px;",
                  
                  # Tipos SAP
                  div(
                    style = "margin-bottom: 25px;",
                    h5(style = "color: #667eea; font-weight: 700; margin-bottom: 15px;",
                       icon("list-ol"), " Tipos SAP:"),
                    
                    tags$div(
                      style = "line-height: 2.2;",
                      tags$div(
                        style = "display: flex; align-items: center; padding: 8px; background: linear-gradient(90deg, #87CEEB 0%, rgba(135, 206, 235, 0.1) 100%); border-radius: 8px; margin-bottom: 8px;",
                        tags$span(style = "font-weight: 700; color: #4682B4; width: 30px;", "1"),
                        tags$span(style = "color: #333; font-size: 13px;", "Condicionamento, limpeza")
                      ),
                      tags$div(
                        style = "display: flex; align-items: center; padding: 8px; background: linear-gradient(90deg, #90EE90 0%, rgba(144, 238, 144, 0.1) 100%); border-radius: 8px; margin-bottom: 8px;",
                        tags$span(style = "font-weight: 700; color: #32CD32; width: 30px;", "2"),
                        tags$span(style = "color: #333; font-size: 13px;", "Melhorias, modificações")
                      ),
                      tags$div(
                        style = "display: flex; align-items: center; padding: 8px; background: linear-gradient(90deg, #FFD700 0%, rgba(255, 215, 0, 0.1) 100%); border-radius: 8px; margin-bottom: 8px;",
                        tags$span(style = "font-weight: 700; color: #FFA500; width: 30px;", "3"),
                        tags$span(style = "color: #333; font-size: 13px;", "Manutenção preventiva")
                      ),
                      tags$div(
                        style = "display: flex; align-items: center; padding: 8px; background: linear-gradient(90deg, #FFA500 0%, rgba(255, 165, 0, 0.1) 100%); border-radius: 8px; margin-bottom: 8px;",
                        tags$span(style = "font-weight: 700; color: #FF8C00; width: 30px;", "4"),
                        tags$span(style = "color: #333; font-size: 13px;", "Manutenção por oportunidade")
                      ),
                      tags$div(
                        style = "display: flex; align-items: center; padding: 8px; background: linear-gradient(90deg, #FF6347 0%, rgba(255, 99, 71, 0.1) 100%); border-radius: 8px; margin-bottom: 8px;",
                        tags$span(style = "font-weight: 700; color: #DC143C; width: 30px;", "5"),
                        tags$span(style = "color: #333; font-size: 13px;", "Eliminação de defeito")
                      ),
                      tags$div(
                        style = "display: flex; align-items: center; padding: 8px; background: linear-gradient(90deg, #DC143C 0%, rgba(220, 20, 60, 0.1) 100%); border-radius: 8px;",
                        tags$span(style = "font-weight: 700; color: #8B0000; width: 30px;", "6"),
                        tags$span(style = "color: #333; font-size: 13px;", "Eliminação de falha")
                      )
                    )
                  ),
                  
                  hr(style = "margin: 25px 0; border-color: #e0e0e0;"),
                  
                  # Hierarquia
                  div(
                    style = "margin-bottom: 25px;",
                    h5(style = "color: #667eea; font-weight: 700; margin-bottom: 15px;",
                       icon("sitemap"), " Hierarquia:"),
                    
                    div(
                      style = "background: #e8f5e9; padding: 12px; border-radius: 10px; margin-bottom: 10px; border-left: 4px solid #4CAF50;",
                      tags$strong("PROBLEMAS_COMUNS", style = "color: #2E7D32;"),
                      tags$br(),
                      tags$span("Tipos 1, 2, 3, 4", style = "font-size: 12px; color: #666;")
                    ),
                    
                    div(
                      style = "background: #ffebee; padding: 12px; border-radius: 10px; border-left: 4px solid #f44336;",
                      tags$strong("IAZF", style = "color: #c62828;"),
                      tags$br(),
                      tags$span("Tipos 5, 6", style = "font-size: 12px; color: #666;")
                    )
                  ),
                  
                  hr(style = "margin: 25px 0; border-color: #e0e0e0;"),
                  
                  # Criticidade
                  div(
                    h5(style = "color: #667eea; font-weight: 700; margin-bottom: 15px;",
                       icon("exclamation-triangle"), " Criticidade:"),
                    
                    div(
                      style = "display: grid; gap: 10px;",
                      tags$span(
                        "BAIXA", 
                        style = "background: linear-gradient(135deg, #4682B4 0%, #87CEEB 100%); 
                           color: white; padding: 10px 20px; border-radius: 25px; 
                           text-align: center; font-weight: 700; box-shadow: 0 4px 12px rgba(70, 130, 180, 0.3);"
                      ),
                      tags$span(
                        "MÉDIA", 
                        style = "background: linear-gradient(135deg, #32CD32 0%, #90EE90 100%); 
                           color: white; padding: 10px 20px; border-radius: 25px; 
                           text-align: center; font-weight: 700; box-shadow: 0 4px 12px rgba(50, 205, 50, 0.3);"
                      ),
                      tags$span(
                        "ALTA", 
                        style = "background: linear-gradient(135deg, #FF8C00 0%, #FFA500 100%); 
                           color: white; padding: 10px 20px; border-radius: 25px; 
                           text-align: center; font-weight: 700; box-shadow: 0 4px 12px rgba(255, 140, 0, 0.3);"
                      ),
                      tags$span(
                        "CRÍTICA", 
                        style = "background: linear-gradient(135deg, #DC143C 0%, #8B0000 100%); 
                           color: white; padding: 10px 20px; border-radius: 25px; 
                           text-align: center; font-weight: 700; box-shadow: 0 4px 12px rgba(220, 20, 60, 0.3);"
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 4: CLASSIFICAÇÃO EM LOTE PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "lote",
          
          # Header da Aba
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                   padding: 35px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(79, 172, 254, 0.3);",
                h2(style = "color: white; margin: 0; font-weight: 700;",
                   icon("tasks", style = "margin-right: 15px;"), 
                   "Processamento em Lote"),
                p(style = "color: rgba(255,255,255,0.9); margin: 10px 0 0 0; font-size: 15px;",
                  "Classifique milhares de textos automaticamente usando IA e Dicionários")
              )
            )
          ),
          
          # Card Principal
          fluidRow(
            box(
              title = div(
                icon("layer-group", style = "margin-right: 10px;"),
                "Configuração e Execução"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                # Mensagem Informativa
                div(
                  style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                     padding: 25px; border-radius: 15px; margin-bottom: 30px;
                     border-left: 5px solid #2196F3;",
                  div(
                    style = "display: flex; align-items: center;",
                    icon("info-circle", style = "font-size: 48px; color: #2196F3; margin-right: 20px;"),
                    div(
                      h4(style = "margin: 0 0 10px 0; color: #1565C0; font-weight: 700;",
                         "Processamento Inteligente em Lote"),
                      p(style = "margin: 0; color: #666; font-size: 14px; line-height: 1.6;",
                        "Classifique automaticamente todos os textos do arquivo cruzado usando IA e/ou Dicionário. ",
                        "O sistema processará cada registro e gerará classificações precisas com base nos métodos configurados.")
                    )
                  )
                ),
                
                # Área de Configuração
                conditionalPanel(
                  condition = "output.cruzamento_concluido",
                  
                  # Opções de Processamento
                  div(
                    style = "background: white; padding: 30px; border-radius: 15px; 
                       box-shadow: 0 4px 16px rgba(0,0,0,0.08); margin-bottom: 30px;",
                    
                    h4(style = "margin: 0 0 20px 0; color: #667eea; font-weight: 700;",
                       icon("cog"), " Opções de Processamento"),
                    
                    div(
                      style = "background: #f8f9fa; padding: 20px; border-radius: 12px;",
                      checkboxInput(
                        "extrair_assunto",
                        label = div(
                          style = "font-size: 15px; font-weight: 600;",
                          icon("magic", style = "margin-right: 10px; color: #667eea;"),
                          "Extrair Assunto Principal usando IA (Recomendado)"
                        ),
                        value = TRUE
                      ),
                      p(style = "font-size: 13px; color: #666; margin: 10px 0 0 35px; line-height: 1.6;",
                        icon("lightbulb", style = "color: #ffc107;"),
                        " A IA irá gerar um resumo conciso e objetivo do assunto principal de cada texto (máximo 80 caracteres).")
                    )
                  ),
                  
                  # Botões de Ação
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin-bottom: 30px;",
                    
                    actionButton(
                      "classificar_lote",
                      label = div(
                        icon("rocket", style = "margin-right: 10px; font-size: 18px;"),
                        "INICIAR PROCESSAMENTO",
                        style = "font-size: 15px; font-weight: 700; letter-spacing: 0.5px;"
                      ),
                      class = "btn-primary btn-lg",
                      style = "padding: 22px; border-radius: 35px; width: 100%;
                         box-shadow: 0 8px 24px rgba(102, 126, 234, 0.4);"
                    ),
                    
                    downloadButton(
                      "download_resultados",
                      label = div(
                        icon("download", style = "margin-right: 10px; font-size: 18px;"),
                        "DOWNLOAD RESULTADOS"
                      ),
                      class = "btn-green btn-lg",
                      style = "padding: 22px; border-radius: 35px; width: 100%; font-weight: 700;
                         box-shadow: 0 8px 24px rgba(17, 153, 142, 0.4);"
                    ),
                    
                    actionButton(
                      "limpar_lote",
                      label = div(
                        icon("trash-alt", style = "margin-right: 10px; font-size: 18px;"),
                        "LIMPAR RESULTADOS"
                      ),
                      class = "btn-secondary btn-lg",
                      style = "padding: 22px; border-radius: 35px; width: 100%; font-weight: 700;"
                    )
                  ),
                  
                  # Barra de Progresso
                  div(
                    style = "margin-bottom: 30px;",
                    htmlOutput("progresso_lote")
                  ),
                  
                  # Tabela de Resultados
                  div(
                    style = "background: white; padding: 30px; border-radius: 15px;
                       box-shadow: 0 4px 16px rgba(0,0,0,0.08);",
                    h4(style = "margin: 0 0 25px 0; color: #667eea; font-weight: 700;",
                       icon("table"), " Resultados da Classificação em Lote"),
                    DT::dataTableOutput("tabela_resultados_lote")
                  )
                ),
                
                # Mensagem quando não há dados
                conditionalPanel(
                  condition = "!output.cruzamento_concluido",
                  div(
                    style = "text-align: center; padding: 80px 40px;",
                    icon("inbox", style = "font-size: 96px; color: #e0e0e0; margin-bottom: 25px;"),
                    h3(style = "color: #999; margin: 0 0 15px 0; font-weight: 600;",
                       "Nenhum Dado Disponível"),
                    p(style = "color: #999; font-size: 15px; margin: 0 0 30px 0;",
                      "Faça o upload e cruzamento dos arquivos primeiro para começar o processamento em lote."),
                    actionButton(
                      "ir_para_upload",
                      label = div(
                        icon("arrow-right", style = "margin-right: 10px;"),
                        "IR PARA UPLOAD"
                      ),
                      class = "btn-primary btn-lg",
                      style = "padding: 15px 40px; border-radius: 30px; font-weight: 700;",
                      onclick = "document.querySelector('[data-value=\"upload\"]').click();"
                    )
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 5: DICIONÁRIOS PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "dicionarios",
          
          # Header Hero
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 40px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(102, 126, 234, 0.3);",
                div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  div(
                    h1(style = "color: white; margin: 0; font-weight: 700; font-size: 32px;",
                       icon("book", style = "margin-right: 15px;"), 
                       "Gerenciamento de Dicionários SAP"),
                    p(style = "color: rgba(255,255,255,0.9); margin: 10px 0 0 0; font-size: 16px;",
                      "Configure palavras-chave e critérios para cada tipo de intervenção")
                  ),
                  actionButton(
                    "resetar_dicionarios",
                    label = div(
                      icon("undo-alt"), " Restaurar Padrão"
                    ),
                    style = "background: rgba(255,255,255,0.2); color: white; 
                       border: 2px solid white; border-radius: 30px; 
                       padding: 15px 30px; font-weight: 700;"
                  )
                )
              )
            )
          ),
          
          # Configurações de Método
          fluidRow(
            box(
              title = div(
                icon("sliders-h", style = "margin-right: 10px;"),
                "Configurações de Classificação"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 25px;",
                  
                  # Card Método
                  div(
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);",
                    icon("route", style = "font-size: 56px; color: white; margin-bottom: 20px;"),
                    h4(style = "color: white; margin: 0 0 20px 0; font-weight: 700;",
                       "Método de Classificação"),
                    selectInput(
                      "metodo_classificacao",
                      label = NULL,
                      choices = c(
                        "🔀 Híbrido (Recomendado)" = "HIBRIDO",
                        "📖 Apenas Dicionário" = "DICIONARIO",
                        "🤖 Apenas API" = "API"
                      ),
                      selected = "HIBRIDO"
                    ),
                    p(style = "color: rgba(255,255,255,0.9); font-size: 12px; margin: 10px 0 0 0;",
                      "Híbrido combina o melhor dos dois métodos")
                  ),
                  
                  # Card Dicionário
                  div(
                    style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3);",
                    icon("book-open", style = "font-size: 56px; color: white; margin-bottom: 20px;"),
                    h4(style = "color: white; margin: 0 0 20px 0; font-weight: 700;",
                       "Usar Dicionário"),
                    div(
                      style = "display: flex; justify-content: center;",
                      checkboxInput("usar_dicionario", label = NULL, value = TRUE)
                    ),
                    div(
                      style = "background: rgba(255,255,255,0.2); padding: 12px; 
                         border-radius: 10px; margin-top: 15px;",
                      p(style = "margin: 0; font-size: 12px; color: white;",
                        "✓ Rápido e eficiente", tags$br(),
                        "✓ Baseado em palavras-chave")
                    )
                  ),
                  
                  # Card API
                  div(
                    style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(79, 172, 254, 0.3);",
                    icon("robot", style = "font-size: 56px; color: white; margin-bottom: 20px;"),
                    h4(style = "color: white; margin: 0 0 20px 0; font-weight: 700;",
                       "Usar IA (API)"),
                    div(
                      style = "display: flex; justify-content: center;",
                      checkboxInput("usar_api", label = NULL, value = TRUE)
                    ),
                    div(
                      style = "background: rgba(255,255,255,0.2); padding: 12px; 
                         border-radius: 10px; margin-top: 15px;",
                      p(style = "margin: 0; font-size: 12px; color: white;",
                        "✓ Análise contextual", tags$br(),
                        "✓ Alta precisão")
                    )
                  )
                ),
                
                div(
                  style = "text-align: center; margin-top: 30px;",
                  actionButton(
                    "salvar_config_metodo",
                    label = div(
                      icon("save", style = "margin-right: 10px; font-size: 16px;"),
                      "SALVAR CONFIGURAÇÕES"
                    ),
                    class = "btn-success btn-lg",
                    style = "padding: 18px 50px; border-radius: 35px; font-weight: 700;
                       box-shadow: 0 8px 24px rgba(17, 153, 142, 0.4);"
                  )
                )
              )
            )
          ),
          
          # Abas dos Tipos
          fluidRow(
            box(
              title = div(
                icon("edit", style = "margin-right: 10px;"),
                "Editar Dicionários por Tipo"
              ),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 25px;",
                
                tabsetPanel(
                  id = "tabs_dicionarios",
                  type = "pills",
                  
                  # TIPO 1
                  tabPanel(
                    title = div(icon("circle", style = "color: #87CEEB;"), " Tipo 1"),
                    value = "tipo1",
                    
                    div(
                      style = "padding: 30px 0;",
                      
                      div(
                        class = "header-tipo-1",
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          div(
                            h2(style = "margin: 0; color: white; font-weight: 700;",
                               icon("broom"), " Tipo 1 - Condicionamento e Limpeza"),
                            p(style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.95); font-size: 15px;",
                              "Intervenções de baixa complexidade e rotineiras")
                          ),
                          div(
                            style = "text-align: center; background: rgba(255,255,255,0.3); 
                               padding: 15px 30px; border-radius: 30px; border: 2px solid white;",
                            h3(style = "margin: 0; color: white; font-weight: 800;", "BAIXA"),
                            p(style = "margin: 5px 0 0 0; color: white; font-size: 11px; letter-spacing: 1px;",
                              "CRITICIDADE")
                          )
                        )
                      ),
                      
                      div(
                        style = "background: white; padding: 35px; border-radius: 0 0 20px 20px;",
                        
                        fluidRow(
                          column(
                            6,
                            div(
                              style = "margin-bottom: 25px;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("file-alt"), " Descrição SAP"),
                              textAreaInput(
                                "desc_tipo_1",
                                label = NULL,
                                value = "Condicionamento, limpeza, arrumação, preservação ou pintura",
                                rows = 4
                              )
                            ),
                            div(
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("lightbulb"), " Quando Utilizar"),
                              textAreaInput(
                                "quando_tipo_1",
                                label = NULL,
                                value = "Use para manutenções simples e rotineiras",
                                rows = 5
                              )
                            )
                          ),
                          column(
                            6,
                            div(
                              style = "background: #f8f9fa; padding: 25px; border-radius: 12px;
                                 border-left: 5px solid #87CEEB;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("tags"), " Palavras-Chave"),
                              textAreaInput(
                                "palavras_tipo_1",
                                label = NULL,
                                value = "limpeza\npintura\ncondicionamento\nlubrificação\nhigienização",
                                rows = 14
                              )
                            )
                          )
                        ),
                        
                        div(
                          style = "text-align: center; margin-top: 30px;",
                          actionButton(
                            "salvar_tipo_1",
                            label = div(
                              icon("save", style = "margin-right: 10px;"),
                              "SALVAR TIPO 1"
                            ),
                            class = "btn-lg btn-tipo-1"
                          )
                        )
                      )
                    )
                  ),
                  
                  # TIPO 2
                  tabPanel(
                    title = div(icon("circle", style = "color: #90EE90;"), " Tipo 2"),
                    value = "tipo2",
                    
                    div(
                      style = "padding: 30px 0;",
                      
                      div(
                        class = "header-tipo-2",
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          div(
                            h2(style = "margin: 0; color: white; font-weight: 700;",
                               icon("wrench"), " Tipo 2 - Melhorias e Modificações"),
                            p(style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.95); font-size: 15px;",
                              "Alterações planejadas no sistema")
                          ),
                          div(
                            style = "text-align: center; background: rgba(255,255,255,0.3); 
                               padding: 15px 30px; border-radius: 30px; border: 2px solid white;",
                            h3(style = "margin: 0; color: white; font-weight: 800;", "BAIXA"),
                            p(style = "margin: 5px 0 0 0; color: white; font-size: 11px; letter-spacing: 1px;",
                              "CRITICIDADE")
                          )
                        )
                      ),
                      
                      div(
                        style = "background: white; padding: 35px; border-radius: 0 0 20px 20px;",
                        
                        fluidRow(
                          column(
                            6,
                            div(
                              style = "margin-bottom: 25px;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("file-alt"), " Descrição SAP"),
                              textAreaInput(
                                "desc_tipo_2",
                                label = NULL,
                                value = "Melhorias, modificações, testes, instalação ou regulagem",
                                rows = 4
                              )
                            ),
                            div(
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("lightbulb"), " Quando Utilizar"),
                              textAreaInput(
                                "quando_tipo_2",
                                label = NULL,
                                value = "Use para modificações planejadas",
                                rows = 5
                              )
                            )
                          ),
                          column(
                            6,
                            div(
                              style = "background: #f8f9fa; padding: 25px; border-radius: 12px;
                                 border-left: 5px solid #90EE90;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("tags"), " Palavras-Chave"),
                              textAreaInput(
                                "palavras_tipo_2",
                                label = NULL,
                                value = "melhoria\nmodificação\nteste\ninstalação\nregulagem\nupgrade",
                                rows = 14
                              )
                            )
                          )
                        ),
                        
                        div(
                          style = "text-align: center; margin-top: 30px;",
                          actionButton(
                            "salvar_tipo_2",
                            label = div(
                              icon("save", style = "margin-right: 10px;"),
                              "SALVAR TIPO 2"
                            ),
                            class = "btn-lg btn-tipo-2"
                          )
                        )
                      )
                    )
                  ),
                  
                  # TIPO 3
                  tabPanel(
                    title = div(icon("circle", style = "color: #FFD700;"), " Tipo 3"),
                    value = "tipo3",
                    
                    div(
                      style = "padding: 30px 0;",
                      
                      div(
                        class = "header-tipo-3",
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          div(
                            h2(style = "margin: 0; color: white; font-weight: 700;",
                               icon("calendar-check"), " Tipo 3 - Manutenção Preventiva"),
                            p(style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.95); font-size: 15px;",
                              "Manutenções programadas e inspeções")
                          ),
                          div(
                            style = "text-align: center; background: rgba(255,255,255,0.3); 
                               padding: 15px 30px; border-radius: 30px; border: 2px solid white;",
                            h3(style = "margin: 0; color: white; font-weight: 800;", "MÉDIA"),
                            p(style = "margin: 5px 0 0 0; color: white; font-size: 11px; letter-spacing: 1px;",
                              "CRITICIDADE")
                          )
                        )
                      ),
                      
                      div(
                        style = "background: white; padding: 35px; border-radius: 0 0 20px 20px;",
                        
                        fluidRow(
                          column(
                            6,
                            div(
                              style = "margin-bottom: 25px;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("file-alt"), " Descrição SAP"),
                              textAreaInput(
                                "desc_tipo_3",
                                label = NULL,
                                value = "Manutenção preventiva, preditiva ou inspeção planejada",
                                rows = 4
                              )
                            ),
                            div(
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("lightbulb"), " Quando Utilizar"),
                              textAreaInput(
                                "quando_tipo_3",
                                label = NULL,
                                value = "Use para manutenções programadas",
                                rows = 5
                              )
                            )
                          ),
                          column(
                            6,
                            div(
                              style = "background: #f8f9fa; padding: 25px; border-radius: 12px;
                                 border-left: 5px solid #FFD700;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("tags"), " Palavras-Chave"),
                              textAreaInput(
                                "palavras_tipo_3",
                                label = NULL,
                                value = "preventiva\npreditiva\ninspeção\nplanejada\nprogramada\nverificação",
                                rows = 14
                              )
                            )
                          )
                        ),
                        
                        div(
                          style = "text-align: center; margin-top: 30px;",
                          actionButton(
                            "salvar_tipo_3",
                            label = div(
                              icon("save", style = "margin-right: 10px;"),
                              "SALVAR TIPO 3"
                            ),
                            class = "btn-lg btn-tipo-3"
                          )
                        )
                      )
                    )
                  ),
                  
                  # TIPO 4
                  tabPanel(
                    title = div(icon("circle", style = "color: #FFA500;"), " Tipo 4"),
                    value = "tipo4",
                    
                    div(
                      style = "padding: 30px 0;",
                      
                      div(
                        class = "header-tipo-4",
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          div(
                            h2(style = "margin: 0; color: white; font-weight: 700;",
                               icon("clock"), " Tipo 4 - Manutenção por Oportunidade"),
                            p(style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.95); font-size: 15px;",
                              "Aproveitamento de paradas")
                          ),
                          div(
                            style = "text-align: center; background: rgba(255,255,255,0.3); 
                               padding: 15px 30px; border-radius: 30px; border: 2px solid white;",
                            h3(style = "margin: 0; color: white; font-weight: 800;", "MÉDIA"),
                            p(style = "margin: 5px 0 0 0; color: white; font-size: 11px; letter-spacing: 1px;",
                              "CRITICIDADE")
                          )
                        )
                      ),
                      
                      div(
                        style = "background: white; padding: 35px; border-radius: 0 0 20px 20px;",
                        
                        fluidRow(
                          column(
                            6,
                            div(
                              style = "margin-bottom: 25px;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("file-alt"), " Descrição SAP"),
                              textAreaInput(
                                "desc_tipo_4",
                                label = NULL,
                                value = "Manutenção por oportunidade ou inspeção não programada",
                                rows = 4
                              )
                            ),
                            div(
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("lightbulb"), " Quando Utilizar"),
                              textAreaInput(
                                "quando_tipo_4",
                                label = NULL,
                                value = "Use para manutenções não programadas",
                                rows = 5
                              )
                            )
                          ),
                          column(
                            6,
                            div(
                              style = "background: #f8f9fa; padding: 25px; border-radius: 12px;
                                 border-left: 5px solid #FFA500;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("tags"), " Palavras-Chave"),
                              textAreaInput(
                                "palavras_tipo_4",
                                label = NULL,
                                value = "oportunidade\nnão programada\neventual\ndisponível\nparada",
                                rows = 14
                              )
                            )
                          )
                        ),
                        
                        div(
                          style = "text-align: center; margin-top: 30px;",
                          actionButton(
                            "salvar_tipo_4",
                            label = div(
                              icon("save", style = "margin-right: 10px;"),
                              "SALVAR TIPO 4"
                            ),
                            class = "btn-lg btn-tipo-4"
                          )
                        )
                      )
                    )
                  ),
                  
                  # TIPO 5
                  tabPanel(
                    title = div(icon("circle", style = "color: #FF6347;"), " Tipo 5"),
                    value = "tipo5",
                    
                    div(
                      style = "padding: 30px 0;",
                      
                      div(
                        class = "header-tipo-5",
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          div(
                            h2(style = "margin: 0; color: white; font-weight: 700;",
                               icon("exclamation-triangle"), " Tipo 5 - Eliminação de Defeito (IAZF)"),
                            p(style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.95); font-size: 15px;",
                              "Equipamento com restrição operacional")
                          ),
                          div(
                            style = "text-align: center; background: rgba(255,255,255,0.3); 
                               padding: 15px 30px; border-radius: 30px; border: 2px solid white;",
                            h3(style = "margin: 0; color: white; font-weight: 800;", "ALTA"),
                            p(style = "margin: 5px 0 0 0; color: white; font-size: 11px; letter-spacing: 1px;",
                              "CRITICIDADE")
                          )
                        )
                      ),
                      
                      div(
                        style = "background: white; padding: 35px; border-radius: 0 0 20px 20px;",
                        
                        fluidRow(
                          column(
                            6,
                            div(
                              style = "margin-bottom: 25px;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("file-alt"), " Descrição SAP"),
                              textAreaInput(
                                "desc_tipo_5",
                                label = NULL,
                                value = "Intervenção para eliminação de defeito",
                                rows = 4
                              )
                            ),
                            div(
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("lightbulb"), " Quando Utilizar"),
                              textAreaInput(
                                "quando_tipo_5",
                                label = NULL,
                                value = "Use para correção de defeitos",
                                rows = 5
                              )
                            )
                          ),
                          column(
                            6,
                            div(
                              style = "background: #f8f9fa; padding: 25px; border-radius: 12px;
                                 border-left: 5px solid #FF6347;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("tags"), " Palavras-Chave"),
                              textAreaInput(
                                "palavras_tipo_5",
                                label = NULL,
                                value = "defeito\nproblema\nanomalia\nrestrição\nlimitação\ndegradação",
                                rows = 14
                              )
                            )
                          )
                        ),
                        
                        div(
                          style = "text-align: center; margin-top: 30px;",
                          actionButton(
                            "salvar_tipo_5",
                            label = div(
                              icon("save", style = "margin-right: 10px;"),
                              "SALVAR TIPO 5"
                            ),
                            class = "btn-lg btn-tipo-5"
                          )
                        )
                      )
                    )
                  ),
                  
                  # TIPO 6
                  tabPanel(
                    title = div(icon("circle", style = "color: #DC143C;"), " Tipo 6"),
                    value = "tipo6",
                    
                    div(
                      style = "padding: 30px 0;",
                      
                      div(
                        class = "header-tipo-6",
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          div(
                            h2(style = "margin: 0; color: white; font-weight: 700;",
                               icon("times-circle"), " Tipo 6 - Eliminação de Falha (IAZF)"),
                            p(style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.95); font-size: 15px;",
                              "Sistema indisponível - Emergência")
                          ),
                          div(
                            style = "text-align: center; background: rgba(255,255,255,0.3); 
                               padding: 15px 30px; border-radius: 30px; border: 2px solid white;",
                            h3(style = "margin: 0; color: white; font-weight: 800;", "CRÍTICA"),
                            p(style = "margin: 5px 0 0 0; color: white; font-size: 11px; letter-spacing: 1px;",
                              "CRITICIDADE")
                          )
                        )
                      ),
                      
                      div(
                        style = "background: white; padding: 35px; border-radius: 0 0 20px 20px;",
                        
                        fluidRow(
                          column(
                            6,
                            div(
                              style = "margin-bottom: 25px;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("file-alt"), " Descrição SAP"),
                              textAreaInput(
                                "desc_tipo_6",
                                label = NULL,
                                value = "Intervenção para eliminação de falha",
                                rows = 4
                              )
                            ),
                            div(
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("lightbulb"), " Quando Utilizar"),
                              textAreaInput(
                                "quando_tipo_6",
                                label = NULL,
                                value = "Use para falhas críticas",
                                rows = 5
                              )
                            )
                          ),
                          column(
                            6,
                            div(
                              style = "background: #f8f9fa; padding: 25px; border-radius: 12px;
                                 border-left: 5px solid #DC143C;",
                              h4(style = "margin: 0 0 15px 0; color: #333; font-weight: 700;",
                                 icon("tags"), " Palavras-Chave"),
                              textAreaInput(
                                "palavras_tipo_6",
                                label = NULL,
                                value = "falha\nquebra\npane\nemergência\ncrítica\nparada total\nindisponível",
                                rows = 14
                              )
                            )
                          )
                        ),
                        
                        div(
                          style = "text-align: center; margin-top: 30px;",
                          actionButton(
                            "salvar_tipo_6",
                            label = div(
                              icon("save", style = "margin-right: 10px;"),
                              "SALVAR TIPO 6"
                            ),
                            class = "btn-lg btn-tipo-6"
                          )
                        )
                      )
                    )
                  )
                  
                ) # Fecha tabsetPanel
              )
            )
          )
        ),
        
       
      
        #===========================================================================
        # ABA 6: ESTATÍSTICAS PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "estatisticas",
          
          # Header Hero
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); 
                   padding: 40px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(250, 112, 154, 0.3);",
                div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  div(
                    h1(style = "color: white; margin: 0; font-weight: 700; font-size: 32px;",
                       icon("chart-pie", style = "margin-right: 15px;"), 
                       "Estatísticas e Análises"),
                    p(style = "color: rgba(255,255,255,0.95); margin: 10px 0 0 0; font-size: 16px;",
                      "Métricas detalhadas de desempenho e qualidade")
                  ),
                  div(
                    style = "text-align: right;",
                    actionButton(
                      "atualizar_estatisticas",
                      label = div(icon("sync-alt"), " Atualizar"),
                      style = "background: rgba(255,255,255,0.2); color: white; 
                         border: 2px solid white; border-radius: 30px; 
                         padding: 12px 25px; font-weight: 700;"
                    )
                  )
                )
              )
            )
          ),
          
          # Value Boxes de Métricas
          fluidRow(
            valueBoxOutput("metrica_total_classificados", width = 3),
            valueBoxOutput("metrica_acuracia", width = 3),
            valueBoxOutput("metrica_conformes", width = 3),
            valueBoxOutput("metrica_divergentes", width = 3)
          ),
          
          # Gráficos Principais - Linha 1
          fluidRow(
            column(
              width = 6,
              box(
                title = div(
                  icon("th", style = "margin-right: 10px;"),
                  "Matriz de Confusão"
                ),
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: #e3f2fd; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #2196F3;",
                    tags$strong(icon("info-circle"), " Como Interpretar:", style = "color: #1565C0;"),
                    p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                      "Linhas = Tipo Anterior | Colunas = Tipo Novo | Diagonal = Concordâncias")
                  ),
                  
                  plotOutput("matriz_confusao", height = "400px")
                )
              )
            ),
            
            column(
              width = 6,
              box(
                title = div(
                  icon("bullseye", style = "margin-right: 10px;"),
                  "Acurácia por Tipo"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: #e8f5e9; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #4CAF50;",
                    tags$strong(icon("check-circle"), " Legenda:", style = "color: #2E7D32;"),
                    p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                      "Verde ≥80% | Laranja 60-79% | Vermelho <60%")
                  ),
                  
                  plotOutput("grafico_acuracia_tipo", height = "400px")
                )
              )
            )
          ),
          
          # Gráficos Principais - Linha 2
          fluidRow(
            column(
              width = 6,
              box(
                title = div(
                  icon("chart-area", style = "margin-right: 10px;"),
                  "Distribuição de Confiança"
                ),
                status = "success",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 10px; margin-bottom: 20px;",
                    
                    div(
                      style = "background: #d4edda; padding: 12px; border-radius: 8px; text-align: center;",
                      tags$strong("90-100%", style = "color: #155724; font-size: 13px;"),
                      tags$br(),
                      tags$span("Muito Alta", style = "font-size: 11px; color: #155724;")
                    ),
                    div(
                      style = "background: #d1ecf1; padding: 12px; border-radius: 8px; text-align: center;",
                      tags$strong("80-89%", style = "color: #0c5460; font-size: 13px;"),
                      tags$br(),
                      tags$span("Alta", style = "font-size: 11px; color: #0c5460;")
                    ),
                    div(
                      style = "background: #fff3cd; padding: 12px; border-radius: 8px; text-align: center;",
                      tags$strong("70-79%", style = "color: #856404; font-size: 13px;"),
                      tags$br(),
                      tags$span("Média", style = "font-size: 11px; color: #856404;")
                    ),
                    div(
                      style = "background: #f8d7da; padding: 12px; border-radius: 8px; text-align: center;",
                      tags$strong("<70%", style = "color: #721c24; font-size: 13px;"),
                      tags$br(),
                      tags$span("Baixa", style = "font-size: 11px; color: #721c24;")
                    )
                  ),
                  
                  plotOutput("grafico_distribuicao_confianca", height = "350px")
                )
              )
            ),
            
            column(
              width = 6,
              box(
                title = div(
                  icon("project-diagram", style = "margin-right: 10px;"),
                  "Métodos Utilizados"
                ),
                status = "warning",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: #fff3cd; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #ffc107;",
                    tags$strong(icon("lightbulb"), " Métodos:", style = "color: #856404;"),
                    tags$ul(
                      style = "margin: 10px 0 0 0; font-size: 12px; color: #666; line-height: 1.8;",
                      tags$li("HÍBRIDO_CONCORDANTE: Melhor resultado"),
                      tags$li("DICIONÁRIO: Rápido e offline"),
                      tags$li("API: Alta precisão contextual")
                    )
                  ),
                  
                  plotOutput("grafico_metodos", height = "350px")
                )
              )
            )
          ),
          
          # Tabelas Detalhadas
          fluidRow(
            box(
              title = div(
                icon("table", style = "margin-right: 10px;"),
                "Análise Detalhada de Métricas"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 25px;",
                
                tabsetPanel(
                  type = "pills",
                  
                  tabPanel(
                    title = div(icon("list-ol"), " Por Tipo"),
                    br(),
                    div(
                      style = "padding: 20px;",
                      DT::dataTableOutput("tabela_metricas_tipo")
                    )
                  ),
                  
                  tabPanel(
                    title = div(icon("layer-group"), " Por Categoria"),
                    br(),
                    div(
                      style = "padding: 20px;",
                      DT::dataTableOutput("tabela_metricas_categoria")
                    )
                  ),
                  
                  tabPanel(
                    title = div(icon("cogs"), " Por Método"),
                    br(),
                    div(
                      style = "padding: 20px;",
                      DT::dataTableOutput("tabela_metricas_metodo")
                    )
                  ),
                  
                  tabPanel(
                    title = div(icon("exclamation-triangle"), " Divergências"),
                    br(),
                    div(
                      style = "padding: 20px;",
                      div(
                        style = "background: #fff3cd; padding: 20px; border-radius: 12px; 
                           margin-bottom: 20px; border-left: 4px solid #ffc107;",
                        tags$strong(icon("info-circle"), " Atenção:", style = "color: #856404; font-size: 15px;"),
                        p(style = "margin: 10px 0 0 0; color: #856404; font-size: 13px;",
                          "Esta tabela mostra os registros onde a IA divergiu da classificação anterior. ",
                          "Revise manualmente os casos críticos (Tipos 5 e 6).")
                      ),
                      DT::dataTableOutput("tabela_divergencias_detalhadas")
                    )
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 7: CONFIGURAÇÕES API PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "configuracoes",
          
          # Header Hero
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 40px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(102, 126, 234, 0.3);",
                div(
                  style = "display: flex; align-items: center;",
                  icon("cogs", style = "font-size: 64px; color: white; margin-right: 25px;"),
                  div(
                    h1(style = "color: white; margin: 0; font-weight: 700; font-size: 32px;",
                       "Configurações da API OpenAI"),
                    p(style = "color: rgba(255,255,255,0.95); margin: 10px 0 0 0; font-size: 16px;",
                      "Configure as credenciais de acesso à API da Petrobras")
                  )
                )
              )
            )
          ),
          
          fluidRow(
            # Card de Configuração
            column(
              width = 8,
              box(
                title = div(
                  icon("key", style = "margin-right: 10px;"),
                  "Credenciais de Acesso"
                ),
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 30px;",
                  
                  # Banner Informativo
                  div(
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                       padding: 25px; border-radius: 15px; margin-bottom: 30px;
                       text-align: center;",
                    icon("shield-alt", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                    h4(style = "color: white; margin: 0 0 10px 0; font-weight: 700;",
                       "Ambiente Seguro"),
                    p(style = "color: rgba(255,255,255,0.95); margin: 0; font-size: 14px;",
                      "Todas as credenciais são armazenadas de forma segura e criptografada")
                  ),
                  
                  # Campos de Configuração
                  div(
                    style = "background: #f8f9fa; padding: 30px; border-radius: 15px;",
                    
                    textInput(
                      "config_base_url",
                      label = div(
                        style = "font-weight: 700; font-size: 15px; margin-bottom: 10px;",
                        icon("server", style = "margin-right: 10px; color: #667eea;"),
                        "Base URL da API:"
                      ),
                      value = OPENAI_CONFIG$base_url,
                      width = "100%"
                    ),
                    
                    passwordInput(
                      "config_api_key",
                      label = div(
                        style = "font-weight: 700; font-size: 15px; margin-bottom: 10px;",
                        icon("key", style = "margin-right: 10px; color: #667eea;"),
                        "API Key (Chave de Acesso):"
                      ),
                      value = OPENAI_CONFIG$api_key,
                      width = "100%"
                    ),
                    
                    textInput(
                      "config_model",
                      label = div(
                        style = "font-weight: 700; font-size: 15px; margin-bottom: 10px;",
                        icon("brain", style = "margin-right: 10px; color: #667eea;"),
                        "Modelo de IA:"
                      ),
                      value = OPENAI_CONFIG$model,
                      width = "100%"
                    ),
                    
                    textInput(
                      "config_api_version",
                      label = div(
                        style = "font-weight: 700; font-size: 15px; margin-bottom: 10px;",
                        icon("code-branch", style = "margin-right: 10px; color: #667eea;"),
                        "Versão da API:"
                      ),
                      value = OPENAI_CONFIG$api_version,
                      width = "100%"
                    )
                  ),
                  
                  # Botões de Ação
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-top: 30px;",
                    
                    actionButton(
                      "salvar_config",
                      label = div(
                        icon("save", style = "margin-right: 10px; font-size: 18px;"),
                        "SALVAR CONFIGURAÇÕES"
                      ),
                      class = "btn-success btn-lg",
                      style = "padding: 20px; border-radius: 35px; font-weight: 700; width: 100%;
                         box-shadow: 0 8px 24px rgba(17, 153, 142, 0.4);"
                    ),
                    
                    actionButton(
                      "testar_api",
                      label = div(
                        icon("plug", style = "margin-right: 10px; font-size: 18px;"),
                        "TESTAR CONEXÃO"
                      ),
                      class = "btn-info btn-lg",
                      style = "padding: 20px; border-radius: 35px; font-weight: 700; width: 100%;
                         box-shadow: 0 8px 24px rgba(79, 172, 254, 0.4);"
                    )
                  )
                )
              )
            ),
            
            # Card de Status
            column(
              width = 4,
              box(
                title = div(
                  icon("signal", style = "margin-right: 10px;"),
                  "Status da Conexão"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  # Configuração Atual
                  div(
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                       padding: 25px; border-radius: 15px; margin-bottom: 25px;",
                    
                    h5(style = "margin: 0 0 20px 0; color: #1565C0; font-weight: 700;",
                       icon("info-circle"), " Configuração Atual"),
                    
                    div(
                      style = "font-size: 13px; line-height: 2.2;",
                      
                      div(
                        style = "margin-bottom: 15px;",
                        tags$strong("Base URL:", style = "color: #1565C0;"),
                        tags$br(),
                        tags$code(
                          style = "background: rgba(255,255,255,0.6); padding: 5px 10px; 
                             border-radius: 6px; font-size: 11px; display: inline-block;
                             margin-top: 5px; word-break: break-all;",
                          OPENAI_CONFIG$base_url
                        )
                      ),
                      
                      div(
                        style = "margin-bottom: 15px;",
                        tags$strong("Modelo:", style = "color: #1565C0;"),
                        tags$br(),
                        tags$code(
                          style = "background: rgba(255,255,255,0.6); padding: 5px 10px; 
                             border-radius: 6px; font-size: 11px; display: inline-block;
                             margin-top: 5px;",
                          OPENAI_CONFIG$model
                        )
                      ),
                      
                      div(
                        style = "margin-bottom: 15px;",
                        tags$strong("API Version:", style = "color: #1565C0;"),
                        tags$br(),
                        tags$code(
                          style = "background: rgba(255,255,255,0.6); padding: 5px 10px; 
                             border-radius: 6px; font-size: 11px; display: inline-block;
                             margin-top: 5px;",
                          OPENAI_CONFIG$api_version
                        )
                      ),
                      
                      div(
                        tags$strong("API Key:", style = "color: #1565C0;"),
                        tags$br(),
                        tags$code(
                          style = "background: rgba(255,255,255,0.6); padding: 5px 10px; 
                             border-radius: 6px; font-size: 11px; display: inline-block;
                             margin-top: 5px;",
                          paste0(substr(OPENAI_CONFIG$api_key, 1, 12), "••••••••")
                        )
                      )
                    )
                  ),
                  
                  # Resultado do Teste
                  htmlOutput("resultado_teste_api")
                )
              ),
              
              # Card de Parâmetros
              box(
                title = div(
                  icon("sliders-h", style = "margin-right: 10px;"),
                  "Parâmetros de Requisição"
                ),
                status = "warning",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 20px;",
                  
                  div(
                    style = "background: white; padding: 20px; border-radius: 12px; 
                       border: 2px solid #e9ecef;",
                    
                    div(
                      style = "margin-bottom: 20px; padding-bottom: 20px; border-bottom: 2px dashed #e9ecef;",
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        tags$strong("Temperature:", style = "color: #333;"),
                        tags$code(
                          "0.3",
                          style = "background: #667eea; color: white; padding: 5px 12px; 
                             border-radius: 15px; font-weight: 700;"
                        )
                      ),
                      p(style = "margin: 8px 0 0 0; font-size: 11px; color: #666;",
                        "Controla aleatoriedade (menor = mais preciso)")
                    ),
                    
                    div(
                      style = "margin-bottom: 20px; padding-bottom: 20px; border-bottom: 2px dashed #e9ecef;",
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        tags$strong("Max Tokens:", style = "color: #333;"),
                        tags$code(
                          "500",
                          style = "background: #11998e; color: white; padding: 5px 12px; 
                             border-radius: 15px; font-weight: 700;"
                        )
                      ),
                      p(style = "margin: 8px 0 0 0; font-size: 11px; color: #666;",
                        "Tamanho máximo da resposta")
                    ),
                    
                    div(
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        tags$strong("Timeout:", style = "color: #333;"),
                        tags$code(
                          "30s",
                          style = "background: #f093fb; color: white; padding: 5px 12px; 
                             border-radius: 15px; font-weight: 700;"
                        )
                      ),
                      p(style = "margin: 8px 0 0 0; font-size: 11px; color: #666;",
                        "Tempo máximo de espera")
                    )
                  )
                )
              )
            )
          ),
          
          # Seção de Explicação
          fluidRow(
            box(
              title = div(
                icon("graduation-cap", style = "margin-right: 10px;"),
                "Como Funciona a API neste Projeto"
              ),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              
              div(
                style = "padding: 30px;",
                
                # Fluxo Visual
                div(
                  style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin-bottom: 30px;",
                  
                  div(
                    style = "background: white; padding: 25px; border-radius: 15px; 
                       box-shadow: 0 4px 16px rgba(0,0,0,0.08); text-align: center;
                       border-top: 5px solid #4CAF50;",
                    div(style = "font-size: 48px; margin-bottom: 15px;", "1️⃣"),
                    h5(style = "color: #4CAF50; margin: 0 0 10px 0; font-weight: 700;", "Entrada"),
                    p(style = "font-size: 12px; color: #666; margin: 0;",
                      "Texto de manutenção é enviado")
                  ),
                  
                  div(
                    style = "background: white; padding: 25px; border-radius: 15px; 
                       box-shadow: 0 4px 16px rgba(0,0,0,0.08); text-align: center;
                       border-top: 5px solid #2196F3;",
                    div(style = "font-size: 48px; margin-bottom: 15px;", "2️⃣"),
                    h5(style = "color: #2196F3; margin: 0 0 10px 0; font-weight: 700;", "Análise"),
                    p(style = "font-size: 12px; color: #666; margin: 0;",
                      "IA processa e analisa contexto")
                  ),
                  
                  div(
                    style = "background: white; padding: 25px; border-radius: 15px; 
                       box-shadow: 0 4px 16px rgba(0,0,0,0.08); text-align: center;
                       border-top: 5px solid #FF9800;",
                    div(style = "font-size: 48px; margin-bottom: 15px;", "3️⃣"),
                    h5(style = "color: #FF9800; margin: 0 0 10px 0; font-weight: 700;", "Classificação"),
                    p(style = "font-size: 12px; color: #666; margin: 0;",
                      "Determina tipo SAP (1-6)")
                  ),
                  
                  div(
                    style = "background: white; padding: 25px; border-radius: 15px; 
                       box-shadow: 0 4px 16px rgba(0,0,0,0.08); text-align: center;
                       border-top: 5px solid #9C27B0;",
                    div(style = "font-size: 48px; margin-bottom: 15px;", "4️⃣"),
                    h5(style = "color: #9C27B0; margin: 0 0 10px 0; font-weight: 700;", "Resultado"),
                    p(style = "font-size: 12px; color: #666; margin: 0;",
                      "Retorna classificação completa")
                  )
                ),
                
                # Modos de Operação
                h4(style = "margin: 30px 0 20px 0; color: #667eea; font-weight: 700;",
                   icon("route"), " Modos de Operação"),
                
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px;",
                  
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                       padding: 25px; border-radius: 15px; border-left: 5px solid #4CAF50;",
                    div(style = "text-align: center; font-size: 48px; margin-bottom: 15px;", "📖"),
                    h5(style = "text-align: center; color: #2E7D32; margin: 0 0 15px 0; font-weight: 700;",
                       "DICIONÁRIO"),
                    tags$ul(
                      style = "font-size: 13px; color: #2E7D32; line-height: 2;",
                      tags$li("⚡ Rápido e offline"),
                      tags$li("🔤 Palavras-chave"),
                      tags$li("✏️ Personalizável"),
                      tags$li("💰 Sem custo")
                    )
                  ),
                  
                  div(
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                       padding: 25px; border-radius: 15px; border-left: 5px solid #2196F3;",
                    div(style = "text-align: center; font-size: 48px; margin-bottom: 15px;", "🤖"),
                    h5(style = "text-align: center; color: #1565C0; margin: 0 0 15px 0; font-weight: 700;",
                       "API (IA)"),
                    tags$ul(
                      style = "font-size: 13px; color: #1565C0; line-height: 2;",
                      tags$li("🧠 Inteligente"),
                      tags$li("🎯 Contextual"),
                      tags$li("🔬 Alta precisão"),
                      tags$li("🌐 Requer conexão")
                    )
                  ),
                  
                  div(
                    style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); 
                       padding: 25px; border-radius: 15px; border-left: 5px solid #FF9800;",
                    div(style = "text-align: center; font-size: 48px; margin-bottom: 15px;", "🔀"),
                    h5(style = "text-align: center; color: #E65100; margin: 0 0 15px 0; font-weight: 700;",
                       "HÍBRIDO"),
                    tags$ul(
                      style = "font-size: 13px; color: #E65100; line-height: 2;",
                      tags$li("✨ Melhor resultado"),
                      tags$li("✅ Validação cruzada"),
                      tags$li("🏆 Máxima confiança"),
                      tags$li("⭐ Recomendado")
                    )
                  )
                )
              )
            )
          ),
          
          # Segurança
          fluidRow(
            box(
              title = div(
                icon("shield-alt", style = "margin-right: 10px;"),
                "Segurança e Boas Práticas"
              ),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              
              div(
                style = "padding: 30px;",
                
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr; gap: 25px;",
                  
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                       padding: 30px; border-radius: 15px; border-left: 5px solid #4CAF50;",
                    h4(style = "color: #2E7D32; margin: 0 0 20px 0; font-weight: 700;",
                       icon("lock"), " Segurança"),
                    tags$ul(
                      style = "font-size: 14px; color: #2E7D32; line-height: 2.2;",
                      tags$li("🏢 API hospedada em infraestrutura privada Petrobras"),
                      tags$li("🔐 Chave de API criptografada e protegida"),
                      tags$li("🔒 Conexão via HTTPS (TLS 1.2+)"),
                      tags$li("💾 Sem armazenamento externo de dados"),
                      tags$li("📝 Logs auditáveis de todas as requisições")
                    )
                  ),
                  
                  div(
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                       padding: 30px; border-radius: 15px; border-left: 5px solid #2196F3;",
                    h4(style = "color: #1565C0; margin: 0 0 20px 0; font-weight: 700;",
                       icon("check-circle"), " Boas Práticas"),
                    tags$ul(
                      style = "font-size: 14px; color: #1565C0; line-height: 2.2;",
                      tags$li("🔑 Não compartilhe sua API Key"),
                      tags$li("🔀 Use modo HÍBRIDO para melhor resultado"),
                      tags$li("👀 Revise classificações com baixa confiança"),
                      tags$li("💾 Mantenha backups dos dicionários"),
                      tags$li("🧪 Teste a API periodicamente")
                    )
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 8: DOCUMENTAÇÃO PREMIUM
        #===========================================================================
        
        tabItem(
          tabName = "documentacao",
          
          # Header Hero
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 50px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(102, 126, 234, 0.3);
                   text-align: center;",
                icon("book-open", style = "font-size: 72px; color: white; margin-bottom: 20px;"),
                h1(style = "color: white; margin: 0 0 15px 0; font-weight: 700; font-size: 36px;",
                   "Documentação do Sistema"),
                p(style = "color: rgba(255,255,255,0.95); margin: 0; font-size: 18px;",
                  "Guia completo de uso e referência técnica"),
                div(
                  style = "margin-top: 25px;",
                  tags$span(
                    "Versão 3.0",
                    style = "background: rgba(255,255,255,0.2); padding: 10px 25px; 
                       border-radius: 25px; color: white; font-weight: 700;
                       border: 2px solid white;"
                  )
                )
              )
            )
          ),
          
          # Cards de Tipos SAP
          fluidRow(
            box(
              title = div(
                icon("list-ol", style = "margin-right: 10px;"),
                "Tipos de Intervenção SAP - Guia Completo"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                # Tipo 1
                div(
                  style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #87CEEB; box-shadow: 0 4px 16px rgba(135, 206, 235, 0.2);",
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #87CEEB 0%, #4682B4 100%); 
                         width: 80px; height: 80px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin-right: 25px; box-shadow: 0 4px 16px rgba(135, 206, 235, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;", "1")
                    ),
                    div(
                      h3(style = "margin: 0 0 10px 0; color: #1565C0; font-weight: 700;",
                         "Condicionamento e Limpeza"),
                      p(style = "margin: 0; color: #666; font-size: 14px;",
                        "Criticidade: BAIXA | Hierarquia: PROBLEMAS_COMUNS")
                    )
                  ),
                  tags$p(
                    style = "font-size: 14px; line-height: 1.8; margin: 0 0 15px 0; color: #333;",
                    tags$strong("Descrição: "),
                    "Condicionamento, limpeza, arrumação, preservação, pintura ou desinstalação"
                  ),
                  div(
                    style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                    tags$strong(icon("lightbulb"), " Exemplos:", style = "color: #1565C0;"),
                    tags$p(
                      style = "margin: 10px 0 0 0; font-size: 13px; color: #666; font-style: italic;",
                      "Limpeza de equipamentos, pintura de estruturas, higienização de áreas, lubrificação básica"
                    )
                  )
                ),
                
                # Tipo 2
                div(
                  style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #90EE90; box-shadow: 0 4px 16px rgba(144, 238, 144, 0.2);",
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #90EE90 0%, #32CD32 100%); 
                         width: 80px; height: 80px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin-right: 25px; box-shadow: 0 4px 16px rgba(144, 238, 144, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;", "2")
                    ),
                    div(
                      h3(style = "margin: 0 0 10px 0; color: #2E7D32; font-weight: 700;",
                         "Melhorias e Modificações"),
                      p(style = "margin: 0; color: #666; font-size: 14px;",
                        "Criticidade: BAIXA | Hierarquia: PROBLEMAS_COMUNS")
                    )
                  ),
                  tags$p(
                    style = "font-size: 14px; line-height: 1.8; margin: 0 0 15px 0; color: #333;",
                    tags$strong("Descrição: "),
                    "Melhorias, modificações, testes, colocação em operação, instalação ou regulagem"
                  ),
                  div(
                    style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                    tags$strong(icon("lightbulb"), " Exemplos:", style = "color: #2E7D32;"),
                    tags$p(
                      style = "margin: 10px 0 0 0; font-size: 13px; color: #666; font-style: italic;",
                      "Instalação de novos equipamentos, testes de sistemas, ajustes, regulagens, upgrades"
                    )
                  )
                ),
                
                # Tipo 3
                div(
                  style = "background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #FFD700; box-shadow: 0 4px 16px rgba(255, 215, 0, 0.2);",
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #FFD700 0%, #FFA500 100%); 
                         width: 80px; height: 80px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin-right: 25px; box-shadow: 0 4px 16px rgba(255, 215, 0, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;", "3")
                    ),
                    div(
                      h3(style = "margin: 0 0 10px 0; color: #F57F17; font-weight: 700;",
                         "Manutenção Preventiva"),
                      p(style = "margin: 0; color: #666; font-size: 14px;",
                        "Criticidade: MÉDIA | Hierarquia: PROBLEMAS_COMUNS")
                    )
                  ),
                  tags$p(
                    style = "font-size: 14px; line-height: 1.8; margin: 0 0 15px 0; color: #333;",
                    tags$strong("Descrição: "),
                    "Manutenção preventiva, manutenção preditiva ou inspeção planejada"
                  ),
                  div(
                    style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                    tags$strong(icon("lightbulb"), " Exemplos:", style = "color: #F57F17;"),
                    tags$p(
                      style = "margin: 10px 0 0 0; font-size: 13px; color: #666; font-style: italic;",
                      "Inspeções programadas, manutenções preventivas de rotina, verificações periódicas"
                    )
                  )
                ),
                
                # Tipo 4
                div(
                  style = "background: linear-gradient(135deg, #ffe0b2 0%, #ffcc80 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #FFA500; box-shadow: 0 4px 16px rgba(255, 165, 0, 0.2);",
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #FFA500 0%, #FF8C00 100%); 
                         width: 80px; height: 80px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin-right: 25px; box-shadow: 0 4px 16px rgba(255, 165, 0, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;", "4")
                    ),
                    div(
                      h3(style = "margin: 0 0 10px 0; color: #E65100; font-weight: 700;",
                         "Manutenção por Oportunidade"),
                      p(style = "margin: 0; color: #666; font-size: 14px;",
                        "Criticidade: MÉDIA | Hierarquia: PROBLEMAS_COMUNS")
                    )
                  ),
                  tags$p(
                    style = "font-size: 14px; line-height: 1.8; margin: 0 0 15px 0; color: #333;",
                    tags$strong("Descrição: "),
                    "Manutenção por oportunidade ou inspeção não programada"
                  ),
                  div(
                    style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                    tags$strong(icon("lightbulb"), " Exemplos:", style = "color: #E65100;"),
                    tags$p(
                      style = "margin: 10px 0 0 0; font-size: 13px; color: #666; font-style: italic;",
                      "Manutenções durante paradas, aproveitamento de disponibilidade do equipamento"
                    )
                  )
                ),
                
                # Tipo 5
                div(
                  style = "background: linear-gradient(135deg, #ffccbc 0%, #ff8a65 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #FF6347; box-shadow: 0 4px 16px rgba(255, 99, 71, 0.2);",
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #FF6347 0%, #DC143C 100%); 
                         width: 80px; height: 80px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin-right: 25px; box-shadow: 0 4px 16px rgba(255, 99, 71, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;", "5")
                    ),
                    div(
                      h3(style = "margin: 0 0 10px 0; color: #BF360C; font-weight: 700;",
                         "Eliminação de Defeito (IAZF)"),
                      p(style = "margin: 0; color: #666; font-size: 14px;",
                        "Criticidade: ALTA | Hierarquia: IAZF")
                    )
                  ),
                  tags$p(
                    style = "font-size: 14px; line-height: 1.8; margin: 0 0 15px 0; color: #333;",
                    tags$strong("Descrição: "),
                    "Intervenção para eliminação de defeito - Equipamento com restrição"
                  ),
                  div(
                    style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                    tags$strong(icon("exclamation-triangle"), " Exemplos:", style = "color: #BF360C;"),
                    tags$p(
                      style = "margin: 10px 0 0 0; font-size: 13px; color: #666; font-style: italic;",
                      "Correção de problemas que limitam funcionamento, eliminação de anomalias"
                    )
                  )
                ),
                
                # Tipo 6
                div(
                  style = "background: linear-gradient(135deg, #ef9a9a 0%, #e57373 100%); 
                     padding: 30px; border-radius: 15px;
                     border-left: 6px solid #DC143C; box-shadow: 0 4px 16px rgba(220, 20, 60, 0.2);",
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #DC143C 0%, #8B0000 100%); 
                         width: 80px; height: 80px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin-right: 25px; box-shadow: 0 4px 16px rgba(220, 20, 60, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;", "6")
                    ),
                    div(
                      h3(style = "margin: 0 0 10px 0; color: #B71C1C; font-weight: 700;",
                         "Eliminação de Falha (IAZF)"),
                      p(style = "margin: 0; color: #666; font-size: 14px;",
                        "Criticidade: CRÍTICA | Hierarquia: IAZF")
                    )
                  ),
                  tags$p(
                    style = "font-size: 14px; line-height: 1.8; margin: 0 0 15px 0; color: #333;",
                    tags$strong("Descrição: "),
                    "Intervenção para eliminação de falha - Sistema indisponível"
                  ),
                  div(
                    style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                    tags$strong(icon("times-circle"), " Exemplos:", style = "color: #B71C1C;"),
                    tags$p(
                      style = "margin: 10px 0 0 0; font-size: 13px; color: #666; font-style: italic;",
                      "Falhas críticas, quebras, emergências, paradas totais, equipamento inoperante"
                    )
                  )
                )
              )
            )
          ),
          
          # Glossário
          fluidRow(
            box(
              title = div(
                icon("book", style = "margin-right: 10px;"),
                "Glossário de Termos"
              ),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              
              div(
                style = "padding: 30px;",
                
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                  
                  div(
                    style = "background: white; padding: 25px; border-radius: 12px; 
                       box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
                    
                    tags$dl(
                      style = "font-size: 14px; line-height: 2.5;",
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("flag"), " IAZF"
                      ),
                      tags$dd(
                        style = "margin-bottom: 20px; color: #666; padding-left: 30px;",
                        "Incidente de Ativos Zero Falha - Eventos críticos (Tipos 5 e 6)"
                      ),
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("tools"), " PROBLEMAS_COMUNS"
                      ),
                      tags$dd(
                        style = "margin-bottom: 20px; color: #666; padding-left: 30px;",
                        "Manutenções rotineiras e melhorias (Tipos 1, 2, 3, 4)"
                      ),
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("tag"), " Tipo de Intervenção"
                      ),
                      tags$dd(
                        style = "margin-bottom: 20px; color: #666; padding-left: 30px;",
                        "Classificação SAP que define a natureza da manutenção (1 a 6)"
                      ),
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("exclamation-circle"), " Criticidade"
                      ),
                      tags$dd(
                        style = "color: #666; padding-left: 30px;",
                        "Nível de urgência: BAIXA, MÉDIA, ALTA ou CRÍTICA"
                      )
                    )
                  ),
                  
                  div(
                    style = "background: white; padding: 25px; border-radius: 12px; 
                       box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
                    
                    tags$dl(
                      style = "font-size: 14px; line-height: 2.5;",
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("percentage"), " Confiança"
                      ),
                      tags$dd(
                        style = "margin-bottom: 20px; color: #666; padding-left: 30px;",
                        "Percentual de certeza da classificação (0-100%)"
                      ),
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("project-diagram"), " Modo Híbrido"
                      ),
                      tags$dd(
                        style = "margin-bottom: 20px; color: #666; padding-left: 30px;",
                        "Combinação de dicionário e API para máxima precisão"
                      ),
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("coins"), " Token"
                      ),
                      tags$dd(
                        style = "margin-bottom: 20px; color: #666; padding-left: 30px;",
                        "Unidade de processamento da API (~4 caracteres = 1 token)"
                      ),
                      
                      tags$dt(
                        style = "color: #667eea; font-weight: 700; font-size: 15px;",
                        icon("thermometer-half"), " Temperature"
                      ),
                      tags$dd(
                        style = "color: #666; padding-left: 30px;",
                        "Controla aleatoriedade da IA (0 = determinístico, 1 = criativo)"
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # ABA 9: HISTÓRICO PREMIUM E ROBUSTO
        #===========================================================================
        
        tabItem(
          tabName = "historico",
          
          # Header Hero
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: linear-gradient(135deg, #a8edea 0%, #fed6e3 100%); 
                   padding: 40px; border-radius: 20px; margin-bottom: 30px;
                   box-shadow: 0 10px 40px rgba(168, 237, 234, 0.3);",
                div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  div(
                    h1(style = "color: #333; margin: 0; font-weight: 700; font-size: 32px;",
                       icon("history", style = "margin-right: 15px;"), 
                       "Histórico de Processamentos"),
                    p(style = "color: #666; margin: 10px 0 0 0; font-size: 16px;",
                      "Navegue pelo histórico completo de classificações realizadas")
                  ),
                  div(
                    style = "text-align: right;",
                    h2(style = "color: #333; margin: 0; font-weight: 800; font-size: 42px;",
                       textOutput("total_historico_inline", inline = TRUE)),
                    p(style = "color: #666; margin: 5px 0 0 0; font-size: 14px;",
                      "Sessões Salvas")
                  )
                )
              )
            )
          ),
          
          # Painel de Controle
          fluidRow(
            box(
              title = div(
                icon("gamepad", style = "margin-right: 10px;"),
                "Painel de Controle"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                fluidRow(
                  # Informações do Processamento Atual
                  column(
                    width = 8,
                    div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                         padding: 30px; border-radius: 15px; color: white;
                         box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);",
                      
                      h4(style = "margin: 0 0 20px 0; font-weight: 700;",
                         icon("info-circle"), " Processamento Atual"),
                      
                      uiOutput("info_historico")
                    )
                  ),
                  
                  # Botões de Navegação e Ações
                  column(
                    width = 4,
                    div(
                      style = "background: #f8f9fa; padding: 25px; border-radius: 15px;
                         border: 2px solid #e9ecef;",
                      
                      h5(style = "margin: 0 0 20px 0; color: #667eea; font-weight: 700; text-align: center;",
                         icon("hand-pointer"), " Ações Rápidas"),
                      
                      # Navegação
                      div(
                        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 20px;",
                        
                        actionButton(
                          "voltar_historico",
                          label = div(icon("arrow-left"), " Anterior"),
                          class = "btn-warning",
                          style = "padding: 12px; border-radius: 25px; font-weight: 700; width: 100%;"
                        ),
                        
                        actionButton(
                          "avancar_historico",
                          label = div("Próximo ", icon("arrow-right")),
                          class = "btn-warning",
                          style = "padding: 12px; border-radius: 25px; font-weight: 700; width: 100%;"
                        )
                      ),
                      
                      hr(style = "margin: 20px 0; border-color: #dee2e6;"),
                      
                      # Ações de Sessão
                      div(
                        style = "display: grid; gap: 12px;",
                        
                        actionButton(
                          "salvar_sessao",
                          label = div(
                            icon("save", style = "margin-right: 8px;"),
                            "Salvar Sessão"
                          ),
                          class = "btn-success",
                          style = "padding: 14px; border-radius: 25px; font-weight: 700; width: 100%;"
                        ),
                        
                        actionButton(
                          "carregar_sessao",
                          label = div(
                            icon("folder-open", style = "margin-right: 8px;"),
                            "Carregar Sessão"
                          ),
                          class = "btn-info",
                          style = "padding: 14px; border-radius: 25px; font-weight: 700; width: 100%;"
                        ),
                        
                        actionButton(
                          "exportar_historico",
                          label = div(
                            icon("file-export", style = "margin-right: 8px;"),
                            "Exportar Histórico"
                          ),
                          class = "btn-primary",
                          style = "padding: 14px; border-radius: 25px; font-weight: 700; width: 100%;"
                        ),
                        
                        actionButton(
                          "limpar_historico",
                          label = div(
                            icon("trash-alt", style = "margin-right: 8px;"),
                            "Limpar Tudo"
                          ),
                          class = "btn-danger",
                          style = "padding: 14px; border-radius: 25px; font-weight: 700; width: 100%;"
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # Gráficos e Detalhes
          fluidRow(
            column(
              width = 7,
              box(
                title = div(
                  icon("chart-line", style = "margin-right: 10px;"),
                  "Evolução de Métricas ao Longo do Tempo"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: #e3f2fd; padding: 20px; border-radius: 12px; 
                       margin-bottom: 20px; border-left: 4px solid #2196F3;",
                    tags$strong(icon("lightbulb"), " Dica:", style = "color: #1565C0;"),
                    p(style = "margin: 5px 0 0 0; font-size: 13px; color: #666;",
                      "Acompanhe a evolução da acurácia e conformidade entre diferentes processamentos")
                  ),
                  
                  plotOutput("grafico_evolucao_metricas", height = "380px")
                )
              )
            ),
            
            column(
              width = 5,
              box(
                title = div(
                  icon("clipboard-list", style = "margin-right: 10px;"),
                  "Detalhes do Processamento"
                ),
                status = "success",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                       padding: 25px; border-radius: 15px; min-height: 380px;",
                    
                    h5(style = "margin: 0 0 20px 0; color: #2E7D32; font-weight: 700;",
                       icon("info-circle"), " Informações Detalhadas"),
                    
                    uiOutput("detalhes_processamento_atual")
                  )
                )
              )
            )
          ),
          
          # Timeline de Processamentos
          fluidRow(
            box(
              title = div(
                icon("stream", style = "margin-right: 10px;"),
                "Timeline de Processamentos"
              ),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                div(
                  style = "background: #fff3cd; padding: 20px; border-radius: 12px; 
                     margin-bottom: 25px; border-left: 4px solid #ffc107;",
                  div(
                    style = "display: flex; align-items: center;",
                    icon("info-circle", style = "font-size: 32px; color: #856404; margin-right: 15px;"),
                    div(
                      tags$strong("Sobre o Histórico:", style = "color: #856404; font-size: 15px;"),
                      p(style = "margin: 5px 0 0 0; color: #856404; font-size: 13px;",
                        "Cada linha representa um processamento completo com data, hora, quantidade de registros e métricas de desempenho.")
                    )
                  )
                ),
                
                DT::dataTableOutput("tabela_historico")
              )
            )
          ),
          
          # Estatísticas do Histórico
          fluidRow(
            column(
              width = 4,
              box(
                title = div(
                  icon("calculator", style = "margin-right: 10px;"),
                  "Estatísticas Gerais"
                ),
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  div(
                    style = "text-align: center; margin-bottom: 20px;",
                    div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                         width: 100px; height: 100px; border-radius: 50%; 
                         display: flex; align-items: center; justify-content: center;
                         margin: 0 auto 15px; box-shadow: 0 8px 24px rgba(102, 126, 234, 0.4);",
                      h2(style = "color: white; margin: 0; font-weight: 800;",
                         textOutput("total_sessoes", inline = TRUE))
                    ),
                    p(style = "margin: 0; color: #667eea; font-weight: 700; font-size: 16px;",
                      "Total de Sessões")
                  ),
                  
                  hr(style = "margin: 25px 0; border-color: #e9ecef;"),
                  
                  div(
                    style = "font-size: 14px; line-height: 2.5;",
                    
                    div(
                      style = "display: flex; justify-content: space-between; padding: 12px; 
                         background: #f8f9fa; border-radius: 8px; margin-bottom: 10px;",
                      tags$strong("Total Processado:", style = "color: #333;"),
                      tags$span(
                        textOutput("total_processado_historico", inline = TRUE),
                        style = "color: #667eea; font-weight: 700;"
                      )
                    ),
                    
                    div(
                      style = "display: flex; justify-content: space-between; padding: 12px; 
                         background: #f8f9fa; border-radius: 8px; margin-bottom: 10px;",
                      tags$strong("Acurácia Média:", style = "color: #333;"),
                      tags$span(
                        textOutput("acuracia_media_historico", inline = TRUE),
                        style = "color: #11998e; font-weight: 700;"
                      )
                    ),
                    
                    div(
                      style = "display: flex; justify-content: space-between; padding: 12px; 
                         background: #f8f9fa; border-radius: 8px;",
                      tags$strong("Última Sessão:", style = "color: #333;"),
                      tags$span(
                        textOutput("data_ultima_sessao", inline = TRUE),
                        style = "color: #f093fb; font-weight: 700;"
                      )
                    )
                  )
                )
              )
            ),
            
            column(
              width = 8,
              box(
                title = div(
                  icon("chart-bar", style = "margin-right: 10px;"),
                  "Comparativo de Performance"
                ),
                status = "info",
                solidHeader = TRUE,
                width = NULL,
                
                div(
                  style = "padding: 25px;",
                  
                  tabsetPanel(
                    type = "pills",
                    
                    tabPanel(
                      title = div(icon("tachometer-alt"), " Acurácia"),
                      br(),
                      plotOutput("grafico_historico_acuracia", height = "300px")
                    ),
                    
                    tabPanel(
                      title = div(icon("check-double"), " Conformidade"),
                      br(),
                      plotOutput("grafico_historico_conformidade", height = "300px")
                    ),
                    
                    tabPanel(
                      title = div(icon("database"), " Volume"),
                      br(),
                      plotOutput("grafico_historico_volume", height = "300px")
                    )
                  )
                )
              )
            )
          ),
          
          # Ações em Lote
          fluidRow(
            box(
              title = div(
                icon("tasks", style = "margin-right: 10px;"),
                "Gerenciamento de Sessões"
              ),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                div(
                  style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px;",
                  
                  # Card Exportar Tudo
                  div(
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);
                       transition: all 0.3s ease; cursor: pointer;",
                    icon("file-export", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                    h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                       "Exportar Tudo"),
                    downloadButton(
                      "exportar_historico_completo",
                      label = "Download",
                      style = "background: rgba(255,255,255,0.2); color: white; 
                         border: 2px solid white; border-radius: 25px; 
                         padding: 10px 25px; font-weight: 700;"
                    )
                  ),
                  
                  # Card Comparar Sessões
                  div(
                    style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3);
                       transition: all 0.3s ease; cursor: pointer;",
                    icon("exchange-alt", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                    h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                       "Comparar Sessões"),
                    actionButton(
                      "comparar_sessoes",
                      label = "Comparar",
                      style = "background: rgba(255,255,255,0.2); color: white; 
                         border: 2px solid white; border-radius: 25px; 
                         padding: 10px 25px; font-weight: 700;"
                    )
                  ),
                  
                  # Card Backup
                  div(
                    style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(79, 172, 254, 0.3);
                       transition: all 0.3s ease; cursor: pointer;",
                    icon("database", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                    h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                       "Backup Automático"),
                    actionButton(
                      "criar_backup",
                      label = "Criar Backup",
                      style = "background: rgba(255,255,255,0.2); color: white; 
                         border: 2px solid white; border-radius: 25px; 
                         padding: 10px 25px; font-weight: 700;"
                    )
                  ),
                  
                  # Card Limpar
                  div(
                    style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                       padding: 30px; border-radius: 15px; text-align: center;
                       box-shadow: 0 8px 24px rgba(240, 147, 251, 0.3);
                       transition: all 0.3s ease; cursor: pointer;",
                    icon("trash-alt", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                    h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                       "Limpar Histórico"),
                    actionButton(
                      "limpar_historico_confirm",
                      label = "Limpar",
                      style = "background: rgba(255,255,255,0.2); color: white; 
                         border: 2px solid white; border-radius: 25px; 
                         padding: 10px 25px; font-weight: 700;"
                    )
                  )
                )
              )
            )
          ),
          
          # Tabela Principal do Histórico
          fluidRow(
            box(
              title = div(
                icon("table", style = "margin-right: 10px;"),
                "Lista Completa de Processamentos"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 30px;",
                
                div(
                  style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                     padding: 25px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 5px solid #2196F3;",
                  div(
                    style = "display: flex; align-items: center;",
                    icon("filter", style = "font-size: 40px; color: #2196F3; margin-right: 20px;"),
                    div(
                      h5(style = "margin: 0 0 10px 0; color: #1565C0; font-weight: 700;",
                         "Filtros e Ordenação"),
                      p(style = "margin: 0; color: #666; font-size: 13px;",
                        "Clique nos cabeçalhos das colunas para ordenar. Use a caixa de busca para filtrar registros específicos.")
                    )
                  )
                ),
                
                DT::dataTableOutput("tabela_historico")
              )
            )
          )
        )
        
      ) # Fecha tabItems
    
  ) # Fecha dashboardBody
  
) # Fecha dashboardPage

cat("✅ Interface Premium carregada com sucesso!\n")
cat("🎨 Visual moderno e elegante aplicado\n")
cat("📊 Preparando servidor...\n\n")


#=============================================================================
# SERVIDOR (SERVER COMPLETO) - VERSÃO PREMIUM
#=============================================================================

server <- function(input, output, session) {
  
  cat("\n🚀 Servidor inicializado...\n")
  
  #===========================================================================
  # VALORES REATIVOS CENTRALIZADOS
  #===========================================================================
  
  values <- reactiveValues(
    dados_ordens = NULL,
    dados_textos = NULL,
    dados_cruzados = NULL,
    col_tip_intervencao = NULL,
    resultados_lote = NULL,
    processando = FALSE,
    dados_com_assuntos = NULL
  )
  
  #===========================================================================
  # SISTEMA DE HISTÓRICO AVANÇADO
  #===========================================================================
  
  historico <- reactiveValues(
    processamentos = list(),
    indice_atual = 0,
    max_historico = 50,
    sessao_id = format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  
  # Função para adicionar ao histórico
  adicionar_ao_historico <- function(dados_resultado, metadados = list()) {
    
    cat("\n💾 Salvando no histórico...\n")
    
    snapshot <- list(
      timestamp = Sys.time(),
      dados = dados_resultado,
      metadados = metadados,
      metricas = calcular_metricas_snapshot(dados_resultado),
      id = paste0("PROC_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    
    if(historico$indice_atual < length(historico$processamentos)) {
      historico$processamentos <- historico$processamentos[1:historico$indice_atual]
    }
    
    historico$processamentos <- append(historico$processamentos, list(snapshot))
    historico$indice_atual <- length(historico$processamentos)
    
    if(length(historico$processamentos) > historico$max_historico) {
      historico$processamentos <- tail(historico$processamentos, historico$max_historico)
      historico$indice_atual <- length(historico$processamentos)
    }
    
    cat("✅ Histórico atualizado:", length(historico$processamentos), "sessões\n")
    
    return(snapshot$id)
  }
  
  # Função para calcular métricas
  calcular_metricas_snapshot <- function(dados) {
    
    if(is.null(dados) || nrow(dados) == 0) {
      return(list(
        total = 0, conformes = 0, divergentes = 0,
        acuracia = 0, confianca_media = 0
      ))
    }
    
    dados_validos <- dados %>%
      filter(!is.na(tipo_intervencao_antigo), !is.na(tipo_novo))
    
    if(nrow(dados_validos) == 0) {
      return(list(
        total = nrow(dados), conformes = 0, divergentes = 0,
        acuracia = 0, confianca_media = 0
      ))
    }
    
    conformes <- sum(dados_validos$tipo_intervencao_antigo == dados_validos$tipo_novo, na.rm = TRUE)
    total <- nrow(dados_validos)
    acuracia <- (conformes / total) * 100
    
    return(list(
      total = total,
      conformes = conformes,
      divergentes = total - conformes,
      acuracia = round(acuracia, 2),
      confianca_media = round(mean(dados_validos$confianca, na.rm = TRUE), 2)
    ))
  }
  
  # Função para obter processamento atual
  obter_processamento_atual <- function() {
    if(historico$indice_atual > 0 && historico$indice_atual <= length(historico$processamentos)) {
      return(historico$processamentos[[historico$indice_atual]])
    }
    return(NULL)
  }
  
  # Função para navegar
  navegar_historico <- function(direcao) {
    if(direcao == "anterior" && historico$indice_atual > 1) {
      historico$indice_atual <- historico$indice_atual - 1
      return(TRUE)
    }
    if(direcao == "proximo" && historico$indice_atual < length(historico$processamentos)) {
      historico$indice_atual <- historico$indice_atual + 1
      return(TRUE)
    }
    return(FALSE)
  }
  
  # Reactive para processamento atual
  processamento_atual <- reactive({
    obter_processamento_atual()
  })
  
  #===========================================================================
  # CONFIGURAÇÕES DO USUÁRIO
  #===========================================================================
  
  CONFIG_USUARIO <- reactiveValues(
    dicionarios = DICIONARIOS_SAP,
    usar_dicionario = TRUE,
    usar_api = TRUE,
    prioridade = "HIBRIDO"
  )
  
  #===========================================================================
  # MÉTRICAS CONSOLIDADAS
  #===========================================================================
  
  metricas <- reactive({
    req(values$resultados_lote)
    
    dados <- values$resultados_lote
    
    dados_validos <- dados %>%
      filter(!is.na(tipo_intervencao_antigo), !is.na(tipo_novo))
    
    if(nrow(dados_validos) == 0) return(NULL)
    
    dados_validos <- dados_validos %>%
      mutate(
        conforme = tipo_intervencao_antigo == tipo_novo,
        status_conformidade = ifelse(conforme, "CONFORME", "DIVERGENTE")
      )
    
    acuracia <- mean(dados_validos$conforme, na.rm = TRUE) * 100
    total <- nrow(dados_validos)
    conformes <- sum(dados_validos$conforme, na.rm = TRUE)
    divergentes <- total - conformes
    
    matriz <- table(
      Anterior = dados_validos$tipo_intervencao_antigo,
      Novo = dados_validos$tipo_novo
    )
    
    metricas_tipo <- dados_validos %>%
      group_by(tipo_intervencao_antigo) %>%
      summarise(
        total = n(),
        conformes = sum(conforme),
        divergentes = sum(!conforme),
        acuracia = mean(conforme) * 100,
        confianca_media = mean(confianca, na.rm = TRUE),
        .groups = "drop"
      )
    
    metricas_categoria <- dados_validos %>%
      mutate(
        categoria_anterior = ifelse(tipo_intervencao_antigo %in% c(5, 6), "IAZF", "PROBLEMAS_COMUNS")
      ) %>%
      group_by(categoria_anterior, categoria) %>%
      summarise(total = n(), .groups = "drop")
    
    metricas_metodo <- if("metodo" %in% names(dados_validos)) {
      dados_validos %>%
        group_by(metodo) %>%
        summarise(
          total = n(),
          conformes = sum(conforme),
          acuracia = mean(conforme) * 100,
          confianca_media = mean(confianca, na.rm = TRUE),
          .groups = "drop"
        )
    } else NULL
    
    divergencias <- dados_validos %>%
      filter(!conforme) %>%
      select(
        nota_key, texto_completo, tipo_intervencao_antigo,
        tipo_novo, categoria, criticidade, confianca, resumo
      ) %>%
      arrange(desc(confianca))
    
    return(list(
      dados_validos = dados_validos,
      acuracia = acuracia,
      total = total,
      conformes = conformes,
      divergentes = divergentes,
      matriz = matriz,
      metricas_tipo = metricas_tipo,
      metricas_categoria = metricas_categoria,
      metricas_metodo = metricas_metodo,
      divergencias = divergencias
    ))
  })
  # Reactive value para dados com assuntos
  dados_com_assuntos <- reactiveVal(NULL)
  #===========================================================================
  # DASHBOARD - VALUE BOXES PREMIUM
  #===========================================================================
  
  output$total_textos <- renderValueBox({
    total <- if(is.null(values$dados_cruzados)) 0 else nrow(values$dados_cruzados)
    valueBox(
      value = format(total, big.mark = ","),
      subtitle = "Textos Carregados",
      icon = icon("file-text"),
      color = "purple"
    )
  })
  
  output$textos_iazf <- renderValueBox({
    iazf_count <- if(is.null(values$resultados_lote)) {
      0
    } else {
      sum(values$resultados_lote$categoria == "IAZF", na.rm = TRUE)
    }
    valueBox(
      value = format(iazf_count, big.mark = ","),
      subtitle = "Textos IAZF (Críticos)",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$precisao <- renderValueBox({
    precisao <- if(is.null(values$resultados_lote)) {
      "N/A"
    } else {
      paste0(round(mean(values$resultados_lote$confianca, na.rm = TRUE), 1), "%")
    }
    valueBox(
      value = precisao,
      subtitle = "Confiança Média",
      icon = icon("bullseye"),
      color = "teal"
    )
  })
  
  output$taxa_conformidade <- renderValueBox({
    m <- metricas()
    
    if(is.null(m)) {
      valor <- "N/A"
      cor <- "light-blue"
    } else {
      valor <- paste0(round(m$acuracia, 1), "%")
      cor <- if(m$acuracia >= 85) "green" else if(m$acuracia >= 70) "yellow" else "red"
    }
    
    valueBox(
      value = valor,
      subtitle = "Taxa de Conformidade",
      icon = icon("check-double"),
      color = cor
    )
  })
  
  # Value box inline para dashboard header
  output$dashboard_total_inline <- renderText({
    total <- if(is.null(values$dados_cruzados)) 0 else nrow(values$dados_cruzados)
    format(total, big.mark = ".")
  })
  
  #===========================================================================
  # DASHBOARD - GRÁFICOS PREMIUM
  #===========================================================================
  
  output$grafico_comparacao_antes_depois <- renderPlot({
    req(values$resultados_lote)
    
    dados <- values$resultados_lote %>%
      filter(!is.na(tipo_intervencao_antigo), !is.na(tipo_novo))
    
    if(nrow(dados) == 0) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Sem dados para comparação", 
                 size = 6, color = "#999")
    } else {
      antes <- dados %>%
        count(tipo_intervencao_antigo, name = "count") %>%
        mutate(
          tipo_num = as.numeric(tipo_intervencao_antigo),
          tipo = paste0("Tipo ", tipo_intervencao_antigo),
          periodo = "Anterior"
        ) %>%
        select(tipo_num, tipo, periodo, count)
      
      depois <- dados %>%
        count(tipo_novo, name = "count") %>%
        mutate(
          tipo_num = as.numeric(tipo_novo),
          tipo = paste0("Tipo ", tipo_novo),
          periodo = "Novo"
        ) %>%
        select(tipo_num, tipo, periodo, count)
      
      comparacao <- bind_rows(antes, depois)
      
      ggplot(comparacao, aes(x = tipo, y = count, fill = periodo)) +
        geom_col(position = "dodge", alpha = 0.9, width = 0.7) +
        geom_text(
          aes(label = count), 
          position = position_dodge(width = 0.7),
          vjust = -0.5, 
          fontface = "bold", 
          size = 5
        ) +
        scale_fill_manual(
          values = c("Anterior" = "#bdc3c7", "Novo" = "#667eea"),
          name = ""
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "top",
          legend.text = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold", color = "#333"),
          axis.text.y = element_text(size = 11, color = "#666"),
          axis.title = element_text(size = 13, face = "bold", color = "#333"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        ) +
        labs(x = "", y = "Quantidade")
    }
  })
  
  output$grafico_distribuicao_tipos <- renderPlot({
    if(is.null(values$resultados_lote) || nrow(values$resultados_lote) == 0) {
      dados <- data.frame(
        tipo = paste0("Tipo ", 1:6),
        count = rep(0, 6)
      )
    } else {
      dados <- values$resultados_lote %>%
        filter(!is.na(tipo_novo)) %>%
        count(tipo_novo, name = "count") %>%
        mutate(tipo = paste0("Tipo ", tipo_novo))
      
      todos_tipos <- data.frame(tipo = paste0("Tipo ", 1:6))
      dados <- todos_tipos %>%
        left_join(dados, by = "tipo") %>%
        mutate(count = ifelse(is.na(count), 0, count))
    }
    
    cores_tipos <- c(
      "Tipo 1" = "#87CEEB", "Tipo 2" = "#90EE90", "Tipo 3" = "#FFD700",
      "Tipo 4" = "#FFA500", "Tipo 5" = "#FF6347", "Tipo 6" = "#DC143C"
    )
    
    ggplot(dados, aes(x = tipo, y = count, fill = tipo)) +
      geom_col(alpha = 0.9, width = 0.7) +
      geom_text(aes(label = count), vjust = -0.5, fontface = "bold", size = 6, color = "#333") +
      scale_fill_manual(values = cores_tipos) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 12, face = "bold", color = "#333"),
        axis.text.y = element_text(size = 11, color = "#666"),
        axis.title = element_text(size = 13, face = "bold", color = "#333"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(x = "", y = "Quantidade") +
      ylim(0, max(dados$count) * 1.2)
  })
  
  output$grafico_hierarquia <- renderPlot({
    if(is.null(values$resultados_lote) || nrow(values$resultados_lote) == 0) {
      dados <- data.frame(
        categoria = c("PROBLEMAS_COMUNS", "IAZF"),
        count = c(0, 0)
      )
    } else {
      dados <- values$resultados_lote %>%
        count(categoria, name = "count")
    }
    
    ggplot(dados, aes(x = categoria, y = count, fill = categoria)) +
      geom_col(alpha = 0.9, width = 0.6) +
      geom_text(aes(label = count), vjust = -0.5, fontface = "bold", size = 6, color = "#333") +
      scale_fill_manual(
        values = c("PROBLEMAS_COMUNS" = "#11998e", "IAZF" = "#f5576c")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 13, face = "bold", color = "#333"),
        axis.text.y = element_text(size = 11, color = "#666"),
        axis.title = element_text(size = 13, face = "bold", color = "#333"),
        panel.grid.major.x = element_blank()
      ) +
      labs(x = "", y = "Quantidade") +
      ylim(0, max(dados$count) * 1.2)
  })
  
  output$grafico_conformidade <- renderPlot({
    m <- metricas()
    
    if(is.null(m)) {
      dados <- data.frame(
        status = c("CONFORME", "DIVERGENTE"),
        count = c(0, 0)
      )
    } else {
      dados <- data.frame(
        status = c("CONFORME", "DIVERGENTE"),
        count = c(m$conformes, m$divergentes)
      )
    }
    
    ggplot(dados, aes(x = "", y = count, fill = status)) +
      geom_col(width = 1, alpha = 0.9) +
      coord_polar(theta = "y") +
      scale_fill_manual(
        values = c("CONFORME" = "#11998e", "DIVERGENTE" = "#f5576c"),
        name = ""
      ) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 13, face = "bold")
      ) +
      geom_text(
        aes(label = paste0(count, "\n(", round(count/sum(count)*100, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size = 5.5,
        fontface = "bold",
        color = "white"
      )
  })
  
  output$tabela_recentes <- DT::renderDataTable({
    if(is.null(values$resultados_lote) || nrow(values$resultados_lote) == 0) {
      exemplo <- data.frame(
        Assunto = c("Aguardando classificações..."),
        Tipo = c("-"),
        Categoria = c("-"),
        Criticidade = c("-"),
        Confiança = c("-")
      )
    } else {
      exemplo <- tail(values$resultados_lote, 15) %>%
        mutate(
          Assunto = if("assunto_principal" %in% names(.)) {
            substr(assunto_principal, 1, 70)
          } else {
            substr(texto_completo, 1, 70)
          },
          Confiança = paste0(confianca, "%")
        ) %>%
        select(
          Assunto, 
          Tipo = tipo_novo, 
          Categoria = categoria,
          Criticidade = criticidade, 
          Confiança
        )
    }
    
    DT::datatable(
      exemplo,
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(
          list(width = '400px', targets = 0)
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Criticidade',
        backgroundColor = styleEqual(
          c('BAIXA', 'MEDIA', 'ALTA', 'CRITICA'),
          c('#4682B4', '#32CD32', '#FF8C00', '#DC143C')
        ),
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      ) %>%
      formatStyle(
        'Assunto',
        fontWeight = 'bold',
        color = '#667eea'
      )
  })
  
  #===========================================================================
  # UPLOAD DE ARQUIVOS
  #===========================================================================
  
  observeEvent(input$arquivo_ordens, {
    req(input$arquivo_ordens)
    
    tryCatch({
      ext <- tools::file_ext(input$arquivo_ordens$datapath)
      
      if(ext == "csv") {
        values$dados_ordens <- read.csv(input$arquivo_ordens$datapath, stringsAsFactors = FALSE)
      } else if(ext %in% c("xlsx", "xls")) {
        values$dados_ordens <- read_excel(input$arquivo_ordens$datapath)
      }
      
      showNotification(
        "✅ Arquivo de Ordens carregado com sucesso!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("❌ Erro ao carregar arquivo:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  observeEvent(input$arquivo_textos, {
    req(input$arquivo_textos)
    
    tryCatch({
      ext <- tools::file_ext(input$arquivo_textos$datapath)
      
      if(ext == "csv") {
        values$dados_textos <- read.csv(input$arquivo_textos$datapath, stringsAsFactors = FALSE)
      } else if(ext %in% c("xlsx", "xls")) {
        values$dados_textos <- read_excel(input$arquivo_textos$datapath)
      }
      
      showNotification(
        "✅ Arquivo de Textos carregado com sucesso!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("❌ Erro ao carregar arquivo:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  #===========================================================================
  # CRUZAMENTO DE DADOS
  #===========================================================================
  
  observeEvent(input$cruzar, {
    req(values$dados_ordens, values$dados_textos)
    
    withProgress(message = '🔗 Cruzando dados...', value = 0, {
      
      incProgress(0.3, detail = "Identificando colunas...")
      
      resultado <- cruzar_dados(values$dados_ordens, values$dados_textos)
      
      incProgress(0.7, detail = "Finalizando...")
      
      if(resultado$sucesso) {
        values$dados_cruzados <- resultado$dados
        values$col_tip_intervencao <- resultado$col_tip_intervencao
        values$dados_com_assuntos <- NULL
        
        output$status_cruzamento <- renderUI({
          HTML(paste0(
            "<div style='padding: 25px; background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%); 
                         border-radius: 15px; border-left: 6px solid #28a745;
                         box-shadow: 0 4px 16px rgba(40, 167, 69, 0.2);'>",
            "<div style='display: flex; align-items: center; margin-bottom: 15px;'>",
            "<span style='font-size: 48px; margin-right: 20px;'>✅</span>",
            "<div>",
            "<h3 style='color: #155724; margin: 0 0 10px 0; font-weight: 700;'>Cruzamento Concluído!</h3>",
            "<p style='color: #155724; margin: 0; font-size: 14px;'>Os arquivos foram cruzados com sucesso</p>",
            "</div>",
            "</div>",
            "<div style='background: rgba(255,255,255,0.6); padding: 15px; border-radius: 10px;'>",
            "<div style='display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; text-align: center;'>",
            "<div>",
            "<div style='font-size: 28px; color: #155724; font-weight: 800;'>", format(nrow(resultado$dados), big.mark = "."), "</div>",
            "<div style='font-size: 11px; color: #155724; margin-top: 5px;'>TOTAL</div>",
            "</div>",
            "<div>",
            "<div style='font-size: 28px; color: #155724; font-weight: 800;'>", format(resultado$estatisticas$com_texto, big.mark = "."), "</div>",
            "<div style='font-size: 11px; color: #155724; margin-top: 5px;'>COM TEXTO</div>",
            "</div>",
            "<div>",
            "<div style='font-size: 28px; color: #155724; font-weight: 800;'>", format(resultado$estatisticas$correspondencias, big.mark = "."), "</div>",
            "<div style='font-size: 11px; color: #155724; margin-top: 5px;'>CORRESPONDÊNCIAS</div>",
            "</div>",
            "</div>",
            "</div>",
            "</div>"
          ))
        })
        
        showNotification(
          "✅ Dados cruzados com sucesso!",
          type = "message",
          duration = 5
        )
        
      } else {
        output$status_cruzamento <- renderUI({
          HTML(paste0(
            "<div style='padding: 25px; background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%); 
                         border-radius: 15px; border-left: 6px solid #dc3545;
                         box-shadow: 0 4px 16px rgba(220, 53, 69, 0.2);'>",
            "<div style='display: flex; align-items: center;'>",
            "<span style='font-size: 48px; margin-right: 20px;'>❌</span>",
            "<div>",
            "<h3 style='color: #721c24; margin: 0 0 10px 0; font-weight: 700;'>Erro no Cruzamento</h3>",
            "<p style='color: #721c24; margin: 0; font-size: 14px;'>", resultado$erro, "</p>",
            "</div>",
            "</div>",
            "</div>"
          ))
        })
        
        showNotification(
          paste("❌", resultado$erro),
          type = "error",
          duration = 10
        )
      }
    })
  })
  
  #===========================================================================
  # PREVIEW DOS DADOS CRUZADOS
  #===========================================================================
  
  output$preview_cruzado <- DT::renderDataTable({
    req(values$dados_cruzados)
    
    # Verificar se há dados com assuntos processados
    if(!is.null(values$dados_com_assuntos)) {
      dados_exibir <- values$dados_com_assuntos
      tem_assuntos <- TRUE
    } else {
      dados_exibir <- values$dados_cruzados
      tem_assuntos <- FALSE
    }
    
    # Limitar a 100 primeiras linhas para preview
    dados_exibir <- head(dados_exibir, 100)
    
    # Definir colunas prioritárias
    colunas_exibir <- c()
    
    if("nota_key" %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, "nota_key")
    }
    
    if(tem_assuntos && "assunto_principal" %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, "assunto_principal")
    }
    
    if(!is.null(values$col_tip_intervencao) && 
       values$col_tip_intervencao %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, values$col_tip_intervencao)
    }
    
    if("texto_completo" %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, "texto_completo")
    }
    
    # Adicionar outras colunas relevantes
    colunas_extras <- c(
      "Ordem", "ordem", "Centro de trabalho", "centro_trabalho",
      "Descrição da ordem", "descricao_ordem", "Data de início", "data_inicio"
    )
    
    for(col in colunas_extras) {
      if(col %in% names(dados_exibir) && !(col %in% colunas_exibir)) {
        colunas_exibir <- c(colunas_exibir, col)
      }
    }
    
    colunas_existentes <- colunas_exibir[colunas_exibir %in% names(dados_exibir)]
    
    if(length(colunas_existentes) == 0) {
      dados_tabela <- dados_exibir
    } else {
      dados_tabela <- dados_exibir %>%
        select(all_of(colunas_existentes))
    }
    
    # Renomear colunas
    nomes_colunas <- names(dados_tabela)
    nomes_amigaveis <- nomes_colunas
    
    mapa_nomes <- list(
      "nota_key" = "📋 Nota",
      "assunto_principal" = "📝 Assunto Principal",
      "texto_completo" = "📄 Texto Completo",
      "Ordem" = "🔢 Ordem",
      "ordem" = "🔢 Ordem",
      "Centro de trabalho" = "🏭 Centro Trabalho",
      "centro_trabalho" = "🏭 Centro Trabalho"
    )
    
    for(i in seq_along(nomes_colunas)) {
      col_original <- nomes_colunas[i]
      
      if(!is.null(values$col_tip_intervencao) && col_original == values$col_tip_intervencao) {
        nomes_amigaveis[i] <- "🔧 Tipo Intervenção"
      } else if(!is.null(mapa_nomes[[col_original]])) {
        nomes_amigaveis[i] <- mapa_nomes[[col_original]]
      }
    }
    
    names(dados_tabela) <- nomes_amigaveis
    
    # Criar DataTable
    dt <- DT::datatable(
      dados_tabela,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = '📋 Copiar'),
          list(extend = 'csv', text = '📊 CSV'),
          list(extend = 'excel', text = '📗 Excel')
        ),
        language = list(
          search = "🔍 Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros"
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE,
      filter = 'top',
      extensions = 'Buttons'
    )
    
    # Estilos condicionais
    if("📝 Assunto Principal" %in% nomes_amigaveis) {
      dt <- dt %>%
        formatStyle(
          '📝 Assunto Principal',
          backgroundColor = '#e3f2fd',
          fontWeight = 'bold',
          fontSize = '13px',
          color = '#1565c0'
        )
    }
    
    if("🔧 Tipo Intervenção" %in% nomes_amigaveis) {
      dt <- dt %>%
        formatStyle(
          '🔧 Tipo Intervenção',
          backgroundColor = styleInterval(
            c(1, 2, 3, 4, 5),
            c('#87CEEB', '#90EE90', '#FFD700', '#FFA500', '#FF6347', '#DC143C')
          ),
          color = 'white',
          fontWeight = 'bold',
          textAlign = 'center'
        )
    }
    
    dt
  })
  
  #===========================================================================
  # ESTATÍSTICAS DOS DADOS CRUZADOS
  #===========================================================================
  
  output$estatisticas_cruzados <- renderUI({
    req(values$dados_cruzados)
    
    dados <- values$dados_cruzados
    total_registros <- nrow(dados)
    com_texto <- sum(!is.na(dados$texto_completo) & nchar(trimws(dados$texto_completo)) > 0)
    sem_texto <- total_registros - com_texto
    taxa_sucesso <- round((com_texto / total_registros) * 100, 1)
    
    HTML(paste0(
      "<div style='display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin-bottom: 25px;'>",
      
      # Card 1: Total
      "<div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 25px; border-radius: 15px; text-align: center; color: white;
                   box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3); transition: all 0.3s ease;'
           onmouseover='this.style.transform=\"translateY(-8px)\"; this.style.boxShadow=\"0 12px 32px rgba(102, 126, 234, 0.4)\";'
           onmouseout='this.style.transform=\"translateY(0)\"; this.style.boxShadow=\"0 8px 24px rgba(102, 126, 234, 0.3)\";'>",
      "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>📊 TOTAL</div>",
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", format(total_registros, big.mark = "."), "</div>",
      "<div style='font-size: 12px; opacity: 0.8; margin-top: 10px;'>registros</div>",
      "</div>",
      
      # Card 2: Com Texto
      "<div style='background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
                   padding: 25px; border-radius: 15px; text-align: center; color: white;
                   box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3); transition: all 0.3s ease;'
           onmouseover='this.style.transform=\"translateY(-8px)\"; this.style.boxShadow=\"0 12px 32px rgba(17, 153, 142, 0.4)\";'
           onmouseout='this.style.transform=\"translateY(0)\"; this.style.boxShadow=\"0 8px 24px rgba(17, 153, 142, 0.3)\";'>",
      "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>✅ COM TEXTO</div>",
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", format(com_texto, big.mark = "."), "</div>",
      "<div style='font-size: 12px; opacity: 0.8; margin-top: 10px;'>registros</div>",
      "</div>",
      
      # Card 3: Sem Texto
      "<div style='background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                   padding: 25px; border-radius: 15px; text-align: center; color: white;
                   box-shadow: 0 8px 24px rgba(240, 147, 251, 0.3); transition: all 0.3s ease;'
           onmouseover='this.style.transform=\"translateY(-8px)\"; this.style.boxShadow=\"0 12px 32px rgba(240, 147, 251, 0.4)\";'
           onmouseout='this.style.transform=\"translateY(0)\"; this.style.boxShadow=\"0 8px 24px rgba(240, 147, 251, 0.3)\";'>",
      "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>⚠️ SEM TEXTO</div>",
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", format(sem_texto, big.mark = "."), "</div>",
      "<div style='font-size: 12px; opacity: 0.8; margin-top: 10px;'>registros</div>",
      "</div>",
      
      # Card 4: Taxa
      "<div style='background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                   padding: 25px; border-radius: 15px; text-align: center; color: white;
                   box-shadow: 0 8px 24px rgba(79, 172, 254, 0.3); transition: all 0.3s ease;'
           onmouseover='this.style.transform=\"translateY(-8px)\"; this.style.boxShadow=\"0 12px 32px rgba(79, 172, 254, 0.4)\";'
           onmouseout='this.style.transform=\"translateY(0)\"; this.style.boxShadow=\"0 8px 24px rgba(79, 172, 254, 0.3)\";'>",
      "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>🎯 TAXA SUCESSO</div>",
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", taxa_sucesso, "%</div>",
      "<div style='font-size: 12px; opacity: 0.8; margin-top: 10px;'>efetividade</div>",
      "</div>",
      
      "</div>"
    ))
  })
  
  output$info_preview <- renderUI({
    req(values$dados_cruzados)
    
    total <- nrow(values$dados_cruzados)
    preview_size <- min(100, total)
    
    if(!is.null(values$dados_com_assuntos)) {
      total_processados <- nrow(values$dados_com_assuntos)
      
      HTML(paste0(
        "<div style='background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%); 
                     padding: 25px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #28a745; box-shadow: 0 4px 16px rgba(40, 167, 69, 0.2);'>",
        "<div style='display: flex; align-items: center; justify-content: space-between;'>",
        "<div>",
        "<h3 style='color: #155724; margin: 0 0 15px 0; font-weight: 700;'>",
        "✅ Assuntos Extraídos com Sucesso!</h3>",
        "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 20px;'>",
        "<div style='background: rgba(255,255,255,0.6); padding: 15px; border-radius: 10px;'>",
        "<div style='font-size: 12px; color: #155724; margin-bottom: 5px;'>PROCESSADOS</div>",
        "<div style='font-size: 28px; font-weight: 800; color: #155724;'>", 
        format(total_processados, big.mark = "."), "</div>",
        "</div>",
        "<div style='background: rgba(255,255,255,0.6); padding: 15px; border-radius: 10px;'>",
        "<div style='font-size: 12px; color: #155724; margin-bottom: 5px;'>TOTAL ARQUIVO</div>",
        "<div style='font-size: 28px; font-weight: 800; color: #155724;'>", 
        format(total, big.mark = "."), "</div>",
        "</div>",
        "</div>",
        "</div>",
        "<div style='text-align: center;'>",
        "<span style='font-size: 72px;'>📝</span>",
        "</div>",
        "</div>",
        "</div>"
      ))
      
    } else {
      
      HTML(paste0(
        "<div style='background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); 
                     padding: 25px; border-radius: 15px; margin-bottom: 25px;
                     border-left: 6px solid #ffc107; box-shadow: 0 4px 16px rgba(255, 193, 7, 0.2);'>",
        "<div style='display: flex; align-items: center; justify-content: space-between;'>",
        "<div>",
        "<h3 style='color: #856404; margin: 0 0 15px 0; font-weight: 700;'>",
        "ℹ️ Preview dos Dados Cruzados</h3>",
        "<div style='font-size: 14px; color: #856404; line-height: 2;'>",
        "<p style='margin: 5px 0;'><strong>📊 Mostrando:</strong> Primeiras ", preview_size, " linhas</p>",
        "<p style='margin: 5px 0;'><strong>📋 Total no arquivo:</strong> ", format(total, big.mark = "."), " registros</p>",
        "<p style='margin: 5px 0;'><strong>💡 Dica:</strong> Clique em 'Extrair Assuntos' para melhor visualização</p>",
        "</div>",
        "</div>",
        "<div style='text-align: center;'>",
        "<span style='font-size: 72px;'>📊</span>",
        "</div>",
        "</div>",
        "</div>"
      ))
    }
  })
  
  #===========================================================================
  # PROCESSAR ASSUNTOS NO PREVIEW
  #===========================================================================
  
  observeEvent(input$processar_assuntos_preview, {
    req(values$dados_cruzados)
    
    withProgress(message = '📝 Extraindo assuntos principais...', value = 0, {
      
      dados_preview <- head(values$dados_cruzados, 100)
      dados_preview$assunto_principal <- NA_character_
      
      total <- nrow(dados_preview)
      
      for(i in 1:total) {
        texto <- dados_preview$texto_completo[i]
        
        if(!is.na(texto) && nchar(trimws(texto)) > 0) {
          assunto <- extrair_assunto_principal(texto)
          dados_preview$assunto_principal[i] <- assunto
        } else {
          dados_preview$assunto_principal[i] <- "Sem texto"
        }
        
        incProgress(1/total, detail = paste("Processando", i, "de", total))
        Sys.sleep(0.1)
      }
      
      values$dados_com_assuntos <- dados_preview
      
      showNotification(
        "✅ Assuntos extraídos com sucesso!",
        type = "message",
        duration = 5
      )
    })
  })
  
  # Limpar assuntos quando cruzar novamente
  observeEvent(input$cruzar, {
    values$dados_com_assuntos <- NULL
  })
  
  #===========================================================================
  # CLASSIFICAÇÃO INDIVIDUAL
  #===========================================================================
  
  # Atualizar select de notas
  observeEvent(values$dados_cruzados, {
    if(!is.null(values$dados_cruzados)) {
      
      notas_disponiveis <- values$dados_cruzados %>%
        select(nota_key, texto_completo) %>%
        filter(!is.na(texto_completo), nchar(trimws(texto_completo)) > 0)
      
      if(nrow(notas_disponiveis) > 0) {
        choices_notas <- setNames(
          as.list(1:nrow(notas_disponiveis)),
          paste0("Nota ", notas_disponiveis$nota_key, " - ", 
                 substr(notas_disponiveis$texto_completo, 1, 50), "...")
        )
        
        updateSelectInput(
          session, 
          "nota_referencia",
          choices = c("Nenhuma" = "", choices_notas)
        )
      }
    }
  })
  
  # Preencher texto ao selecionar nota
  observeEvent(input$nota_referencia, {
    if(!is.null(input$nota_referencia) && input$nota_referencia != "") {
      
      idx <- as.integer(input$nota_referencia)
      
      if(!is.na(idx) && idx <= nrow(values$dados_cruzados)) {
        registro <- values$dados_cruzados[idx, ]
        
        updateTextAreaInput(
          session, 
          "texto_individual",
          value = registro$texto_completo
        )
        
        if(!is.null(values$col_tip_intervencao) && 
           values$col_tip_intervencao %in% names(registro)) {
          
          tipo_ant <- registro[[values$col_tip_intervencao]]
          
          if(!is.na(tipo_ant)) {
            updateNumericInput(
              session, 
              "tipo_anterior", 
              value = as.integer(tipo_ant)
            )
          }
        }
      }
    }
  })
  
  # Extrair assunto individual
  observeEvent(input$extrair_assunto_individual, {
    req(input$texto_individual)
    
    if(nchar(trimws(input$texto_individual)) == 0) {
      showNotification(
        "⚠️ Digite um texto para extrair o assunto.",
        type = "warning"
      )
      return()
    }
    
    withProgress(message = '📝 Extraindo assunto...', value = 0, {
      
      incProgress(0.5, detail = "Consultando IA...")
      
      assunto <- extrair_assunto_principal(input$texto_individual)
      
      incProgress(1, detail = "Concluído!")
      
      output$assunto_extraido <- renderUI({
        HTML(paste0(
          "<div style='padding: 20px; background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                       border-radius: 12px; border-left: 6px solid #2196F3; 
                       margin: 20px 0; box-shadow: 0 4px 16px rgba(33, 150, 243, 0.2);'>",
          "<div style='display: flex; align-items: center;'>",
          "<span style='font-size: 48px; margin-right: 20px;'>📝</span>",
          "<div style='flex: 1;'>",
          "<strong style='color: #1565C0; font-size: 16px; display: block; margin-bottom: 10px;'>",
          "Assunto Principal Extraído:</strong>",
          "<div style='background: white; padding: 15px; border-radius: 8px; box-shadow: inset 0 2px 4px rgba(0,0,0,0.06);'>",
          "<span style='color: #333; font-size: 15px; font-weight: 600; line-height: 1.6;'>", 
          assunto, "</span>",
          "</div>",
          "</div>",
          "</div>",
          "</div>"
        ))
      })
      
      showNotification("✅ Assunto extraído!", type = "message", duration = 3)
    })
  })
  
  # Classificar individual
  observeEvent(input$classificar_individual, {
    req(input$texto_individual)
    
    if(nchar(trimws(input$texto_individual)) == 0) {
      showNotification(
        "⚠️ Digite um texto para classificar.",
        type = "warning"
      )
      return()
    }
    
    withProgress(message = '🤖 Classificando com IA...', value = 0, {
      
      incProgress(0.5, detail = "Analisando texto...")
      
      resultado <- classificar_hibrido(input$texto_individual, CONFIG_USUARIO)
      
      incProgress(1, detail = "Concluído!")
      
      tipo_anterior <- input$tipo_anterior
      tem_comparacao <- !is.na(tipo_anterior) && tipo_anterior >= 1 && tipo_anterior <= 6
      
      cor_criticidade <- switch(
        resultado$criticidade,
        "BAIXA" = "#4682B4",
        "MEDIA" = "#32CD32",
        "ALTA" = "#FF8C00",
        "CRITICA" = "#DC143C"
      )
      
      cor_hierarquia <- ifelse(resultado$categoria == "IAZF", "#f5576c", "#11998e")
      
      icone <- switch(
        as.character(resultado$tipo),
        "1" = "🧽", "2" = "🔧", "3" = "🔍",
        "4" = "⏰", "5" = "⚠️", "6" = "🚨"
      )
      
      mudanca_html <- ""
      
      if(tem_comparacao) {
        
        mudou <- tipo_anterior != resultado$tipo
        
        criticidade_anterior <- switch(
          as.character(tipo_anterior),
          "1" = "BAIXA", "2" = "BAIXA",
          "3" = "MEDIA", "4" = "MEDIA",
          "5" = "ALTA", "6" = "CRITICA"
        )
        
        ordem_criticidade <- c("BAIXA" = 1, "MEDIA" = 2, "ALTA" = 3, "CRITICA" = 4)
        nivel_anterior <- ordem_criticidade[criticidade_anterior]
        nivel_novo <- ordem_criticidade[resultado$criticidade]
        
        if(mudou) {
          
          icone_mudanca <- if(nivel_novo > nivel_anterior) {
            "⬆️"
          } else if(nivel_novo < nivel_anterior) {
            "⬇️"
          } else {
            "↔️"
          }
          
          cor_mudanca <- if(nivel_novo > nivel_anterior) {
            "linear-gradient(135deg, #f5576c 0%, #f093fb 100%)"
          } else if(nivel_novo < nivel_anterior) {
            "linear-gradient(135deg, #11998e 0%, #38ef7d 100%)"
          } else {
            "linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)"
          }
          
          texto_mudanca <- if(nivel_novo > nivel_anterior) {
            "AUMENTO DE CRITICIDADE"
          } else if(nivel_novo < nivel_anterior) {
            "REDUÇÃO DE CRITICIDADE"
          } else {
            "MUDANÇA DE TIPO (MESMA CRITICIDADE)"
          }
          
          mudanca_html <- paste0(
            "<div style='background: ", cor_mudanca, "; padding: 25px; border-radius: 15px; 
                         margin-bottom: 25px; color: white; box-shadow: 0 8px 24px rgba(0,0,0,0.15);'>",
            "<div style='display: flex; align-items: center; justify-content: space-between;'>",
            "<div style='flex: 1;'>",
            "<div style='font-size: 48px; margin-bottom: 15px;'>", icone_mudanca, "</div>",
            "<h3 style='margin: 0 0 10px 0; font-weight: 800; font-size: 20px;'>", texto_mudanca, "</h3>",
            "<p style='font-size: 14px; opacity: 0.95; margin: 0;'>",
            "Tipo anterior: <strong>", tipo_anterior, "</strong> → Tipo sugerido: <strong>", resultado$tipo, "</strong>",
            "</p>",
            "</div>",
            "<div style='text-align: right;'>",
            "<div style='background: rgba(255,255,255,0.25); padding: 15px 25px; border-radius: 20px; ",
            "border: 3px solid white; display: inline-block;'>",
            "<div style='font-size: 13px; margin-bottom: 5px; opacity: 0.9;'>", criticidade_anterior, "</div>",
            "<div style='font-size: 32px; margin: 5px 0;'>➜</div>",
            "<div style='font-size: 16px; font-weight: 800; margin-top: 5px;'>", resultado$criticidade, "</div>",
            "</div>",
            "</div>",
            "</div>",
            "</div>"
          )
          
        } else {
          
          mudanca_html <- paste0(
            "<div style='background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%); 
                         padding: 25px; border-radius: 15px; margin-bottom: 25px;
                         border-left: 6px solid #28a745; box-shadow: 0 4px 16px rgba(40, 167, 69, 0.2);'>",
            "<div style='display: flex; align-items: center;'>",
            "<span style='font-size: 56px; margin-right: 20px;'>✅</span>",
            "<div>",
            "<h3 style='color: #155724; margin: 0 0 10px 0; font-weight: 800; font-size: 20px;'>",
            "CLASSIFICAÇÃO CONFIRMADA</h3>",
            "<p style='color: #155724; font-size: 14px; margin: 0;'>",
            "A IA confirmou o tipo anterior (<strong>", tipo_anterior, "</strong>). ",
            "Não há necessidade de reclassificação.",
            "</p>",
            "</div>",
            "</div>",
            "</div>"
          )
        }
      }
      
      # Comparação Visual Grande
      comparacao_visual <- ""
      if(tem_comparacao) {
        comparacao_visual <- paste0(
          "<div style='display: grid; grid-template-columns: 1fr auto 1fr; gap: 25px; ",
          "align-items: center; margin-bottom: 30px; padding: 30px; ",
          "background: white; border-radius: 15px; box-shadow: 0 4px 16px rgba(0,0,0,0.08);'>",
          
          # Tipo Anterior
          "<div style='text-align: center; padding: 25px; ",
          "background: linear-gradient(135deg, #f5f5f5 0%, #e0e0e0 100%); ",
          "border-radius: 12px; box-shadow: inset 0 2px 8px rgba(0,0,0,0.1);'>",
          "<div style='color: #999; font-size: 13px; font-weight: 700; ",
          "margin-bottom: 15px; text-transform: uppercase; letter-spacing: 2px;'>TIPO ANTERIOR</div>",
          "<div style='font-size: 72px; color: #95a5a6; font-weight: 800; ",
          "line-height: 1; text-shadow: 2px 2px 4px rgba(0,0,0,0.1);'>", tipo_anterior, "</div>",
          "<div style='margin-top: 15px; padding: 8px 20px; ",
          "background: rgba(149, 165, 166, 0.2); border-radius: 20px; display: inline-block;'>",
          "<span style='font-size: 13px; color: #666; font-weight: 700;'>", criticidade_anterior, "</span>",
          "</div>",
          "</div>",
          
          # Seta
          "<div style='text-align: center;'>",
          "<div style='font-size: 64px; color: ", 
          ifelse(mudou, 
                 ifelse(nivel_novo > nivel_anterior, "#f5576c", "#11998e"), 
                 "#667eea"
          ), ";'>",
          ifelse(mudou, 
                 ifelse(nivel_novo > nivel_anterior, "⬆️", "⬇️"), 
                 "✓"
          ),
          "</div>",
          "<div style='font-size: 12px; color: #666; font-weight: 700; ",
          "margin-top: 10px; letter-spacing: 1px;'>",
          ifelse(mudou,
                 ifelse(nivel_novo > nivel_anterior, "AUMENTOU", "REDUZIU"),
                 "CONFIRMADO"
          ),
          "</div>",
          "</div>",
          
          # Tipo Novo
          "<div style='text-align: center; padding: 25px; ",
          "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); ",
          "border-radius: 12px; box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);'>",
          "<div style='color: white; font-size: 13px; font-weight: 700; ",
          "margin-bottom: 15px; text-transform: uppercase; letter-spacing: 2px;'>TIPO SUGERIDO</div>",
          "<div style='font-size: 72px; color: white; font-weight: 800; ",
          "line-height: 1; text-shadow: 2px 2px 4px rgba(0,0,0,0.2);'>", resultado$tipo, "</div>",
          "<div style='margin-top: 15px; padding: 8px 20px; ",
          "background: rgba(255,255,255,0.3); border-radius: 20px; ",
          "display: inline-block; border: 2px solid white;'>",
          "<span style='font-size: 13px; color: white; font-weight: 700;'>", resultado$criticidade, "</span>",
          "</div>",
          "</div>",
          
          "</div>"
        )
      }
      
      output$resultado_individual <- renderUI({
        HTML(paste0(
          "<div style='padding: 30px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); ",
          "border-radius: 15px; border-left: 6px solid ", cor_hierarquia, "; ",
          "margin-top: 25px; box-shadow: 0 4px 16px rgba(0,0,0,0.08);'>",
          
          "<h3 style='color: #333; margin: 0 0 25px 0; font-weight: 700; font-size: 22px;'>",
          icone, " Resultado da Classificação</h3>",
          
          mudanca_html,
          comparacao_visual,
          
          # Cards de Métricas
          "<div style='display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin-bottom: 25px;'>",
          
          # Card Tipo
          "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; ",
          "box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid #667eea;'>",
          "<div style='color: #999; font-size: 12px; font-weight: 700; margin-bottom: 10px; ",
          "text-transform: uppercase; letter-spacing: 1px;'>TIPO SAP", 
          ifelse(tem_comparacao, " SUGERIDO", ""), "</div>",
          "<div style='font-size: 48px; color: #667eea; font-weight: 800; margin: 15px 0;'>", 
          resultado$tipo, "</div>",
          "</div>",
          
          # Card Confiança
          "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; ",
          "box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid #11998e;'>",
          "<div style='color: #999; font-size: 12px; font-weight: 700; margin-bottom: 10px; ",
          "text-transform: uppercase; letter-spacing: 1px;'>CONFIANÇA</div>",
          "<div style='font-size: 48px; color: #11998e; font-weight: 800; margin: 15px 0;'>", 
          resultado$confianca, "%</div>",
          "<div style='font-size: 12px; color: #666; padding: 5px 15px; ",
          "background: #f8f9fa; border-radius: 15px; display: inline-block;'>",
          ifelse(resultado$confianca >= 90, "Muito Alta",
                 ifelse(resultado$confianca >= 80, "Alta",
                        ifelse(resultado$confianca >= 70, "Média", "Revisar"))),
          "</div>",
          "</div>",
          
          # Card Criticidade
          "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; ",
          "box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid ", cor_criticidade, ";'>",
          "<div style='color: #999; font-size: 12px; font-weight: 700; margin-bottom: 10px; ",
          "text-transform: uppercase; letter-spacing: 1px;'>CRITICIDADE</div>",
          "<div style='margin: 15px 0;'>",
          "<span style='font-size: 20px; color: white; background: ", cor_criticidade, "; ",
          "padding: 12px 30px; border-radius: 30px; font-weight: 800; ",
          "display: inline-block; box-shadow: 0 4px 12px rgba(0,0,0,0.2);'>", 
          resultado$criticidade, "</span>",
          "</div>",
          "</div>",
          
          "</div>",
          
          # Badge Hierarquia
          "<div style='margin-bottom: 25px; text-align: center;'>",
          "<strong style='color: #666; font-size: 15px; margin-right: 15px;'>Hierarquia:</strong>",
          "<span style='background: ", cor_hierarquia, "; color: white; ",
          "padding: 12px 35px; border-radius: 30px; font-weight: 800; font-size: 17px; ",
          "box-shadow: 0 4px 16px rgba(0,0,0,0.15); display: inline-block;'>",
          resultado$categoria, "</span>",
          "</div>",
          
          # Descrição SAP
          "<div style='background: white; padding: 25px; border-radius: 12px; ",
          "box-shadow: 0 4px 12px rgba(0,0,0,0.08); margin-bottom: 20px; ",
          "border-left: 5px solid #667eea;'>",
          "<div style='display: flex; align-items: center; margin-bottom: 15px;'>",
          "<span style='font-size: 32px; margin-right: 15px;'>📖</span>",
          "<strong style='color: #667eea; font-size: 17px; font-weight: 700;'>Descrição SAP</strong>",
          "</div>",
          "<p style='color: #495057; line-height: 1.8; font-size: 15px; margin: 0;'>", 
          resultado$descricao, "</p>",
          "</div>",
          
          # Resumo da Análise
          "<div style='background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); ",
          "padding: 25px; border-radius: 12px; border-left: 5px solid #ffc107; ",
          "box-shadow: 0 4px 12px rgba(255, 193, 7, 0.2);'>",
          "<div style='display: flex; align-items: center; margin-bottom: 15px;'>",
          "<span style='font-size: 32px; margin-right: 15px;'>💡</span>",
          "<strong style='color: #856404; font-size: 17px; font-weight: 700;'>Resumo da Análise</strong>",
          "</div>",
          "<p style='color: #856404; line-height: 1.8; font-size: 15px; margin: 0;'>", 
          resultado$resumo, "</p>",
          "</div>",
          
          "</div>"
        ))
      })
      
      showNotification(
        "✅ Texto classificado com sucesso!",
        type = "message",
        duration = 3
      )
    })
  })
  
  # Limpar classificação individual
  observeEvent(input$limpar_individual, {
    updateTextAreaInput(session, "texto_individual", value = "")
    updateNumericInput(session, "tipo_anterior", value = NA)
    updateSelectInput(session, "nota_referencia", selected = "")
    
    output$assunto_extraido <- renderUI({ NULL })
    output$resultado_individual <- renderUI({
      div(
        style = "text-align: center; padding: 80px 40px; background: white; 
                 border-radius: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
        icon("robot", style = "font-size: 72px; color: #e0e0e0; margin-bottom: 20px;"),
        h4(style = "color: #999; margin: 0 0 10px 0; font-weight: 600;",
           "Aguardando Entrada"),
        p(style = "color: #999; font-size: 14px; margin: 0;",
          "Digite um texto e clique em 'Classificar' para ver o resultado")
      )
    })
  })
  
  # Estado inicial
  output$resultado_individual <- renderUI({
    div(
      style = "text-align: center; padding: 80px 40px; background: white; 
               border-radius: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
      icon("robot", style = "font-size: 72px; color: #e0e0e0; margin-bottom: 20px;"),
      h4(style = "color: #999; margin: 0 0 10px 0; font-weight: 600;",
         "Aguardando Entrada"),
      p(style = "color: #999; font-size: 14px; margin: 0;",
        "Digite um texto e clique em 'Classificar' para ver o resultado")
    )
  })
  
  # ... (Continua na próxima mensagem com Classificação em Lote)
  
  
  #===========================================================================
  # CLASSIFICAÇÃO EM LOTE
  #===========================================================================
  
  observeEvent(input$classificar_lote, {
    
    cat("\n=== INÍCIO CLASSIFICAÇÃO LOTE ===\n")
    cat("Botão clicado!\n")
    
    # ❌ REMOVER ESTA VALIDAÇÃO:
    # req(input$arquivo_lote)
    
    # ✅ MANTER APENAS ESTA:
    if(is.null(values$dados_cruzados)) {
      cat("❌ ERRO: Execute o cruzamento de dados primeiro!\n")
      showNotification("❌ Execute o cruzamento de dados primeiro!", type = "error")
      return()
    }
    cat("✅ Dados cruzados OK -", nrow(values$dados_cruzados), "linhas\n")
    
    # Verificação do método
    if(is.null(input$metodo_classificacao)) {
      cat("❌ ERRO: Método não selecionado\n")
      showNotification("❌ Selecione um método de classificação!", type = "error")
      return()
    }
    cat("✅ Método:", input$metodo_classificacao, "\n")
    
    # Verificação de processamento em andamento
    if(isTRUE(values$processando)) {
      cat("⚠️ AVISO: Processamento já em andamento\n")
      showNotification("⚠️ Já existe uma classificação em andamento!", type = "warning")
      return()
    }
    
    cat("🚀 Iniciando processamento...\n")
    
    # Marcar como processando
    values$processando <- TRUE
    
    # Desabilitar botão (se shinyjs estiver disponível)
    if("shinyjs" %in% loadedNamespaces()) {
      shinyjs::disable("classificar_lote")
    }
    
    tryCatch({
      
      total <- nrow(values$dados_cruzados)
      cat("Total de registros:", total, "\n")
      
      # Preparar estrutura de resultados
      resultados <- values$dados_cruzados %>%
        select(nota_key, texto_completo)
      
      cat("✅ Estrutura base criada\n")
      
      # Adicionar tipo antigo se existir
      if(!is.null(values$col_tip_intervencao) && 
         values$col_tip_intervencao %in% names(values$dados_cruzados)) {
        resultados <- resultados %>%
          mutate(tipo_intervencao_antigo = values$dados_cruzados[[values$col_tip_intervencao]])
        cat("✅ Tipo antigo adicionado\n")
      } else {
        resultados <- resultados %>%
          mutate(tipo_intervencao_antigo = NA)
        cat("⚠️ Tipo antigo não disponível\n")
      }
      
      # Inicializar colunas de resultado
      resultados <- resultados %>%
        mutate(
          assunto_principal = NA_character_,
          tipo_novo = NA_integer_,
          categoria = NA_character_,
          criticidade = NA_character_,
          confianca = NA_real_,
          descricao = NA_character_,
          resumo = NA_character_,
          metodo = NA_character_,
          status_conformidade = NA_character_
        )
      
      cat("✅ Colunas inicializadas\n")
      
      # Processar com barra de progresso
      withProgress(message = 'Classificando em lote...', value = 0, {
        
        for(i in 1:total) {
          
          if(i %% 10 == 0) {  # Log a cada 10 registros
            cat("Processando linha", i, "de", total, "\n")
          }
          
          texto <- resultados$texto_completo[i]
          
          if(!is.na(texto) && nchar(trimws(texto)) > 0) {
            
            # Extrair assunto principal
            if(isTRUE(input$extrair_assunto)) {
              tryCatch({
                assunto <- extrair_assunto_principal(texto)
                resultados$assunto_principal[i] <- assunto
              }, error = function(e) {
                cat("  ⚠️ Erro ao extrair assunto:", as.character(e), "\n")
                resultados$assunto_principal[i] <- substr(texto, 1, 60)
              })
            } else {
              # Se não extrair, usar início do texto
              resultados$assunto_principal[i] <- substr(texto, 1, 60)
            }
            
            # Classificar
            tryCatch({
              classificacao <- classificar_hibrido(texto, CONFIG_USUARIO)
              
              if(!is.null(classificacao)) {
                resultados$tipo_novo[i] <- classificacao$tipo
                resultados$categoria[i] <- classificacao$categoria
                resultados$criticidade[i] <- classificacao$criticidade
                resultados$confianca[i] <- classificacao$confianca
                resultados$descricao[i] <- classificacao$descricao
                resultados$resumo[i] <- classificacao$resumo
                resultados$metodo[i] <- classificacao$metodo
                
                # Verificar conformidade
                tipo_antigo <- resultados$tipo_intervencao_antigo[i]
                if(!is.na(tipo_antigo) && !is.na(classificacao$tipo)) {
                  resultados$status_conformidade[i] <- ifelse(
                    tipo_antigo == classificacao$tipo,
                    "CONFORME",
                    "DIVERGENTE"
                  )
                } else {
                  resultados$status_conformidade[i] <- "SEM_REFERENCIA"
                }
              }
            }, error = function(e) {
              cat("  ⚠️ Erro ao classificar linha", i, ":", as.character(e), "\n")
              resultados$tipo_novo[i] <- NA
              resultados$categoria[i] <- "ERRO"
              resultados$criticidade[i] <- "ERRO"
              resultados$confianca[i] <- 0
              resultados$metodo[i] <- "Erro"
              resultados$status_conformidade[i] <- "ERRO"
            })
            
          } else {
            cat("  ⚠️ Texto vazio na linha", i, "\n")
          }
          
          # Atualizar progresso
          incProgress(1/total, detail = paste("Processando", i, "de", total))
          
          # Pequena pausa para não sobrecarregar API
          Sys.sleep(0.1)
        }
      })
      
      cat("✅ Processamento concluído\n")
      
      # Salvar resultados
      values$resultados_lote <- resultados
      
      # ✅ ADICIONAR AO HISTÓRICO
      metadados <- list(
        metodo = input$metodo_classificacao,
        extrair_assunto = input$extrair_assunto,
        usar_dicionario = CONFIG_USUARIO$usar_dicionario,
        usar_api = CONFIG_USUARIO$usar_api,
        total_linhas = nrow(resultados)
      )
      
      snapshot_id <- adicionar_ao_historico(resultados, metadados)
      cat("💾 Snapshot salvo:", snapshot_id, "\n")
      
      # Estatísticas
      conformes <- sum(resultados$status_conformidade == "CONFORME", na.rm = TRUE)
      divergentes <- sum(resultados$status_conformidade == "DIVERGENTE", na.rm = TRUE)
      sem_ref <- sum(resultados$status_conformidade == "SEM_REFERENCIA", na.rm = TRUE)
      erros <- sum(resultados$status_conformidade == "ERRO", na.rm = TRUE)
      
      cat("\n=== ESTATÍSTICAS ===\n")
      cat("Conformes:", conformes, "\n")
      cat("Divergentes:", divergentes, "\n")
      cat("Sem referência:", sem_ref, "\n")
      cat("Erros:", erros, "\n")
      
      # Notificação de sucesso
      msg <- paste0(
        "✅ Classificação concluída!\n",
        "Conformes: ", conformes, " | ",
        "Divergentes: ", divergentes, " | ",
        "Sem ref: ", sem_ref
      )
      
      if(erros > 0) {
        msg <- paste0(msg, " | Erros: ", erros)
      }
      
      showNotification(msg, type = "message", duration = 10)
      
    }, error = function(e) {
      
      cat("\n❌ ERRO DURANTE PROCESSAMENTO:\n")
      cat(as.character(e), "\n")
      print(traceback())
      
      showNotification(
        paste("❌ Erro durante classificação:", substr(as.character(e), 1, 100)),
        type = "error",
        duration = NULL
      )
      
    }, finally = {
      
      # Sempre liberar o processamento
      values$processando <- FALSE
      
      # Reabilitar botão
      if("shinyjs" %in% loadedNamespaces()) {
        shinyjs::enable("classificar_lote")
      }
      
      cat("\n=== FIM CLASSIFICAÇÃO LOTE ===\n\n")
      
    })
  })
  
  
  # Antes do renderDataTable
  observe({
    req(values$resultados_lote)
    
    cat("\n=== DEBUG TABELA ===\n")
    cat("Colunas disponíveis:", paste(names(values$resultados_lote), collapse = ", "), "\n")
    cat("Tem assunto_principal?", "assunto_principal" %in% names(values$resultados_lote), "\n")
    cat("Total de linhas:", nrow(values$resultados_lote), "\n")
    
    if("assunto_principal" %in% names(values$resultados_lote)) {
      cat("Exemplo assunto:", head(values$resultados_lote$assunto_principal, 1), "\n")
    }
  })
  output$tabela_resultados_lote <- DT::renderDataTable({
    
    req(values$resultados_lote)
    
    # Preparar dados para exibição
    dados_exibicao <- values$resultados_lote %>%
      mutate(
        # Garantir que assunto_principal existe
        assunto_principal = if("assunto_principal" %in% names(.)) {
          assunto_principal
        } else {
          substr(texto_completo, 1, 60)  # Fallback
        },
        
        # Resumos truncados para melhor visualização
        resumo_resumido = ifelse(
          !is.null(resumo) & !is.na(resumo),
          substr(resumo, 1, 80),
          "Sem resumo"
        ),
        
        # Formatar confiança como percentual
        confianca_formatada = paste0(round(confianca, 1), "%")
      ) %>%
      select(
        Nota = nota_key,
        `Assunto Principal` = assunto_principal,
        `Tipo Antigo` = tipo_intervencao_antigo,
        `Tipo Novo` = tipo_novo,
        `Status` = status_conformidade,
        Categoria = categoria,
        Criticidade = criticidade,
        `Confiança` = confianca_formatada,
        Método = metodo,
        `Resumo da Análise` = resumo_resumido
      )
    
    # Criar tabela interativa
    DT::datatable(
      dados_exibicao,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'csv',
            filename = paste0('classificacao_lote_', format(Sys.time(), "%Y%m%d_%H%M%S"))
          ),
          list(
            extend = 'excel',
            filename = paste0('classificacao_lote_', format(Sys.time(), "%Y%m%d_%H%M%S"))
          )
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        ),
        columnDefs = list(
          list(width = '80px', targets = 0),      # Nota
          list(width = '200px', targets = 1),     # Assunto Principal
          list(width = '80px', targets = c(2,3)), # Tipos
          list(width = '100px', targets = 4),     # Status
          list(width = '120px', targets = 5),     # Categoria
          list(width = '80px', targets = 6),      # Criticidade
          list(width = '80px', targets = 7),      # Confiança
          list(width = '100px', targets = 8),     # Método
          list(width = '250px', targets = 9)      # Resumo
        )
      ),
      class = 'cell-border stripe hover',
      filter = 'top',
      rownames = FALSE,
      escape = FALSE  # Permite HTML nas células se necessário
    ) %>%
      # Estilo para Status
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('CONFORME', 'DIVERGENTE', 'SEM_REFERENCIA'),
          c('#2e8b57', '#ff6b35', '#95a5a6')
        ),
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      ) %>%
      # Estilo para Criticidade
      formatStyle(
        'Criticidade',
        backgroundColor = styleEqual(
          c('BAIXA', 'MEDIA', 'ALTA', 'CRITICA'),
          c('#4682B4', '#32CD32', '#FF8C00', '#DC143C')
        ),
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      ) %>%
      # Estilo para Categoria
      formatStyle(
        'Categoria',
        backgroundColor = styleEqual(
          c('PROBLEMAS_COMUNS', 'IAZF'),
          c('#2e8b57', '#ff6b35')
        ),
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      ) %>%
      # Estilo para Método
      formatStyle(
        'Método',
        backgroundColor = styleEqual(
          c('Dicionário', 'API', 'Híbrido'),
          c('#6c757d', '#007bff', '#17a2b8')
        ),
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      ) %>%
      # Destaque para Assunto Principal
      formatStyle(
        'Assunto Principal',
        fontWeight = 'bold',
        color = '#1f4e79'
      ) %>%
      # Tooltip para colunas truncadas
      formatStyle(
        c('Assunto Principal', 'Resumo da Análise'),
        cursor = 'help'
      )
  })
  
  output$download_resultados_lote <- downloadHandler(
    filename = function() {
      paste0("classificacao_lote_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      
      df <- resultados_lote()
      
      # Renomear colunas para exportação
      names(df) <- c(
        "Linha",
        "Tipo de Intervenção",
        "Criticidade",
        "Confiança (%)",
        "Método",
        "Assunto Principal",  # ← INCLUÍDO
        "Texto Original"
      )
      
      write.xlsx(df, file, rowNames = FALSE)
    }
  )
  
  output$download_resultados <- downloadHandler(
    filename = function() {
      paste0("resultados_classificacao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(values$resultados_lote, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  observeEvent(input$limpar_lote, {
    values$resultados_lote <- NULL
    output$progresso_lote <- renderUI({ NULL })
    showNotification("🗑️ Resultados limpos!", type = "message")
  })
  
  #===========================================================================
  # GERENCIAMENTO DE DICIONÁRIOS
  #===========================================================================
  
  observeEvent(input$salvar_config_metodo, {
    
    CONFIG_USUARIO$prioridade <- input$metodo_classificacao
    CONFIG_USUARIO$usar_dicionario <- input$usar_dicionario
    CONFIG_USUARIO$usar_api <- input$usar_api
    
    showNotification(
      "✅ Configurações de método salvas com sucesso!",
      type = "message",
      duration = 3
    )
  })
  
  observeEvent(input$resetar_dicionarios, {
    
    showModal(modalDialog(
      title = "⚠️ Confirmar Restauração",
      "Tem certeza que deseja restaurar todos os dicionários para os valores padrão?",
      "Esta ação não pode ser desfeita.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_reset", "Sim, Restaurar", class = "btn-warning")
      )
    ))
  })
  
  observeEvent(input$confirmar_reset, {
    
    CONFIG_USUARIO$dicionarios <- DICIONARIOS_SAP
    
    for(tipo_num in 1:6) {
      tipo_key <- paste0("tipo_", tipo_num)
      dicionario <- DICIONARIOS_SAP[[tipo_key]]
      
      updateTextAreaInput(session, paste0("desc_tipo_", tipo_num),
                          value = dicionario$descricao)
      updateTextAreaInput(session, paste0("quando_tipo_", tipo_num),
                          value = dicionario$quando_utilizar)
      updateTextAreaInput(session, paste0("palavras_tipo_", tipo_num),
                          value = paste(dicionario$palavras_chave, collapse = "\n"))
    }
    
    removeModal()
    showNotification("✅ Dicionários restaurados para os valores padrão!", type = "message")
  })
  
  for(tipo_num in 1:6) {
    local({
      num <- tipo_num
      tipo_key <- paste0("tipo_", num)
      
      observeEvent(input[[paste0("salvar_tipo_", num)]], {
        
        descricao <- input[[paste0("desc_tipo_", num)]]
        quando <- input[[paste0("quando_tipo_", num)]]
        palavras_texto <- input[[paste0("palavras_tipo_", num)]]
        
        palavras <- strsplit(palavras_texto, "\n")[[1]]
        palavras <- trimws(palavras)
        palavras <- palavras[nchar(palavras) > 0]
        palavras <- tolower(palavras)
        
        CONFIG_USUARIO$dicionarios[[tipo_key]]$descricao <- descricao
        CONFIG_USUARIO$dicionarios[[tipo_key]]$quando_utilizar <- quando
        CONFIG_USUARIO$dicionarios[[tipo_key]]$palavras_chave <- palavras
        
        showNotification(
          paste0("✅ Tipo ", num, " salvo com sucesso! (", length(palavras), " palavras-chave)"),
          type = "message",
          duration = 3
        )
      })
    })
  }
  
  output$tabela_resumo_dicionarios <- DT::renderDataTable({
    
    resumo <- do.call(rbind, lapply(1:6, function(i) {
      tipo_key <- paste0("tipo_", i)
      dic <- CONFIG_USUARIO$dicionarios[[tipo_key]]
      
      data.frame(
        Tipo = i,
        Categoria = dic$categoria_principal,
        Criticidade = dic$criticidade,
        `Qtd Palavras` = length(dic$palavras_chave),
        Descrição = substr(dic$descricao, 1, 50),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
    
    DT::datatable(
      resumo,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Criticidade',
        backgroundColor = styleEqual(
          c('BAIXA', 'MEDIA', 'ALTA', 'CRITICA'),
          c('#4682B4', '#32CD32', '#FF8C00', '#DC143C')
        ),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  #===========================================================================
  # ESTATÍSTICAS - VALUE BOXES
  #===========================================================================
  
  output$metrica_total_classificados <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m)) 0 else m$total
    
    valueBox(
      value = valor,
      subtitle = "Total Classificados",
      icon = icon("clipboard-check"),
      color = "navy"
    )
  })
  
  output$metrica_acuracia <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m)) "N/A" else paste0(round(m$acuracia, 1), "%")
    
    valueBox(
      value = valor,
      subtitle = "Acurácia Geral",
      icon = icon("bullseye"),
      color = "teal"
    )
  })
  
  output$metrica_conformes <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m)) 0 else m$conformes
    
    valueBox(
      value = valor,
      subtitle = "Conformes",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$metrica_divergentes <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m)) 0 else m$divergentes
    
    valueBox(
      value = valor,
      subtitle = "Divergentes",
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  
  #===========================================================================
  # ESTATÍSTICAS - GRÁFICOS
  #===========================================================================
  
  output$matriz_confusao <- renderPlot({
    m <- metricas()
    req(m)
    
    matriz_df <- as.data.frame(m$matriz)
    names(matriz_df) <- c("Anterior", "Novo", "Freq")
    
    ggplot(matriz_df, aes(x = Novo, y = Anterior, fill = Freq)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = Freq), color = "white", size = 6, fontface = "bold") +
      scale_fill_gradient(low = "#e3f2fd", high = "#1976d2") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "right",
        panel.grid = element_blank()
      ) +
      labs(
        title = "",
        x = "Tipo Novo (Classificado)",
        y = "Tipo Anterior (Original)",
        fill = "Quantidade"
      ) +
      coord_fixed()
  })
  
  output$grafico_acuracia_tipo <- renderPlot({
    m <- metricas()
    req(m)
    
    dados <- m$metricas_tipo %>%
      mutate(
        tipo_label = paste0("Tipo ", tipo_intervencao_antigo),
        cor = ifelse(acuracia >= 80, "#2e8b57", 
                     ifelse(acuracia >= 60, "#FF8C00", "#DC143C"))
      )
    
    ggplot(dados, aes(x = reorder(tipo_label, -acuracia), y = acuracia, fill = cor)) +
      geom_col(alpha = 0.8, width = 0.7) +
      geom_text(aes(label = paste0(round(acuracia, 1), "%")), 
                vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_identity() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major.x = element_blank()
      ) +
      labs(
        title = "",
        x = "",
        y = "Acurácia (%)"
      ) +
      ylim(0, 105)
  })
  
  output$grafico_distribuicao_confianca <- renderPlot({
    m <- metricas()
    req(m)
    
    ggplot(m$dados_validos, aes(x = confianca, fill = status_conformidade)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      scale_fill_manual(
        values = c("CONFORME" = "#2e8b57", "DIVERGENTE" = "#ff6b35"),
        name = "Status"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      ) +
      labs(
        title = "",
        x = "Confiança (%)",
        y = "Quantidade"
      )
  })
  
  output$grafico_metodos <- renderPlot({
    m <- metricas()
    req(m)
    
    if("metodo" %in% names(m$dados_validos)) {
      dados_metodo <- m$dados_validos %>%
        count(metodo, name = "total")
      
      ggplot(dados_metodo, aes(x = "", y = total, fill = metodo)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Set2", name = "Método") +
        theme_void() +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 11)
        ) +
        geom_text(aes(label = total), 
                  position = position_stack(vjust = 0.5),
                  size = 5, fontface = "bold", color = "white")
    } else {
      ggplot() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Dados de método não disponíveis",
                 size = 5, color = "#999")
    }
  })
  
  #===========================================================================
  # ESTATÍSTICAS - TABELAS
  #===========================================================================
  
  output$tabela_metricas_tipo <- DT::renderDataTable({
    m <- metricas()
    req(m)
    
    dados <- m$metricas_tipo %>%
      mutate(
        Tipo = paste0("Tipo ", tipo_intervencao_antigo),
        `Total` = total,
        `Conformes` = conformes,
        `Divergentes` = divergentes,
        `Acurácia (%)` = round(acuracia, 1),
        `Confiança Média (%)` = round(confianca_media, 1)
      ) %>%
      select(Tipo, Total, Conformes, Divergentes, `Acurácia (%)`, `Confiança Média (%)`)
    
    DT::datatable(
      dados,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  output$tabela_metricas_categoria <- DT::renderDataTable({
    m <- metricas()
    req(m)
    
    DT::datatable(
      m$metricas_categoria,
      options = list(pageLength = 10),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  output$tabela_metricas_metodo <- DT::renderDataTable({
    m <- metricas()
    req(m)
    
    if(!is.null(m$metricas_metodo)) {
      dados <- m$metricas_metodo %>%
        mutate(
          `Acurácia (%)` = round(acuracia, 1),
          `Confiança Média (%)` = round(confianca_media, 1)
        ) %>%
        select(Método = metodo, Total = total, Conformes = conformes, 
               `Acurácia (%)`, `Confiança Média (%)`)
      
      DT::datatable(
        dados,
        options = list(pageLength = 10),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    } else {
      data.frame(Mensagem = "Dados de método não disponíveis")
    }
  })
  
  output$resultado_teste_api <- renderUI({
    HTML("<div style='padding: 15px; background: #f8f9fa; border-radius: 8px;'>
        <p style='color: #666;'>Clique em 'Testar Conexão' para verificar a API</p>
        </div>")
  })
  
  observeEvent(input$testar_api, {
    
    output$resultado_teste_api <- renderUI({
      HTML("<div style='padding: 15px; background: #fff3cd; border-radius: 8px;'>
          <p style='color: #856404;'>⏳ Testando conexão...</p>
          </div>")
    })
    
    tryCatch({
      
      url <- paste0(
        OPENAI_CONFIG$base_url,
        "/deployments/",
        OPENAI_CONFIG$model,
        "/chat/completions?api-version=",
        OPENAI_CONFIG$api_version
      )
      
      response <- POST(
        url = url,
        add_headers(
          `api-key` = OPENAI_CONFIG$api_key,
          `Content-Type` = "application/json"
        ),
        body = toJSON(list(
          messages = list(
            list(role = "user", content = "Teste de conexão")
          ),
          max_tokens = 10
        ), auto_unbox = TRUE),
        encode = "json",
        timeout(10)
      )
      
      if(status_code(response) == 200) {
        output$resultado_teste_api <- renderUI({
          HTML("<div style='padding: 15px; background: #d4edda; border-radius: 8px; border-left: 4px solid #28a745;'>
              <strong style='color: #155724;'>✅ CONEXÃO OK</strong>
              <p style='color: #155724; margin: 5px 0 0 0;'>API respondeu com sucesso!</p>
              </div>")
        })
      } else {
        output$resultado_teste_api <- renderUI({
          HTML(paste0("<div style='padding: 15px; background: #f8d7da; border-radius: 8px; border-left: 4px solid #dc3545;'>
              <strong style='color: #721c24;'>❌ ERRO HTTP ", status_code(response), "</strong>
              <p style='color: #721c24; margin: 5px 0 0 0;'>", content(response, "text"), "</p>
              </div>"))
        })
      }
      
    }, error = function(e) {
      output$resultado_teste_api <- renderUI({
        HTML(paste0("<div style='padding: 15px; background: #f8d7da; border-radius: 8px; border-left: 4px solid #dc3545;'>
            <strong style='color: #721c24;'>❌ ERRO DE CONEXÃO</strong>
            <p style='color: #721c24; margin: 5px 0 0 0;'>", e$message, "</p>
            </div>"))
      })
    })
  })
  
  
  output$tabela_divergencias_detalhadas <- DT::renderDataTable({
    m <- metricas()
    req(m)
    
    dados <- m$divergencias %>%
      mutate(
        Nota = nota_key,
        `Texto (preview)` = substr(texto_completo, 1, 60),
        `Tipo Anterior` = tipo_intervencao_antigo,
        `Tipo Novo` = tipo_novo,
        Categoria = categoria,
        Criticidade = criticidade,
        `Confiança (%)` = confianca,
        `Resumo` = substr(resumo, 1, 80)
      ) %>%
      select(Nota, `Texto (preview)`, `Tipo Anterior`, `Tipo Novo`, 
             Categoria, Criticidade, `Confiança (%)`, Resumo)
    
    DT::datatable(
      dados,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px"
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      filter = 'top'
    ) %>%
      formatStyle(
        'Criticidade',
        backgroundColor = styleEqual(
          c('BAIXA', 'MEDIA', 'ALTA', 'CRITICA'),
          c('#4682B4', '#32CD32', '#FF8C00', '#DC143C')
        ),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  #===========================================================================
  # CONFIGURAÇÕES API
  #===========================================================================
  
  observeEvent(input$salvar_config, {
    OPENAI_CONFIG$base_url <<- input$config_base_url
    OPENAI_CONFIG$api_key <<- input$config_api_key
    OPENAI_CONFIG$model <<- input$config_model
    OPENAI_CONFIG$api_version <<- input$config_api_version
    
    showNotification("✅ Configurações da API salvas!", type = "message")
  })
  
  output$status_api <- renderText({
    paste(
      "Status: Configurada",
      paste("Base URL:", OPENAI_CONFIG$base_url),
      paste("Modelo:", OPENAI_CONFIG$model),
      paste("API Version:", OPENAI_CONFIG$api_version),
      paste("Chave:", substr(OPENAI_CONFIG$api_key, 1, 12), "..."),
      sep = "\n"
    )
  })
  
  
  #===========================================================================
  # PROCESSAR ASSUNTOS NO PREVIEW
  #===========================================================================
  
  observeEvent(input$processar_assuntos_preview, {
    req(values$dados_cruzados)
    
    withProgress(message = 'Extraindo assuntos principais...', value = 0, {
      
      dados_preview <- head(values$dados_cruzados, 100)
      dados_preview$assunto_principal <- NA_character_
      
      total <- nrow(dados_preview)
      
      for(i in 1:total) {
        
        texto <- dados_preview$texto_completo[i]
        
        if(!is.na(texto) && nchar(trimws(texto)) > 0) {
          assunto <- extrair_assunto_principal(texto)
          dados_preview$assunto_principal[i] <- assunto
        } else {
          dados_preview$assunto_principal[i] <- "Sem texto"
        }
        
        incProgress(1/total, detail = paste("Processando", i, "de", total))
        Sys.sleep(0.1)
      }
      
      dados_com_assuntos(dados_preview)
      
      showNotification("✅ Assuntos extraídos com sucesso!", type = "message", duration = 5)
    })
  })
  
  observeEvent(input$cruzar, {
    dados_com_assuntos(NULL)
  })
  
  #===========================================================================
  # NAVEGAÇÃO NO HISTÓRICO
  #===========================================================================
  
  observeEvent(input$voltar_historico, {
    if(navegar_historico("anterior")) {
      proc <- processamento_atual()
      if(!is.null(proc)) {
        values$resultados_lote <- proc$dados
        showNotification(
          paste("⬅️ Voltou para:", format(proc$timestamp, "%d/%m/%Y %H:%M:%S")),
          type = "message",
          duration = 3
        )
      }
    } else {
      showNotification("⚠️ Não há processamento anterior", type = "warning")
    }
  })
  
  observeEvent(input$avancar_historico, {
    if(navegar_historico("proximo")) {
      proc <- processamento_atual()
      if(!is.null(proc)) {
        values$resultados_lote <- proc$dados
        showNotification(
          paste("➡️ Avançou para:", format(proc$timestamp, "%d/%m/%Y %H:%M:%S")),
          type = "message",
          duration = 3
        )
      }
    } else {
      showNotification("⚠️ Não há próximo processamento", type = "warning")
    }
  })
  
  #===========================================================================
  # INFORMAÇÕES DO HISTÓRICO
  #===========================================================================
  
  output$info_historico <- renderUI({
    
    total <- length(historico$processamentos)
    atual <- historico$indice_atual
    
    if(total == 0) {
      return(HTML(paste0(
        "<div style='padding: 20px; background: #f8f9fa; border-radius: 8px; text-align: center;'>",
        "<h4 style='color: #999;'>📭 Nenhum processamento no histórico</h4>",
        "<p style='color: #666;'>Execute uma classificação em lote para começar.</p>",
        "</div>"
      )))
    }
    
    proc_atual <- processamento_atual()
    
    if(is.null(proc_atual)) {
      return(HTML("<p>Erro ao carregar processamento</p>"))
    }
    
    HTML(paste0(
      "<div style='padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 10px; color: white; box-shadow: 0 4px 10px rgba(0,0,0,0.2);'>",
      
      "<div style='display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;'>",
      "<h3 style='margin: 0;'>📍 Processamento ", atual, " de ", total, "</h3>",
      "<span style='background: rgba(255,255,255,0.2); padding: 8px 15px; border-radius: 20px; font-size: 14px;'>",
      "ID: ", proc_atual$id, "</span>",
      "</div>",
      
      "<div style='display: grid; grid-template-columns: repeat(4, 1fr); gap: 15px; margin-top: 20px;'>",
      
      "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; text-align: center;'>",
      "<div style='font-size: 12px; opacity: 0.9; margin-bottom: 5px;'>TOTAL</div>",
      "<div style='font-size: 28px; font-weight: bold;'>", proc_atual$metricas$total, "</div>",
      "</div>",
      
      "<div style='background: rgba(46, 139, 87, 0.3); padding: 15px; border-radius: 8px; text-align: center;'>",
      "<div style='font-size: 12px; opacity: 0.9; margin-bottom: 5px;'>CONFORMES</div>",
      "<div style='font-size: 28px; font-weight: bold;'>", proc_atual$metricas$conformes, "</div>",
      "</div>",
      
      "<div style='background: rgba(255, 107, 53, 0.3); padding: 15px; border-radius: 8px; text-align: center;'>",
      "<div style='font-size: 12px; opacity: 0.9; margin-bottom: 5px;'>DIVERGENTES</div>",
      "<div style='font-size: 28px; font-weight: bold;'>", proc_atual$metricas$divergentes, "</div>",
      "</div>",
      
      "<div style='background: rgba(255, 215, 0, 0.3); padding: 15px; border-radius: 8px; text-align: center;'>",
      "<div style='font-size: 12px; opacity: 0.9; margin-bottom: 5px;'>ACURÁCIA</div>",
      "<div style='font-size: 28px; font-weight: bold;'>", proc_atual$metricas$acuracia, "%</div>",
      "</div>",
      
      "</div>",
      
      "<div style='margin-top: 15px; padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.2);'>",
      "<p style='margin: 5px 0; font-size: 13px;'><strong>🕒 Data/Hora:</strong> ", 
      format(proc_atual$timestamp, "%d/%m/%Y às %H:%M:%S"), "</p>",
      "<p style='margin: 5px 0; font-size: 13px;'><strong>⚙️ Método:</strong> ", 
      proc_atual$metadados$metodo, "</p>",
      "<p style='margin: 5px 0; font-size: 13px;'><strong>📊 Confiança Média:</strong> ", 
      proc_atual$metricas$confianca_media, "%</p>",
      "</div>",
      
      "</div>"
    ))
  })
  
  #===========================================================================
  # GRÁFICO DE EVOLUÇÃO
  #===========================================================================
  
  output$grafico_evolucao_metricas <- renderPlot({
    
    req(length(historico$processamentos) > 0)
    
    dados_evolucao <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i) {
      proc <- historico$processamentos[[i]]
      data.frame(
        indice = i,
        timestamp = proc$timestamp,
        acuracia = proc$metricas$acuracia,
        confianca = proc$metricas$confianca_media,
        conformes = proc$metricas$conformes,
        divergentes = proc$metricas$divergentes,
        atual = (i == historico$indice_atual)
      )
    }))
    
    ggplot(dados_evolucao, aes(x = indice)) +
      geom_line(aes(y = acuracia, color = "Acurácia"), size = 1.5) +
      geom_line(aes(y = confianca, color = "Confiança"), size = 1.5, linetype = "dashed") +
      geom_point(aes(y = acuracia, size = atual), color = "#1f4e79") +
      geom_point(aes(y = confianca, size = atual), color = "#2e8b57") +
      scale_color_manual(values = c("Acurácia" = "#1f4e79", "Confiança" = "#2e8b57")) +
      scale_size_manual(values = c("TRUE" = 5, "FALSE" = 3), guide = "none") +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_text(size = 12, face = "bold")
      ) +
      labs(
        title = "",
        x = "Número do Processamento",
        y = "Percentual (%)"
      ) +
      ylim(0, 100)
  })
  
  #===========================================================================
  # TABELA DE HISTÓRICO
  #===========================================================================
  output$tabela_historico <- DT::renderDataTable({
    
    req(length(historico$processamentos) > 0)
    
    # Construir dados da tabela
    dados_tabela <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i) {
      proc <- historico$processamentos[[i]]
      
      data.frame(
        Numero = i,
        DataHora = format(proc$timestamp, "%d/%m/%Y %H:%M:%S"),
        ID = proc$id,
        Metodo = proc$metadados$metodo,
        Total = proc$metricas$total,
        Conformes = proc$metricas$conformes,
        Divergentes = proc$metricas$divergentes,
        Acuracia = proc$metricas$acuracia,
        Confianca = proc$metricas$confianca_media,
        Atual = ifelse(i == historico$indice_atual, "✓", ""),
        stringsAsFactors = FALSE
      )
    }))
    
    # Renomear colunas para exibição
    colnames(dados_tabela) <- c(
      "#", 
      "Data/Hora", 
      "ID", 
      "Método", 
      "Total", 
      "Conformes", 
      "Divergentes", 
      "Acurácia (%)", 
      "Confiança (%)", 
      "Atual"
    )
    
    # Criar DataTable
    dt <- DT::datatable(
      dados_tabela,
      options = list(
        pageLength = 10,
        order = list(list(0, 'desc')),  # Ordenar por número decrescente
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = '📋 Copiar'),
          list(extend = 'csv', text = '📊 CSV'),
          list(extend = 'excel', text = '📗 Excel')
        ),
        language = list(
          search = "🔍 Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ processamentos",
          infoEmpty = "Nenhum processamento disponível",
          paginate = list(
            first = "Primeiro",
            last = "Último",
            `next` = "Próximo",
            previous = "Anterior"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover',
      extensions = 'Buttons',
      escape = FALSE
    )
    
    # Aplicar estilos
    
    # Estilo para coluna "Atual" (destacar processamento ativo)
    dt <- dt %>%
      formatStyle(
        'Atual',
        target = 'row',
        backgroundColor = styleEqual('✓', '#e3f2fd'),
        fontWeight = styleEqual('✓', 'bold')
      )
    
    # Estilo específico para a célula "Atual"
    dt <- dt %>%
      formatStyle(
        'Atual',
        backgroundColor = styleEqual('✓', '#2e8b57'),
        color = styleEqual('✓', 'white'),
        fontWeight = 'bold',
        textAlign = 'center',
        fontSize = '18px'
      )
    
    # Estilo para Acurácia com barra de progresso
    dt <- dt %>%
      formatStyle(
        'Acurácia (%)',
        background = styleColorBar(c(0, 100), '#1f4e79'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      )
    
    # Estilo para Confiança com barra de progresso
    dt <- dt %>%
      formatStyle(
        'Confiança (%)',
        background = styleColorBar(c(0, 100), '#2e8b57'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      )
    
    # Estilo para Método
    dt <- dt %>%
      formatStyle(
        'Método',
        backgroundColor = styleEqual(
          c('DICIONARIO', 'API', 'HIBRIDO'),
          c('#6c757d', '#007bff', '#17a2b8')
        ),
        color = 'white',
        fontWeight = 'bold',
        textAlign = 'center'
      )
    
    # Estilo para números (Total, Conformes, Divergentes)
    dt <- dt %>%
      formatStyle(
        c('Total', 'Conformes', 'Divergentes'),
        fontWeight = 'bold',
        textAlign = 'center'
      )
    
    # Estilo para Conformes (verde)
    dt <- dt %>%
      formatStyle(
        'Conformes',
        color = '#2e8b57',
        fontWeight = 'bold'
      )
    
    # Estilo para Divergentes (laranja)
    dt <- dt %>%
      formatStyle(
        'Divergentes',
        color = '#ff6b35',
        fontWeight = 'bold'
      )
    
    # Estilo para coluna # (número)
    dt <- dt %>%
      formatStyle(
        '#',
        backgroundColor = '#f8f9fa',
        fontWeight = 'bold',
        textAlign = 'center',
        width = '50px'
      )
    
    # Estilo para Data/Hora
    dt <- dt %>%
      formatStyle(
        'Data/Hora',
        fontFamily = 'monospace',
        fontSize = '12px'
      )
    
    # Estilo para ID
    dt <- dt %>%
      formatStyle(
        'ID',
        fontFamily = 'monospace',
        fontSize = '11px',
        color = '#666'
      )
    
    return(dt)
  })
  
  
  #===========================================================================
  # SALVAR/CARREGAR SESSÃO
  #===========================================================================
  
  output$download_sessao <- downloadHandler(
    filename = function() {
      paste0("sessao_", historico$sessao_id, ".rds")
    },
    content = function(file) {
      saveRDS(list(
        processamentos = historico$processamentos,
        indice_atual = historico$indice_atual,
        sessao_id = historico$sessao_id,
        data_salvo = Sys.time()
      ), file)
    }
  )
  
  observeEvent(input$salvar_sessao, {
    showModal(modalDialog(
      title = "💾 Salvar Sessão",
      "Deseja salvar todo o histórico de processamentos?",
      footer = tagList(
        modalButton("Cancelar"),
        downloadButton("download_sessao", "Salvar", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$carregar_sessao, {
    showModal(modalDialog(
      title = "📂 Carregar Sessão",
      fileInput("arquivo_sessao", "Selecione o arquivo de sessão (.rds)", accept = ".rds"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_carregar_sessao", "Carregar", class = "btn-info")
      )
    ))
  })
  
  observeEvent(input$confirmar_carregar_sessao, {
    req(input$arquivo_sessao)
    
    tryCatch({
      sessao <- readRDS(input$arquivo_sessao$datapath)
      
      historico$processamentos <- sessao$processamentos
      historico$indice_atual <- sessao$indice_atual
      
      proc_atual <- processamento_atual()
      if(!is.null(proc_atual)) {
        values$resultados_lote <- proc_atual$dados
      }
      
      removeModal()
      showNotification(
        paste("✅ Sessão carregada:", length(sessao$processamentos), "processamentos"),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(paste("❌ Erro ao carregar:", e$message), type = "error")
    })
  })
  
  observeEvent(input$limpar_historico, {
    showModal(modalDialog(
      title = "⚠️ Confirmar Limpeza",
      "Tem certeza que deseja limpar TODO o histórico?",
      "Esta ação não pode ser desfeita!",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_limpar_historico", "Sim, Limpar", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmar_limpar_historico, {
    historico$processamentos <- list()
    historico$indice_atual <- 0
    values$resultados_lote <- NULL
    
    removeModal()
    showNotification("🗑️ Histórico limpo!", type = "warning", duration = 3)
  })
  
  #===========================================================================
  # DETALHES DO PROCESSAMENTO ATUAL
  #===========================================================================
  
  output$detalhes_processamento_atual <- renderUI({
    
    proc <- processamento_atual()
    
    if(is.null(proc)) {
      return(HTML("<p style='text-align: center; color: #999;'>Nenhum processamento selecionado</p>"))
    }
    
    HTML(paste0(
      "<div style='padding: 15px;'>",
      
      "<h4 style='color: #1f4e79; margin-bottom: 15px;'>📋 Informações Detalhadas</h4>",
      
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr style='border-bottom: 1px solid #e0e0e0;'>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>ID:</td>",
      "<td style='padding: 10px;'>", proc$id, "</td>",
      "</tr>",
      
      "<tr style='border-bottom: 1px solid #e0e0e0;'>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>Data/Hora:</td>",
      "<td style='padding: 10px;'>", format(proc$timestamp, "%d/%m/%Y %H:%M:%S"), "</td>",
      "</tr>",
      
      "<tr style='border-bottom: 1px solid #e0e0e0;'>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>Método:</td>",
      "<td style='padding: 10px;'>", proc$metadados$metodo, "</td>",
      "</tr>",
      
      "<tr style='border-bottom: 1px solid #e0e0e0;'>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>Extrair Assunto:</td>",
      "<td style='padding: 10px;'>", ifelse(proc$metadados$extrair_assunto, "✓ Sim", "✗ Não"), "</td>",
      "</tr>",
      
      "<tr style='border-bottom: 1px solid #e0e0e0;'>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>Usar Dicionário:</td>",
      "<td style='padding: 10px;'>", ifelse(proc$metadados$usar_dicionario, "✓ Sim", "✗ Não"), "</td>",
      "</tr>",
      
      "<tr style='border-bottom: 1px solid #e0e0e0;'>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>Usar API:</td>",
      "<td style='padding: 10px;'>", ifelse(proc$metadados$usar_api, "✓ Sim", "✗ Não"), "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 10px; font-weight: bold; color: #666;'>Total de Linhas:</td>",
      "<td style='padding: 10px;'>", format(proc$metadados$total_linhas, big.mark = ","), "</td>",
      "</tr>",
      
      "</table>",
      
      "</div>"
    ))
  })
  
} # Fim do servidor

#=============================================================================
# EXECUTAR APLICAÇÃO
#=============================================================================

# Fecha dashboardBody
# Fecha dashboardPage

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("           SISTEMA SAP PETROBRAS - VERSÃO COMPLETA COM EXTRAÇÃO            \n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("📅 Inicializado em:", format(Sys.time(), "%d/%m/%Y às %H:%M:%S"), "\n\n")
cat("📋 FUNCIONALIDADES DISPONÍVEIS:\n")
cat("  📁 Upload de arquivos CSV/Excel\n")
cat("  🔗 Cruzamento automático por número de nota\n")
cat("  🤖 Classificação SAP individual com IA\n")
cat("  📝 Extração de Assunto Principal (NOVO)\n")
cat("  📦 Classificação em lote (batch processing)\n")
cat("  📚 Dicionários personalizáveis (6 tipos)\n")
cat("  🔀 Modo Híbrido (Dicionário + API)\n")
cat("  💬 Resumo executivo da análise\n")
cat("  📊 Dashboard interativo com métricas\n")
cat("  📈 Estatísticas e matriz de confusão\n")
cat("  📋 Comparação Tipo Antigo vs Tipo Novo\n")
cat("  ⚖️ Status de Conformidade\n")
cat("  📥 Download de resultados em CSV\n\n")
cat("🔑 API OPENAI PETROBRAS:\n")
cat("  Base URL:", OPENAI_CONFIG$base_url, "\n")
cat("  Modelo:", OPENAI_CONFIG$model, "\n")
cat("  API Version:", OPENAI_CONFIG$api_version, "\n\n")
cat("✅ Sistema pronto para uso!\n\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

shinyApp(ui = ui, server = server)

