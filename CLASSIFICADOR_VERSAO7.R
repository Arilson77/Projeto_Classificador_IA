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
# FUNÇÃO AUXILIAR: CRIAR CARD MODERNO PARA CADA TIPO
#==============================================================================

#==============================================================================
# FUNÇÃO AUXILIAR CORRIGIDA: CRIAR CARD MODERNO PARA CADA TIPO
#==============================================================================

criar_card_tipo_dicionario <- function(tipo_num, cor, criticidade) {
  
  tipo_key <- paste0("tipo_", tipo_num)
  dicionario <- DICIONARIOS_SAP[[tipo_key]]
  
  # Retornar tagList ao invés de tags$div
  tagList(
    # Header do Card
    div(
      style = paste0(
        "background: linear-gradient(135deg, ", cor, " 0%, ", 
        adjustcolor(cor, alpha.f = 0.7), " 100%); ",
        "padding: 25px; ",
        "border-radius: 12px 12px 0 0; ",
        "margin: -15px -15px 0 -15px; ",
        "box-shadow: 0 4px 10px rgba(0,0,0,0.1);"
      ),
      
      fluidRow(
        column(
          width = 8,
          h2(
            style = "color: white; margin: 0; font-weight: 300;",
            icon("clipboard-list"),
            paste("Tipo", tipo_num, "-", dicionario$categoria_principal)
          ),
          p(
            style = "color: rgba(255,255,255,0.95); margin: 10px 0 0 0; font-size: 15px;",
            dicionario$descricao
          )
        ),
        column(
          width = 4,
          div(
            style = "text-align: right;",
            div(
              style = paste0(
                "display: inline-block; ",
                "background: rgba(255,255,255,0.3); ",
                "padding: 12px 25px; ",
                "border-radius: 25px; ",
                "border: 2px solid white;"
              ),
              h3(
                style = "color: white; margin: 0; font-weight: bold;",
                criticidade
              ),
              p(
                style = "color: white; margin: 5px 0 0 0; font-size: 12px;",
                "CRITICIDADE"
              )
            )
          )
        )
      )
    ),
    
    # Corpo do Card
    div(
      style = "padding: 30px 20px;",
      
      fluidRow(
        # Coluna Esquerda
        column(
          width = 6,
          
          # Descrição
          div(
            style = "margin-bottom: 25px;",
            div(
              style = "display: flex; align-items: center; margin-bottom: 10px;",
              icon("file-alt", style = paste0("font-size: 24px; color: ", cor, "; margin-right: 10px;")),
              h4(style = "margin: 0; color: #333;", "Descrição SAP")
            ),
            textAreaInput(
              paste0("desc_tipo_", tipo_num),
              label = NULL,
              value = dicionario$descricao,
              rows = 4,
              width = "100%",
              placeholder = "Descreva o tipo de intervenção..."
            )
          ),
          
          # Quando Utilizar
          div(
            div(
              style = "display: flex; align-items: center; margin-bottom: 10px;",
              icon("lightbulb", style = paste0("font-size: 24px; color: ", cor, "; margin-right: 10px;")),
              h4(style = "margin: 0; color: #333;", "Quando Utilizar")
            ),
            textAreaInput(
              paste0("quando_tipo_", tipo_num),
              label = NULL,
              value = dicionario$quando_utilizar,
              rows = 5,
              width = "100%",
              placeholder = "Descreva os critérios de uso..."
            )
          )
        ),
        
        # Coluna Direita
        column(
          width = 6,
          
          div(
            style = paste0(
              "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); ",
              "padding: 20px; ",
              "border-radius: 10px; ",
              "border-left: 5px solid ", cor, "; ",
              "height: 100%;"
            ),
            
            div(
              style = "display: flex; align-items: center; margin-bottom: 15px;",
              icon("tags", style = paste0("font-size: 24px; color: ", cor, "; margin-right: 10px;")),
              h4(style = "margin: 0; color: #333;", "Palavras-Chave")
            ),
            
            p(
              style = "color: #666; font-size: 13px; margin-bottom: 10px;",
              icon("info-circle"),
              " Digite uma palavra-chave por linha."
            ),
            
            textAreaInput(
              paste0("palavras_tipo_", tipo_num),
              label = NULL,
              value = paste(dicionario$palavras_chave, collapse = "\n"),
              rows = 12,
              width = "100%",
              placeholder = "Digite uma palavra por linha..."
            ),
            
            div(
              style = "background: white; padding: 10px; border-radius: 8px; margin-top: 10px;",
              p(
                style = "margin: 0; color: #666; font-size: 12px;",
                icon("chart-bar"),
                " Total de palavras: ",
                tags$strong(
                  style = paste0("color: ", cor, ";"),
                  length(dicionario$palavras_chave)
                )
              )
            )
          )
        )
      ),
      
      # Botão de Salvar
      hr(style = "margin: 30px 0; border-color: rgba(0,0,0,0.1);"),
      
      div(
        style = "text-align: center;",
        actionButton(
          paste0("salvar_tipo_", tipo_num),
          label = tagList(
            icon("save"),
            paste("Salvar Tipo", tipo_num)
          ),
          class = "btn-lg",
          style = paste0(
            "background: ", cor, "; ",
            "color: white; ",
            "border: none; ",
            "padding: 12px 40px; ",
            "border-radius: 25px; ",
            "font-weight: bold; ",
            "box-shadow: 0 4px 10px rgba(0,0,0,0.15);"
          )
        )
      )
    )
  ) # Fecha tagList
}


#=============================================================================
# FUNÇÃO: CRUZAMENTO DE DADOS
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
cat("              SISTEMA SAP PETROBRAS - CARREGANDO INTERFACE...              \n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("\n")

#=============================================================================
# INTERFACE DO USUÁRIO (UI COMPLETO)
#=============================================================================
ui <- dashboardPage(
  
  skin = "blue",
  
  #===========================================================================
  # CABEÇALHO
  #===========================================================================
  
  # Header
  dashboardHeader(
    title = span(
      icon("industry", style = "margin-right: 10px;"),
      "Sistema SAP Petrobras"
    ),
    titleWidth = 300,
    
    # CSS CUSTOMIZADO
    tags$head(
      tags$style(HTML("
        /* Estilo global */
        body {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        
        /* Pills das tabs */
        .nav-pills > li > a {
          border-radius: 25px;
          margin: 0 5px;
          transition: all 0.3s;
          font-weight: 500;
        }
        
        .nav-pills > li.active > a,
        .nav-pills > li.active > a:hover,
        .nav-pills > li.active > a:focus {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          box-shadow: 0 4px 10px rgba(0,0,0,0.2);
        }
        
        /* Inputs modernos */
        .form-control {
          border-radius: 8px;
          border: 2px solid #e9ecef;
          transition: all 0.3s;
        }
        
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
        }
        
        /* Botões com hover */
        .btn {
          transition: all 0.3s;
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.2);
        }
        
        /* Boxes */
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        }
        
        .box-header {
          border-radius: 10px 10px 0 0;
        }
        
        /* Checkbox customizado */
        input[type='checkbox'] {
          width: 30px;
          height: 30px;
          cursor: pointer;
        }
        
        /* Scrollbar customizada */
        ::-webkit-scrollbar {
          width: 10px;
          height: 10px;
        }
        
        ::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 10px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border-radius: 10px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: #764ba2;
        }
        
        /* Animações */
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .box {
          animation: fadeIn 0.5s ease-out;
        }
      "))
    )
  ),
  
  #===========================================================================
  # BARRA LATERAL (MENU)
  #===========================================================================
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "menu_principal",
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("📁 Upload e Cruzamento", tabName = "upload", icon = icon("upload")),
      menuItem("🤖 Classificação Individual", tabName = "individual", icon = icon("robot")),
      menuItem("📦 Classificação em Lote", tabName = "lote", icon = icon("list")),
      menuItem("📚 Dicionários", tabName = "dicionarios", icon = icon("book")),
      menuItem("📈 Estatísticas", tabName = "estatisticas", icon = icon("chart-line")),
      menuItem("⚙️ Configurações API", tabName = "config", icon = icon("cog")),
      menuItem("📜 Histórico", tabName = "historico", icon = icon("history")),
      menuItem("📖 Documentação", tabName = "documentacao", icon = icon("book"))
      
    )
  ),
  
  #===========================================================================
  # CORPO PRINCIPAL
  #===========================================================================
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f4f4; }
        .btn-primary { background-color: #1f4e79; border-color: #1f4e79; }
        .btn-green { background-color: #2e8b57; border-color: #2e8b57; }
        .btn-warning { background-color: #FF8C00; border-color: #FF8C00; }
        .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .small-box { border-radius: 8px; }
        .progress-bar { background-color: #1f4e79; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #1f4e79; }
      "))
    ),
    
    tabItems(
      
      #=======================================================================
      # ABA 1: DASHBOARD
      #=======================================================================
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                valueBoxOutput("total_textos", width = 3),
                valueBoxOutput("textos_iazf", width = 3),
                valueBoxOutput("precisao", width = 3),
                valueBoxOutput("taxa_conformidade", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "📊 Comparação: Tipo Anterior vs Tipo Novo",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_comparacao_antes_depois", height = "350px")
                ),
                
                box(
                  title = "📈 Distribuição por Tipos de Intervenção",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_distribuicao_tipos", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "🎯 Distribuição por Hierarquia",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_hierarquia", height = "350px")
                ),
                
                box(
                  title = "⚖️ Status de Conformidade",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_conformidade", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Últimas Classificações",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("tabela_recentes")
                )
              )
      ),
      
      #=======================================================================
      # ABA 2: UPLOAD E CRUZAMENTO
      #=======================================================================
      
      tabItem(tabName = "upload",
              
              fluidRow(
                box(
                  title = "📁 Upload de Arquivos",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  
                  h4("Arquivo 1: Ordens"),
                  fileInput("arquivo_ordens",
                            "Selecione o arquivo de ORDENS (CSV/Excel):",
                            accept = c(".csv", ".xlsx", ".xls")),
                  
                  h4("Arquivo 2: Textos"),
                  fileInput("arquivo_textos",
                            "Selecione o arquivo de TEXTOS (CSV/Excel):",
                            accept = c(".csv", ".xlsx", ".xls")),
                  
                  actionButton("cruzar",
                               "🔗 Cruzar Dados",
                               class = "btn-primary btn-block",
                               style = "margin-top: 15px;")
                ),
                
                box(
                  title = "📊 Status do Cruzamento",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  
                  htmlOutput("status_cruzamento"),
                  
                  br(),
                  
                  conditionalPanel(
                    condition = "output.cruzamento_concluido",
                    downloadButton("download_cruzado",
                                   "📥 Download Dados Cruzados",
                                   class = "btn-green btn-block")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "👁️ Preview dos Dados Cruzados",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  htmlOutput("estatisticas_cruzados"),
                  conditionalPanel(
                    condition = "output.cruzamento_concluido",
                    actionButton("processar_assuntos_preview",
                                 "📝 Extrair Assuntos (Preview)",
                                 class = "btn-info",
                                 style = "margin-bottom: 15px;")
                  ),
                  htmlOutput("info_preview"),
                  DT::dataTableOutput("preview_cruzado")
                )
              )
      ),
      
      #=======================================================================
      # ABA 3: CLASSIFICAÇÃO INDIVIDUAL
      #=======================================================================
      
      tabItem(tabName = "individual",
              
              fluidRow(
                box(
                  title = "🤖 Classificação Individual com Comparação",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  
                  textAreaInput("texto_individual",
                                "Digite ou cole o texto para análise:",
                                height = "150px",
                                placeholder = "Exemplo: Executar manutenção preventiva da bomba P-101..."),
                  
                  fluidRow(
                    column(6,
                           numericInput("tipo_anterior",
                                        "Tipo de Intervenção Anterior (opcional):",
                                        value = NA,
                                        min = 1,
                                        max = 6,
                                        step = 1)
                    ),
                    column(6,
                           selectInput("nota_referencia",
                                       "Ou selecione uma Nota dos dados carregados:",
                                       choices = NULL)
                    )
                  ),
                  
                  fluidRow(
                    column(4,
                           actionButton("classificar_individual",
                                        "🚀 Classificar",
                                        class = "btn-primary btn-block")
                    ),
                    column(4,
                           actionButton("extrair_assunto_individual",
                                        "📝 Extrair Assunto",
                                        class = "btn-info btn-block")
                    ),
                    column(4,
                           actionButton("limpar_individual",
                                        "🗑️ Limpar",
                                        class = "btn-secondary btn-block")
                    )
                  ),
                  
                  br(),
                  htmlOutput("assunto_extraido"),
                  htmlOutput("resultado_individual")
                ),
                
                box(
                  title = "ℹ️ Informações SAP",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  
                  h5("Tipos SAP:"),
                  tags$ul(
                    tags$li(tags$strong("1."), " Condicionamento, limpeza"),
                    tags$li(tags$strong("2."), " Melhorias, modificações"),
                    tags$li(tags$strong("3."), " Manutenção preventiva"),
                    tags$li(tags$strong("4."), " Manutenção por oportunidade"),
                    tags$li(tags$strong("5."), " Eliminação de defeito"),
                    tags$li(tags$strong("6."), " Eliminação de falha")
                  ),
                  
                  hr(),
                  
                  h5("Hierarquia:"),
                  tags$p(tags$strong("PROBLEMAS_COMUNS:"), "Tipos 1-4"),
                  tags$p(tags$strong("IAZF:"), "Tipos 5-6"),
                  
                  hr(),
                  
                  h5("Criticidade:"),
                  tags$div(
                    tags$span("BAIXA", style = "background: #4682B4; color: white; padding: 3px 8px; border-radius: 3px; margin: 2px;"),
                    tags$br(),
                    tags$span("MÉDIA", style = "background: #32CD32; color: white; padding: 3px 8px; border-radius: 3px; margin: 2px;"),
                    tags$br(),
                    tags$span("ALTA", style = "background: #FF8C00; color: white; padding: 3px 8px; border-radius: 3px; margin: 2px;"),
                    tags$br(),
                    tags$span("CRÍTICA", style = "background: #DC143C; color: white; padding: 3px 8px; border-radius: 3px; margin: 2px;")
                  )
                )
              )
      ),
      
      #=======================================================================
      # ABA 4: CLASSIFICAÇÃO EM LOTE
      #=======================================================================
      
      tabItem(tabName = "lote",
              
              fluidRow(
                box(
                  title = "📦 Classificação em Lote",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  p("Classifique automaticamente todos os textos do arquivo cruzado usando IA e/ou Dicionário."),
                  
                  conditionalPanel(
                    condition = "output.cruzamento_concluido",
                    
                    fluidRow(
                      column(12,
                             checkboxInput("extrair_assunto", 
                                           "📝 Extrair Assunto Principal usando IA (recomendado)", 
                                           value = TRUE),
                             p(style = "font-size: 12px; color: #666; margin-top: -10px;",
                               "A IA irá gerar um resumo conciso do assunto principal de cada texto (máx. 80 caracteres).")
                      )
                    ),
                    
                    hr(),
                    
                    fluidRow(
                      column(4,
                             actionButton("classificar_lote",
                                          "🚀 Iniciar Classificação em Lote",
                                          class = "btn-primary btn-block")
                      ),
                      column(4,
                             downloadButton("download_resultados",
                                            "📥 Download Resultados",
                                            class = "btn-green btn-block")
                      ),
                      column(4,
                             actionButton("limpar_lote",
                                          "🗑️ Limpar Resultados",
                                          class = "btn-secondary btn-block")
                      )
                    ),
                    
                    br(),
                    
                    htmlOutput("progresso_lote"),
                    
                    br(),
                    
                    h4("📋 Resultados da Classificação em Lote:"),
                    DT::dataTableOutput("tabela_resultados_lote")
                  ),
                  
                  conditionalPanel(
                    condition = "!output.cruzamento_concluido",
                    div(
                      style = "text-align: center; padding: 50px; color: #999;",
                      icon("info-circle", style = "font-size: 48px; margin-bottom: 15px;"),
                      h4("Faça o upload e cruzamento dos arquivos primeiro!"),
                      p("Vá para a aba 'Upload e Cruzamento' para começar.")
                    )
                  )
                )
              )
      ),
      
      #=======================================================================
      # ABA 5: DICIONÁRIOS - VERSÃO DEFINITIVA FUNCIONAL
      #=======================================================================
      
      tabItem(
        tabName = "dicionarios",
        
        # CSS customizado para os gradientes
        tags$head(
          tags$style(HTML("
      /* Header principal */
      .header-dicionarios {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        padding: 30px;
        border-radius: 15px;
        margin-bottom: 25px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
        color: white;
      }
      
      /* Cards de configuração */
      .card-metodo {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        padding: 25px;
        border-radius: 12px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        text-align: center;
        color: white;
      }
      
      .card-dicionario {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        padding: 25px;
        border-radius: 12px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        text-align: center;
        color: white;
      }
      
      .card-api {
        background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
        padding: 25px;
        border-radius: 12px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        text-align: center;
        color: white;
      }
      
      /* Headers dos tipos */
      .header-tipo-1 {
        background: linear-gradient(135deg, #87CEEB 0%, rgba(135, 206, 235, 0.7) 100%);
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        color: white;
      }
      
      .header-tipo-2 {
        background: linear-gradient(135deg, #90EE90 0%, rgba(144, 238, 144, 0.7) 100%);
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        color: white;
      }
      
      .header-tipo-3 {
        background: linear-gradient(135deg, #FFD700 0%, rgba(255, 215, 0, 0.7) 100%);
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        color: white;
      }
      
      .header-tipo-4 {
        background: linear-gradient(135deg, #FFA500 0%, rgba(255, 165, 0, 0.7) 100%);
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        color: white;
      }
      
      .header-tipo-5 {
        background: linear-gradient(135deg, #FF6347 0%, rgba(255, 99, 71, 0.7) 100%);
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        color: white;
      }
      
      .header-tipo-6 {
        background: linear-gradient(135deg, #DC143C 0%, rgba(220, 20, 60, 0.7) 100%);
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        color: white;
      }
      
      /* Botões personalizados */
      .btn-tipo-1 {
        background: #87CEEB;
        color: white;
        border: none;
        padding: 12px 40px;
        border-radius: 25px;
        font-weight: bold;
      }
      
      .btn-tipo-2 {
        background: #90EE90;
        color: white;
        border: none;
        padding: 12px 40px;
        border-radius: 25px;
        font-weight: bold;
      }
      
      .btn-tipo-3 {
        background: #FFD700;
        color: white;
        border: none;
        padding: 12px 40px;
        border-radius: 25px;
        font-weight: bold;
      }
      
      .btn-tipo-4 {
        background: #FFA500;
        color: white;
        border: none;
        padding: 12px 40px;
        border-radius: 25px;
        font-weight: bold;
      }
      
      .btn-tipo-5 {
        background: #FF6347;
        color: white;
        border: none;
        padding: 12px 40px;
        border-radius: 25px;
        font-weight: bold;
      }
      
      .btn-tipo-6 {
        background: #DC143C;
        color: white;
        border: none;
        padding: 12px 40px;
        border-radius: 25px;
        font-weight: bold;
      }
      
      .card-info-box {
        background: rgba(255,255,255,0.2);
        padding: 10px;
        border-radius: 8px;
        margin-top: 15px;
      }
    "))
        ),
        
        # Header principal
        fluidRow(
          column(
            width = 12,
            div(
              class = "header-dicionarios",
              fluidRow(
                column(
                  width = 8,
                  h2(
                    style = "margin: 0; font-weight: 300;",
                    icon("book"), " Gerenciamento de Dicionários SAP"
                  ),
                  p(
                    style = "margin: 10px 0 0 0; opacity: 0.9;",
                    "Configure palavras-chave, descrições e critérios para cada tipo de intervenção"
                  )
                ),
                column(
                  width = 4,
                  div(
                    style = "text-align: right;",
                    actionButton(
                      "resetar_dicionarios",
                      label = tagList(icon("undo"), "Restaurar Padrão"),
                      style = "background: rgba(255,255,255,0.2); color: white; border: 2px solid white; border-radius: 25px; padding: 12px 30px;"
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Configurações de Método
        fluidRow(
          column(
            width = 12,
            box(
              title = tagList(icon("cogs"), "Configurações de Classificação"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              
              fluidRow(
                # Card Método
                column(
                  width = 4,
                  div(
                    class = "card-metodo",
                    icon("route", style = "font-size: 48px; margin-bottom: 15px;"),
                    selectInput(
                      "metodo_classificacao",
                      label = h4("Método de Classificação", style = "color: white;"),
                      choices = c(
                        "Híbrido (Recomendado)" = "HIBRIDO",
                        "Apenas Dicionário" = "DICIONARIO",
                        "Apenas API" = "API"
                      ),
                      selected = "HIBRIDO"
                    ),
                    p(style = "opacity: 0.9; font-size: 13px;", "Híbrido combina o melhor dos dois métodos")
                  )
                ),
                
                # Card Dicionário
                column(
                  width = 4,
                  div(
                    class = "card-dicionario",
                    icon("book-open", style = "font-size: 48px; margin-bottom: 15px;"),
                    h4("Usar Dicionário", style = "color: white;"),
                    checkboxInput("usar_dicionario", label = NULL, value = TRUE),
                    div(
                      class = "card-info-box",
                      p(style = "margin: 0; font-size: 13px;", HTML("✓ Rápido e eficiente<br>✓ Baseado em palavras-chave"))
                    )
                  )
                ),
                
                # Card API
                column(
                  width = 4,
                  div(
                    class = "card-api",
                    icon("robot", style = "font-size: 48px; margin-bottom: 15px;"),
                    h4("Usar IA (API)", style = "color: white;"),
                    checkboxInput("usar_api", label = NULL, value = TRUE),
                    div(
                      class = "card-info-box",
                      p(style = "margin: 0; font-size: 13px;", HTML("✓ Análise contextual<br>✓ Alta precisão"))
                    )
                  )
                )
              ),
              
              hr(),
              
              div(
                style = "text-align: center;",
                actionButton(
                  "salvar_config_metodo",
                  label = tagList(icon("save"), "Salvar Configurações"),
                  class = "btn-success btn-lg",
                  style = "padding: 12px 40px; border-radius: 25px; font-weight: bold;"
                )
              )
            )
          )
        ),
        
        # Abas dos Tipos
        fluidRow(
          column(
            width = 12,
            box(
              title = tagList(icon("edit"), "Editar Dicionários por Tipo"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              
              tabsetPanel(
                id = "tabs_dicionarios",
                type = "pills",
                
                # ============ TIPO 1 ============
                tabPanel(
                  title = "Tipo 1",
                  icon = icon("circle", style = "color: #87CEEB;"),
                  value = "tipo1",
                  
                  br(),
                  
                  div(
                    class = "header-tipo-1",
                    h3(style = "margin: 0;", icon("clipboard-list"), " Tipo 1 - Condicionamento e Limpeza"),
                    p(style = "margin-top: 10px;", "Intervenções de baixa complexidade e rotineiras")
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      textAreaInput("desc_tipo_1", label = h4(icon("file-alt"), " Descrição"),
                                    value = "Condicionamento, limpeza e pequenos reparos", rows = 5),
                      textAreaInput("quando_tipo_1", label = h4(icon("lightbulb"), " Quando Utilizar"),
                                    value = "Use para manutenções simples e rotineiras", rows = 5)
                    ),
                    column(
                      width = 6,
                      textAreaInput("palavras_tipo_1", label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
                                    value = "limpeza\npintura\ncondicionamento\nlubrificação", rows = 12)
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("salvar_tipo_1", label = tagList(icon("save"), "Salvar Tipo 1"),
                                 class = "btn-lg btn-tipo-1")
                  )
                ),
                
                # ============ TIPO 2 ============
                tabPanel(
                  title = "Tipo 2",
                  icon = icon("circle", style = "color: #90EE90;"),
                  value = "tipo2",
                  
                  br(),
                  
                  div(
                    class = "header-tipo-2",
                    h3(style = "margin: 0;", icon("clipboard-list"), " Tipo 2 - Melhorias e Modificações"),
                    p(style = "margin-top: 10px;", "Alterações planejadas no sistema")
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      textAreaInput("desc_tipo_2", label = h4(icon("file-alt"), " Descrição"),
                                    value = "Melhorias, modificações e testes", rows = 5),
                      textAreaInput("quando_tipo_2", label = h4(icon("lightbulb"), " Quando Utilizar"),
                                    value = "Use para modificações planejadas", rows = 5)
                    ),
                    column(
                      width = 6,
                      textAreaInput("palavras_tipo_2", label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
                                    value = "melhoria\nmodificação\nteste\natualização", rows = 12)
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("salvar_tipo_2", label = tagList(icon("save"), "Salvar Tipo 2"),
                                 class = "btn-lg btn-tipo-2")
                  )
                ),
                
                # ============ TIPO 3 ============
                tabPanel(
                  title = "Tipo 3",
                  icon = icon("circle", style = "color: #FFD700;"),
                  value = "tipo3",
                  
                  br(),
                  
                  div(
                    class = "header-tipo-3",
                    h3(style = "margin: 0;", icon("clipboard-list"), " Tipo 3 - Manutenção Preventiva"),
                    p(style = "margin-top: 10px;", "Manutenções programadas e inspeções")
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      textAreaInput("desc_tipo_3", label = h4(icon("file-alt"), " Descrição"),
                                    value = "Manutenção preventiva e preditiva", rows = 5),
                      textAreaInput("quando_tipo_3", label = h4(icon("lightbulb"), " Quando Utilizar"),
                                    value = "Use para manutenções programadas", rows = 5)
                    ),
                    column(
                      width = 6,
                      textAreaInput("palavras_tipo_3", label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
                                    value = "preventiva\npreditiva\ninspeção\nverificação", rows = 12)
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("salvar_tipo_3", label = tagList(icon("save"), "Salvar Tipo 3"),
                                 class = "btn-lg btn-tipo-3")
                  )
                ),
                
                # ============ TIPO 4 ============
                tabPanel(
                  title = "Tipo 4",
                  icon = icon("circle", style = "color: #FFA500;"),
                  value = "tipo4",
                  
                  br(),
                  
                  div(
                    class = "header-tipo-4",
                    h3(style = "margin: 0;", icon("clipboard-list"), " Tipo 4 - Oportunidade"),
                    p(style = "margin-top: 10px;", "Manutenção por oportunidade")
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      textAreaInput("desc_tipo_4", label = h4(icon("file-alt"), " Descrição"),
                                    value = "Manutenção por oportunidade", rows = 5),
                      textAreaInput("quando_tipo_4", label = h4(icon("lightbulb"), " Quando Utilizar"),
                                    value = "Use para manutenções não programadas", rows = 5)
                    ),
                    column(
                      width = 6,
                      textAreaInput("palavras_tipo_4", label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
                                    value = "oportunidade\nnão programada\nadicional", rows = 12)
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("salvar_tipo_4", label = tagList(icon("save"), "Salvar Tipo 4"),
                                 class = "btn-lg btn-tipo-4")
                  )
                ),
                
                # ============ TIPO 5 ============
                tabPanel(
                  title = "Tipo 5",
                  icon = icon("circle", style = "color: #FF6347;"),
                  value = "tipo5",
                  
                  br(),
                  
                  div(
                    class = "header-tipo-5",
                    h3(style = "margin: 0;", icon("clipboard-list"), " Tipo 5 - Defeito"),
                    p(style = "margin-top: 10px;", "Eliminação de defeitos")
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      textAreaInput("desc_tipo_5", label = h4(icon("file-alt"), " Descrição"),
                                    value = "Eliminação de defeito", rows = 5),
                      textAreaInput("quando_tipo_5", label = h4(icon("lightbulb"), " Quando Utilizar"),
                                    value = "Use para correção de defeitos", rows = 5)
                    ),
                    column(
                      width = 6,
                      textAreaInput("palavras_tipo_5", label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
                                    value = "defeito\nproblema\nanomalia\ncorreção", rows = 12)
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("salvar_tipo_5", label = tagList(icon("save"), "Salvar Tipo 5"),
                                 class = "btn-lg btn-tipo-5")
                  )
                ),
                
                # ============ TIPO 6 ============
                tabPanel(
                  title = "Tipo 6",
                  icon = icon("circle", style = "color: #DC143C;"),
                  value = "tipo6",
                  
                  br(),
                  
                  div(
                    class = "header-tipo-6",
                    h3(style = "margin: 0;", icon("clipboard-list"), " Tipo 6 - Falha"),
                    p(style = "margin-top: 10px;", "Eliminação de falhas críticas")
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      textAreaInput("desc_tipo_6", label = h4(icon("file-alt"), " Descrição"),
                                    value = "Eliminação de falha", rows = 5),
                      textAreaInput("quando_tipo_6", label = h4(icon("lightbulb"), " Quando Utilizar"),
                                    value = "Use para falhas críticas", rows = 5)
                    ),
                    column(
                      width = 6,
                      textAreaInput("palavras_tipo_6", label = h4(icon("tags"), " Palavras-Chave (uma por linha)"),
                                    value = "falha\nquebra\nemergência\ncrítico", rows = 12)
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("salvar_tipo_6", label = tagList(icon("save"), "Salvar Tipo 6"),
                                 class = "btn-lg btn-tipo-6")
                  )
                )
                
              ) # Fecha tabsetPanel
            ) # Fecha box
          ) # Fecha column
        ) # Fecha fluidRow
        
      ), # Fecha tabItem("dicionarios")
      
      #=======================================================================
      # ABA 6: ESTATÍSTICAS
      #=======================================================================
      
      tabItem(tabName = "estatisticas",
              
              fluidRow(
                valueBoxOutput("metrica_total_classificados", width = 3),
                valueBoxOutput("metrica_acuracia", width = 3),
                valueBoxOutput("metrica_conformes", width = 3),
                valueBoxOutput("metrica_divergentes", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "📊 Matriz de Confusão (Tipo Antigo vs Tipo Novo)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("matriz_confusao", height = "400px")
                ),
                
                box(
                  title = "🎯 Acurácia por Tipo",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_acuracia_tipo", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 Distribuição de Confiança",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_distribuicao_confianca", height = "350px")
                ),
                
                box(
                  title = "🔀 Métodos de Classificação Utilizados",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_metodos", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Detalhamento de Métricas",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  tabsetPanel(
                    tabPanel("Por Tipo",
                             br(),
                             DT::dataTableOutput("tabela_metricas_tipo")
                    ),
                    
                    tabPanel("Por Categoria",
                             br(),
                             DT::dataTableOutput("tabela_metricas_categoria")
                    ),
                    
                    tabPanel("Por Método",
                             br(),
                             DT::dataTableOutput("tabela_metricas_metodo")
                    ),
                    
                    tabPanel("Divergências",
                             br(),
                             DT::dataTableOutput("tabela_divergencias_detalhadas")
                    )
                  )
                )
              )
      ),
      
      #=============================================================================
      # ABA 7: CONFIGURAÇÕES API - VERSÃO ELEGANTE
      #=============================================================================
      
      tabItem(tabName = "config",
              
              fluidRow(
                # Coluna principal - Configurações
                column(width = 8,
                       
                       box(
                         title = div(
                           icon("cogs", style = "margin-right: 10px;"),
                           "Configurações da API OpenAI Petrobras"
                         ),
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,
                         
                         div(
                           style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: white;",
                           
                           div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                               tags$span(style = "font-size: 48px; margin-right: 20px;", "🔐"),
                               div(
                                 tags$h4("Credenciais de Acesso", style = "margin: 0; color: white;"),
                                 tags$p("Configure as credenciais da API OpenAI da Petrobras", style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 13px;")
                               )
                           )
                         ),
                         
                         textInput("config_base_url",
                                   div(icon("server"), " Base URL:"),
                                   value = OPENAI_CONFIG$base_url,
                                   width = "100%"),
                         
                         passwordInput("config_api_key",
                                       div(icon("key"), " API Key:"),
                                       value = OPENAI_CONFIG$api_key,
                                       width = "100%"),
                         
                         textInput("config_model",
                                   div(icon("brain"), " Modelo:"),
                                   value = OPENAI_CONFIG$model,
                                   width = "100%"),
                         
                         textInput("config_api_version",
                                   div(icon("code-branch"), " API Version:"),
                                   value = OPENAI_CONFIG$api_version,
                                   width = "100%"),
                         
                         hr(),
                         
                         fluidRow(
                           column(6,
                                  actionButton("salvar_config",
                                               div(icon("save"), " Salvar Configurações"),
                                               class = "btn-success btn-block",
                                               style = "height: 50px; font-size: 15px;")
                           ),
                           column(6,
                                  actionButton("testar_api",
                                               div(icon("plug"), " Testar Conexão"),
                                               class = "btn-info btn-block",
                                               style = "height: 50px; font-size: 15px;")
                           )
                         )
                       )
                ),
                
                # Coluna lateral - Status e Resultado
                column(width = 4,
                       
                       box(
                         title = div(icon("chart-line"), " Status da API"),
                         status = "info",
                         solidHeader = TRUE,
                         width = NULL,
                         
                         div(
                           style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #17a2b8;",
                           
                           div(style = "margin-bottom: 15px;",
                               tags$strong(style = "color: #17a2b8;", "📡 Configuração Atual:"),
                               tags$hr(style = "margin: 10px 0;"),
                               
                               div(style = "font-size: 12px; line-height: 1.8;",
                                   tags$div(
                                     tags$strong("Base URL:"),
                                     tags$br(),
                                     tags$span(style = "color: #666; font-size: 11px;", OPENAI_CONFIG$base_url)
                                   ),
                                   tags$br(),
                                   tags$div(
                                     tags$strong("Modelo:"),
                                     tags$br(),
                                     tags$span(style = "color: #666; font-size: 11px;", OPENAI_CONFIG$model)
                                   ),
                                   tags$br(),
                                   tags$div(
                                     tags$strong("API Version:"),
                                     tags$br(),
                                     tags$span(style = "color: #666; font-size: 11px;", OPENAI_CONFIG$api_version)
                                   ),
                                   tags$br(),
                                   tags$div(
                                     tags$strong("API Key:"),
                                     tags$br(),
                                     tags$span(style = "color: #666; font-size: 11px;", 
                                               paste0(substr(OPENAI_CONFIG$api_key, 1, 12), "..."))
                                   )
                               )
                           )
                         ),
                         
                         br(),
                         
                         htmlOutput("resultado_teste_api")
                       )
                )
              ),
              
              # Seção de Explicação
              fluidRow(
                box(
                  title = div(icon("info-circle"), " Como a API Funciona neste Projeto"),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  
                  div(
                    style = "padding: 20px;",
                    
                    # Introdução
                    div(
                      style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 20px; border-radius: 8px; margin-bottom: 20px; border-left: 5px solid #2196F3;",
                      
                      tags$h4("🤖 Visão Geral", style = "color: #1565C0; margin-top: 0;"),
                      tags$p(
                        "Este projeto utiliza a ", 
                        tags$strong("API OpenAI da Petrobras"), 
                        " para classificação inteligente de textos de manutenção SAP. A API é hospedada na infraestrutura privada da Petrobras e usa o modelo ",
                        tags$code("gpt-4o-petrobras"),
                        " para análise de linguagem natural.",
                        style = "font-size: 14px; line-height: 1.8;"
                      )
                    ),
                    
                    # Fluxo de Funcionamento
                    tags$h4("🔄 Fluxo de Funcionamento", style = "color: #1f4e79; margin-top: 30px;"),
                    
                    div(
                      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px; margin: 20px 0;",
                      
                      # Passo 1
                      div(
                        style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 4px solid #4CAF50;",
                        div(style = "text-align: center; font-size: 36px; margin-bottom: 10px;", "1️⃣"),
                        tags$strong("Entrada de Texto", style = "color: #4CAF50; font-size: 14px;"),
                        tags$p("O usuário fornece um texto de manutenção para análise", style = "font-size: 12px; color: #666; margin-top: 10px;")
                      ),
                      
                      # Passo 2
                      div(
                        style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 4px solid #2196F3;",
                        div(style = "text-align: center; font-size: 36px; margin-bottom: 10px;", "2️⃣"),
                        tags$strong("Análise Híbrida", style = "color: #2196F3; font-size: 14px;"),
                        tags$p("Dicionário + API analisam o texto simultaneamente", style = "font-size: 12px; color: #666; margin-top: 10px;")
                      ),
                      
                      # Passo 3
                      div(
                        style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 4px solid #FF9800;",
                        div(style = "text-align: center; font-size: 36px; margin-bottom: 10px;", "3️⃣"),
                        tags$strong("Classificação", style = "color: #FF9800; font-size: 14px;"),
                        tags$p("Sistema determina o tipo SAP mais adequado (1-6)", style = "font-size: 12px; color: #666; margin-top: 10px;")
                      ),
                      
                      # Passo 4
                      div(
                        style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 4px solid #9C27B0;",
                        div(style = "text-align: center; font-size: 36px; margin-bottom: 10px;", "4️⃣"),
                        tags$strong("Resultado", style = "color: #9C27B0; font-size: 14px;"),
                        tags$p("Retorna tipo, categoria, criticidade e resumo", style = "font-size: 12px; color: #666; margin-top: 10px;")
                      )
                    ),
                    
                    # Modos de Operação
                    tags$h4("⚙️ Modos de Operação", style = "color: #1f4e79; margin-top: 30px;"),
                    
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; margin: 20px 0;",
                      
                      # Modo Dicionário
                      div(
                        style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #4CAF50;",
                        div(style = "text-align: center; font-size: 32px; margin-bottom: 10px;", "🔤"),
                        tags$strong("DICIONÁRIO", style = "color: #2E7D32; font-size: 14px; display: block; text-align: center;"),
                        tags$hr(style = "margin: 10px 0;"),
                        tags$ul(
                          style = "font-size: 12px; color: #2E7D32; padding-left: 20px;",
                          tags$li("Rápido e offline"),
                          tags$li("Baseado em palavras-chave"),
                          tags$li("Personalizável"),
                          tags$li("Sem custo de API")
                        )
                      ),
                      
                      # Modo API
                      div(
                        style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #2196F3;",
                        div(style = "text-align: center; font-size: 32px; margin-bottom: 10px;", "🤖"),
                        tags$strong("API (IA)", style = "color: #1565C0; font-size: 14px; display: block; text-align: center;"),
                        tags$hr(style = "margin: 10px 0;"),
                        tags$ul(
                          style = "font-size: 12px; color: #1565C0; padding-left: 20px;",
                          tags$li("Inteligência artificial"),
                          tags$li("Análise contextual"),
                          tags$li("Maior precisão"),
                          tags$li("Requer conexão")
                        )
                      ),
                      
                      # Modo Híbrido
                      div(
                        style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #FF9800;",
                        div(style = "text-align: center; font-size: 32px; margin-bottom: 10px;", "🔀"),
                        tags$strong("HÍBRIDO", style = "color: #E65100; font-size: 14px; display: block; text-align: center;"),
                        tags$hr(style = "margin: 10px 0;"),
                        tags$ul(
                          style = "font-size: 12px; color: #E65100; padding-left: 20px;",
                          tags$li("Melhor dos dois mundos"),
                          tags$li("Validação cruzada"),
                          tags$li("Máxima confiabilidade"),
                          tags$li("Recomendado")
                        )
                      )
                    ),
                    
                    # Endpoints Utilizados
                    tags$h4("🌐 Endpoints da API", style = "color: #1f4e79; margin-top: 30px;"),
                    
                    div(
                      style = "background: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;",
                      
                      tags$div(
                        style = "margin-bottom: 15px;",
                        tags$strong("1. Classificação SAP:", style = "color: #1f4e79;"),
                        tags$br(),
                        tags$code(
                          style = "background: #e9ecef; padding: 5px 10px; border-radius: 4px; font-size: 12px; display: block; margin-top: 5px;",
                          "POST /deployments/gpt-4o-petrobras/chat/completions"
                        ),
                        tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                               "Analisa textos e classifica em tipos SAP (1-6)")
                      ),
                      
                      tags$div(
                        style = "margin-bottom: 15px;",
                        tags$strong("2. Extração de Assunto:", style = "color: #1f4e79;"),
                        tags$br(),
                        tags$code(
                          style = "background: #e9ecef; padding: 5px 10px; border-radius: 4px; font-size: 12px; display: block; margin-top: 5px;",
                          "POST /deployments/gpt-4o-petrobras/chat/completions"
                        ),
                        tags$p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                               "Extrai resumo conciso do assunto principal (máx. 80 caracteres)")
                      )
                    ),
                    
                    # Parâmetros de Requisição
                    tags$h4("📝 Parâmetros de Requisição", style = "color: #1f4e79; margin-top: 30px;"),
                    
                    div(
                      style = "background: white; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                      
                      tags$table(
                        style = "width: 100%; font-size: 13px;",
                        tags$thead(
                          tags$tr(
                            tags$th("Parâmetro", style = "padding: 10px; background: #f8f9fa; border-bottom: 2px solid #dee2e6;"),
                            tags$th("Valor", style = "padding: 10px; background: #f8f9fa; border-bottom: 2px solid #dee2e6;"),
                            tags$th("Descrição", style = "padding: 10px; background: #f8f9fa; border-bottom: 2px solid #dee2e6;")
                          )
                        ),
                        tags$tbody(
                          tags$tr(
                            tags$td("temperature", style = "padding: 10px; border-bottom: 1px solid #dee2e6;"),
                            tags$td(tags$code("0.3"), style = "padding: 10px; border-bottom: 1px solid #dee2e6;"),
                            tags$td("Controla aleatoriedade (menor = mais preciso)", style = "padding: 10px; border-bottom: 1px solid #dee2e6; color: #666;")
                          ),
                          tags$tr(
                            tags$td("max_tokens", style = "padding: 10px; border-bottom: 1px solid #dee2e6;"),
                            tags$td(tags$code("500"), style = "padding: 10px; border-bottom: 1px solid #dee2e6;"),
                            tags$td("Tamanho máximo da resposta", style = "padding: 10px; border-bottom: 1px solid #dee2e6; color: #666;")
                          ),
                          tags$tr(
                            tags$td("timeout", style = "padding: 10px;"),
                            tags$td(tags$code("30s"), style = "padding: 10px;"),
                            tags$td("Tempo máximo de espera", style = "padding: 10px; color: #666;")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # Seção de Segurança
              fluidRow(
                box(
                  title = div(icon("shield-alt"), " Segurança e Boas Práticas"),
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  div(
                    style = "padding: 15px;",
                    
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                      
                      # Coluna 1
                      div(
                        tags$h5("🔒 Segurança", style = "color: #2e8b57;"),
                        tags$ul(
                          style = "font-size: 13px; line-height: 1.8;",
                          tags$li("API hospedada em infraestrutura privada Petrobras"),
                          tags$li("Chave de API criptografada e protegida"),
                          tags$li("Conexão via HTTPS (TLS 1.2+)"),
                          tags$li("Sem armazenamento externo de dados"),
                          tags$li("Logs auditáveis de todas as requisições")
                        )
                      ),
                      
                      # Coluna 2
                      div(
                        tags$h5("✅ Boas Práticas", style = "color: #2e8b57;"),
                        tags$ul(
                          style = "font-size: 13px; line-height: 1.8;",
                          tags$li("Não compartilhe sua API Key"),
                          tags$li("Use modo HÍBRIDO para melhor resultado"),
                          tags$li("Revise classificações com baixa confiança"),
                          tags$li("Mantenha backups dos dicionários personalizados"),
                          tags$li("Teste a API periodicamente")
                        )
                      )
                    )
                  )
                )
              )),
      
      #=============================================================================
      # ABA 8: DOCUMENTAÇÃO DO PROJETO (CORRIGIDA)
      #=============================================================================
      
      tabItem(tabName = "documentacao",
              
              fluidRow(
                box(
                  title = div(
                    icon("book-open", style = "margin-right: 10px;"),
                    "Documentação do Sistema SAP Petrobras"
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  div(
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                 padding: 30px; border-radius: 10px; color: white; 
                 margin-bottom: 30px; text-align: center;",
                    
                    tags$h2("🤖 Sistema de Classificação SAP com IA", 
                            style = "margin: 0 0 15px 0;"),
                    tags$p("Versão 3.0 - Powered by OpenAI Petrobras", 
                           style = "font-size: 16px; opacity: 0.9; margin: 0;")
                  )
                )
              ),
              
              # Seção 1: Sobre o Projeto
              fluidRow(
                box(
                  title = div(icon("info-circle"), " Sobre o Projeto"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  div(
                    style = "padding: 20px;",
                    
                    tags$h4("🎯 Objetivo", style = "color: #1f4e79;"),
                    tags$p(
                      "Este sistema foi desenvolvido para automatizar e padronizar a classificação de textos de manutenção no SAP.",
                      style = "font-size: 14px; line-height: 1.8; text-align: justify;"
                    )
                  )
                )
              )
      ), # ← FECHA tabItem("documentacao")
      
      #=======================================================================
      # ABA 9: HISTÓRICO
      #=======================================================================
      
      tabItem(
        tabName = "historico",
        
        fluidRow(
          box(
            title = "📜 Histórico de Processamentos",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 8,
                uiOutput("info_historico")
              ),
              column(
                width = 4,
                div(
                  style = "text-align: right;",
                  actionButton("voltar_historico", "⬅️ Anterior", 
                               class = "btn-warning", style = "margin: 5px;"),
                  actionButton("avancar_historico", "Próximo ➡️", 
                               class = "btn-warning", style = "margin: 5px;"),
                  br(), br(),
                  actionButton("salvar_sessao", "💾 Salvar Sessão", 
                               class = "btn-success", style = "margin: 5px;"),
                  actionButton("carregar_sessao", "📂 Carregar Sessão", 
                               class = "btn-info", style = "margin: 5px;"),
                  actionButton("limpar_historico", "🗑️ Limpar Histórico", 
                               class = "btn-danger", style = "margin: 5px;")
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "📊 Comparação de Métricas",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotOutput("grafico_evolucao_metricas", height = "350px")
            )
          ),
          column(
            width = 6,
            box(
              title = "🔍 Detalhes do Processamento Atual",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              uiOutput("detalhes_processamento_atual")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📋 Lista de Processamentos",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("tabela_historico")
          )
        )
        
      ) # Fecha tabItem de historico
      
    ) # Fecha tabItems
  ) # Fecha dashboardBody
) # Fecha dashboardPage

cat("✅ Interface carregada com sucesso!\n")
cat("📊 Preparando servidor...\n\n")

#=============================================================================
# SERVIDOR (SERVER COMPLETO)
#=============================================================================
server <- function(input, output, session) {
  
  #===========================================================================
  # VALORES REATIVOS
  #===========================================================================
  
  values <- reactiveValues(
    dados_ordens = NULL,
    dados_textos = NULL,
    dados_cruzados = NULL,
    col_tip_intervencao = NULL,
    historico = data.frame(),
    resultados_lote = NULL,
    processando = FALSE
  )
  
  #===========================================================================
  # SISTEMA DE MEMÓRIA E HISTÓRICO
  #===========================================================================
  
  # Estrutura de memória expandida
  historico <- reactiveValues(
    processamentos = list(),      # Lista de todos os processamentos
    indice_atual = 0,             # Índice do processamento atual
    max_historico = 20,           # Máximo de processamentos guardados
    sessao_id = format(Sys.time(), "%Y%m%d_%H%M%S")  # ID da sessão
  )
  
  # Função para adicionar processamento ao histórico
  adicionar_ao_historico <- function(dados_resultado, metadados = list()) {
    
    cat("\n📝 Adicionando ao histórico...\n")
    
    # Criar snapshot do processamento
    snapshot <- list(
      timestamp = Sys.time(),
      dados = dados_resultado,
      metadados = metadados,
      metricas = calcular_metricas_snapshot(dados_resultado),
      id = paste0("PROC_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    
    # Se não estamos no final do histórico, remover processamentos "futuros"
    if(historico$indice_atual < length(historico$processamentos)) {
      historico$processamentos <- historico$processamentos[1:historico$indice_atual]
    }
    
    # Adicionar novo processamento
    historico$processamentos <- append(historico$processamentos, list(snapshot))
    historico$indice_atual <- length(historico$processamentos)
    
    # Limitar tamanho do histórico
    if(length(historico$processamentos) > historico$max_historico) {
      historico$processamentos <- tail(historico$processamentos, historico$max_historico)
      historico$indice_atual <- length(historico$processamentos)
    }
    
    cat("✅ Histórico atualizado:", length(historico$processamentos), "processamentos\n")
    cat("📍 Índice atual:", historico$indice_atual, "\n\n")
    
    return(snapshot$id)
  }
  
  # Função para calcular métricas de um snapshot
  calcular_metricas_snapshot <- function(dados) {
    
    if(is.null(dados) || nrow(dados) == 0) {
      return(list(
        total = 0,
        conformes = 0,
        divergentes = 0,
        acuracia = 0,
        confianca_media = 0
      ))
    }
    
    dados_validos <- dados %>%
      filter(!is.na(tipo_intervencao_antigo), !is.na(tipo_novo))
    
    if(nrow(dados_validos) == 0) {
      return(list(
        total = nrow(dados),
        conformes = 0,
        divergentes = 0,
        acuracia = 0,
        confianca_media = 0
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
  
  # Função para navegar no histórico
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
  
  # Reactive para dados do processamento atual
  processamento_atual <- reactive({
    obter_processamento_atual()
  })
  
  # Reactive para verificar se pode desfazer
  pode_desfazer <- reactive({
    historico$indice_atual > 1
  })
  
  # Reactive para verificar se pode refazer
  pode_refazer <- reactive({
    historico$indice_atual < length(historico$processamentos)
  })
  
  
  CONFIG_USUARIO <- reactiveValues(
    dicionarios = DICIONARIOS_SAP,
    usar_dicionario = TRUE,
    usar_api = TRUE,
    prioridade = "HIBRIDO"
  )  
  # Reactive value para dados com assuntos
  dados_com_assuntos <- reactiveVal(NULL)
  
  
  #===========================================================================
  # FUNÇÕES DE MÉTRICAS
  #===========================================================================
  
  metricas <- reactive({
    req(values$resultados_lote)
    
    dados <- values$resultados_lote
    
    dados_validos <- dados %>%
      filter(!is.na(tipo_intervencao_antigo), !is.na(tipo_novo))
    
    if(nrow(dados_validos) == 0) {
      return(NULL)
    }
    
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
      summarise(
        total = n(),
        .groups = "drop"
      )
    
    if("metodo" %in% names(dados_validos)) {
      metricas_metodo <- dados_validos %>%
        group_by(metodo) %>%
        summarise(
          total = n(),
          conformes = sum(conforme),
          acuracia = mean(conforme) * 100,
          confianca_media = mean(confianca, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      metricas_metodo <- NULL
    }
    
    divergencias <- dados_validos %>%
      filter(!conforme) %>%
      select(
        nota_key,
        texto_completo,
        tipo_intervencao_antigo,
        tipo_novo,
        categoria,
        criticidade,
        confianca,
        resumo
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
  
  #===========================================================================
  # DASHBOARD - VALUE BOXES
  #===========================================================================
  
  output$total_textos <- renderValueBox({
    total <- if(is.null(values$dados_cruzados)) 0 else nrow(values$dados_cruzados)
    valueBox(
      value = total,
      subtitle = "Textos Carregados",
      icon = icon("file-text"),
      color = "navy"
    )
  })
  
  output$textos_iazf <- renderValueBox({
    iazf_count <- if(is.null(values$resultados_lote)) {
      0
    } else {
      sum(values$resultados_lote$categoria == "IAZF", na.rm = TRUE)
    }
    valueBox(
      value = iazf_count,
      subtitle = "Textos IAZF",
      icon = icon("exclamation-triangle"),
      color = "orange"
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
      cor <- "gray"
    } else {
      valor <- paste0(round(m$acuracia, 1), "%")
      cor <- if(m$acuracia >= 80) "blue" else if(m$acuracia >= 60) "orange" else "red"
    }
    
    valueBox(
      value = valor,
      subtitle = "Taxa de Conformidade",
      icon = icon("check-double"),
      color = cor
    )
  })
  
  #===========================================================================
  # DASHBOARD - GRÁFICOS
  #===========================================================================
  
  output$grafico_comparacao_antes_depois <- renderPlot({
    req(values$resultados_lote)
    
    dados <- values$resultados_lote %>%
      filter(!is.na(tipo_intervencao_antigo), !is.na(tipo_novo))
    
    if(nrow(dados) == 0) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados para comparação", size = 5)
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
        geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
        geom_text(aes(label = count), 
                  position = position_dodge(width = 0.7),
                  vjust = -0.5, fontface = "bold", size = 4) +
        scale_fill_manual(
          values = c("Anterior" = "#95a5a6", "Novo" = "#1f4e79"),
          name = ""
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.text.x = element_text(size = 11, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"),
          panel.grid.major.x = element_blank()
        ) +
        labs(title = "", x = "", y = "Quantidade")
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
      "Tipo 1" = "#87CEEB",
      "Tipo 2" = "#90EE90",
      "Tipo 3" = "#FFD700",
      "Tipo 4" = "#FFA500",
      "Tipo 5" = "#FF6347",
      "Tipo 6" = "#DC143C"
    )
    
    ggplot(dados, aes(x = tipo, y = count, fill = tipo)) +
      geom_col(alpha = 0.8, width = 0.7) +
      geom_text(aes(label = count), vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_manual(values = cores_tipos) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major.x = element_blank()
      ) +
      labs(title = "", x = "", y = "Quantidade") +
      ylim(0, max(dados$count) * 1.15)
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
      geom_col(alpha = 0.8, width = 0.6) +
      geom_text(aes(label = count), vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_manual(values = c("PROBLEMAS_COMUNS" = "#2e8b57", "IAZF" = "#ff6b35")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 12, face = "bold")
      ) +
      labs(title = "", x = "", y = "Quantidade")
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
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(
        values = c("CONFORME" = "#2e8b57", "DIVERGENTE" = "#ff6b35"),
        name = ""
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 12, face = "bold")
      ) +
      geom_text(
        aes(label = paste0(count, "\n(", round(count/sum(count)*100, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size = 5,
        fontface = "bold",
        color = "white"
      )
  })
  
  output$tabela_recentes <- DT::renderDataTable({
    if(is.null(values$resultados_lote) || nrow(values$resultados_lote) == 0) {
      exemplo <- data.frame(
        Texto = c("Aguardando classificações..."),
        Tipo = c("-"),
        Categoria = c("-"),
        Criticidade = c("-"),
        Confianca = c("-")
      )
    } else {
      exemplo <- tail(values$resultados_lote, 10) %>%
        mutate(
          Texto = if("assunto_principal" %in% names(.)) assunto_principal else substr(texto_completo, 1, 60),
          Confiança = paste0(confianca, "%")
        ) %>%
        select(Texto, Tipo = tipo_novo, Categoria = categoria,
               Criticidade = criticidade, Confiança)
    }
    
    DT::datatable(exemplo,
                  options = list(pageLength = 5, scrollX = TRUE),
                  class = 'cell-border stripe',
                  rownames = FALSE)
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
      
      showNotification("✅ Arquivo de Ordens carregado!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("❌ Erro:", e$message), type = "error", duration = 5)
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
      
      showNotification("✅ Arquivo de Textos carregado!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("❌ Erro:", e$message), type = "error", duration = 5)
    })
  })
  
  #===========================================================================
  # CRUZAMENTO DE DADOS
  #===========================================================================
  
  observeEvent(input$cruzar, {
    req(values$dados_ordens, values$dados_textos)
    
    withProgress(message = 'Cruzando dados...', value = 0, {
      
      incProgress(0.3, detail = "Identificando colunas...")
      
      resultado <- cruzar_dados(values$dados_ordens, values$dados_textos)
      
      incProgress(0.7, detail = "Finalizando...")
      
      if(resultado$sucesso) {
        values$dados_cruzados <- resultado$dados
        values$col_tip_intervencao <- resultado$col_tip_intervencao
        
        output$status_cruzamento <- renderUI({
          HTML(paste0(
            "<div style='padding: 20px; background: #d4edda; border-radius: 8px; border-left: 5px solid #28a745;'>",
            "<h4 style='color: #155724; margin: 0 0 10px 0;'>✅ Cruzamento Concluído!</h4>",
            "<p style='margin: 5px 0;'><strong>Total de registros:</strong> ", nrow(resultado$dados), "</p>",
            "<p style='margin: 5px 0;'><strong>Com texto:</strong> ", resultado$estatisticas$com_texto, "</p>",
            "<p style='margin: 5px 0;'><strong>Correspondências:</strong> ", resultado$estatisticas$correspondencias, "</p>",
            "</div>"
          ))
        })
        
        showNotification("✅ Dados cruzados com sucesso!", type = "message", duration = 5)
        
      } else {
        output$status_cruzamento <- renderUI({
          HTML(paste0(
            "<div style='padding: 20px; background: #f8d7da; border-radius: 8px; border-left: 5px solid #dc3545;'>",
            "<h4 style='color: #721c24; margin: 0 0 10px 0;'>❌ Erro no Cruzamento</h4>",
            "<p>", resultado$erro, "</p>",
            "</div>"
          ))
        })
        
        showNotification(paste("❌", resultado$erro), type = "error", duration = 10)
      }
    })
  })
  
  #===========================================================================
  # TABELA DE DADOS CRUZADOS - VERSÃO MELHORADA
  #===========================================================================
  
  output$preview_cruzado <- DT::renderDataTable({
    req(values$dados_cruzados)
    
    # Verificar se há dados com assuntos processados
    if(!is.null(dados_com_assuntos())) {
      dados_exibir <- dados_com_assuntos()
      tem_assuntos <- TRUE
    } else {
      dados_exibir <- values$dados_cruzados
      tem_assuntos <- FALSE
    }
    
    # Limitar a 100 primeiras linhas para preview
    dados_exibir <- head(dados_exibir, 100)
    
    # Definir colunas prioritárias para exibição
    colunas_exibir <- c()
    
    # 1. Nota (obrigatória)
    if("nota_key" %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, "nota_key")
    }
    
    # 2. Assunto Principal (se disponível)
    if(tem_assuntos && "assunto_principal" %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, "assunto_principal")
    }
    
    # 3. Tipo de Intervenção Antigo (se disponível)
    if(!is.null(values$col_tip_intervencao) && 
       values$col_tip_intervencao %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, values$col_tip_intervencao)
    }
    
    # 4. Texto Completo (obrigatório)
    if("texto_completo" %in% names(dados_exibir)) {
      colunas_exibir <- c(colunas_exibir, "texto_completo")
    }
    
    # 5. Outras colunas relevantes do arquivo de ordens
    colunas_extras <- c(
      "Ordem", "ordem", "ORDEM",
      "Centro de trabalho", "centro_trabalho", "CENTRO_TRABALHO",
      "Descrição da ordem", "descricao_ordem", "DESC_ORDEM",
      "Data de início", "data_inicio", "DATA_INICIO",
      "Status", "status", "STATUS",
      "Prioridade", "prioridade", "PRIORIDADE"
    )
    
    for(col in colunas_extras) {
      if(col %in% names(dados_exibir) && !(col %in% colunas_exibir)) {
        colunas_exibir <- c(colunas_exibir, col)
      }
    }
    
    # Selecionar apenas colunas existentes
    colunas_existentes <- colunas_exibir[colunas_exibir %in% names(dados_exibir)]
    
    if(length(colunas_existentes) == 0) {
      # Fallback: mostrar todas as colunas
      dados_tabela <- dados_exibir
    } else {
      dados_tabela <- dados_exibir %>%
        select(all_of(colunas_existentes))
    }
    
    # Renomear colunas para nomes amigáveis
    nomes_colunas <- names(dados_tabela)
    nomes_amigaveis <- nomes_colunas
    
    # Mapeamento de nomes
    mapa_nomes <- list(
      "nota_key" = "📋 Nota",
      "assunto_principal" = "📝 Assunto Principal",
      "texto_completo" = "📄 Texto Completo",
      "Ordem" = "🔢 Ordem",
      "ordem" = "🔢 Ordem",
      "ORDEM" = "🔢 Ordem",
      "Centro de trabalho" = "🏭 Centro Trabalho",
      "centro_trabalho" = "🏭 Centro Trabalho",
      "CENTRO_TRABALHO" = "🏭 Centro Trabalho",
      "Descrição da ordem" = "📋 Descrição",
      "descricao_ordem" = "📋 Descrição",
      "DESC_ORDEM" = "📋 Descrição",
      "Data de início" = "📅 Data Início",
      "data_inicio" = "📅 Data Início",
      "DATA_INICIO" = "📅 Data Início",
      "Status" = "⚡ Status",
      "status" = "⚡ Status",
      "STATUS" = "⚡ Status",
      "Prioridade" = "🎯 Prioridade",
      "prioridade" = "🎯 Prioridade",
      "PRIORIDADE" = "🎯 Prioridade"
    )
    
    # Aplicar nomes amigáveis
    for(i in seq_along(nomes_colunas)) {
      col_original <- nomes_colunas[i]
      
      # Verificar se é tipo de intervenção
      if(!is.null(values$col_tip_intervencao) && col_original == values$col_tip_intervencao) {
        nomes_amigaveis[i] <- "🔧 Tipo Intervenção"
      } else if(!is.null(mapa_nomes[[col_original]])) {
        nomes_amigaveis[i] <- mapa_nomes[[col_original]]
      }
    }
    
    names(dados_tabela) <- nomes_amigaveis
    
    # Configurar larguras de colunas
    col_defs <- list(
      list(width = '80px', targets = 0)  # Nota
    )
    
    # Se tem assunto principal
    if("📝 Assunto Principal" %in% nomes_amigaveis) {
      idx_assunto <- which(nomes_amigaveis == "📝 Assunto Principal") - 1
      col_defs <- append(col_defs, list(list(width = '250px', targets = idx_assunto)))
    }
    
    # Texto completo
    if("📄 Texto Completo" %in% nomes_amigaveis) {
      idx_texto <- which(nomes_amigaveis == "📄 Texto Completo") - 1
      col_defs <- append(col_defs, list(list(width = '400px', targets = idx_texto)))
    }
    
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
          list(extend = 'excel', text = '📗 Excel'),
          list(extend = 'print', text = '🖨️ Imprimir')
        ),
        columnDefs = col_defs,
        language = list(
          search = "🔍 Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Nenhum registro disponível",
          infoFiltered = "(filtrado de _MAX_ registros)",
          paginate = list(
            first = "Primeiro",
            last = "Último",
            `next` = "Próximo",
            previous = "Anterior"
          )
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE,
      filter = 'top',
      extensions = 'Buttons'
    )
    
    # Aplicar estilos condicionais
    
    # Estilo para Assunto Principal
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
    
    # Estilo para Nota
    if("📋 Nota" %in% nomes_amigaveis) {
      dt <- dt %>%
        formatStyle(
          '📋 Nota',
          backgroundColor = '#f5f5f5',
          fontWeight = 'bold',
          textAlign = 'center'
        )
    }
    
    # Estilo para Tipo Intervenção
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
    
    # Estilo para Status (se existir)
    if("⚡ Status" %in% nomes_amigaveis) {
      dt <- dt %>%
        formatStyle(
          '⚡ Status',
          backgroundColor = styleEqual(
            c('ABERTO', 'FECHADO', 'EM ANDAMENTO', 'PENDENTE'),
            c('#fff3cd', '#d4edda', '#cfe2ff', '#f8d7da')
          ),
          fontWeight = 'bold',
          textAlign = 'center'
        )
    }
    
    # Estilo para Prioridade (se existir)
    if("🎯 Prioridade" %in% nomes_amigaveis) {
      dt <- dt %>%
        formatStyle(
          '🎯 Prioridade',
          backgroundColor = styleEqual(
            c('ALTA', 'MÉDIA', 'MEDIA', 'BAIXA'),
            c('#dc3545', '#ffc107', '#ffc107', '#28a745')
          ),
          color = 'white',
          fontWeight = 'bold',
          textAlign = 'center'
        )
    }
    
    dt
  })
  
  #===========================================================================
  # INFORMAÇÕES DO PREVIEW
  #===========================================================================
  
  output$info_preview <- renderUI({
    req(values$dados_cruzados)
    
    total <- nrow(values$dados_cruzados)
    preview_size <- min(100, total)
    
    # Verificar se há assuntos processados
    if(!is.null(dados_com_assuntos())) {
      total_processados <- nrow(dados_com_assuntos())
      
      HTML(paste0(
        "<div style='padding: 15px; background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%); border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #28a745; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
        "<div style='display: flex; align-items: center; justify-content: space-between;'>",
        "<div>",
        "<h4 style='color: #155724; margin: 0 0 10px 0;'>✅ Assuntos Extraídos com Sucesso!</h4>",
        "<p style='margin: 5px 0; color: #155724;'><strong>📊 Total processado:</strong> ", total_processados, " registros</p>",
        "<p style='margin: 5px 0; color: #155724;'><strong>📋 Total no arquivo:</strong> ", total, " registros</p>",
        "</div>",
        "<div style='text-align: right;'>",
        "<span style='font-size: 48px;'>📝</span>",
        "</div>",
        "</div>",
        "</div>"
      ))
      
    } else {
      
      HTML(paste0(
        "<div style='padding: 15px; background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%); border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #ffc107; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
        "<div style='display: flex; align-items: center; justify-content: space-between;'>",
        "<div>",
        "<h4 style='color: #856404; margin: 0 0 10px 0;'>ℹ️ Preview dos Dados Cruzados</h4>",
        "<p style='margin: 5px 0; color: #856404;'><strong>📊 Mostrando:</strong> Primeiras ", preview_size, " linhas</p>",
        "<p style='margin: 5px 0; color: #856404;'><strong>📋 Total no arquivo:</strong> ", total, " registros</p>",
        "<p style='margin: 5px 0; color: #856404;'><strong>💡 Dica:</strong> Ative a extração de assuntos para melhor visualização</p>",
        "</div>",
        "<div style='text-align: right;'>",
        "<span style='font-size: 48px;'>📊</span>",
        "</div>",
        "</div>",
        "</div>"
      ))
    }
  })
  
  #===========================================================================
  # ESTATÍSTICAS DOS DADOS CRUZADOS
  #===========================================================================
  
  output$estatisticas_cruzados <- renderUI({
    req(values$dados_cruzados)
    
    dados <- values$dados_cruzados
    
    # Calcular estatísticas
    total_registros <- nrow(dados)
    com_texto <- sum(!is.na(dados$texto_completo) & nchar(trimws(dados$texto_completo)) > 0)
    sem_texto <- total_registros - com_texto
    taxa_sucesso <- round((com_texto / total_registros) * 100, 1)
    
    # Verificar tipos de intervenção
    tipos_info <- ""
    if(!is.null(values$col_tip_intervencao) && 
       values$col_tip_intervencao %in% names(dados)) {
      
      tipos_dist <- dados %>%
        count(!!sym(values$col_tip_intervencao), name = "qtd") %>%
        arrange(desc(qtd))
      
      tipos_html <- paste(
        sapply(1:min(6, nrow(tipos_dist)), function(i) {
          tipo <- tipos_dist[[values$col_tip_intervencao]][i]
          qtd <- tipos_dist$qtd[i]
          pct <- round((qtd / total_registros) * 100, 1)
          
          cor <- switch(as.character(tipo),
                        "1" = "#87CEEB",
                        "2" = "#90EE90",
                        "3" = "#FFD700",
                        "4" = "#FFA500",
                        "5" = "#FF6347",
                        "6" = "#DC143C",
                        "#95a5a6"
          )
          
          paste0(
            "<div style='display: flex; justify-content: space-between; align-items: center; margin: 5px 0;'>",
            "<span style='background: ", cor, "; color: white; padding: 3px 10px; border-radius: 15px; font-weight: bold; min-width: 60px; text-align: center;'>Tipo ", tipo, "</span>",
            "<span style='flex: 1; margin: 0 10px;'>",
            "<div style='background: #e0e0e0; border-radius: 10px; height: 20px; position: relative; overflow: hidden;'>",
            "<div style='background: ", cor, "; height: 100%; width: ", pct, "%;'></div>",
            "</div>",
            "</span>",
            "<span style='font-weight: bold; min-width: 80px; text-align: right;'>", qtd, " (", pct, "%)</span>",
            "</div>"
          )
        }),
        collapse = ""
      )
      
      tipos_info <- paste0(
        "<div style='margin-top: 15px; padding: 15px; background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);'>",
        "<h5 style='margin: 0 0 10px 0; color: #1f4e79;'>📊 Distribuição por Tipo de Intervenção:</h5>",
        tipos_html,
        "</div>"
      )
    }
    
    HTML(paste0(
      "<div style='display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-bottom: 15px;'>",
      
      # Card 1: Total
      "<div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; border-radius: 8px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<div style='font-size: 14px; opacity: 0.9; margin-bottom: 5px;'>📊 TOTAL DE REGISTROS</div>",
      "<div style='font-size: 32px; font-weight: bold;'>", format(total_registros, big.mark = ","), "</div>",
      "</div>",
      
      # Card 2: Com Texto
      "<div style='background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 20px; border-radius: 8px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<div style='font-size: 14px; opacity: 0.9; margin-bottom: 5px;'>✅ COM TEXTO</div>",
      "<div style='font-size: 32px; font-weight: bold;'>", format(com_texto, big.mark = ","), "</div>",
      "</div>",
      
      # Card 3: Sem Texto
      "<div style='background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); padding: 20px; border-radius: 8px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<div style='font-size: 14px; opacity: 0.9; margin-bottom: 5px;'>⚠️ SEM TEXTO</div>",
      "<div style='font-size: 32px; font-weight: bold;'>", format(sem_texto, big.mark = ","), "</div>",
      "</div>",
      
      # Card 4: Taxa de Sucesso
      "<div style='background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); padding: 20px; border-radius: 8px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<div style='font-size: 14px; opacity: 0.9; margin-bottom: 5px;'>🎯 TAXA DE SUCESSO</div>",
      "<div style='font-size: 32px; font-weight: bold;'>", taxa_sucesso, "%</div>",
      "</div>",
      
      "</div>",
      
      tipos_info
    ))
  })
  
  output$cruzamento_concluido <- reactive({
    return(!is.null(values$dados_cruzados))
  })
  outputOptions(output, 'cruzamento_concluido', suspendWhenHidden = FALSE)
  
  output$download_cruzado <- downloadHandler(
    filename = function() {
      paste0("dados_cruzados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(values$dados_cruzados, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  #===========================================================================
  # CLASSIFICAÇÃO INDIVIDUAL
  #===========================================================================
  
  observeEvent(values$dados_cruzados, {
    if(!is.null(values$dados_cruzados)) {
      
      notas_disponiveis <- values$dados_cruzados %>%
        select(nota_key, texto_completo) %>%
        filter(!is.na(texto_completo), nchar(trimws(texto_completo)) > 0)
      
      if(nrow(notas_disponiveis) > 0) {
        choices_notas <- setNames(
          as.list(1:nrow(notas_disponiveis)),
          paste0("Nota ", notas_disponiveis$nota_key, " - ", substr(notas_disponiveis$texto_completo, 1, 50), "...")
        )
        
        updateSelectInput(session, "nota_referencia",
                          choices = c("Nenhuma" = "", choices_notas))
      }
    }
  })
  
  observeEvent(input$nota_referencia, {
    if(!is.null(input$nota_referencia) && input$nota_referencia != "") {
      
      idx <- as.integer(input$nota_referencia)
      
      if(!is.na(idx) && idx <= nrow(values$dados_cruzados)) {
        
        registro <- values$dados_cruzados[idx, ]
        
        updateTextAreaInput(session, "texto_individual",
                            value = registro$texto_completo)
        
        if(!is.null(values$col_tip_intervencao) && 
           values$col_tip_intervencao %in% names(registro)) {
          
          tipo_ant <- registro[[values$col_tip_intervencao]]
          
          if(!is.na(tipo_ant)) {
            updateNumericInput(session, "tipo_anterior", value = as.integer(tipo_ant))
          }
        }
      }
    }
  })
  
  observeEvent(input$extrair_assunto_individual, {
    req(input$texto_individual)
    
    if(nchar(trimws(input$texto_individual)) == 0) {
      showNotification("⚠️ Digite um texto para extrair o assunto.", type = "warning")
      return()
    }
    
    withProgress(message = 'Extraindo assunto...', value = 0, {
      
      incProgress(0.5, detail = "Consultando IA...")
      
      assunto <- extrair_assunto_principal(input$texto_individual)
      
      incProgress(1, detail = "Concluído!")
      
      output$assunto_extraido <- renderUI({
        HTML(paste0(
          "<div style='padding: 15px; background: #e3f2fd; border-radius: 8px; border-left: 4px solid #2196F3; margin-top: 10px;'>",
          "<strong style='color: #1565C0;'>📝 Assunto Principal Extraído:</strong><br>",
          "<span style='color: #424242; font-size: 14px; font-weight: 500;'>", assunto, "</span>",
          "</div>"
        ))
      })
      
      showNotification("✅ Assunto extraído!", type = "message", duration = 3)
    })
  })
  
  observeEvent(input$classificar_individual, {
    req(input$texto_individual)
    
    if(nchar(trimws(input$texto_individual)) == 0) {
      showNotification("⚠️ Digite um texto para classificar.", type = "warning")
      return()
    }
    
    withProgress(message = 'Classificando...', value = 0, {
      
      incProgress(0.5, detail = "Consultando IA...")
      
      resultado <- classificar_hibrido(input$texto_individual, CONFIG_USUARIO)
      
      incProgress(1, detail = "Concluído!")
      
      tipo_anterior <- input$tipo_anterior
      tem_comparacao <- !is.na(tipo_anterior) && tipo_anterior >= 1 && tipo_anterior <= 6
      
      cor_criticidade <- switch(resultado$criticidade,
                                "BAIXA" = "#4682B4",
                                "MEDIA" = "#32CD32",
                                "ALTA" = "#FF8C00",
                                "CRITICA" = "#DC143C"
      )
      
      cor_hierarquia <- ifelse(resultado$categoria == "IAZF", "#ff6b35", "#2e8b57")
      
      icone <- switch(as.character(resultado$tipo),
                      "1" = "🧽", "2" = "🔧", "3" = "🔍",
                      "4" = "⏰", "5" = "⚠️", "6" = "🚨"
      )
      
      mudanca_html <- ""
      
      if(tem_comparacao) {
        
        mudou <- tipo_anterior != resultado$tipo
        
        categoria_anterior <- ifelse(tipo_anterior %in% c(5, 6), "IAZF", "PROBLEMAS_COMUNS")
        mudou_categoria <- categoria_anterior != resultado$categoria
        
        ordem_criticidade <- c("BAIXA" = 1, "MEDIA" = 2, "ALTA" = 3, "CRITICA" = 4)
        
        criticidade_anterior <- switch(as.character(tipo_anterior),
                                       "1" = "BAIXA", "2" = "BAIXA",
                                       "3" = "MEDIA", "4" = "MEDIA",
                                       "5" = "ALTA", "6" = "CRITICA"
        )
        
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
            "#ff6b35"
          } else if(nivel_novo < nivel_anterior) {
            "#2e8b57"
          } else {
            "#1f4e79"
          }
          
          texto_mudanca <- if(nivel_novo > nivel_anterior) {
            "AUMENTO DE CRITICIDADE"
          } else if(nivel_novo < nivel_anterior) {
            "REDUÇÃO DE CRITICIDADE"
          } else {
            "MUDANÇA DE TIPO (MESMA CRITICIDADE)"
          }
          
          mudanca_html <- paste0(
            "<div style='background: ", cor_mudanca, "; padding: 15px; border-radius: 8px; margin-bottom: 15px; color: white;'>",
            "<div style='display: flex; align-items: center; justify-content: space-between;'>",
            
            "<div style='flex: 1;'>",
            "<strong style='font-size: 16px;'>", icone_mudanca, " ", texto_mudanca, "</strong><br>",
            "<span style='font-size: 13px; opacity: 0.9;'>Tipo anterior: ", tipo_anterior, " → Tipo sugerido: ", resultado$tipo, "</span>",
            "</div>",
            
            "<div style='text-align: right;'>",
            "<div style='background: rgba(255,255,255,0.2); padding: 8px 15px; border-radius: 20px; display: inline-block;'>",
            "<span style='font-size: 12px;'>", criticidade_anterior, "</span>",
            " → ",
            "<span style='font-size: 12px; font-weight: bold;'>", resultado$criticidade, "</span>",
            "</div>",
            "</div>",
            
            "</div>"
          )
          
          if(mudou_categoria) {
            mudanca_html <- paste0(
              mudanca_html,
              "<div style='margin-top: 10px; padding: 10px; background: rgba(255,255,255,0.15); border-radius: 5px;'>",
              "<strong>⚠️ ATENÇÃO:</strong> Mudança de hierarquia: ",
              "<strong>", categoria_anterior, "</strong> → <strong>", resultado$categoria, "</strong>",
              "</div>"
            )
          }
          
          mudanca_html <- paste0(mudanca_html, "</div>")
          
        } else {
          mudanca_html <- paste0(
            "<div style='background: #d4edda; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #28a745;'>",
            "<strong style='color: #155724; font-size: 16px;'>✅ CLASSIFICAÇÃO CONFIRMADA</strong><br>",
            "<span style='color: #155724; font-size: 13px;'>",
            "A IA confirmou o tipo anterior (", tipo_anterior, "). Não há necessidade de reclassificação.",
            "</span>",
            "</div>"
          )
        }
      }
      
      output$resultado_individual <- renderUI({
        HTML(paste0(
          "<div style='padding: 20px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 10px; border-left: 5px solid ", cor_hierarquia, "; margin-top: 15px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);'>",
          
          "<h4 style='color: #1f4e79; margin-bottom: 20px;'>", icone, " Resultado da Classificação</h4>",
          
          mudanca_html,
          
          # NOVO: Comparação Visual Grande (Tipo Anterior vs Tipo Novo)
          ifelse(tem_comparacao,
                 paste0(
                   "<div style='display: grid; grid-template-columns: 1fr auto 1fr; gap: 20px; align-items: center; margin-bottom: 25px; padding: 20px; background: white; border-radius: 10px; box-shadow: 0 3px 8px rgba(0,0,0,0.12);'>",
                   
                   # Tipo Anterior
                   "<div style='text-align: center; padding: 15px; background: linear-gradient(135deg, #f5f5f5 0%, #e0e0e0 100%); border-radius: 8px;'>",
                   "<div style='color: #666; font-size: 13px; font-weight: bold; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 1px;'>TIPO ANTERIOR</div>",
                   "<div style='font-size: 56px; color: #95a5a6; font-weight: bold; line-height: 1;'>", tipo_anterior, "</div>",
                   "<div style='margin-top: 10px; padding: 5px 15px; background: rgba(149, 165, 166, 0.2); border-radius: 15px; display: inline-block;'>",
                   "<span style='font-size: 12px; color: #666; font-weight: bold;'>", criticidade_anterior, "</span>",
                   "</div>",
                   "</div>",
                   
                   # Seta de Mudança
                   "<div style='text-align: center;'>",
                   "<div style='font-size: 48px; color: ", 
                   ifelse(mudou, 
                          ifelse(nivel_novo > nivel_anterior, "#ff6b35", "#2e8b57"), 
                          "#1f4e79"
                   ), 
                   ";'>",
                   ifelse(mudou, 
                          ifelse(nivel_novo > nivel_anterior, "⬆️", "⬇️"), 
                          "✓"
                   ),
                   "</div>",
                   "<div style='font-size: 11px; color: #666; font-weight: bold; margin-top: 5px;'>",
                   ifelse(mudou,
                          ifelse(nivel_novo > nivel_anterior, "AUMENTOU", "REDUZIU"),
                          "MANTEVE"
                   ),
                   "</div>",
                   "</div>",
                   
                   # Tipo Novo (Sugerido)
                   "<div style='text-align: center; padding: 15px; background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-radius: 8px; border: 2px solid #1f4e79;'>",
                   "<div style='color: #1565c0; font-size: 13px; font-weight: bold; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 1px;'>TIPO SUGERIDO</div>",
                   "<div style='font-size: 56px; color: #1f4e79; font-weight: bold; line-height: 1;'>", resultado$tipo, "</div>",
                   "<div style='margin-top: 10px; padding: 5px 15px; background: ", cor_criticidade, "; border-radius: 15px; display: inline-block;'>",
                   "<span style='font-size: 12px; color: white; font-weight: bold;'>", resultado$criticidade, "</span>",
                   "</div>",
                   "</div>",
                   
                   "</div>"
                 ),
                 ""
          ),
          
          # Cards de Informações (3 colunas)
          "<div style='display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; margin-bottom: 20px;'>",
          
          # Card 1: Tipo SAP
          "<div style='background: white; padding: 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 3px solid #1f4e79;'>",
          "<div style='color: #666; font-size: 12px; font-weight: bold; margin-bottom: 10px; text-transform: uppercase;'>TIPO SAP", 
          ifelse(tem_comparacao, " SUGERIDO", ""), "</div>",
          "<div style='font-size: 36px; color: #1f4e79; font-weight: bold; margin: 10px 0;'>", resultado$tipo, "</div>",
          ifelse(tem_comparacao && tipo_anterior != resultado$tipo,
                 paste0("<div style='font-size: 11px; color: #999; margin-top: 5px; padding: 3px 10px; background: #f5f5f5; border-radius: 10px; display: inline-block;'>Anterior: ", tipo_anterior, "</div>"),
                 ""),
          "</div>",
          
          # Card 2: Confiança
          "<div style='background: white; padding: 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 3px solid #2e8b57;'>",
          "<div style='color: #666; font-size: 12px; font-weight: bold; margin-bottom: 10px; text-transform: uppercase;'>CONFIANÇA</div>",
          "<div style='font-size: 36px; color: #2e8b57; font-weight: bold; margin: 10px 0;'>", resultado$confianca, "%</div>",
          "<div style='font-size: 11px; color: #999; margin-top: 5px; padding: 3px 10px; background: #f5f5f5; border-radius: 10px; display: inline-block;'>",
          ifelse(resultado$confianca >= 90, "Muito Alta",
                 ifelse(resultado$confianca >= 80, "Alta",
                        ifelse(resultado$confianca >= 70, "Média", "Revisar"))),
          "</div>",
          "</div>",
          
          # Card 3: Criticidade
          "<div style='background: white; padding: 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 3px solid ", cor_criticidade, ";'>",
          "<div style='color: #666; font-size: 12px; font-weight: bold; margin-bottom: 10px; text-transform: uppercase;'>CRITICIDADE</div>",
          "<div style='margin: 10px 0;'>",
          "<span style='font-size: 18px; color: white; background: ", cor_criticidade, "; padding: 10px 20px; border-radius: 25px; font-weight: bold; display: inline-block;'>", 
          resultado$criticidade, "</span>",
          "</div>",
          "</div>",
          
          "</div>",
          
          # Badge de Hierarquia
          "<div style='margin-bottom: 20px; text-align: center;'>",
          "<strong style='color: #666; font-size: 14px; margin-right: 10px;'>Hierarquia:</strong>",
          "<span style='background: ", cor_hierarquia, "; color: white; padding: 10px 25px; border-radius: 25px; font-weight: bold; font-size: 16px; box-shadow: 0 2px 5px rgba(0,0,0,0.15);'>",
          resultado$categoria, "</span>",
          "</div>",
          
          # Descrição SAP
          "<div style='background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 15px; border-left: 4px solid #1f4e79;'>",
          "<div style='display: flex; align-items: center; margin-bottom: 10px;'>",
          "<span style='font-size: 24px; margin-right: 10px;'>📖</span>",
          "<strong style='color: #1f4e79; font-size: 15px;'>Descrição SAP</strong>",
          "</div>",
          "<p style='color: #495057; line-height: 1.8; font-size: 14px; margin: 0;'>", resultado$descricao, "</p>",
          "</div>",
          
          # Resumo da Análise
          "<div style='background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #ffc107; box-shadow: 0 2px 5px rgba(0,0,0,0.1);'>",
          "<div style='display: flex; align-items: center; margin-bottom: 10px;'>",
          "<span style='font-size: 24px; margin-right: 10px;'>💡</span>",
          "<strong style='color: #856404; font-size: 15px;'>Resumo da Análise</strong>",
          "</div>",
          "<p style='color: #856404; line-height: 1.8; font-size: 14px; margin: 0;'>", resultado$resumo, "</p>",
          "</div>",
          
          "</div>"
        ))
      })
      
      showNotification("✅ Texto classificado com sucesso!", type = "message", duration = 3)
    })
  })
  
  
  observeEvent(input$limpar_individual, {
    updateTextAreaInput(session, "texto_individual", value = "")
    updateNumericInput(session, "tipo_anterior", value = NA)
    updateSelectInput(session, "nota_referencia", selected = "")
    
    output$assunto_extraido <- renderUI({ NULL })
    output$resultado_individual <- renderUI({
      tags$div(
        style = "text-align: center; padding: 50px; color: #999;",
        icon("robot", style = "font-size: 48px; margin-bottom: 15px;"),
        tags$p("Digite um texto e clique em 'Classificar' para ver o resultado.",
               style = "font-size: 14px;")
      )
    })
  })
  
  output$resultado_individual <- renderUI({
    tags$div(
      style = "text-align: center; padding: 50px; color: #999;",
      icon("robot", style = "font-size: 48px; margin-bottom: 15px;"),
      tags$p("Digite um texto e clique em 'Classificar' para ver o resultado.",
             style = "font-size: 14px;")
    )
  })
  
  
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

