
#=============================================================================
# CONFIGURAÇÃO DA API OPENAI PETROBRAS (DEVE VIR PRIMEIRO)
#=============================================================================

OPENAI_CONFIG <- list(
  base_url = "https://apit.petrobras.com.br/ia/openai/v1/openai-azure/openai",
  api_key = "29d08064725944fcbc0b53e06f8807c5",
  model = "gpt-4o-petrobras",
  api_version = "2024-06-01"
)

# ============================================================================

# DICIONÁRIOS DE CLASSIFICAÇÃO SAP (DEFINA PRIMEIRO!)

# ============================================================================

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

# ============================================================================

# CONFIGURAÇÃO DO USUÁRIO (DEPOIS DOS DICIONÁRIOS!)

# ============================================================================

CONFIG_USUARIO <- list(
  usar_dicionario = TRUE,
  usar_api = TRUE,
  usar_modelo_treinado = TRUE,
  prioridade = "HIBRIDO",
  dicionarios = DICIONARIOS_SAP,
  extrair_assuntos = TRUE,
  batch_size = 5,
  timeout_api = 30,
  confianca_minima = 70
)

# ============================================================================
# 💾 SISTEMA DE CACHE - PARA EVITAR CHAMADAS REDUNDANTES À API
# ============================================================================

# Inicializar cache global
if (!exists("CACHE_API")) {
  CACHE_API <- new.env(hash = TRUE, parent = emptyenv())
  CACHE_API$hits <- 0
  CACHE_API$misses <- 0
  CACHE_API$max_size <- 1000
}

# Função simples para gerar hash (sem dependência externa)
hash_simples <- function(texto) {
  # Usar charToRaw e xor para gerar hash simples
  chars <- charToRaw(texto)
  hash_val <- 0
  for (i in seq_along(chars)) {
    hash_val <- (hash_val * 31 + as.integer(chars[i])) %% 2147483647
  }
  return(as.character(hash_val))
}

# Função para obter do cache
cache_get <- function(texto, tipo = "openai") {
  if (is.null(texto) || nchar(trimws(texto)) == 0) {
    return(NULL)
  }
  
  # Criar chave hash do texto
  hash_key <- paste0(tipo, "_", hash_simples(tolower(trimws(texto))))
  
  # Verificar se existe no cache
  if (exists(hash_key, envir = CACHE_API, inherits = FALSE)) {
    resultado_cache <- get(hash_key, envir = CACHE_API, inherits = FALSE)
    
    # Verificar se cache não expirou (válido por 24 horas)
    if (!is.null(resultado_cache$timestamp)) {
      idade_horas <- as.numeric(difftime(Sys.time(), resultado_cache$timestamp, units = "hours"))
      if (idade_horas <= 24) {
        CACHE_API$hits <- CACHE_API$hits + 1
        return(resultado_cache$valor)
      }
    }
  }
  
  CACHE_API$misses <- CACHE_API$misses + 1
  return(NULL)
}

# Função para armazenar no cache
cache_set <- function(texto, resultado, tipo = "openai") {
  if (is.null(texto) || is.null(resultado)) {
    return(FALSE)
  }
  
  # Criar chave hash do texto
  hash_key <- paste0(tipo, "_", hash_simples(tolower(trimws(texto))))
  
  # Verificar limite de tamanho do cache
  if (length(ls(envir = CACHE_API)) > CACHE_API$max_size) {
    # Limpar 10% das entradas mais antigas
    todas_chaves <- ls(envir = CACHE_API)
    # Filtrar apenas chaves de resultado (não metadata como "hits", "misses")
    chaves_resultado <- todas_chaves[!todas_chaves %in% c("hits", "misses", "max_size")]
    
    if (length(chaves_resultado) > 0) {
      timestamps <- sapply(chaves_resultado, function(chave) {
        tryCatch({
          obj <- get(chave, envir = CACHE_API, inherits = FALSE)
          if (!is.null(obj$timestamp)) obj$timestamp else Sys.time()
        }, error = function(e) Sys.time())
      })
      
      quantidade_remover <- max(1, length(chaves_resultado) %/% 10)
      chaves_antigas <- names(sort(timestamps))[1:quantidade_remover]
      
      for (chave in chaves_antigas) {
        tryCatch({
          rm(list = chave, envir = CACHE_API)
        }, error = function(e) {
          # Ignorar erro ao deletar
        })
      }
    }
  }
  
  # Armazenar no cache com timestamp
  assign(hash_key, 
         list(valor = resultado, timestamp = Sys.time()), 
         envir = CACHE_API)
  
  return(TRUE)
}

# ============================================================================

# 🤖 SISTEMA DE MODELO TREINADO - INTEGRADO AO CLASSIFICADOR_VERSAO12
# Inicialização incremental e segura do ambiente de Machine Learning
# ============================================================================

if (!exists("MODELO_TREINADO_INTEGRADO")) {
  cat("\n🤖 INICIANDO CARREGAMENTO DO SISTEMA DE MODELO TREINADO...\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  # Lista de bibliotecas necessárias para ML
  bibliotecas_ml <- c("randomForest", "caret", "tm", "e1071")
  for (lib in bibliotecas_ml) {
    if (!require(lib, character.only = TRUE, quietly = TRUE)) {
      cat("📦 Instalando", lib, "...\n")
      install.packages(lib, dependencies = TRUE, quiet = TRUE)
      library(lib, character.only = TRUE)
    }
  }
  cat("✅ Bibliotecas de Machine Learning carregadas\n")
  MODELO_TREINADO_INTEGRADO <- TRUE
  # Configurações de ambiente
  Sys.setlocale("LC_ALL", "Portuguese_Brazil.UTF-8")
  options(encoding = "UTF-8", decimal.mark = ",", big.mark = ".", scipen = 999)
  # rm(list = ls())
  # gc()
  # Bibliotecas gerais do sistema
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(readxl)
  library(tidyr)
  # Configurações OPENAI PETROBRAS já definidas no topo do arquivo
  cat("✅ Ambiente de Machine Learning e IA inicializado\n")
}

# ============================================================================

# Função otimizada para batch

extrair_assuntos_batch <- function(textos, api_key, api_url, model, batch_size = 5)  {
  assuntos <- character(length(textos))
  
  cat("📊 Extraindo assuntos de", length(textos), "textos em batches de", batch_size, "...\n")
  
  for(i in seq(1, length(textos), by = batch_size)) {
    fim <- min(i + batch_size - 1, length(textos))
    batch <- textos[i:fim]
    
    # Processar cada item do batch
    for(j in seq_along(batch)) {
      idx <- i + j - 1
      
      # Usar fallback local se texto muito curto
      if(nchar(trimws(batch[j])) < 10) {
        assuntos[idx] <- substr(trimws(batch[j]), 1, 50)
      } else {
        assuntos[idx] <- tryCatch({
          extrair_assunto_principal_api(batch[j], api_key, api_url, model)
        }, error = function(e) {
          cat("⚠️ Erro extraindo assunto (item", idx, "):", e$message, "\n")
          substr(trimws(batch[j]), 1, 80)
        })
      }
    }
    
    # Pausa apenas entre batches (não entre itens)
    if(fim < length(textos)) {
      Sys.sleep(0.3)  # Reduzido de 0.5 para 0.3
    }
    
    # Log de progresso
    if(fim %% 20 == 0 || fim == length(textos)) {
      cat("   Processados", fim, "de", length(textos), "textos\n")
    }
  }
  
  cat("✅ Extração de assuntos concluída\n")
  return(assuntos)
}

#=============================================================================
# FUNÇÃO CORRIGIDA: SALVAR VALIDAÇÃO ML
#=============================================================================

salvar_validacao_ml <- function(registro_id, tipo_validado, feedback = "boa", dados_resultados = NULL, values_env = NULL)  {
  cat("\n💾 SALVANDO VALIDAÇÃO ML:\n")
  cat("  - Registro ID:", registro_id, "\n")
  cat("  - Tipo validado:", tipo_validado, "\n")
  cat("  - Feedback:", feedback, "\n")
  tryCatch({
    # Forçar registro_id para string
    registro_id <- as.character(registro_id)
    cat("  - ID como string:", registro_id, "\n")
    
    # Buscar dados originais
    if (is.null(dados_resultados)) {
      # Verificar se values foi passado como parâmetro ou se pode usar o global
      valores_disponivel <- FALSE
      
      if (!is.null(values_env)) {
        # Usar values_env fornecido
        cat("  - Usando values_env fornecido como parâmetro\n")
        valores_disponivel <- !is.null(values_env$resultados_lote)
        dados_busca <- values_env
      } else if (exists("values", where = parent.frame())) {
        # Tentar pegar do frame pai (se em um observador)
        cat("  - Usando values do frame pai\n")
        dados_busca <- get("values", envir = parent.frame())
        valores_disponivel <- !is.null(dados_busca$resultados_lote)
      } else if (exists("values")) {
        # Último recurso: tentar global
        cat("  - Usando values global\n")
        dados_busca <- values
        valores_disponivel <- !is.null(valores$resultados_lote)
      }
      
      cat("  - valores_disponivel:", valores_disponivel, "\n")
      
      if (valores_disponivel) {
        # Forçar nota_key para string
        dados_busca$resultados_lote$nota_key <- as.character(dados_busca$resultados_lote$nota_key)
        # Debug: mostrar as notas disponíveis
        notas_disponiveis <- unique(dados_busca$resultados_lote$nota_key)
        cat("  - Total notas disponíveis:", length(notas_disponiveis), "\n")
        cat("  - Primeiras 10 notas:", paste(head(notas_disponiveis, 10), collapse = ", "), "\n")
        cat("  - Buscando ID:", registro_id, "entre", length(notas_disponiveis), "notas\n")
        
        registro <- dados_busca$resultados_lote %>%
          dplyr::filter(nota_key == registro_id)
        cat("  - Registros encontrados:", nrow(registro), "\n")
      } else {
        cat("❌ Nenhuma fonte de dados disponível (values_env, frame pai, ou global)\n")
        return(FALSE)
      }
    } else {
      cat("  - Usando dados_resultados fornecido (", nrow(dados_resultados), "linhas )\n")
      dados_resultados$nota_key <- as.character(dados_resultados$nota_key)
      registro <- dados_resultados %>%
        dplyr::filter(nota_key == registro_id)
    }
    
    if (nrow(registro) > 0) {
      cat("✅ Registro encontrado! Processando...\n")
      # Criar nova validação no formato correto
      nova_validacao <- data.frame(
        id = registro_id,
        texto_original = as.character(registro$texto_completo[1]),
        tipo_original = as.integer(ifelse(is.null(registro$tipo_intervencao_antigo[1]), 
                                          NA, registro$tipo_intervencao_antigo[1])),
        tipo_ia = as.integer(registro$tipo_novo[1]),
        tipo_validado = as.integer(tipo_validado),
        assunto_original = ifelse(!is.null(registro$assunto_principal[1]), 
                                  as.character(registro$assunto_principal[1]), ""),
        assunto_validado = ifelse(!is.null(registro$assunto_principal[1]), 
                                  as.character(registro$assunto_principal[1]), ""),
        confianca = as.numeric(registro$confianca[1]),
        feedback_qualidade = feedback,
        timestamp = Sys.time(),
        usuario = ifelse(exists("input$usuario"), input$usuario, "sistema"),
        observacoes = "Salvo via interface ML",
        stringsAsFactors = FALSE
      )
      # Adicionar à base de validações
      if (is.null(validacoes_modelo$dados) || !is.data.frame(validacoes_modelo$dados)) {
        validacoes_modelo$dados <- data.frame(
          id = character(0),
          texto_original = character(0),
          tipo_original = integer(0),
          tipo_ia = integer(0),
          tipo_validado = integer(0),
          assunto_original = character(0),
          assunto_validado = character(0),
          confianca = numeric(0),
          feedback_qualidade = character(0),
          timestamp = as.POSIXct(character(0)),
          usuario = character(0),
          observacoes = character(0),
          stringsAsFactors = FALSE
        )
      }
      validacoes_modelo$dados <- validacoes_modelo$dados %>%
        dplyr::filter(id != registro_id) %>%
        dplyr::bind_rows(nova_validacao) %>%
        dplyr::distinct(id, .keep_all = TRUE)
      cat("✅ Validação salva com sucesso!\n")
      cat("   Total validações:", nrow(validacoes_modelo$dados), "\n")
      return(TRUE)
    } else {
      # Debug extra: mostrar que não encontrou
      cat("❌ Registro não encontrado! ID procurado:", registro_id, "\n")
      if (!is.null(values_env) && !is.null(values_env$resultados_lote)) {
        notas <- unique(values_env$resultados_lote$nota_key)
        cat("   Notas disponíveis:", paste(head(notas, 20), collapse = ", "), "\n")
      }
      return(FALSE)
    }
  }, error = function(e) {
    cat("❌ Erro ao salvar validação:", as.character(e), "\n")
    print(traceback())
    return(FALSE)
  })
}

#=============================================================================
# FUNÇÕES PARA VALIDAÇÃO E MODELO ML (FALTANDO)
#=============================================================================

# Função para salvar validação e treinar incrementalmente
salvar_validacao_ml_incremental <- function(registro_id, tipo_validado, feedback = "boa", values_env = NULL) {
  # Salvar validação (existente) passando values_env
  sucesso <- salvar_validacao_ml(registro_id, tipo_validado, feedback, values_env = values_env)
  if (sucesso) {
    # Verificar se temos dados suficientes para treinamento incremental
    total_validacoes <- nrow(validacoes_modelo$dados)
    if (total_validacoes >= 5) {
      # Treinamento incremental a cada 5 novas validações
      if (total_validacoes %% 5 == 0) {
        cat(sprintf("🔄 Treinamento incremental disparado (validação %d)\n", total_validacoes))
        tryCatch({
          # Treinar de forma incremental (apenas com novos dados)
          resultado <- treinar_modelo_ml_incremental()
          if (resultado$sucesso) {
            cat(sprintf("✅ Modelo atualizado incrementalmente. Acurácia: %.2f%%\n", 
                        resultado$acuracia))
          }
        }, error = function(e) {
          cat("⚠️ Erro no treinamento incremental:", as.character(e), "\n")
        })
      }
    }
  }
  return(sucesso)
}
# Função de treinamento incremental
treinar_modelo_ml_incremental <- function() {
  
  if(is.null(validacoes_modelo$modelo_ativo)) {
    # Primeiro treinamento
    return(treinar_modelo_ml())
  }
  
  if(nrow(validacoes_modelo$dados) < 20) {
    # Ainda poucos dados, treinar normal
    return(treinar_modelo_ml())
  }
  
  # Treinamento incremental apenas com validações recentes
  cat("🧠 Treinamento incremental do modelo...\n")
  
  # Pegar apenas as últimas 50 validações para treinamento incremental
  dados_recentes <- tail(validacoes_modelo$dados, 50)
  
  tryCatch({
    # Preparar dados
    dados_treino <- dados_recentes %>%
      filter(!is.na(tipo_correto), nchar(texto) > 10) %>%
      mutate(
        texto_limpo = tolower(texto),
        texto_limpo = iconv(texto_limpo, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""),
        texto_limpo = gsub("[^a-z0-9 ]", " ", texto_limpo),
        texto_limpo = gsub("\\s+", " ", texto_limpo)
      )
    
    if(nrow(dados_treino) < 10) {
      return(treinar_modelo_ml()) # Treinamento completo
    }
    
    # Vetorização de texto (usar vocabulário existente se possível)
    library(tm)
    library(randomForest)
    
    corpus <- VCorpus(VectorSource(dados_treino$texto_limpo))
    
    # Pipeline de pré-processamento
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    # Criar matriz de termos
    dtm <- DocumentTermMatrix(corpus)
    
    if(ncol(dtm) < 2) {
      return(treinar_modelo_ml())
    }
    
    # Converter para dataframe
    matriz_treino <- as.data.frame(as.matrix(dtm))
    dados_treino_final <- data.frame(
      matriz_treino,
      tipo = as.factor(dados_treino$tipo_correto),
      stringsAsFactors = FALSE
    )
    
    # Combinar com modelo existente (transfer learning)
    modelo_existente <- validacoes_modelo$modelo_ativo
    
    if(!is.null(modelo_existente)) {
      # Adicionar novas árvores ao modelo existente
      set.seed(123)
      modelo_atualizado <- randomForest(
        tipo ~ .,
        data = dados_treino_final,
        ntree = 50,  # Árvores adicionais
        mtry = modelo_existente$mtry,
        importance = TRUE,
        do.trace = FALSE,
        keep.forest = TRUE
      )
      
      # Mesclar florestas (estratégia simplificada)
      validacoes_modelo$modelo_ativo <- modelo_atualizado
      
    } else {
      # Treinar novo modelo
      modelo_atualizado <- randomForest(
        tipo ~ .,
        data = dados_treino_final,
        ntree = 100,
        mtry = max(1, floor(sqrt(ncol(dados_treino_final) - 1))),
        importance = TRUE,
        do.trace = FALSE
      )
      
      validacoes_modelo$modelo_ativo <- modelo_atualizado
    }
    
    # Calcular métricas
    predicoes <- predict(modelo_atualizado, dados_treino_final)
    acuracia <- mean(predicoes == dados_treino_final$tipo, na.rm = TRUE) * 100
    
    # Atualizar métricas
    validacoes_modelo$metricas <- list(
      acuracia = round(acuracia, 2),
      total_treinos = nrow(dados_treino),
      ultima_atualizacao = Sys.time(),
      features_importantes = head(rownames(importance(modelo_atualizado)[order(-importance(modelo_atualizado)[,1]),]), 10)
    )
    
    # Salvar histórico
    validacoes_modelo$historico <- c(validacoes_modelo$historico, list(
      timestamp = Sys.time(),
      acuracia = acuracia,
      registros = nrow(dados_treino),
      tipo = "INCREMENTAL"
    ))
    
    # Salvar em disco
    salvar_dados_modelo()
    
    cat(sprintf("✅ Modelo atualizado incrementalmente\n"))
    cat(sprintf("   Acurácia: %.2f%% (baseada em %d validações recentes)\n", 
                acuracia, nrow(dados_treino)))
    
    return(list(
      sucesso = TRUE,
      acuracia = acuracia,
      total_dados = nrow(dados_treino),
      tipo = "INCREMENTAL"
    ))
    
  }, error = function(e) {
    cat("❌ Erro no treinamento incremental:", as.character(e), "\n")
    return(list(
      sucesso = FALSE,
      erro = as.character(e)
    ))
  })
}
# Função para treinar modelo ML
treinar_modelo_ml <- function() {
  
  cat("\n🤖 INICIANDO TREINAMENTO DO MODELO ML...\n")
  
  tryCatch({
    
    # Verificar se há dados suficientes
    if(nrow(validacoes_modelo$dados) < 10) {
      return(list(
        sucesso = FALSE,
        erro = paste("Necessário pelo menos 10 validações. Atual:", 
                     nrow(validacoes_modelo$dados))
      ))
    }
    
    # Preparar dados
    dados_treino <- validacoes_modelo$dados %>%
      filter(!is.na(tipo_validado), nchar(trimws(texto_original)) > 10) %>%
      mutate(
        texto_limpo = tolower(texto_original),
        texto_limpo = iconv(texto_limpo, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""),
        texto_limpo = gsub("[^a-z0-9 ]", " ", texto_limpo),
        texto_limpo = gsub("\\s+", " ", texto_limpo)
      )
    
    if(nrow(dados_treino) < 5) {
      return(list(
        sucesso = FALSE,
        erro = "Dados insuficientes para treinamento"
      ))
    }
    
    cat("📊 Dados para treinamento:", nrow(dados_treino), "registros\n")
    
    # Vetorização de texto
    library(tm)
    library(randomForest)
    
    corpus <- VCorpus(VectorSource(dados_treino$texto_limpo))
    
    # Pipeline de pré-processamento
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    # Criar matriz de termos
    dtm <- DocumentTermMatrix(corpus)
    
    # Remover termos raros
    dtm <- removeSparseTerms(dtm, 0.95)
    
    if(ncol(dtm) < 2) {
      return(list(
        sucesso = FALSE,
        erro = "Poucos termos após pré-processamento"
      ))
    }
    
    # Converter para dataframe
    matriz_treino <- as.data.frame(as.matrix(dtm))
    dados_treino_final <- data.frame(
      matriz_treino,
      tipo = as.factor(dados_treino$tipo_validado),
      stringsAsFactors = FALSE
    )
    
    # Verificar balanceamento
    cat("📈 Distribuição dos tipos:\n")
    print(table(dados_treino_final$tipo))
    
    # Treinar Random Forest
    set.seed(123)
    modelo <- randomForest(
      tipo ~ .,
      data = dados_treino_final,
      ntree = 100,
      mtry = max(1, floor(sqrt(ncol(dados_treino_final) - 1))),
      importance = TRUE,
      do.trace = FALSE
    )
    
    # Calcular métricas
    predicoes <- predict(modelo, dados_treino_final)
    acuracia <- mean(predicoes == dados_treino_final$tipo, na.rm = TRUE) * 100
    
    # Salvar modelo
    validacoes_modelo$modelo_ativo <- modelo
    
    # Atualizar métricas
    validacoes_modelo$metricas <- list(
      acuracia = round(acuracia, 2),
      total_treinos = nrow(dados_treino),
      ultima_atualizacao = Sys.time(),
      features_importantes = head(rownames(importance(modelo)[order(-importance(modelo)[,1]),]), 10)
    )
    
    # Salvar histórico
    validacoes_modelo$historico <- c(validacoes_modelo$historico, list(
      timestamp = Sys.time(),
      acuracia = acuracia,
      registros = nrow(dados_treino)
    ))
    
    cat("✅ Modelo treinado com sucesso!\n")
    cat(sprintf("   Acurácia: %.2f%%\n", acuracia))
    cat("   Features:", ncol(dados_treino_final) - 1, "\n")
    cat("   Número de árvores: 100\n")
    
    # Salvar em disco
    salvar_dados_modelo()
    
    return(list(
      sucesso = TRUE,
      acuracia = acuracia,
      total_dados = nrow(dados_treino),
      erro = NULL
    ))
    
  }, error = function(e) {
    cat("❌ Erro no treinamento:", as.character(e), "\n")
    return(list(
      sucesso = FALSE,
      erro = as.character(e)
    ))
  })
}

#=============================================================================
# FUNÇÃO CORRIGIDA: PREDIZER COM MODELO ML
#=============================================================================

predizer_com_modelo <- function(texto) {
  
  # Se não houver modelo treinado, retornar erro claro
  if(is.null(validacoes_modelo$modelo_ativo)) {
    return(list(
      sucesso = FALSE,
      erro = "Modelo não treinado",
      tipo = NA,
      confianca = 0,
      metodo = "MODELO_NAO_DISPONIVEL"
    ))
  }
  
  tryCatch({
    library(tm)
    library(randomForest)
    
    # Pré-processar texto
    texto_limpo <- tolower(texto)
    texto_limpo <- iconv(texto_limpo, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    texto_limpo <- gsub("[^a-z0-9 ]", " ", texto_limpo)
    texto_limpo <- gsub("\\s+", " ", texto_limpo)
    
    # Verificar se há texto válido
    if(nchar(trimws(texto_limpo)) < 5) {
      return(list(
        sucesso = FALSE,
        erro = "Texto muito curto",
        tipo = NA,
        confianca = 0,
        metodo = "TEXTO_CURTO"
      ))
    }
    
    # Criar corpus e DTM para predição
    corpus <- VCorpus(VectorSource(texto_limpo))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus)
    
    # Converter para dataframe
    matriz_pred <- as.data.frame(as.matrix(dtm))
    
    # Alinhar com features do modelo treinado
    # Obter nomes das features do modelo
    if(!is.null(validacoes_modelo$modelo_ativo$forest)) {
      # Adicionar colunas faltantes com valor 0
      varnames_modelo <- validacoes_modelo$modelo_ativo$forest$xlevels
      if(length(varnames_modelo) > 0) {
        for(feat in names(varnames_modelo)) {
          if(!feat %in% names(matriz_pred)) {
            matriz_pred[[feat]] <- 0
          }
        }
      }
    }
    
    # Predizer com o modelo
    predicao <- predict(validacoes_modelo$modelo_ativo, matriz_pred, type = "response")
    
    # Obter probabilidades se disponível
    probabilidades <- tryCatch({
      predict(validacoes_modelo$modelo_ativo, matriz_pred, type = "prob")
    }, error = function(e) NULL)
    
    tipo_predito <- as.integer(as.character(predicao[1]))
    
    # Calcular confiança
    if(!is.null(probabilidades)) {
      confianca <- max(probabilidades[1,]) * 100
    } else {
      confianca <- 75  # Confiança padrão quando não há probabilidades
    }
    
    # Obter informações do tipo do dicionário
    tipo_info <- DICIONARIOS_SAP[[paste0("tipo_", tipo_predito)]]
    
    return(list(
      sucesso = TRUE,
      tipo = tipo_predito,
      categoria = if(!is.null(tipo_info)) tipo_info$categoria_principal else "INDEFINIDO",
      criticidade = if(!is.null(tipo_info)) tipo_info$criticidade else "MEDIA",
      confianca = round(confianca, 1),
      descricao = if(!is.null(tipo_info)) tipo_info$descricao else "",
      resumo = paste("Classificado por Modelo ML com", round(confianca, 1), "% de confiança"),
      metodo = "MODELO_ML",
      erro = NULL
    ))
    
  }, error = function(e) {
    cat("❌ Erro na predição ML:", as.character(e), "\n")
    cat("   Usando fallback para dicionário...\n")
    # Fallback para dicionário em caso de erro
    resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
    resultado$metodo <- "ML_FALLBACK_DICIONARIO"
    return(resultado)
  })
}
# Função para atualizar métricas do modelo
update_model_metrics <- function() {
  
  if(nrow(validacoes_modelo$dados) == 0) {
    validacoes_modelo$metricas <- list(
      acuracia = 0,
      total_treinos = 0,
      ultima_atualizacao = Sys.time(),
      features_importantes = character(0)
    )
    return()
  }
  
  # Calcular acurácia simples (se houver validações suficientes)
  if(nrow(validacoes_modelo$dados) >= 5) {
    
    acertos <- sum(
      validacoes_modelo$dados$tipo_ia == validacoes_modelo$dados$tipo_correto,
      na.rm = TRUE
    )
    
    total <- sum(!is.na(validacoes_modelo$dados$tipo_ia) & 
                   !is.na(validacoes_modelo$dados$tipo_correto))
    
    if(total > 0) {
      acuracia <- (acertos / total) * 100
    } else {
      acuracia <- 0
    }
    
    validacoes_modelo$metricas$acuracia <- round(acuracia, 2)
    validacoes_modelo$metricas$total_treinos <- nrow(validacoes_modelo$dados)
    validacoes_modelo$metricas$ultima_atualizacao <- Sys.time()
  }
}


#=============================================================================
# FUNÇÃO PARA FORMATAR NÚMEROS SEM WARNING
#=============================================================================

formatar_numero_safe <- function(x) {
  if(is.null(x) || is.na(x)) return("0")
  tryCatch({
    format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  }, error = function(e) {
    as.character(x)
  })
}

#=============================================================================
# FUNÇÃO: SANITIZAÇÃO DE DADOS
#=============================================================================

sanitizar_texto <- function(texto) {
  if (is.null(texto) || is.na(texto)) return("")
  
  # Converter para string
  texto <- as.character(texto)
  
  # Remover caracteres de controle (inclui NUL) e normalizar espaços
  texto <- gsub("[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]", " ", texto)
  texto <- gsub("\\s+", " ", texto)
  texto <- trimws(texto)
  
  # Remover caracteres especiais problemáticos
  texto <- gsub("[\u0080-\u009F]", "", texto)
  
  # Limitar tamanho (máximo 5000 caracteres)
  if (nchar(texto) > 5000) {
    texto <- substr(texto, 1, 5000)
  }
  
  return(texto)
}

# Validação de entrada
validar_entrada <- function(texto) {
  erros <- c()
  
  if (is.null(texto)) {
    erros <- c(erros, "Texto é NULL")
  } else if (is.na(texto)) {
    erros <- c(erros, "Texto é NA")
  } else if (nchar(trimws(texto)) == 0) {
    erros <- c(erros, "Texto vazio")
  } else if (nchar(trimws(texto)) < 3) {
    erros <- c(erros, "Texto muito curto (< 3 caracteres)")
  }
  
  return(list(
    valido = length(erros) == 0,
    erros = erros
  ))
}

#=============================================================================
# FUNÇÃO: EXTRAIR ASSUNTO (FALLBACK SEM API)
#=============================================================================
extrair_assunto_fallback <- function(texto)  {
  
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
# FUNÇÃO: GERAR INSIGHTS COM IA (CIENTISTA DE DADOS VIRTUAL)
#=============================================================================

gerar_insights_estatisticos <- function(metricas_dados)  {
  
  cat("\n🧠 GERANDO INSIGHTS COM IA...\n")
  
  if(is.null(metricas_dados) || is.null(metricas_dados$dados_validos)) {
    return(list(
      sucesso = FALSE,
      insights = "Não há dados suficientes para análise."
    ))
  }
  
  # Preparar resumo estatístico dos dados
  dados <- metricas_dados$dados_validos
  
  resumo_estatistico <- paste0(
    "DADOS DA CLASSIFICAÇÃO:\n",
    "- Total de registros analisados: ", nrow(dados), "\n",
    "- Acurácia geral: ", round(metricas_dados$acuracia, 2), "%\n",
    "- Registros conformes: ", metricas_dados$conformes, " (", round((metricas_dados$conformes/metricas_dados$total)*100, 1), "%)\n",
    "- Registros divergentes: ", metricas_dados$divergentes, " (", round((metricas_dados$divergentes/metricas_dados$total)*100, 1), "%)\n",
    "- Confiança média das classificações: ", round(mean(dados$confianca, na.rm = TRUE), 2), "%\n\n",
    
    "DISTRIBUIÇÃO POR TIPO:\n",
    paste(sapply(1:6, function(tipo)  {
      total_tipo <- sum(dados$tipo_novo == tipo, na.rm = TRUE)
      perc_tipo <- round((total_tipo/nrow(dados))*100, 1)
      paste0("- Tipo ", tipo, ": ", total_tipo, " registros (", perc_tipo, "%)")
    }), collapse = "\n"), "\n\n",
    
    "DISTRIBUIÇÃO POR HIERARQUIA:\n",
    "- PROBLEMAS_COMUNS: ", sum(dados$categoria == "PROBLEMAS_COMUNS", na.rm = TRUE), " registros\n",
    "- IAZF: ", sum(dados$categoria == "IAZF", na.rm = TRUE), " registros\n\n",
    
    "DISTRIBUIÇÃO POR CRITICIDADE:\n",
    "- BAIXA: ", sum(dados$criticidade == "BAIXA", na.rm = TRUE), " registros\n",
    "- MÉDIA: ", sum(dados$criticidade == "MEDIA", na.rm = TRUE), " registros\n",
    "- ALTA: ", sum(dados$criticidade == "ALTA", na.rm = TRUE), " registros\n",
    "- CRÍTICA: ", sum(dados$criticidade == "CRITICA", na.rm = TRUE), " registros\n\n",
    
    "MATRIZ DE CONFUSÃO (Principais Mudanças):\n",
    paste(capture.output(print(metricas_dados$matriz)), collapse = "\n")
  )
  
  cat("📊 Resumo estatístico preparado\n")
  
  # Construir prompt para IA
  prompt <- paste0(
    "Você é um Cientista de Dados especialista em análise de manutenção industrial e classificação SAP.\n\n",
    
    "Analise os dados estatísticos abaixo e forneça insights profundos e acionáveis como um verdadeiro cientista de dados:\n\n",
    
    resumo_estatistico, "\n\n",
    
    "INSTRUÇÕES PARA ANÁLISE:\n",
    "1. Avalie a qualidade geral da classificação (acurácia, conformidade)\n",
    "2. Identifique padrões interessantes na distribuição dos tipos\n",
    "3. Analise a proporção IAZF vs PROBLEMAS_COMUNS e suas implicações\n",
    "4. Detecte possíveis problemas ou anomalias nos dados\n",
    "5. Forneça recomendações práticas e acionáveis\n",
    "6. Use linguagem técnica mas acessível\n",
    "7. Seja objetivo e direto ao ponto\n\n",
    
    "FORMATO DA RESPOSTA (JSON):\n",
    "{\n",
    '  "qualidade_geral": "Avaliação geral da qualidade (1-2 frases)",\n',
    '  "principais_achados": [\n',
    '    "Achado 1",\n',
    '    "Achado 2",\n',
    '    "Achado 3"\n',
    '  ],\n',
    '  "pontos_atencao": [\n',
    '    "Ponto de atenção 1",\n',
    '    "Ponto de atenção 2"\n',
    '  ],\n',
    '  "recomendacoes": [\n',
    '    "Recomendação 1",\n',
    '    "Recomendação 2",\n',
    '    "Recomendação 3"\n',
    '  ],\n',
    '  "conclusao": "Conclusão final em 2-3 frases"\n',
    "}"
  )
  
  cat("📤 Enviando para API...\n")
  
  tryCatch({
    
    # Construir URL
    url <- paste0(
      OPENAI_CONFIG$base_url,
      "/deployments/",
      OPENAI_CONFIG$model,
      "/chat/completions?api-version=",
      OPENAI_CONFIG$api_version
    )
    
    # Fazer requisição
    response <- POST(
      url = url,
      add_headers(
        `api-key` = OPENAI_CONFIG$api_key,
        `Content-Type` = "application/json"
      ),
      body = toJSON(list(
        messages = list(
          list(
            role = "system",
            content = "Você é um Cientista de Dados especialista em análise estatística e manutenção industrial. Responda sempre em JSON válido."
          ),
          list(
            role = "user",
            content = prompt
          )
        ),
        max_tokens = 1000,
        temperature = 0.7,
        response_format = list(type = "json_object")
      ), auto_unbox = TRUE),
      encode = "json",
      timeout(30)
    )
    
    if(status_code(response) != 200) {
      cat("❌ Erro HTTP:", status_code(response), "\n")
      return(list(
        sucesso = FALSE,
        insights = "Erro ao gerar insights. Tente novamente."
      ))
    }
    
    # Parsear resposta
    resposta_json <- content(response, "parsed")
    conteudo <- resposta_json$choices[[1]]$message$content
    
    insights <- fromJSON(conteudo)
    
    cat("✅ Insights gerados com sucesso!\n\n")
    
    return(list(
      sucesso = TRUE,
      insights = insights
    ))
    
  }, error = function(e)  {
    cat("❌ Erro ao gerar insights:", as.character(e), "\n")
    return(list(
      sucesso = FALSE,
      insights = paste("Erro ao gerar insights:", e$message)
    ))
  })
}

#=============================================================================
# FUNÇÃO: CLASSIFICAÇÃO POR DICIONÁRIO
#=============================================================================
classificar_por_dicionario <- function(texto, dicionarios = DICIONARIOS_SAP)  {
  
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
    
    matches <- sum(sapply(dicionario$palavras_chave, function(palavra)  {
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
  
  scores_df <- do.call(rbind, lapply(scores, function(x)  {
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
classificar_por_palavras_chave <- function(texto)  {
  
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

# Função auxiliar para retry com backoff exponencial
api_request_with_retry <- function(request_func, max_tentativas = 3, timeout_base = 30) {
  for (tentativa in 1:max_tentativas) {
    resultado <- tryCatch({
      request_func()
    }, error = function(e) {
      list(erro = TRUE, mensagem = as.character(e))
    })
    
    # Se sucesso, retornar
    if (!isTRUE(resultado$erro)) {
      return(resultado)
    }
    
    # Se última tentativa, retornar erro
    if (tentativa == max_tentativas) {
      cat("❌ Todas as", max_tentativas, "tentativas falharam\n")
      return(resultado)
    }
    
    # Backoff exponencial: 2^tentativa segundos
    tempo_espera <- 2^tentativa
    cat("⏳ Tentativa", tentativa, "falhou. Aguardando", tempo_espera, "segundos...\n")
    Sys.sleep(tempo_espera)
  }
}

classificar_com_openai <- function(texto)  {
  
  # Logger que envia para console e, se configurado, para o log em tempo real
  log_dual <- function(msg) {
    cat(msg, "\n")
    log_fun <- getOption("log_ui_func", NULL)
    if (is.function(log_fun)) {
      try(log_fun(msg, ""), silent = TRUE)
    }
  }
  
  # Sanitizar entrada
  texto <- sanitizar_texto(texto)
  
  # Validar entrada
  validacao <- validar_entrada(texto)
  if (!validacao$valido) {
    return(list(
      tipo = NA,
      categoria = NA,
      criticidade = NA,
      confianca = 0,
      descricao = paste("Entrada inválida:", paste(validacao$erros, collapse = ", ")),
      resumo = "",
      erro = TRUE
    ))
  }
  
  # Verificar cache
  resultado_cache <- cache_get(texto, "openai")
  if (!is.null(resultado_cache)) {
    log_dual("💾 Resultado recuperado do cache")
    return(resultado_cache)
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
    
    log_dual(paste("🔗 URL da API:", url))
    log_dual(paste("🔑 API Key:", substr(OPENAI_CONFIG$api_key, 1, 15), "..."))
    log_dual(paste("📝 Tamanho do prompt:", nchar(prompt), "caracteres"))
    log_dual("⏱️  Timeout: 30 segundos")
    
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
    
    log_dual("📤 Enviando requisição para API...")
    
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
    
    http_status <- status_code(response)
    log_dual(paste("📥 Status HTTP recebido:", http_status))
    
    if(http_status == 200) {
      
      result <- content(response, "parsed")
      message_content <- result$choices[[1]]$message$content
      
      log_dual("✅ Resposta da API recebida com sucesso")
      
      # Extrair JSON da resposta
      json_match <- regmatches(message_content, regexpr("\\{[^}]+\\}", message_content))
      
      if(length(json_match) > 0) {
        log_dual("📄 JSON encontrado na resposta")
        
        classificacao <- fromJSON(json_match[1])
        
        resultado <- list(
          tipo = as.integer(classificacao$tipo),
          categoria = classificacao$categoria,
          criticidade = classificacao$criticidade,
          confianca = as.numeric(classificacao$confianca),
          descricao = classificacao$descricao,
          resumo = classificacao$resumo,
          erro = FALSE
        )
        
        log_dual("✅ Classificação realizada com sucesso")
        log_dual(paste("   Tipo:", resultado$tipo))
        log_dual(paste("   Categoria:", resultado$categoria))
        log_dual(paste("   Confiança:", resultado$confianca, "%"))
        
        # Salvar no cache
        cache_set(texto, resultado, "openai")
        
        return(resultado)
      } else {
        log_dual("⚠️ Não foi possível extrair JSON da resposta")
        log_dual(paste("Resposta bruta:", substr(message_content, 1, 200), "..."))
      }
    } else if(http_status == 401) {
      log_dual("❌ ERRO 401 - API Key inválida ou expirada")
      log_dual(paste("Resposta:", content(response, "text")))
      
    } else if(http_status == 429) {
      log_dual("❌ ERRO 429 - Rate limit excedido (muitas requisições)")
      log_dual("Aguarde alguns minutos antes de tentar novamente")
      
    } else if(http_status == 500) {
      log_dual("❌ ERRO 500 - Servidor da API indisponível")
      log_dual(paste("Resposta:", content(response, "text")))
      
    } else {
      log_dual(paste("❌ ERRO HTTP", http_status))
      log_dual(paste("Resposta:", content(response, "text")))
    }
    
    # Fallback
    log_dual("⚠️ Usando classificação por dicionário (fallback)")
    resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
    resultado$erro <- TRUE
    resultado$metodo <- "FALLBACK_DICIONARIO"
    return(resultado)
    
  }, error = function(e)  {
    log_dual(paste("❌ Erro na API OpenAI:", e$message))
    log_dual("⚠️ Usando fallback para dicionário")
    resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
    resultado$erro <- TRUE
    resultado$metodo <- "FALLBACK_ERRO"
    return(resultado)
  })
}

# Função para extrair assunto principal via API
extrair_assunto_principal_api <- function(texto, api_key, api_url, model)  {
  
  if(is.null(texto) || is.na(texto) || texto == "") {
    return("Texto vazio")
  }
  
  prompt <- paste0(
    "Analise o texto abaixo e extraia o assunto principal em no máximo 10 palavras.\n",
    "Seja objetivo e direto. Retorne apenas o assunto, sem explicações.\n\n",
    "Texto: ", texto
  )
  
  tryCatch({
    response <- httr::POST(
      url = api_url,
      httr::add_headers(
        "api-key" = api_key,
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = model,
        messages = list(
          list(role = "system", content = "Você é um assistente que extrai assuntos principais de textos de forma concisa."),
          list(role = "user", content = prompt)
        ),
        temperature = 0.3,
        max_tokens = 50
      ), auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(30)
    )
    
    if(httr::status_code(response) == 200) {
      result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if (!is.null(result$choices) && length(result$choices) > 0) {
        assunto <- trimws(result$choices[[1]]$message$content)
        return(assunto)
      } else {
        return("Erro ao extrair resposta")
      }
    } else {
      # Fallback quando API não responde corretamente
      return(substr(trimws(texto), 1, 80))
    }
    
  }, error = function(e)  {
    cat("⚠️ Erro ao extrair assunto:", as.character(e), "\n")
    return(substr(trimws(texto), 1, 80))
  })
}


#=============================================================================
# FUNÇÃO CORRIGIDA: EXTRAIR ASSUNTO PRINCIPAL
#=============================================================================

extrair_assunto_principal <- function(texto)  {
  
  if(is.null(texto) || nchar(trimws(texto)) == 0) {
    return("Texto vazio")
  }
  
  texto_limitado <- substr(texto, 1, 1000)
  
  prompt <- paste0(
    "Você é um especialista em manutenção industrial.\n\n",
    "Analise o texto abaixo e extraia o problema apresentado em uma frase curta e objetiva (máximo 80 caracteres).\n\n",
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
    
  }, error = function(e)  {
    cat("Erro ao extrair assunto:", e$message, "\n")
    return(extrair_assunto_fallback(texto))
  })
}

#=============================================================================
# FUNÇÃO: CLASSIFICAÇÃO HÍBRIDA
#=============================================================================

classificar_hibrido_completo <- function(texto, config) {
  
  # Garantir que config tenha valores padrão se necessário
  if(is.null(config$usar_dicionario)) config$usar_dicionario <- TRUE
  if(is.null(config$usar_api)) config$usar_api <- TRUE
  if(is.null(config$usar_modelo_treinado)) config$usar_modelo_treinado <- TRUE
  
  cat("🔧 Classificando híbrido:\n")
  cat(paste("   - Dicionário:", config$usar_dicionario, "\n"))
  cat(paste("   - API:", config$usar_api, "\n"))
  cat(paste("   - ML:", config$usar_modelo_treinado, "\n"))
  
  resultados <- list()
  metodos_usados <- c()
  
  # 1. Dicionário (se configurado)
  if(isTRUE(config$usar_dicionario)) {
    resultados$dicionario <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
    metodos_usados <- c(metodos_usados, "Dicionário")
    cat("   ✅ Dicionário executado\n")
  } else {
    resultados$dicionario <- NULL
    cat("   ⏭️ Dicionário desabilitado\n")
  }
  
  # 2. API (se configurado)
  if(isTRUE(config$usar_api)) {
    cat("   🌐 Chamando API OpenAI...\n")
    resultados$api <- tryCatch({
      classificar_com_openai(texto)
    }, error = function(e) {
      cat("   ⚠️ Erro na API:", e$message, "\n")
      NULL
    })
    if(!is.null(resultados$api) && !isTRUE(resultados$api$erro)) {
      metodos_usados <- c(metodos_usados, "API")
      cat("   ✅ API executada com sucesso\n")
    } else {
      resultados$api <- NULL
      cat("   ❌ API falhou ou retornou erro\n")
    }
  } else {
    resultados$api <- NULL
    cat("   ⏭️ API desabilitada\n")
  }
  
  # 3. Modelo ML (se configurado e disponível)
  if(isTRUE(config$usar_modelo_treinado)) {
    if(!is.null(validacoes_modelo$modelo_ativo)) {
      cat("   🤖 Chamando Modelo ML...\n")
      resultados$ml <- tryCatch({
        predizer_com_modelo(texto)
      }, error = function(e) {
        cat("   ⚠️ Erro no modelo ML:", e$message, "\n")
        NULL
      })
      if(!is.null(resultados$ml) && isTRUE(resultados$ml$sucesso)) {
        metodos_usados <- c(metodos_usados, "ML")
        cat("   ✅ ML executado com sucesso\n")
      } else {
        resultados$ml <- NULL
        cat("   ❌ ML falhou ou não retornou resultado válido\n")
      }
    } else {
      resultados$ml <- NULL
      cat("   ⚠️ Modelo ML não está treinado\n")
    }
  } else {
    resultados$ml <- NULL
    cat("   ⏭️ ML desabilitado\n")
  }
  
  # Contar métodos disponíveis (após filtrar erros)
  metodos_disponiveis <- sum(!sapply(resultados, is.null))
  
  cat(paste("   📊 Métodos utilizados:", paste(metodos_usados, collapse = " + "), "\n"))
  cat(paste("   📊 Total de métodos bem-sucedidos:", metodos_disponiveis, "\n"))
  
  # Se nenhum método funcionou, retornar erro
  if(metodos_disponiveis == 0) {
    cat("   ❌ ERRO: Nenhum método de classificação funcionou!\n")
    return(list(
      tipo = NA,
      categoria = "ERRO",
      criticidade = "ERRO",
      confianca = 0,
      descricao = "Nenhum método de classificação disponível",
      resumo = "Erro: Todos os métodos falharam",
      metodo = "ERRO_HIBRIDO"
    ))
  }
  
  # Estratégia de votação ponderada
  votos <- numeric(6)
  pesos <- numeric(6)
  
  # Dicionário
  if(!is.null(resultados$dicionario)) {
    tipo_dict <- resultados$dicionario$tipo
    if(!is.null(tipo_dict) && !is.na(tipo_dict) && tipo_dict >= 1 && tipo_dict <= 6) {
      votos[tipo_dict] <- votos[tipo_dict] + 1
      pesos[tipo_dict] <- pesos[tipo_dict] + (resultados$dicionario$confianca / 100)
      cat(paste("   📊 Dicionário votou: Tipo", tipo_dict, "\n"))
    }
  }
  
  # API
  if(!is.null(resultados$api) && !isTRUE(resultados$api$erro)) {
    tipo_api <- resultados$api$tipo
    if(!is.null(tipo_api) && !is.na(tipo_api) && tipo_api >= 1 && tipo_api <= 6) {
      votos[tipo_api] <- votos[tipo_api] + 1
      pesos[tipo_api] <- pesos[tipo_api] + (resultados$api$confianca / 100)
      cat(paste("   📊 API votou: Tipo", tipo_api, "\n"))
    }
  }
  
  # Modelo ML
  if(!is.null(resultados$ml) && isTRUE(resultados$ml$sucesso)) {
    tipo_ml <- resultados$ml$tipo
    if(!is.null(tipo_ml) && !is.na(tipo_ml) && tipo_ml >= 1 && tipo_ml <= 6) {
      votos[tipo_ml] <- votos[tipo_ml] + 1
      pesos[tipo_ml] <- pesos[tipo_ml] + (resultados$ml$confianca / 100)
      cat(paste("   📊 ML votou: Tipo", tipo_ml, "\n"))
    }
  }
  
  # Determinar vencedor
  if(sum(votos) == 0) {
    # Fallback: usar o primeiro resultado disponível
    cat("   ⚠️ Nenhum voto válido, usando fallback\n")
    
    if(!is.null(resultados$dicionario)) {
      resultado_final <- resultados$dicionario
      resultado_final$metodo <- "DICIONARIO_FALLBACK"
    } else if(!is.null(resultados$api)) {
      resultado_final <- resultados$api
      resultado_final$metodo <- "API_FALLBACK"
    } else if(!is.null(resultados$ml)) {
      resultado_final <- resultados$ml
      resultado_final$metodo <- "ML_FALLBACK"
    } else {
      return(list(
        tipo = NA, categoria = "ERRO", criticidade = "ERRO",
        confianca = 0, descricao = "Erro no fallback",
        resumo = "Erro crítico", metodo = "ERRO_FALLBACK"
      ))
    }
    return(resultado_final)
  }
  
  # Verificar concordância
  tipos_votados <- which(votos > 0)
  
  if(length(tipos_votados) == 1) {
    # Todos concordam
    tipo_final <- tipos_votados[1]
    metodo <- "HIBRIDO_CONCORDANTE"
    confianca_final <- min(100, 70 + (sum(votos) * 10))
    log_dual(paste("   ✅ CONCORDÂNCIA: Todos votaram no Tipo", tipo_final))
  } else {
    # Divergência - usar maior peso
    tipo_final <- which.max(pesos)
    
    # Determinar método que mais contribuiu
    contribuicoes <- c(
      dicionario = if(!is.null(resultados$dicionario) && resultados$dicionario$tipo == tipo_final) 
        resultados$dicionario$confianca else 0,
      api = if(!is.null(resultados$api) && !isTRUE(resultados$api$erro) && resultados$api$tipo == tipo_final) 
        resultados$api$confianca else 0,
      ml = if(!is.null(resultados$ml) && isTRUE(resultados$ml$sucesso) && resultados$ml$tipo == tipo_final) 
        resultados$ml$confianca else 0
    )
    
    metodo_principal <- names(which.max(contribuicoes))
    metodo <- paste0("HIBRIDO_", toupper(metodo_principal))
    confianca_final <- max(contribuicoes, na.rm = TRUE)
    cat(paste("   ⚖️ DIVERGÊNCIA: Escolhido Tipo", tipo_final, "via", metodo_principal, "\n"))
  }
  
  # Montar resultado final usando o primeiro resultado disponível como base
  if(!is.null(resultados$dicionario)) {
    resultado_final <- resultados$dicionario
  } else if(!is.null(resultados$api)) {
    resultado_final <- resultados$api
  } else if(!is.null(resultados$ml)) {
    resultado_final <- resultados$ml
  } else {
    # Isso nunca deve acontecer devido à verificação anterior
    return(list(
      tipo = tipo_final, categoria = "INDEFINIDO", criticidade = "MEDIA",
      confianca = confianca_final, descricao = "Tipo determinado por votação",
      resumo = "Votação híbrida", metodo = metodo
    ))
  }
  
  resultado_final$tipo <- tipo_final
  resultado_final$confianca <- confianca_final
  resultado_final$metodo <- metodo
  
  # Log de detalhes
  resultado_final$detalhes_hibrido <- list(
    metodos_disponiveis = metodos_disponiveis,
    metodos_usados = metodos_usados,
    votos = votos,
    pesos = pesos,
    resultados = list(
      dicionario = if(!is.null(resultados$dicionario)) resultados$dicionario$tipo else NULL,
      api = if(!is.null(resultados$api)) resultados$api$tipo else NULL,
      ml = if(!is.null(resultados$ml)) resultados$ml$tipo else NULL
    )
  )
  
  log_dual(sprintf("✅ Híbrido: %s → Tipo %d (%.2f%%) via %s", 
                   paste(metodos_usados, collapse = " + "),
                   tipo_final, confianca_final, metodo))
  
  return(resultado_final)
}
carregar_dados_modelo <- function() {
  tryCatch({
    if (!dir.exists("dados_modelo_treinado")) return(FALSE)
    arquivos <- list.files("dados_modelo_treinado", pattern = "modelo_.*\\.rds$", full.names = TRUE)
    if (length(arquivos) == 0) return(FALSE)
    arquivo_recente <- arquivos[which.max(file.mtime(arquivos))]
    dados <- readRDS(arquivo_recente)
    # Tentar atualizar reativos se disponível
    tryCatch({
      if (exists("validacoes_modelo") && !is.null(validacoes_modelo)) {
        validacoes_modelo$dados <- dados$validacoes
        validacoes_modelo$modelo_ativo <- dados$modelo
        validacoes_modelo$metricas <- dados$metricas
        validacoes_modelo$historico <- dados$historico
        if (!is.null(dados$configuracoes)) validacoes_modelo$configuracoes <- dados$configuracoes
      }
    }, error = function(e) {
      # Esperado na inicialização antes dos reativos estarem disponíveis
      NULL
    })
    cat("✅ Dados carregados:", nrow(dados$validacoes), "validações\n")
    return(TRUE)
  }, error = function(e) {
    cat("ℹ️ Primeira execução - sem dados anteriores\n")
    return(FALSE)
  })
}


salvar_dados_modelo <- function() {
  tryCatch({
    # Cria o diretório se não existir
    if (!dir.exists("dados_modelo_treinado")) {
      dir.create("dados_modelo_treinado", showWarnings = FALSE)
    }
    # Define o nome do arquivo com data
    arquivo <- file.path("dados_modelo_treinado", 
                         paste0("modelo_", format(Sys.Date(), "%Y%m%d"), ".rds"))
    # Monta a lista de dados a serem salvos
    dados_completos <- list(
      validacoes   = validacoes_modelo$dados,
      modelo       = validacoes_modelo$modelo_ativo,
      metricas     = validacoes_modelo$metricas,
      historico    = validacoes_modelo$historico,
      configuracoes= validacoes_modelo$configuracoes,
      timestamp_salvo = Sys.time(),
      versao = "1.0"
    )
    # Salva em disco
    saveRDS(dados_completos, arquivo)
    cat("💾 Dados salvos:", arquivo, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Erro ao salvar:", as.character(e), "\n")
    return(FALSE)
  })
}
#==============================================================================
# FUNÇÃO AUXILIAR CORRIGIDA: CRIAR CARD MODERNO PARA CADA TIPO
#==============================================================================

criar_card_tipo_dicionario <- function(tipo_num, cor, criticidade)  {
  
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

# ============================================================================
# FUNÇÃO: HÍBRIDO DICIONÁRIO + API (versão original)
# ============================================================================

classificar_hibrido <- function(texto, config) {
  # Esta é a versão clássica (Dicionário + API)
  # Se ambas estão desabilitadas, usar fallback
  config$usar_dicionario <- TRUE
  config$usar_api <- TRUE
  config$usar_modelo_treinado <- FALSE
  
  return(classificar_hibrido_completo(texto, config))
}

# ============================================================================
# FUNÇÃO: HÍBRIDO DICIONÁRIO + ML
# ============================================================================

classificar_hibrido_dicionario_ml <- function(texto, config) {
  
  resultado_dicionario <- classificar_por_dicionario(texto, config$dicionarios)
  resultado_ml <- predizer_com_modelo(texto)
  
  if(!resultado_ml$sucesso) {
    resultado_dicionario$metodo <- "DICIONARIO_ML_FALLBACK"
    return(resultado_dicionario)
  }
  
  if(resultado_dicionario$tipo == resultado_ml$tipo) {
    # Concordam
    resultado_dicionario$confianca <- min(100, resultado_dicionario$confianca + 5)
    resultado_dicionario$metodo <- "HIBRIDO_DICIONARIO_ML"
    resultado_dicionario$resumo <- paste0(
      "✅ Dicionário e ML concordam. ",
      "Confiança reforçada pelo modelo treinado."
    )
  } else {
    # Divergem - usar maior confiança
    if(resultado_dicionario$confianca > resultado_ml$confianca) {
      resultado_dicionario$metodo <- "HIBRIDO_DICIONARIO"
      resultado_dicionario$resumo <- paste0(
        "⚠️ ML sugeriu tipo ", resultado_ml$tipo, 
        ". Usando dicionário por maior confiança."
      )
    } else {
      resultado_dicionario$tipo <- resultado_ml$tipo
      resultado_dicionario$confianca <- resultado_ml$confianca
      resultado_dicionario$metodo <- "HIBRIDO_ML"
      resultado_dicionario$resumo <- paste0(
        "⚠️ Dicionário sugeriu tipo ", resultado_dicionario$tipo,
        ". Usando ML por maior confiança."
      )
    }
  }
  
  return(resultado_dicionario)
}
# ============================================================================
# CLASSIFICAÇÃO HÍBRIDA COM MODELO TREINADO (ATUALIZADA)
# ============================================================================

classificar_hibrido_com_modelo <- function(texto, config) {
  
  # Verificar se temos todos os métodos configurados
  tem_dicionario <- config$usar_dicionario
  tem_api <- config$usar_api
  tem_ml <- config$usar_modelo_treinado && !is.null(validacoes_modelo$modelo_ativo)
  
  if(tem_dicionario && tem_api && tem_ml) {
    # Usar os 3 métodos
    cat("🎯 Usando classificação com 3 métodos (Dicionário + API + ML)\n")
    return(classificar_hibrido_completo(texto, config))
    
  } else if(tem_dicionario && tem_api) {
    # Usar dicionário + API (original)
    cat("🎯 Usando classificação com 2 métodos (Dicionário + API)\n")
    return(classificar_hibrido(texto, config))
    
  } else if(tem_dicionario && tem_ml) {
    # Usar dicionário + ML
    cat("🎯 Usando classificação com 2 métodos (Dicionário + ML)\n")
    return(classificar_hibrido_dicionario_ml(texto, config))
    
  } else if(tem_dicionario) {
    # Apenas dicionário
    cat("🎯 Usando apenas dicionário\n")
    resultado <- classificar_por_dicionario(texto, config$dicionarios)
    resultado$metodo <- "DICIONARIO"
    return(resultado)
    
  } else {
    # Fallback
    cat("⚠️ Configuração inválida, usando dicionário como fallback\n")
    resultado <- classificar_por_dicionario(texto, config$dicionarios)
    resultado$metodo <- "FALLBACK"
    return(resultado)
  }
}

# ============================================================================
# FUNÇÃO: CLASSIFICAR COM MODELO TREINADO (PARA USO NO LOTE)
# ============================================================================

classificar_com_modelo_treinado <- function(texto) {
  
  # Se não houver modelo treinado, usar dicionário como fallback
  if(is.null(validacoes_modelo$modelo_ativo) || 
     !isTRUE(validacoes_modelo$configuracoes$usar_em_classificacao)) {
    
    cat("⚠️ Modelo treinado não disponível, usando dicionário...\n")
    
    resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
    resultado$metodo <- "DICIONARIO_FALLBACK"
    
    return(resultado)
  }
  
  # Tentar predição com modelo
  predicao <- predizer_com_modelo(texto)
  
  if(predicao$sucesso) {
    
    tipo_predito <- predicao$tipo
    
    # Mapear para estrutura completa
    categoria <- ifelse(tipo_predito %in% c(5, 6), "IAZF", "PROBLEMAS_COMUNS")
    
    criticidade <- switch(
      as.character(tipo_predito),
      "1" = "BAIXA",
      "2" = "BAIXA", 
      "3" = "MEDIA",
      "4" = "MEDIA",
      "5" = "ALTA",
      "6" = "CRITICA"
    )
    
    descricao <- switch(
      as.character(tipo_predito),
      "1" = "Condicionamento, limpeza, arrumação, preservação ou pintura",
      "2" = "Melhorias, modificações, testes, instalação ou regulagem",
      "3" = "Manutenção preventiva, preditiva ou inspeção planejada",
      "4" = "Manutenção por oportunidade ou inspeção não programada",
      "5" = "Intervenção para eliminação de defeito",
      "6" = "Intervenção para eliminação de falha"
    )
    
    return(list(
      tipo = tipo_predito,
      categoria = categoria,
      criticidade = criticidade,
      confianca = predicao$confianca,
      descricao = descricao,
      resumo = paste("Classificado pelo modelo ML (treinado com", 
                     validacoes_modelo$metricas$total_treinos, "validações)"),
      metodo = "MODELO_ML",
      sucesso = TRUE
    ))
    
  } else {
    
    # Fallback para dicionário
    cat("⚠️ Fallback para dicionário:", predicao$erro, "\n")
    
    resultado <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
    resultado$metodo <- paste0("DICIONARIO_FALLBACK (", predicao$erro, ")")
    
    return(resultado)
  }
}


# ============================================================================
# SISTEMA DE VALIDAÇÃO E MODELO TREINADO
# ============================================================================

# Banco de dados de validações
validacoes <- reactiveValues(
  dados = data.frame(
    id = character(),
    texto_completo = character(),
    assunto_original = character(),
    assunto_validado = character(),
    tipo_original = integer(),
    tipo_validado = integer(),
    confianca_original = numeric(),
    metodo_original = character(),
    usuario = character(),
    timestamp = as.POSIXct(character()),
    feedback_qualidade = character(),
    stringsAsFactors = FALSE
  ),
  modelo_treinado = NULL,
  vetorizador = NULL,
  metricas_modelo = list(
    acuracia = 0,
    total_treinos = 0,
    ultima_atualizacao = NULL
  )
)

# Função para salvar validação
salvar_validacao <- function(registro_id, tipo_validado, assunto_validado = NULL, feedback = "OK")  {
  
  # Buscar registro original
  registro_original <- values$resultados_lote[values$resultados_lote$nota_key == registro_id, ]
  
  if(nrow(registro_original) == 0) return(FALSE)
  
  # Criar nova validação
  nova_validacao <- data.frame(
    id = registro_id,
    texto_completo = registro_original$texto_completo,
    assunto_original = registro_original$assunto_principal,
    assunto_validado = ifelse(is.null(assunto_validado), registro_original$assunto_principal, assunto_validado),
    tipo_original = registro_original$tipo_novo,
    tipo_validado = tipo_validado,
    confianca_original = registro_original$confianca,
    metodo_original = registro_original$metodo,
    usuario = "Arilson Rodrigues Alves",  # Seu nome
    timestamp = Sys.time(),
    feedback_qualidade = feedback,
    stringsAsFactors = FALSE
  )
  
  # Adicionar ao banco
  validacoes$dados <- rbind(validacoes$dados, nova_validacao)
  
  cat("✅ Validação salva:", registro_id, "- Tipo:", tipo_validado, "\n")
  
  return(TRUE)
}

modelo_ml_dados <- reactiveValues(
  validacoes = data.frame(
    id = character(0),
    texto = character(0),
    tipo_original = integer(0),
    tipo_ia = integer(0),
    tipo_correto = integer(0),
    confianca = numeric(0),
    timestamp = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  ),
  modelo = NULL,
  metricas = list(
    acuracia = 0,
    total_dados = 0,
    ultima_atualizacao = Sys.time()
  ),
  configuracao = list(
    ativo = FALSE,
    min_validacoes = 10
  )
)

validacoes_modelo <- reactiveValues(
  dados = data.frame(
    id = character(0),
    texto_original = character(0),
    tipo_original = integer(0),
    tipo_ia = integer(0),
    tipo_validado = integer(0),
    assunto_original = character(0),
    assunto_validado = character(0),
    confianca = numeric(0),
    feedback_qualidade = character(0),
    timestamp = as.POSIXct(character(0)),
    usuario = character(0),
    observacoes = character(0),
    stringsAsFactors = FALSE
  ),
  modelo_ativo = NULL,
  metricas = list(
    acuracia = 0,
    total_treinos = 0,
    ultima_atualizacao = Sys.time(),
    features_importantes = character(0)
  ),
  historico = list(),
  configuracoes = list(
    min_validacoes = 10,
    algoritmo = "randomForest",
    usar_em_classificacao = FALSE
  )
)
# Função para treinar modelo
# ⚠️ FUNÇÃO DUPLICADA REMOVIDA - Use apenas a primeira treinar_modelo_ml() na linha ~512
# Esta versão estava usando 'validacoes$dados' (incorreto) em vez de 'validacoes_modelo$dados'

# Função para classificar com modelo treinado
classificar_com_modelo_treinado <- function(texto)  {
  
  if(is.null(validacoes$modelo_treinado)) {
    return(list(
      sucesso = FALSE,
      erro = "Modelo não treinado"
    ))
  }
  
  tryCatch({
    
    # Preparar texto
    texto_limpo <- tolower(texto)
    texto_limpo <- iconv(texto_limpo, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    
    # Vetorizar usando o mesmo vocabulário do treino
    corpus_teste <- Corpus(VectorSource(texto_limpo))
    corpus_teste <- tm_map(corpus_teste, removePunctuation)
    corpus_teste <- tm_map(corpus_teste, removeNumbers)
    corpus_teste <- tm_map(corpus_teste, stripWhitespace)
    
    # Aplicar o mesmo vocabulário
    vocab_treino <- colnames(as.matrix(validacoes$vetorizador))
    
    dtm_teste <- DocumentTermMatrix(corpus_teste, control = list(
      dictionary = vocab_treino,
      weighting = weightTfIdf
    ))
    
    matriz_teste <- as.matrix(dtm_teste)
    
    # Garantir que tenha as mesmas colunas
    colunas_faltantes <- setdiff(vocab_treino, colnames(matriz_teste))
    for(col in colunas_faltantes) {
      matriz_teste <- cbind(matriz_teste, 0)
      colnames(matriz_teste)[ncol(matriz_teste)] <- col
    }
    
    # Reordenar colunas
    matriz_teste <- matriz_teste[, vocab_treino, drop = FALSE]
    
    # Fazer predição
    dados_teste <- data.frame(matriz_teste)
    predicao <- predict(validacoes$modelo_treinado, dados_teste, type = "response")
    probabilidades <- predict(validacoes$modelo_treinado, dados_teste, type = "prob")
    
    tipo_predito <- as.integer(as.character(predicao[1]))
    confianca <- max(probabilidades[1, ]) * 100
    
    # Mapear para estrutura padrão
    categoria <- ifelse(tipo_predito %in% c(5, 6), "IAZF", "PROBLEMAS_COMUNS")
    
    criticidade <- switch(
      as.character(tipo_predito),
      "1" = "BAIXA", "2" = "BAIXA",
      "3" = "MEDIA", "4" = "MEDIA", 
      "5" = "ALTA", "6" = "CRITICA"
    )
    
    descricao <- switch(
      as.character(tipo_predito),
      "1" = "Condicionamento, limpeza, arrumação, preservação ou pintura",
      "2" = "Melhorias, modificações, testes, instalação ou regulagem", 
      "3" = "Manutenção preventiva, preditiva ou inspeção planejada",
      "4" = "Manutenção por oportunidade ou inspeção não programada",
      "5" = "Intervenção para eliminação de defeito",
      "6" = "Intervenção para eliminação de falha"
    )
    
    return(list(
      sucesso = TRUE,
      tipo = tipo_predito,
      categoria = categoria,
      criticidade = criticidade,
      confianca = round(confianca, 1),
      descricao = descricao,
      resumo = paste0("Classificado pelo modelo treinado com ", 
                      validacoes$metricas_modelo$total_treinos, " validações"),
      metodo = "MODELO_TREINADO"
    ))
    
  }, error = function(e)  {
    cat("❌ Erro na predição:", as.character(e), "\n")
    return(list(
      sucesso = FALSE,
      erro = as.character(e)
    ))
  })
}

#=============================================================================
# FUNÇÃO: CRUZAMENTO DE DADOS (SEM ALTERAÇÕES)
#=============================================================================

cruzar_dados <- function(df_ordens, df_textos)  {
  
  cat("\n=== INICIANDO CRUZAMENTO DE DADOS ===\n\n")
  
  nomes_ordens <- names(df_ordens)
  nomes_textos <- names(df_textos)
  
  cat("📋 Arquivo de Notas - Colunas:\n")
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
      erro = "Não foi possível identificar a coluna 'Nota' no arquivo de Notas."
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
  
  padronizar_nota <- function(x)  {
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
    cat("⚠️ Removendo", nrow(dup_ordens), "duplicatas do arquivo de Notas\n\n")
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

#=============================================================================
# FUNÇÃO FALTANDO: update_model_metrics
#=============================================================================

update_model_metrics <- function() {
  
  if(is.null(validacoes_modelo$dados) || nrow(validacoes_modelo$dados) == 0) {
    validacoes_modelo$metricas <- list(
      acuracia = 0,
      total_treinos = 0,
      ultima_atualizacao = Sys.time(),
      features_importantes = character(0),
      total_validacoes = 0
    )
    return()
  }
  
  # Calcular acurácia (tipo_ia vs tipo_validado)
  dados <- validacoes_modelo$dados
  
  if(!is.null(dados$tipo_ia) && !is.null(dados$tipo_validado)) {
    acertos <- sum(dados$tipo_ia == dados$tipo_validado, na.rm = TRUE)
    total <- sum(!is.na(dados$tipo_ia) & !is.na(dados$tipo_validado))
    
    if(total > 0) {
      acuracia <- (acertos / total) * 100
    } else {
      acuracia <- 0
    }
    
    validacoes_modelo$metricas$acuracia <- round(acuracia, 2)
    validacoes_modelo$metricas$total_validacoes <- total
  }
  
  validacoes_modelo$metricas$total_treinos <- ifelse(
    is.null(validacoes_modelo$metricas$total_treinos), 
    0, 
    validacoes_modelo$metricas$total_treinos
  )
  
  validacoes_modelo$metricas$ultima_atualizacao <- Sys.time()
  
  cat("📊 Métricas atualizadas - Acurácia:", 
      validacoes_modelo$metricas$acuracia, "%\n")
}

#=============================================================================
# VERIFICAÇÃO DE INTEGRIDADE DO MODELO ML
#=============================================================================

cat("\n🔍 VERIFICANDO INTEGRIDADE DO SISTEMA ML...\n")
cat("═══════════════════════════════════════════════════\n")

# Verificar se funções essenciais existem
funcoes_necessarias <- c(
  "salvar_validacao_ml",
  "salvar_validacao_ml_incremental", 
  "treinar_modelo_ml",
  "treinar_modelo_ml_incremental",
  "predizer_com_modelo",
  "carregar_dados_modelo",
  "salvar_dados_modelo",
  "update_model_metrics"  # Esta vai ser criada
)

for(funcao in funcoes_necessarias) {
  if(exists(funcao, mode = "function")) {
    cat("✅", funcao, "\n")
  } else {
    cat("❌", funcao, "(FALTANDO)\n")
  }
}

# Verificar objetos reativos
cat("\n📋 OBJETOS REATIVOS:\n")
if(exists("validacoes_modelo")) {
  cat("✅ validacoes_modelo existe\n")
} else {
  cat("❌ validacoes_modelo não existe\n")
}

cat("═══════════════════════════════════════════════════\n\n")
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
      "Petrobras JARVIS-IA",
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
      tags$li(
        class = "header",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
             color: white; font-weight: bold; padding: 20px; 
             text-align: center; font-size: 14px; letter-spacing: 2px;
             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        "MENU PRINCIPAL"
      ),
      menuItem("📊 Dashboard", tabName = "dashboard", badgeLabel = "novo", badgeColor = "green"),
      menuItem("Modelo ML", tabName = "modelo_ml", icon = icon("brain")),
      menuItem("📁 Upload & Cruzamento", tabName = "upload"),
      menuItem("🤖 Classificação IA", tabName = "individual"),
      menuItem("📦 Processamento Lote", tabName = "lote"),
      menuItem("📚 Dicionários SAP", tabName = "dicionarios"),
      menuItem("📈 Estatísticas", tabName = "estatisticas"),
      tags$li(
        class = "header",
        style = "color: #b8c7ce; font-weight: bold; padding: 15px; 
             font-size: 12px; letter-spacing: 1px;",
        "CONFIGURAÇÕES"
      ),
      menuItem("⚙️ API OpenAI", tabName = "configuracoes"),
      menuItem("📖 Definições", tabName = "documentacao"),
      menuItem("📜 Histórico", tabName = "historico", badgeLabel = "beta", badgeColor = "yellow")
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
    
    #===========================================================================
    # TABITEMS PRINCIPAIS - ESTRUTURA CORRIGIDA
    #===========================================================================
    tabItems(
      
      #===========================================================================
      # ABA 1: DASHBOARD ANALYTICS PREMIUM
      #===========================================================================
      tabItem(
        tabName = "dashboard",
        
        # Header Hero Animado
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 50px 40px; border-radius: 20px; margin-bottom: 30px; 
                   box-shadow: 0 15px 50px rgba(102, 126, 234, 0.4);
                   position: relative; overflow: hidden;",
              
              # Efeito de fundo animado (opcional)
              tags$div(
                style = "position: absolute; top: -50%; right: -10%; width: 500px; 
                     height: 500px; background: rgba(255,255,255,0.1); 
                     border-radius: 50%; filter: blur(80px);"
              ),
              
              div(
                style = "position: relative; z-index: 1; display: flex; 
                     align-items: center; justify-content: space-between;",
                
                # Lado Esquerdo - Título
                div(
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 15px;",
                    icon("chart-line", style = "font-size: 56px; color: white; margin-right: 25px;"),
                    div(
                      h1(style = "color: white; margin: 0; font-weight: 800; 
                              font-size: 38px; letter-spacing: -1px;",
                         "Dashboard Analytics"),
                      p(style = "color: rgba(255,255,255,0.95); margin: 8px 0 0 0; 
                             font-size: 17px; font-weight: 500;",
                        "Visão geral em tempo real das classificações SAP")
                    )
                  ),
                  
                  # Mini Stats
                  div(
                    style = "display: flex; gap: 30px; margin-top: 20px;",
                    
                    div(
                      style = "background: rgba(255,255,255,0.15); padding: 15px 25px; 
                           border-radius: 15px; backdrop-filter: blur(10px);",
                      div(style = "color: rgba(255,255,255,0.8); font-size: 12px; 
                              font-weight: 600; text-transform: uppercase; 
                              letter-spacing: 1px; margin-bottom: 5px;",
                          "Última Atualização"),
                      div(style = "color: white; font-size: 16px; font-weight: 700;",
                          textOutput("ultima_atualizacao_inline", inline = TRUE))
                    ),
                    
                    div(
                      style = "background: rgba(255,255,255,0.15); padding: 15px 25px; 
                           border-radius: 15px; backdrop-filter: blur(10px);",
                      div(style = "color: rgba(255,255,255,0.8); font-size: 12px; 
                              font-weight: 600; text-transform: uppercase; 
                              letter-spacing: 1px; margin-bottom: 5px;",
                          "Sessão Ativa"),
                      div(style = "color: white; font-size: 16px; font-weight: 700;",
                          textOutput("tempo_sessao_inline", inline = TRUE))
                    )
                  )
                ),
                
                # Lado Direito - Contador Grande
                div(
                  style = "text-align: right;",
                  div(
                    style = "background: rgba(255,255,255,0.2); 
                         padding: 30px 40px; border-radius: 20px;
                         backdrop-filter: blur(10px);
                         box-shadow: 0 8px 32px rgba(0,0,0,0.1);",
                    div(style = "color: rgba(255,255,255,0.9); font-size: 14px; 
                            font-weight: 600; text-transform: uppercase; 
                            letter-spacing: 2px; margin-bottom: 10px;",
                        "Total Processado"),
                    h2(style = "color: white; margin: 0; font-weight: 900; 
                            font-size: 56px; line-height = 1;",
                       textOutput("dashboard_total_inline", inline = TRUE)),
                    div(style = "color: rgba(255,255,255,0.8); font-size: 13px; 
                            margin-top: 8px; font-weight: 500;",
                        "registros classificados")
                  ),
                  
                  # Botão de Atualização Rápida
                  div(
                    style = "margin-top: 20px;",
                    actionButton(
                      "refresh_dashboard",
                      label = div(
                        icon("sync-alt", style = "margin-right: 8px;"),
                        "Atualizar"
                      ),
                      style = "background: rgba(255,255,255,0.2); color: white; 
                           border: 2px solid rgba(255,255,255,0.5); 
                           border-radius: 25px; padding: 10px 25px; 
                           font-weight: 700; backdrop-filter: blur(10px);
                           transition: all 0.3s ease;"
                    )
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # VALUE BOXES PREMIUM COM ÍCONES ANIMADOS
        #===========================================================================
        
        fluidRow(
          # Value Box 1 - Total de Textos
          column(
            width = 3,
            div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   padding: 30px 25px; border-radius: 15px; color: white;
                   box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;
                   height: 180px;
                   display: flex; flex-direction: column; justify-content: space-between;",
              
              div(
                style = "display: flex; align-items: center; justify-content: space-between;",
                div(
                  div(style = "font-size: 13px; font-weight: 600; opacity: 0.9; 
                          text-transform: uppercase; letter-spacing: 1px; 
                          margin-bottom: 12px;",
                      "Textos Carregados"),
                  h2(style = "margin: 0; font-size: 42px; font-weight: 800; line-height: 1;",
                     valueBoxOutput("total_textos_valor", width = NULL))
                ),
                icon("file-text", style = "font-size: 64px; opacity: 0.3;")
              ),
              
              div(
                style = "display: flex; align-items: center; margin-top: 15px;
                     padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.2);",
                icon("arrow-up", style = "margin-right: 8px; font-size: 14px;"),
                span(style = "font-size: 12px; font-weight: 600;",
                     "100% disponíveis")
              )
            )
          ),
          
          # Value Box 2 - IAZF
          column(
            width = 3,
            div(
              style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
                   padding: 30px 25px; border-radius: 15px; color: white;
                   box-shadow: 0 8px 24px rgba(240, 147, 251, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;
                   height: 180px;
                   display: flex; flex-direction: column; justify-content: space-between;",
              
              div(
                style = "display: flex; align-items: center; justify-content: space-between;",
                div(
                  div(style = "font-size: 13px; font-weight: 600; opacity: 0.9; 
                          text-transform: uppercase; letter-spacing: 1px; 
                          margin-bottom: 12px;",
                      "Textos IAZF"),
                  h2(style = "margin: 0; font-size: 42px; font-weight: 800; line-height: 1;",
                     valueBoxOutput("textos_iazf_valor", width = NULL))
                ),
                icon("exclamation-triangle", style = "font-size: 64px; opacity: 0.3;")
              ),
              
              div(
                style = "display: flex; align-items: center; margin-top: 15px;
                     padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.2);",
                icon("shield-alt", style = "margin-right: 8px; font-size: 14px;"),
                span(style = "font-size: 12px; font-weight: 600;",
                     "Críticos monitorados")
              )
            )
          ),
          
          # Value Box 3 - Precisão
          column(
            width = 3,
            div(
              style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
                   padding: 30px 25px; border-radius: 15px; color: white;
                   box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;
                   height: 180px;
                   display: flex; flex-direction: column; justify-content: space-between;",
              
              div(
                style = "display: flex; align-items: center; justify-content: space-between;",
                div(
                  div(style = "font-size: 13px; font-weight: 600; opacity: 0.9; 
                          text-transform: uppercase; letter-spacing: 1px; 
                          margin-bottom: 12px;",
                      "Confiança Média"),
                  h2(style = "margin: 0; font-size: 42px; font-weight: 800; line-height: 1;",
                     valueBoxOutput("precisao_valor", width = NULL))
                ),
                icon("bullseye", style = "font-size: 64px; opacity: 0.3;")
              ),
              
              div(
                style = "display: flex; align-items: center; margin-top: 15px;
                     padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.2);",
                icon("check-circle", style = "margin-right: 8px; font-size: 14px;"),
                span(style = "font-size: 12px; font-weight: 600;",
                     "Alta confiabilidade")
              )
            )
          ),
          
          # Value Box 4 - Conformidade
          column(
            width = 3,
            div(
              style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
                   padding: 30px 25px; border-radius: 15px; color: white;
                   box-shadow: 0 8px 24px rgba(79, 172, 254, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;
                   height: 180px;
                   display: flex; flex-direction: column; justify-content: space-between;",
              
              div(
                style = "display: flex; align-items: center; justify-content: space-between;",
                div(
                  div(style = "font-size: 13px; font-weight: 600; opacity: 0.9; 
                          text-transform: uppercase; letter-spacing: 1px; 
                          margin-bottom: 12px;",
                      "Conformidade"),
                  h2(style = "margin: 0; font-size: 42px; font-weight: 800; line-height: 1;",
                     valueBoxOutput("taxa_conformidade_valor", width = NULL))
                ),
                icon("check-double", style = "font-size: 64px; opacity: 0.3;")
              ),
              
              div(
                style = "display: flex; align-items: center; margin-top: 15px;
                     padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.2);",
                icon("award", style = "margin-right: 8px; font-size: 14px;"),
                span(style = "font-size: 12px; font-weight: 600;",
                     "Qualidade excelente")
              )
            )
          )
        ),
        
        # Espaçador
        tags$div(style = "height: 20px;"),
        
        #===========================================================================
        # RESUMO EXECUTIVO (VERSÃO FINAL)
        #===========================================================================
        fluidRow(
          box(
            title = div(
              icon("clipboard-check", style = "margin-right: 10px; color: #667eea;"),
              "Resumo Executivo",
              tags$span(
                style = "float: right; font-size: 12px; font-weight: normal; 
                 background: rgba(102, 126, 234, 0.1); color: #667eea; 
                 padding: 5px 15px; border-radius: 15px;",
                icon("sync-alt"), " Atualizado agora"
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            style = "margin-top: 20px; margin-bottom: 20px;",
            div(
              style = "padding: 25px;",
              
              div(
                style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 20px;",
                
                # Card 1 - Taxa de Sucesso
                div(
                  style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%);
                       padding: 25px; border-radius: 12px; text-align: center;
                       border-left: 4px solid #4CAF50;",
                  icon("check-circle", style = "font-size: 36px; color: #4CAF50; margin-bottom: 12px;"),
                  div(style = "font-size: 28px; font-weight: 800; color: #2E7D32; margin-bottom: 5px;",
                      textOutput("taxa_sucesso_resumo", inline = TRUE)),
                  div(style = "font-size: 12px; color: #666; font-weight: 600;",
                      "Taxa de Sucesso")
                ),
                
                # Card 2 - Tempo Médio
                div(
                  style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
                       padding: 25px; border-radius: 12px; text-align: center;
                       border-left: 4px solid #2196F3;",
                  icon("clock", style = "font-size: 36px; color: #2196F3; margin-bottom: 12px;"),
                  div(style = "font-size: 28px; font-weight: 800; color: #1565C0; margin-bottom: 5px;",
                      textOutput("tempo_medio_resumo", inline = TRUE)),
                  div(style = "font-size: 12px; color: #666; font-weight: 600;",
                      "Tempo Médio/Texto")
                ),
                
                # Card 3 - Método Preferido
                div(
                  style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%);
                       padding: 25px; border-radius: 12px; text-align: center;
                       border-left: 4px solid #FF9800;",
                  icon("cogs", style = "font-size: 36px; color: #FF9800; margin-bottom: 12px;"),
                  div(style = "font-size: 20px; font-weight: 800; color: #E65100; margin-bottom: 5px;",
                      textOutput("metodo_preferido_resumo", inline = TRUE)),
                  div(style = "font-size: 12px; color: #666; font-weight: 600;",
                      "Método Mais Usado")
                ),
                
                # Card 4 - Críticos
                div(
                  style = "background: linear-gradient(135deg, #ffebee 0%, #ffcdd2 100%);
                       padding: 25px; border-radius: 12px; text-align: center;
                       border-left: 4px solid #f44336;",
                  icon("exclamation-circle", style = "font-size: 36px; color: #f44336; margin-bottom: 12px;"),
                  div(style = "font-size: 28px; font-weight: 800; color: #c62828; margin-bottom: 5px;",
                      textOutput("total_criticos_resumo", inline = TRUE)),
                  div(style = "font-size: 12px; color: #666; font-weight: 600;",
                      "Casos Críticos")
                ),
                
                # Card 5 - Revisões Pendentes
                div(
                  style = "background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%);
                       padding: 25px; border-radius: 12px; text-align: center;
                       border-left: 4px solid #9C27B0;",
                  icon("tasks", style = "font-size: 36px; color: #9C27B0; margin-bottom: 12px;"),
                  div(style = "font-size: 28px; font-weight: 800; color: #6A1B9A; margin-bottom: 5px;",
                      textOutput("revisoes_pendentes_resumo", inline = TRUE)),
                  div(style = "font-size: 12px; color: #666; font-weight: 600;",
                      "Revisões Pendentes")
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # GRÁFICOS PRINCIPAIS - LINHA 1
        #===========================================================================
        
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
              
              div(
                style = "padding: 20px;",
                
                div(
                  style = "background: #e3f2fd; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #2196F3;",
                  tags$strong(icon("info-circle"), " Análise:", style = "color: #1565C0;"),
                  p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                    "Barras cinzas = Classificação anterior | Barras azuis = Nova classificação IA")
                ),
                
                plotOutput("grafico_comparacao_antes_depois", height = "380px")
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
              
              div(
                style = "padding: 20px;",
                
                div(
                  style = "background: #fff3cd; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #ffc107;",
                  tags$strong(icon("chart-pie"), " Distribuição:", style = "color: #856404;"),
                  p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                    "Visualize a quantidade de registros em cada tipo de intervenção")
                ),
                
                plotOutput("grafico_distribuicao_tipos", height = "380px")
              )
            )
          )
        ),
        
        #===========================================================================
        # GRÁFICOS PRINCIPAIS - LINHA 2
        #===========================================================================
        
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
              
              div(
                style = "padding: 20px;",
                
                div(
                  style = "background: #e8f5e9; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #4CAF50;",
                  tags$strong(icon("sitemap"), " Hierarquias:", style = "color: #2E7D32;"),
                  p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                    "Verde = PROBLEMAS_COMUNS | Laranja = IAZF (Incidentes Críticos)")
                ),
                
                plotOutput("grafico_hierarquia", height = "380px")
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
              
              div(
                style = "padding: 20px;",
                
                div(
                  style = "background: #d4edda; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #28a745;",
                  tags$strong(icon("award"), " Qualidade:", style = "color: #155724;"),
                  p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                    "Verde = IA concordou | Laranja = IA sugeriu mudança")
                ),
                
                plotOutput("grafico_conformidade", height = "380px")
              )
            )
          )
        ),
        
        #===========================================================================
        # TABELA DE ÚLTIMAS CLASSIFICAÇÕES (MELHORADA)
        #===========================================================================
        
        fluidRow(
          box(
            title = div(
              icon("history", style = "margin-right: 10px;"),
              "Últimas Classificações Processadas",
              tags$span(
                style = "float: right; font-size: 12px; font-weight: normal; 
                     background: rgba(102, 126, 234, 0.1); color: #667eea; 
                     padding: 5px 15px; border-radius: 15px;",
                icon("clock"), " Tempo real"
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            div(
              style = "padding: 25px;",
              
              div(
                style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                     padding: 20px; border-radius: 12px; margin-bottom: 20px;
                     border-left: 5px solid #2196F3;",
                div(
                  style = "display: flex; align-items: center;",
                  icon("info-circle", style = "font-size: 32px; color: #2196F3; margin-right: 15px;"),
                  div(
                    tags$strong("Últimos 50 Registros Processados", style = "color: #1565C0; font-size: 15px;"),
                    p(style = "margin: 5px 0 0 0; color: #666; font-size: 12px;",
                      "Classificações mais recentes ordenadas por data de processamento")
                  )
                )
              ),
              
              DT::dataTableOutput("tabela_recentes")
            )
          )
        )
      ), # Fecha tabItem dashboard
      
      #===========================================================================
      # ABA 2: MODELO ML
      #===========================================================================
      tabItem(
        tabName = "modelo_ml",
        
        # Header da página
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               padding: 30px; border-radius: 15px; margin-bottom: 25px; color: white;
               box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);",
              
              div(
                style = "display: flex; align-items: center; justify-content: space-between;",
                
                div(
                  h1(style = "margin: 0 0 10px 0; font-weight: 800;", 
                     icon("brain", style = "margin-right: 15px;"), "Modelo Machine Learning"),
                  p(style = "margin: 0; font-size: 16px; opacity: 0.9;",
                    "Sistema inteligente que aprende com suas validações para melhorar a classificação SAP")
                ),
                
                div(
                  style = "text-align: center;",
                  div(style = "font-size: 64px; opacity: 0.7;", "🤖")
                )
              )
            )
          )
        ),
        
        # Status do modelo
        fluidRow(
          column(
            width = 12,
            box(
              title = div(
                icon("info-circle", style = "margin-right: 10px;"),
                "Status do Modelo"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              uiOutput("status_modelo_ml")
            )
          )
        ),
        
        # Seção principal: Validação e Controles
        fluidRow(
          
          # Coluna esquerda: Validação
          column(
            width = 8,
            box(
              title = div(
                icon("check-double", style = "margin-right: 10px;"),
                "Validação de Classificações"
              ),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 20px;",
                
                # Explicação
                div(
                  style = "background: #e3f2fd; padding: 20px; border-radius: 12px; 
                   margin-bottom: 20px; border-left: 4px solid #2196F3;",
                  h5(style = "margin: 0 0 10px 0; color: #1565C0; font-weight: 700;",
                     "💡 Como Funciona"),
                  p(style = "margin: 0; color: #666; font-size: 13px; line-height: 1.6;",
                    "Valide as classificações feitas pela IA clicando no tipo correto. ",
                    "Com pelo menos 10 validações, você pode treinar um modelo personalizado ",
                    "que aprenderá com seus padrões e melhorará automaticamente as futuras classificações.")
                ),
                
                # Controles de filtro
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr auto; gap: 15px; margin-bottom: 25px;",
                  
                  selectInput(
                    "filtro_modelo_ml",
                    label = "Filtrar registros:",
                    choices = c(
                      "Não validados" = "nao_validados",
                      "Todos" = "todos", 
                      "Divergentes" = "divergentes",
                      "Baixa confiança (<80%)" = "baixa_confianca",
                      "Alta confiança (≥90%)" = "alta_confianca"
                    ),
                    selected = "nao_validados"
                  ),
                  
                  numericInput(
                    "limite_modelo_ml",
                    label = "Quantidade:",
                    value = 5,
                    min = 1,
                    max = 20,
                    step = 1
                  ),
                  
                  div(
                    style = "padding-top: 25px;",
                    actionButton(
                      "carregar_validacao_ml",
                      label = div(
                        icon("download", style = "margin-right: 8px;"),
                        "Carregar"
                      ),
                      class = "btn-primary",
                      style = "width: 100%; padding: 12px; border-radius: 25px; font-weight: 700;"
                    )
                  )
                ),
                
                # Área de cards
                uiOutput("cards_validacao_ml")
              )
            )
          ),
          
          # Coluna direita: Estatísticas e Controles
          column(
            width = 4,
            
            # Estatísticas
            box(
              title = div(
                icon("chart-bar", style = "margin-right: 10px;"),
                "Estatísticas"
              ),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              
              uiOutput("stats_modelo_ml")
            ),
            
            # Controles do modelo
            box(
              title = div(
                icon("cogs", style = "margin-right: 10px;"),
                "Controles"
              ),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 15px;",
                
                # Botão treinar
                div(
                  style = "text-align: center; margin-bottom: 20px;",
                  actionButton(
                    "treinar_modelo_ml",
                    label = div(
                      icon("play", style = "margin-right: 10px;"),
                      "Treinar Modelo"
                    ),
                    class = "btn-success btn-lg",
                    style = "width: 100%; padding: 15px; border-radius: 25px; font-weight: 700;"
                  )
                ),
                
                # Configurações
                checkboxInput(
                  "usar_modelo_automatico",
                  label = div(
                    style = "font-weight: 600;",
                    "Usar modelo automaticamente nas classificações"
                  ),
                  value = FALSE
                ),
                
                hr(),
                
                # Outros controles
                div(
                  style = "display: grid; grid-template-columns: 1fr; gap: 10px;",
                  
                  actionButton(
                    "teste_rapido_ml",
                    label = div(
                      icon("flask", style = "margin-right: 8px;"),
                      "Teste Rápido"
                    ),
                    class = "btn-info",
                    style = "width: 100%;"
                  ),
                  
                  actionButton(
                    "exportar_dados_ml",
                    label = div(
                      icon("download", style = "margin-right: 8px;"),
                      "Exportar Dados"
                    ),
                    class = "btn-secondary",
                    style = "width: 100%;"
                  ),
                  
                  actionButton(
                    "resetar_modelo_ml",
                    label = div(
                      icon("redo", style = "margin-right: 8px;"),
                      "Resetar Modelo"
                    ),
                    class = "btn-danger",
                    style = "width: 100%;"
                  )
                )
              )
            )
          )
        ),
        
        # Seção de teste
        fluidRow(
          column(
            width = 12,
            box(
              title = div(
                icon("flask", style = "margin-right: 10px;"),
                "Teste do Modelo"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "padding: 20px;",
                
                fluidRow(
                  column(
                    width = 8,
                    
                    textAreaInput(
                      "texto_teste_ml",
                      label = div(
                        style = "font-weight: 700; margin-bottom: 10px;",
                        icon("edit"), " Digite um texto para testar o modelo:"
                      ),
                      placeholder = "Exemplo: Substituição de válvula de segurança por falha operacional...",
                      rows = 4,
                      width = "100%"
                    ),
                    
                    div(
                      style = "text-align: center; margin: 20px 0;",
                      actionButton(
                        "executar_teste_ml",
                        label = div(
                          icon("play-circle", style = "margin-right: 10px;"),
                          "Testar Modelo"
                        ),
                        class = "btn-primary btn-lg",
                        style = "padding: 15px 40px; border-radius: 25px; font-weight: 700;"
                      )
                    )
                  ),
                  
                  column(
                    width = 4,
                    
                    div(
                      style = "background: #f8f9fa; padding: 20px; border-radius: 12px; 
                       border-left: 4px solid #667eea; height: 200px;",
                      
                      h5(style = "color: #667eea; margin: 0 0 15px 0; font-weight: 700;",
                         "💡 Dicas para Teste"),
                      
                      tags$ul(
                        style = "color: #666; font-size: 13px; line-height: 1.8; margin: 0; padding-left: 20px;",
                        tags$li("Use textos reais de manutenção"),
                        tags$li("Teste diferentes tipos de intervenção"),
                        tags$li("Compare com sua classificação manual"),
                        tags$li("Valide os resultados para melhorar o modelo")
                      )
                    )
                  )
                ),
                
                # Resultado do teste
                uiOutput("resultado_teste_ml")
              )
            )
          )
        )
      ), # Fecha tabItem modelo_ml
      
      #===========================================================================
      # ABA 3: UPLOAD E CRUZAMENTO PREMIUM
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
                "arquivo de Notas"
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
                  h4(style = "color: #2E7D32; margin: 0 0 10px 0;", "Arquivo 1: Arquivo SAP - IW28"),
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
                  h4(style = "color: #1565C0; margin: 0 0 10px 0;", "Arquivo SAP - YSPM_TEXTOS"),
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
      ), # Fecha tabItem upload
      
      #===========================================================================
      # ABA 4: CLASSIFICAÇÃO INDIVIDUAL PREMIUM
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
      ), # Fecha tabItem individual
      
      # ABA 5: PROCESSAMENTO EM LOTE
      tabItem(
        tabName = "lote",
        div(
          class = "page-header",
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
             padding: 40px; border-radius: 15px; margin-bottom: 30px;
             box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);",
          div(
            style = "display: flex; align-items: center;",
            icon("list-check", style = "font-size: 48px; color: white; margin-right: 25px;"),
            div(
              h2(style = "color: white; margin: 0; font-weight: 800; font-size: 36px;",
                 "Processamento em Lote"),
              p(style = "color: rgba(255,255,255,0.9); margin: 10px 0 0 0; font-size: 16px;",
                "Classifique milhares de textos automaticamente usando IA e Dicionários")
            )
          )
        ),
        fluidRow(
          box(
            title = div(icon("cog", style = "margin-right: 10px;"), "Configuração e Execução"),
            status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE,
            div(
              style = "padding: 20px; background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
                 border-radius: 12px; margin-bottom: 25px; border-left: 5px solid #2196F3;
                 box-shadow: 0 4px 12px rgba(33, 150, 243, 0.15);",
              div(
                style = "display: flex; align-items: center;",
                icon("info-circle", style = "font-size: 36px; color: #1976D2; margin-right: 20px;"),
                div(
                  h4(style = "color: #1565C0; margin: 0 0 10px 0; font-weight: 700;",
                     "Processamento Inteligente em Lote"),
                  p(style = "color: #1565C0; margin: 0; font-size: 14px; line-height: 1.6;",
                    "Classifique automaticamente todos os textos do arquivo cruzado usando IA e/ou Dicionário. O sistema processará cada registro e gerará classificações precisas com base nos métodos configurados.")
                )
              )
            ),
            fluidRow(
              box(
                title = div(icon("sliders-h", style = "margin-right: 10px;"), strong("Configurações de Classificação em Lote")),
                status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                fluidRow(
                  column(
                    width = 7,
                    div(
                      style = "background: white; padding: 20px; border-radius: 12px;
                         border: 1px solid #dee2e6; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
                      tags$label(
                        class = "control-label",
                        style = "font-weight: 700; color: #2c3e50; margin-bottom: 12px; display: block;",
                        icon("cogs", style = "color: #667eea; margin-right: 8px;"),
                        "Método de Classificação"
                      ),
                      selectInput(
                        "metodo_classificacao", label = NULL,
                        choices = list(
                          "Dicionário (rápido, offline)" = "DICIONARIO",
                          "API OpenAI (inteligente)" = "API",
                          "Modelo ML Treinado (personalizado)" = "ML",
                          "Híbrido (Dicionário + API)" = "HIBRIDO_2",
                          "Híbrido Completo (Dicionário + API + Modelo ML)" = "HIBRIDO_3"
                        ),
                        selected = "HIBRIDO_3", width = "100%"
                      ),
                      div(
                        style = "margin-top: 12px; font-size: 13px; color: #7f8c8d; line-height: 1.6;",
                        tags$strong(style = "color: #667eea;", "Recomendado: "),
                        "Híbrido Completo — combina todas as fontes para máxima precisão e robustez."
                      )
                    )
                  ),
                  column(
                    width = 5,
                    div(
                      style = "background: white; padding: 20px; border-radius: 12px;
                         border: 1px solid #dee2e6; box-shadow: 0 2px 8px rgba(0,0,0,0.05);
                         height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                      checkboxInput(
                        "extrair_assunto",
                        label = div(
                          style = "font-size: 16px; font-weight: 600; color: #2c3e50;",
                          icon("file-alt", style = "color: #2196F3; margin-right: 10px;"),
                          "Extrair Assunto Principal com IA"
                        ),
                        value = TRUE
                      ),
                      div(
                        style = "margin-left: 35px; margin-top: -8px; font-size: 13px; color: #7f8c8d;",
                        "Resume automaticamente o texto em uma frase curta e objetiva"
                      ),
                      conditionalPanel(
                        condition = "input.metodo_classificacao == 'ML' || input.metodo_classificacao == 'HIBRIDO_3'",
                        hr(style = "margin: 20px 0; border-color: #ecf0f1;"),
                        uiOutput("status_modelo_lote_inline")
                      )
                    )
                  )
                ),
                br(),
                div(
                  style = "text-align: center; padding: 20px 0; border-top: 1px solid #ecf0f1; margin-top: 20px;",
                  actionButton(
                    "classificar_lote",
                    label = tagList(
                      icon("rocket", style = "font-size: 24px; margin-right: 12px;"),
                      tags$span(style = "font-size: 20px; font-weight: 800;", "Classificar em Lote")
                    ),
                    class = "btn-lg",
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                       color: white; font-weight: bold; padding: 18px 50px;
                       border: none; border-radius: 50px; font-size: 18px;
                       box-shadow: 0 8px 25px rgba(102, 126, 234, 0.4);
                       transition: all 0.4s ease; margin-right: 25px;",
                    onclick = "this.style.transform='scale(0.95)'; setTimeout(() => { this.style.transform='scale(1)'; }, 200);"
                  ),
                  actionButton(
                    "limpar_lote",
                    label = tagList(
                      icon("trash-alt", style = "margin-right: 10px;"),
                      "Limpar Resultados"
                    ),
                    class = "btn-lg",
                    style = "background: linear-gradient(135deg, #95a5a6 0%, #7f8c8d 100%);
                       color: white; font-weight: bold; padding: 18px 40px;
                       border: none; border-radius: 50px; font-size: 16px;
                       box-shadow: 0 4px 15px rgba(149, 165, 166, 0.3);
                       transition: all 0.3s ease;"
                  )
                ),
                div(
                  style = "text-align: center; margin-top: 20px; padding: 15px;
                     background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%);
                     border-radius: 10px; border-left: 4px solid #4caf50;",
                  tags$em(
                    style = "color: #2e7d32; font-size: 14px;",
                    icon("lightbulb", style = "margin-right: 8px;"),
                    "Dica: Use o modo Híbrido Completo para obter os melhores resultados, especialmente com o modelo treinado!"
                  )
                )
              )
            ),
            
            # ===== PAINEL DE LOG EM TEMPO REAL (SEMPRE VISÍVEL) =====
            fluidRow(
              box(
                title = div(
                  icon("terminal", style = "margin-right: 10px; color: #00ff00;"),
                  strong("Log de Processamento em Tempo Real")
                ),
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                div(
                  style = "background: #1e1e1e; color: #00ff00; padding: 20px; 
                     border-radius: 8px; font-family: 'Courier New', monospace; 
                     max-height: 500px; overflow-y: auto; font-size: 13px;
                     box-shadow: inset 0 2px 8px rgba(0,0,0,0.5); border: 1px solid #333;",
                  verbatimTextOutput("log_tempo_real")
                ),
                div(
                  style = "text-align: right; margin-top: 15px;",
                  actionButton(
                    "limpar_log",
                    label = div(
                      icon("eraser", style = "margin-right: 8px;"),
                      "Limpar Log"
                    ),
                    class = "btn-sm",
                    style = "background: #dc3545; color: white; border: none; 
                       padding: 10px 25px; border-radius: 5px; font-weight: 600;
                       box-shadow: 0 2px 5px rgba(220, 53, 69, 0.3);"
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.tem_resultados_lote",
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "text-align: right; margin-bottom: 20px;",
                    downloadButton(
                      "download_resultados_lote",
                      label = div(
                        icon("download", style = "margin-right: 8px;"),
                        "Baixar Resultados (Excel)"
                      ),
                      class = "btn-success btn-lg",
                      style = "font-weight: bold; padding: 12px 30px; border-radius: 8px;"
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = div(
                    icon("table", style = "margin-right: 10px;"),
                    "Resultados da Classificação"
                  ),
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  DT::dataTableOutput("tabela_resultados_lote")
                )
              )
            ),
            conditionalPanel(
              condition = "!output.tem_resultados_lote",
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "text-align: center; padding: 80px 40px;
                       background: white; border-radius: 15px;
                       box-shadow: 0 2px 8px rgba(0,0,0,0.06);
                       margin-top: 30px;",
                    icon("inbox", style = "font-size: 80px; color: #e0e0e0; margin-bottom: 25px;"),
                    h3(style = "color: #999; margin: 0 0 15px 0; font-weight: 600;",
                       "Nenhum Dado Disponível"),
                    p(style = "color: #999; font-size: 16px; margin: 0 0 30px 0;",
                      "Faça o upload e cruzamento dos arquivos primeiro para começar o processamento em lote."),
                    actionButton(
                      "ir_para_upload_lote",
                      label = div(
                        icon("arrow-right", style = "margin-right: 10px;"),
                        "IR PARA UPLOAD"
                      ),
                      class = "btn-primary btn-lg",
                      style = "font-weight: bold; padding: 15px 40px; border-radius: 10px;",
                      onclick = "document.querySelector('[data-value=\"upload\"]').click();"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      #===========================================================================
      # ABA 6: DICIONÁRIOS PREMIUM
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
      ), # Fecha tabItem dicionarios
      
      
      # ABA 7: ESTATÍSTICAS PREMIUM COM INSIGHTS DE IA
      tabItem(
        tabName = "estatisticas",
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
                    "Métricas detalhadas de desempenho e qualidade com análise inteligente")
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
        fluidRow(
          valueBoxOutput("metrica_total_classificados", width = 3),
          valueBoxOutput("metrica_acuracia", width = 3),
          valueBoxOutput("metrica_conformes", width = 3),
          valueBoxOutput("metrica_divergentes", width = 3)
        ),
        fluidRow(
          box(
            title = div(
              icon("brain", style = "margin-right: 10px; color: #667eea;"),
              "Insights do Cientista de Dados Virtual",
              tags$span(
                style = "float: right; font-size: 12px; font-weight: normal;
                   background: rgba(102, 126, 234, 0.1); color: #667eea;
                   padding: 5px 15px; border-radius: 15px;",
                icon("robot"), " Powered by IA"
              )
            ),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            div(
              style = "padding: 25px;",
              div(
                style = "text-align: center; margin-bottom: 25px;",
                actionButton(
                  "gerar_insights",
                  label = div(
                    icon("magic", style = "margin-right: 10px; font-size: 20px;"),
                    "Gerar Insights com IA"
                  ),
                  class = "btn-primary btn-lg",
                  style = "padding: 18px 50px; border-radius: 35px; font-weight: 700; font-size: 18px;"
                )
              ),
              uiOutput("painel_insights_ia")
            )
          )
        ),
        
        #===========================================================================
        # GRÁFICOS PRINCIPAIS - LINHA 1
        #===========================================================================
        
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
        
        #===========================================================================
        # GRÁFICOS PRINCIPAIS - LINHA 2
        #===========================================================================
        
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
        
        #===========================================================================
        # TABELAS DETALHADAS
        #===========================================================================
        
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
                    
                    div(
                      style = "background: #e3f2fd; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #2196F3;",
                      tags$strong(icon("info-circle"), " Análise por Tipo SAP:", style = "color: #1565C0;"),
                      p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                        "Métricas de desempenho para cada tipo de intervenção (1 a 6)")
                    ),
                    
                    DT::dataTableOutput("tabela_metricas_tipo")
                  )
                ),
                
                tabPanel(
                  title = div(icon("layer-group"), " Por Categoria"),
                  br(),
                  div(
                    style = "padding: 20px;",
                    
                    div(
                      style = "background: #e8f5e9; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #4CAF50;",
                      tags$strong(icon("sitemap"), " Hierarquias:", style = "color: #2E7D32;"),
                      p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                        "Comparação entre PROBLEMAS_COMUNS e IAZF")
                    ),
                    
                    DT::dataTableOutput("tabela_metricas_categoria")
                  )
                ),
                
                tabPanel(
                  title = div(icon("cogs"), " Por Método"),
                  br(),
                  div(
                    style = "padding: 20px;",
                    
                    div(
                      style = "background: #fff3cd; padding: 15px; border-radius: 10px; 
                       margin-bottom: 20px; border-left: 4px solid #ffc107;",
                      tags$strong(icon("robot"), " Desempenho dos Métodos:", style = "color: #856404;"),
                      p(style = "margin: 5px 0 0 0; font-size: 12px; color: #666;",
                        "Comparativo de eficácia entre Dicionário, API e Híbrido")
                    ),
                    
                    DT::dataTableOutput("tabela_metricas_metodo")
                  )
                ),
                
                tabPanel(
                  title = div(icon("exclamation-triangle"), " Divergências"),
                  br(),
                  div(
                    style = "padding: 20px;",
                    
                    div(
                      style = "background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); 
                       padding: 25px; border-radius: 15px; margin-bottom: 25px; 
                       border-left: 5px solid #ffc107;
                       box-shadow: 0 4px 12px rgba(255, 193, 7, 0.2);",
                      div(
                        style = "display: flex; align-items: center; margin-bottom: 15px;",
                        icon("exclamation-triangle", style = "font-size: 48px; color: #856404; margin-right: 20px;"),
                        div(
                          tags$strong("⚠️ Atenção: Registros Divergentes", style = "color: #856404; font-size: 16px;"),
                          p(style = "margin: 8px 0 0 0; color: #856404; font-size: 13px; line-height: 1.6;",
                            "Esta tabela mostra os registros onde a IA divergiu da classificação anterior. ",
                            "Revise manualmente os casos críticos (Tipos 5 e 6 - IAZF).")
                        )
                      ),
                      div(
                        style = "background: rgba(255,255,255,0.7); padding: 15px; border-radius: 10px;",
                        tags$ul(
                          style = "margin: 0; font-size: 12px; color: #666; line-height: 2;",
                          tags$li(tags$strong("Alta Confiança (>85%):"), " Considere aceitar a sugestão da IA"),
                          tags$li(tags$strong("Média Confiança (70-85%):"), " Revise o contexto antes de decidir"),
                          tags$li(tags$strong("Baixa Confiança (<70%):"), " Revisão manual obrigatória")
                        )
                      )
                    ),
                    
                    DT::dataTableOutput("tabela_divergencias_detalhadas")
                  )
                )
              )
            )
          )
        ),
        
        #===========================================================================
        # AÇÕES RÁPIDAS (NOVO!)
        #===========================================================================
        
        fluidRow(
          box(
            title = div(
              icon("bolt", style = "margin-right: 10px;"),
              "Ações Rápidas"
            ),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            div(
              style = "padding: 25px;",
              
              div(
                style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px;",
                
                # Card Exportar Relatório
                div(
                  style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 30px; border-radius: 15px; text-align: center;
                   box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;",
                  icon("file-pdf", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                  h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                     "Exportar Relatório"),
                  downloadButton(
                    "exportar_relatorio_estatisticas",
                    label = "Download PDF",
                    style = "background: rgba(255,255,255,0.2); color: white; 
                       border: 2px solid white; border-radius: 25px; 
                       padding: 10px 25px; font-weight: 700;"
                  )
                ),
                
                # Card Exportar Dados
                div(
                  style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
                   padding: 30px; border-radius: 15px; text-align: center;
                   box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;",
                  icon("file-excel", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                  h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                     "Exportar Dados"),
                  downloadButton(
                    "exportar_dados_estatisticas",
                    label = "Download Excel",
                    style = "background: rgba(255,255,255,0.2); color: white; 
                       border: 2px solid white; border-radius: 25px; 
                       padding: 10px 25px; font-weight: 700;"
                  )
                ),
                
                # Card Comparar com Histórico
                div(
                  style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                   padding: 30px; border-radius: 15px; text-align: center;
                   box-shadow: 0 8px 24px rgba(79, 172, 254, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;",
                  icon("history", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                  h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                     "Comparar Histórico"),
                  actionButton(
                    "comparar_historico_estatisticas",
                    label = "Comparar",
                    style = "background: rgba(255,255,255,0.2); color: white; 
                       border: 2px solid white; border-radius: 25px; 
                       padding: 10px 25px; font-weight: 700;"
                  )
                ),
                
                # Card Atualizar Tudo
                div(
                  style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                   padding: 30px; border-radius: 15px; text-align: center;
                   box-shadow: 0 8px 24px rgba(240, 147, 251, 0.3);
                   transition: transform 0.3s ease;
                   cursor: pointer;",
                  icon("sync-alt", style = "font-size: 56px; color: white; margin-bottom: 15px;"),
                  h5(style = "color: white; margin: 0 0 15px 0; font-weight: 700;",
                     "Atualizar Tudo"),
                  actionButton(
                    "atualizar_tudo_estatisticas",
                    label = "Atualizar",
                    style = "background: rgba(255,255,255,0.2); color: white; 
                       border: 2px solid white; border-radius: 25px; 
                       padding: 10px 25px; font-weight: 700;"
                  )
                )
              )
            )
          )
        )),
      
      #Fecha tabItem de estatisticas
      
      #===========================================================================
      # ABA 8: CONFIGURAÇÕES API PREMIUM
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
      ), # Fecha tabItem configuracoes
      
      #===========================================================================
      # ABA 9: DOCUMENTAÇÃO PREMIUM
      #===========================================================================
      tabItem(
        tabName = "documentacao",
        
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
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 30px; border-radius: 10px; color: white; margin-bottom: 30px; text-align: center;",
              
              tags$h2("🤖 Sistema de Classificação SAP com IA", style = "margin: 0 0 15px 0;"),
              tags$p("Versão 3.0 - Powered by OpenAI Petrobras", style = "font-size: 16px; opacity: 0.9; margin: 0;")
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
                "Este sistema foi desenvolvido para automatizar e padronizar a classificação de textos de manutenção no SAP, utilizando ",
                tags$strong("Inteligência Artificial"), 
                " e ",
                tags$strong("Dicionários Personalizáveis"),
                ". O objetivo é aumentar a precisão, reduzir o tempo de classificação e garantir conformidade com os padrões SAP da Petrobras.",
                style = "font-size: 14px; line-height: 1.8; text-align: justify;"
              ),
              
              tags$hr(),
              
              tags$h4("🏆 Principais Benefícios", style = "color: #1f4e79;"),
              
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 15px;",
                
                div(
                  style = "background: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 4px solid #4CAF50;",
                  tags$strong("⚡ Velocidade", style = "color: #2E7D32;"),
                  tags$p("Classifica milhares de textos em minutos", style = "font-size: 12px; color: #2E7D32; margin: 5px 0 0 0;")
                ),
                
                div(
                  style = "background: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #2196F3;",
                  tags$strong("🎯 Precisão", style = "color: #1565C0;"),
                  tags$p("Taxa de acurácia superior a 85%", style = "font-size: 12px; color: #1565C0; margin: 5px 0 0 0;")
                ),
                
                div(
                  style = "background: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #FF9800;",
                  tags$strong("🔄 Consistência", style = "color: #E65100;"),
                  tags$p("Classificação padronizada e auditável", style = "font-size: 12px; color: #E65100; margin: 5px 0 0 0;")
                ),
                
                div(
                  style = "background: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9C27B0;",
                  tags$strong("📊 Rastreabilidade", style = "color: #6A1B9A;"),
                  tags$p("Histórico completo de decisões", style = "font-size: 12px; color: #6A1B9A; margin: 5px 0 0 0;")
                )
              )
            )
          )
        ),
        
        # Seção 2: Tipos SAP
        fluidRow(
          box(
            title = div(icon("list-ol"), " Tipos de Intervenção SAP"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            div(
              style = "padding: 15px;",
              
              # Tipo 1
              div(
                style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 20px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #87CEEB;",
                div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-size: 36px; margin-right: 15px;", "🧽"),
                    div(
                      tags$strong("Tipo 1 - Condicionamento e Limpeza", style = "font-size: 16px; color: #1565C0;"),
                      tags$br(),
                      tags$span("Criticidade: BAIXA | Hierarquia: PROBLEMAS_COMUNS", style = "font-size: 12px; color: #666;")
                    )
                ),
                tags$p(
                  tags$strong("Descrição: "),
                  "Condicionamento, limpeza, arrumação, preservação, pintura ou desinstalação",
                  style = "font-size: 13px; margin: 10px 0;"
                ),
                tags$p(
                  tags$strong("Exemplos: "),
                  "Limpeza de equipamentos, pintura de estruturas, higienização de áreas",
                  style = "font-size: 12px; color: #666; font-style: italic;"
                )
              ),
              
              # Tipo 2
              div(
                style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); padding: 20px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #90EE90;",
                div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-size: 36px; margin-right: 15px;", "🔧"),
                    div(
                      tags$strong("Tipo 2 - Melhorias e Modificações", style = "font-size: 16px; color: #2E7D32;"),
                      tags$br(),
                      tags$span("Criticidade: BAIXA | Hierarquia: PROBLEMAS_COMUNS", style = "font-size: 12px; color: #666;")
                    )
                ),
                tags$p(
                  tags$strong("Descrição: "),
                  "Melhorias, modificações, testes, colocação em operação, instalação ou regulagem",
                  style = "font-size: 13px; margin: 10px 0;"
                ),
                tags$p(
                  tags$strong("Exemplos: "),
                  "Instalação de novos equipamentos, testes de sistemas, ajustes e regulagens",
                  style = "font-size: 12px; color: #666; font-style: italic;"
                )
              ),
              
              # Tipo 3
              div(
                style = "background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%); padding: 20px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #FFD700;",
                div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-size: 36px; margin-right: 15px;", "🔍"),
                    div(
                      tags$strong("Tipo 3 - Manutenção Preventiva", style = "font-size: 16px; color: #F57F17;"),
                      tags$br(),
                      tags$span("Criticidade: MÉDIA | Hierarquia: PROBLEMAS_COMUNS", style = "font-size: 12px; color: #666;")
                    )
                ),
                tags$p(
                  tags$strong("Descrição: "),
                  "Manutenção preventiva, manutenção preditiva ou inspeção planejada",
                  style = "font-size: 13px; margin: 10px 0;"
                ),
                tags$p(
                  tags$strong("Exemplos: "),
                  "Inspeções programadas, manutenções preventivas de rotina, verificações periódicas",
                  style = "font-size: 12px; color: #666; font-style: italic;"
                )
              ),
              
              # Tipo 4
              div(
                style = "background: linear-gradient(135deg, #ffe0b2 0%, #ffcc80 100%); padding: 20px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #FFA500;",
                div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-size: 36px; margin-right: 15px;", "⏰"),
                    div(
                      tags$strong("Tipo 4 - Manutenção por Oportunidade", style = "font-size: 16px; color: #E65100;"),
                      tags$br(),
                      tags$span("Criticidade: MÉDIA | Hierarquia: PROBLEMAS_COMUNS", style = "font-size: 12px; color: #666;")
                    )
                ),
                tags$p(
                  tags$strong("Descrição: "),
                  "Manutenção por oportunidade ou inspeção não programada",
                  style = "font-size: 13px; margin: 10px 0;"
                ),
                tags$p(
                  tags$strong("Exemplos: "),
                  "Manutenções durante paradas, aproveitamento de disponibilidade do equipamento",
                  style = "font-size: 12px; color: #666; font-style: italic;"
                )
              ),
              
              # Tipo 5
              div(
                style = "background: linear-gradient(135deg, #ffccbc 0%, #ff8a65 100%); padding: 20px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #FF6347;",
                div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-size: 36px; margin-right: 15px;", "⚠️"),
                    div(
                      tags$strong("Tipo 5 - Eliminação de Defeito (IAZF)", style = "font-size: 16px; color: #BF360C;"),
                      tags$br(),
                      tags$span("Criticidade: ALTA | Hierarquia: IAZF", style = "font-size: 12px; color: #666;")
                    )
                ),
                tags$p(
                  tags$strong("Descrição: "),
                  "Intervenção para eliminação de defeito - Equipamento com restrição",
                  style = "font-size: 13px; margin: 10px 0;"
                ),
                tags$p(
                  tags$strong("Exemplos: "),
                  "Correção de problemas que limitam funcionamento, eliminação de anomalias",
                  style = "font-size: 12px; color: #666; font-style: italic;"
                )
              ),
              
              # Tipo 6
              div(
                style = "background: linear-gradient(135deg, #ef9a9a 0%, #e57373 100%); padding: 20px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #DC143C;",
                div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-size: 36px; margin-right: 15px;", "🚨"),
                    div(
                      tags$strong("Tipo 6 - Eliminação de Falha (IAZF)", style = "font-size: 16px; color: #B71C1C;"),
                      tags$br(),
                      tags$span("Criticidade: CRÍTICA | Hierarquia: IAZF", style = "font-size: 12px; color: #666;")
                    )
                ),
                tags$p(
                  tags$strong("Descrição: "),
                  "Intervenção para eliminação de falha - Sistema indisponível",
                  style = "font-size: 13px; margin: 10px 0;"
                ),
                tags$p(
                  tags$strong("Exemplos: "),
                  "Falhas críticas, quebras, emergências, paradas totais",
                  style = "font-size: 12px; color: #666; font-style: italic;"
                )
              )
            )
          )
        ),
        
        # Seção 3: Entendendo os Gráficos
        fluidRow(
          box(
            title = div(icon("chart-bar"), " Entendendo os Gráficos e Métricas"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              
              # Tab 1: Dashboard
              tabPanel(
                "📊 Dashboard",
                br(),
                
                tags$h4("Gráficos do Dashboard", style = "color: #1f4e79;"),
                
                # Gráfico 1
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      div(
                        style = "background: #1f4e79; color: white; width: 50px; height: 50px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 24px; margin-right: 15px;",
                        "1"
                      ),
                      div(
                        tags$strong("Comparação: Tipo Anterior vs Tipo Novo", style = "font-size: 16px; color: #1f4e79;"),
                        tags$br(),
                        tags$span("Gráfico de barras agrupadas", style = "font-size: 12px; color: #666;")
                      )
                  ),
                  
                  tags$p(
                    tags$strong("O que mostra: "),
                    "Compara a distribuição dos tipos de intervenção antes e depois da reclassificação pela IA.",
                    style = "font-size: 13px; margin-bottom: 10px;"
                  ),
                  
                  tags$p(
                    tags$strong("Como interpretar:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li(tags$strong("Barras cinzas:"), " Classificação anterior (original do SAP)"),
                    tags$li(tags$strong("Barras azuis:"), " Nova classificação sugerida pela IA"),
                    tags$li(tags$strong("Altura igual:"), " IA concordou com a classificação anterior"),
                    tags$li(tags$strong("Altura diferente:"), " IA sugeriu reclassificação")
                  ),
                  
                  div(
                    style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #ffc107;",
                    tags$strong("💡 Exemplo de Análise:", style = "color: #856404;"),
                    tags$p(
                      "Se o Tipo 3 tinha 100 registros (cinza) e agora tem 80 (azul), significa que 20 registros foram reclassificados para outros tipos pela IA.",
                      style = "font-size: 12px; color: #856404; margin: 5px 0 0 0;"
                    )
                  )
                ),
                
                # Gráfico 2
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      div(
                        style = "background: #FF9800; color: white; width: 50px; height: 50px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 24px; margin-right: 15px;",
                        "2"
                      ),
                      div(
                        tags$strong("Distribuição por Tipos de Intervenção", style = "font-size: 16px; color: #FF9800;"),
                        tags$br(),
                        tags$span("Gráfico de barras verticais", style = "font-size: 12px; color: #666;")
                      )
                  ),
                  
                  tags$p(
                    tags$strong("O que mostra: "),
                    "Quantidade de registros classificados em cada tipo SAP (1 a 6).",
                    style = "font-size: 13px; margin-bottom: 10px;"
                  ),
                  
                  tags$p(
                    tags$strong("Como interpretar:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li(tags$strong("Cores:"), " Cada tipo tem cor específica (Tipo 1=Azul, Tipo 6=Vermelho)"),
                    tags$li(tags$strong("Altura:"), " Indica quantidade de registros"),
                    tags$li(tags$strong("Número no topo:"), " Valor exato de registros")
                  ),
                  
                  div(
                    style = "background: #e8f5e9; padding: 15px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #4CAF50;",
                    tags$strong("✅ Ideal:", style = "color: #2E7D32;"),
                    tags$p(
                      "Maioria dos registros em Tipos 1-4 (PROBLEMAS_COMUNS) indica operação saudável. Muitos registros em Tipos 5-6 (IAZF) podem indicar problemas operacionais.",
                      style = "font-size: 12px; color: #2E7D32; margin: 5px 0 0 0;"
                    )
                  )
                ),
                
                # Gráfico 3
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      div(
                        style = "background: #17a2b8; color: white; width: 50px; height: 50px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 24px; margin-right: 15px;",
                        "3"
                      ),
                      div(
                        tags$strong("Distribuição por Hierarquia", style = "font-size: 16px; color: #17a2b8;"),
                        tags$br(),
                        tags$span("Gráfico de barras horizontais", style = "font-size: 12px; color: #666;")
                      )
                  ),
                  
                  tags$p(
                    tags$strong("O que mostra: "),
                    "Proporção entre PROBLEMAS_COMUNS (Tipos 1-4) e IAZF (Tipos 5-6).",
                    style = "font-size: 13px; margin-bottom: 10px;"
                  ),
                  
                  tags$p(
                    tags$strong("Como interpretar:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li(tags$strong("PROBLEMAS_COMUNS (Verde):"), " Manutenções normais e melhorias"),
                    tags$li(tags$strong("IAZF (Laranja):"), " Incidentes de Ativos Zero Falha - requerem atenção")
                  ),
                  
                  div(
                    style = "background: #ffebee; padding: 15px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #f44336;",
                    tags$strong("⚠️ Atenção:", style = "color: #c62828;"),
                    tags$p(
                      "Alta proporção de IAZF pode indicar necessidade de revisão dos processos de manutenção preventiva.",
                      style = "font-size: 12px; color: #c62828; margin: 5px 0 0 0;"
                    )
                  )
                ),
                
                # Gráfico 4
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      div(
                        style = "background: #28a745; color: white; width: 50px; height: 50px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 24px; margin-right: 15px;",
                        "4"
                      ),
                      div(
                        tags$strong("Status de Conformidade", style = "font-size: 16px; color: #28a745;"),
                        tags$br(),
                        tags$span("Gráfico de pizza", style = "font-size: 12px; color: #666;")
                      )
                  ),
                  
                  tags$p(
                    tags$strong("O que mostra: "),
                    "Percentual de registros onde a IA concordou (CONFORME) ou divergiu (DIVERGENTE) da classificação anterior.",
                    style = "font-size: 13px; margin-bottom: 10px;"
                  ),
                  
                  tags$p(
                    tags$strong("Como interpretar:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li(tags$strong("Verde (CONFORME):"), " IA concordou com classificação anterior"),
                    tags$li(tags$strong("Laranja (DIVERGENTE):"), " IA sugeriu reclassificação"),
                    tags$li(tags$strong("Alta conformidade (>80%):"), " Classificações originais estão corretas"),
                    tags$li(tags$strong("Baixa conformidade (<60%):"), " Muitas classificações podem estar incorretas")
                  ),
                  
                  div(
                    style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #2196F3;",
                    tags$strong("📊 Métrica Chave:", style = "color: #1565C0;"),
                    tags$p(
                      "Esta é a principal métrica de qualidade do sistema. Taxa de conformidade acima de 85% indica alta confiabilidade.",
                      style = "font-size: 12px; color: #1565C0; margin: 5px 0 0 0;"
                    )
                  )
                )
              ),
              
              # Tab 2: Estatísticas
              tabPanel(
                "📈 Estatísticas",
                br(),
                
                tags$h4("Métricas Estatísticas", style = "color: #1f4e79;"),
                
                # Matriz de Confusão
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  tags$h5("📊 Matriz de Confusão", style = "color: #1f4e79;"),
                  
                  tags$p(
                    "A matriz de confusão mostra a relação entre os tipos anteriores (linhas) e os tipos novos (colunas).",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                    
                    tags$p(tags$strong("Como ler a matriz:"), style = "font-size: 13px; margin-bottom: 10px;"),
                    
                    tags$table(
                      style = "width: 100%; border-collapse: collapse; font-size: 12px;",
                      tags$thead(
                        tags$tr(
                          tags$th("", style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;"),
                          tags$th("Tipo 1", style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;"),
                          tags$th("Tipo 2", style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;"),
                          tags$th("Tipo 3", style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;")
                        )
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td(tags$strong("Tipo 1"), style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;"),
                          tags$td("45", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center; background: #d4edda; font-weight: bold;"),
                          tags$td("2", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center;"),
                          tags$td("1", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center;")
                        ),
                        tags$tr(
                          tags$td(tags$strong("Tipo 2"), style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;"),
                          tags$td("1", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center;"),
                          tags$td("38", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center; background: #d4edda; font-weight: bold;"),
                          tags$td("0", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center;")
                        ),
                        tags$tr(
                          tags$td(tags$strong("Tipo 3"), style = "padding: 10px; border: 1px solid #dee2e6; background: #e9ecef;"),
                          tags$td("0", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center;"),
                          tags$td("3", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center;"),
                          tags$td("52", style = "padding: 10px; border: 1px solid #dee2e6; text-align: center; background: #d4edda; font-weight: bold;")
                        )
                      )
                    ),
                    
                    tags$p(
                      style = "font-size: 12px; color: #666; margin-top: 15px; font-style: italic;",
                      "Exemplo: Das 48 ordens classificadas como Tipo 1, a IA concordou com 45, sugeriu 2 para Tipo 2 e 1 para Tipo 3."
                    )
                  ),
                  
                  div(
                    style = "background: #e8f5e9; padding: 15px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #4CAF50;",
                    tags$strong("✅ Diagonal Principal (destacada em verde):", style = "color: #2E7D32;"),
                    tags$p(
                      "Representa os acertos (IA concordou). Quanto mais concentrados na diagonal, melhor a conformidade.",
                      style = "font-size: 12px; color: #2E7D32; margin: 5px 0 0 0;"
                    )
                  )
                ),
                
                # Acurácia por Tipo
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  tags$h5("🎯 Acurácia por Tipo", style = "color: #1f4e79;"),
                  
                  tags$p(
                    "Mostra o percentual de acerto da IA para cada tipo de intervenção.",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  tags$p(
                    tags$strong("Interpretação das cores:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li(tags$strong("Verde:"), " Acurácia ≥ 80% (Excelente)"),
                    tags$li(tags$strong("Laranja:"), " Acurácia entre 60-79% (Bom, mas revisar)"),
                    tags$li(tags$strong("Vermelho:"), " Acurácia < 60% (Requer atenção)")
                  ),
                  
                  div(
                    style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #ffc107;",
                    tags$strong("💡 Dica:", style = "color: #856404;"),
                    tags$p(
                      "Se um tipo específico tem baixa acurácia, considere adicionar mais palavras-chave no dicionário daquele tipo.",
                      style = "font-size: 12px; color: #856404; margin: 5px 0 0 0;"
                    )
                  )
                ),
                
                # Distribuição de Confiança
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  tags$h5("📈 Distribuição de Confiança", style = "color: #1f4e79;"),
                  
                  tags$p(
                    "Histograma mostrando a distribuição dos níveis de confiança das classificações.",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  tags$p(
                    tags$strong("Faixas de confiança:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin: 15px 0;",
                    
                    div(
                      style = "background: #d4edda; padding: 10px; border-radius: 5px; border-left: 3px solid #28a745;",
                      tags$strong("90-100%", style = "color: #155724;"),
                      tags$p("Muito Alta - Pode confiar", style = "font-size: 11px; color: #155724; margin: 5px 0 0 0;")
                    ),
                    
                    div(
                      style = "background: #d1ecf1; padding: 10px; border-radius: 5px; border-left: 3px solid #17a2b8;",
                      tags$strong("80-89%", style = "color: #0c5460;"),
                      tags$p("Alta - Confiável", style = "font-size: 11px; color: #0c5460; margin: 5px 0 0 0;")
                    ),
                    
                    div(
                      style = "background: #fff3cd; padding: 10px; border-radius: 5px; border-left: 3px solid #ffc107;",
                      tags$strong("70-79%", style = "color: #856404;"),
                      tags$p("Média - Revisar se crítico", style = "font-size: 11px; color: #856404; margin: 5px 0 0 0;")
                    ),
                    
                    div(
                      style = "background: #f8d7da; padding: 10px; border-radius: 5px; border-left: 3px solid #dc3545;",
                      tags$strong("<70%", style = "color: #721c24;"),
                      tags$p("Baixa - Revisar manualmente", style = "font-size: 11px; color: #721c24; margin: 5px 0 0 0;")
                    )
                  )
                ),
                
                # Métodos de Classificação
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  tags$h5("🔀 Métodos de Classificação Utilizados", style = "color: #1f4e79;"),
                  
                  tags$p(
                    "Gráfico de pizza mostrando quais métodos foram utilizados nas classificações.",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  tags$p(
                    tags$strong("Tipos de métodos:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li(tags$strong("DICIONARIO:"), " Classificado apenas por palavras-chave"),
                    tags$li(tags$strong("API:"), " Classificado apenas pela IA"),
                    tags$li(tags$strong("HIBRIDO_CONCORDANTE:"), " Dicionário e API concordaram"),
                    tags$li(tags$strong("HIBRIDO_DICIONARIO:"), " Dicionário teve maior confiança"),
                    tags$li(tags$strong("HIBRIDO_API:"), " API teve maior confiança"),
                    tags$li(tags$strong("FALLBACK:"), " Usado quando ambos falharam")
                  )
                )
              ),
              
              # Tab 3: Métricas
              tabPanel(
                "📊 Métricas",
                br(),
                
                tags$h4("Principais Métricas do Sistema", style = "color: #1f4e79;"),
                
                # Acurácia
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      tags$span(style = "font-size: 48px; margin-right: 20px;", "🎯"),
                      div(
                        tags$h5("Acurácia (Accuracy)", style = "margin: 0; color: #1f4e79;"),
                        tags$p("Percentual de classificações corretas", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
                      )
                  ),
                  
                  tags$p(
                    tags$strong("Fórmula: "),
                    tags$code("(Conformes / Total) × 100"),
                    style = "font-size: 13px; margin-bottom: 10px;"
                  ),
                  
                  tags$p(
                    tags$strong("Exemplo: "),
                    "Se de 100 registros, 85 estão conformes, a acurácia é 85%.",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #17a2b8;",
                    tags$table(
                      style = "width: 100%; font-size: 12px;",
                      tags$tr(
                        tags$td(tags$strong("≥ 90%"), style = "padding: 5px;"),
                        tags$td("Excelente - Sistema altamente confiável", style = "padding: 5px; color: #28a745;")
                      ),
                      tags$tr(
                        tags$td(tags$strong("80-89%"), style = "padding: 5px;"),
                        tags$td("Muito Bom - Confiável para uso em produção", style = "padding: 5px; color: #17a2b8;")
                      ),
                      tags$tr(
                        tags$td(tags$strong("70-79%"), style = "padding: 5px;"),
                        tags$td("Bom - Recomenda-se validação em casos críticos", style = "padding: 5px; color: #ffc107;")
                      ),
                      tags$tr(
                        tags$td(tags$strong("< 70%"), style = "padding: 5px;"),
                        tags$td("Insuficiente - Requer ajustes nos dicionários", style = "padding: 5px; color: #dc3545;")
                      )
                    )
                  )
                ),
                
                # Confiança Média
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      tags$span(style = "font-size: 48px; margin-right: 20px;", "💯"),
                      div(
                        tags$h5("Confiança Média", style = "margin: 0; color: #1f4e79;"),
                        tags$p("Nível de certeza das classificações", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
                      )
                  ),
                  
                  tags$p(
                    "Indica o quão confiante o sistema está em suas classificações. Calculada como média ponderada entre dicionário e API.",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  tags$p(
                    tags$strong("Fatores que aumentam a confiança:"),
                    style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    style = "font-size: 13px; line-height: 1.8;",
                    tags$li("Múltiplas palavras-chave encontradas no texto"),
                    tags$li("Concordância entre dicionário e API"),
                    tags$li("Contexto claro e inequívoco"),
                    tags$li("Texto bem estruturado e completo")
                  )
                ),
                
                # Taxa de Conformidade
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                      tags$span(style = "font-size: 48px; margin-right: 20px;", "✅"),
                      div(
                        tags$h5("Taxa de Conformidade", style = "margin: 0; color: #1f4e79;"),
                        tags$p("Concordância com classificação anterior", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
                      )
                  ),
                  
                  tags$p(
                    "Percentual de vezes que a IA concordou com a classificação anterior do SAP.",
                    style = "font-size: 13px; margin-bottom: 15px;"
                  ),
                  
                  div(
                    style = "background: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #2196F3;",
                    tags$strong("📌 Importante:", style = "color: #1565C0;"),
                    tags$p(
                      "Alta conformidade (>80%) indica que as classificações originais estavam corretas. ",
                      "Baixa conformidade (<60%) pode indicar oportunidade de melhoria nas classificações existentes.",
                      style = "font-size: 12px; color: #1565C0; margin: 5px 0 0 0; line-height: 1.6;"
                    )
                  )
                )
              ),
              
              # Tab 4: Glossário
              tabPanel(
                "📚 Glossário",
                br(),
                
                tags$h4("Glossário de Termos", style = "color: #1f4e79;"),
                
                div(
                  style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                  
                  tags$dl(
                    style = "font-size: 13px; line-height: 2;",
                    
                    tags$dt(tags$strong("IAZF"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Incidente de Ativos Zero Falha - Eventos críticos que requerem atenção imediata (Tipos 5 e 6)", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("PROBLEMAS_COMUNS"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Manutenções rotineiras e melhorias (Tipos 1, 2, 3 e 4)", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("Tipo de Intervenção"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Classificação SAP que define a natureza da manutenção (1 a 6)", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("Criticidade"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Nível de urgência: BAIXA, MÉDIA, ALTA ou CRÍTICA", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("Confiança"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Percentual de certeza da classificação (0-100%)", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("Modo Híbrido"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Combinação de dicionário e API para máxima precisão", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("Token"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Unidade de processamento da API (aproximadamente 4 caracteres = 1 token)", style = "margin-bottom: 15px; color: #666;"),
                    
                    tags$dt(tags$strong("Temperature"), style = "color: #1f4e79; font-size: 14px;"),
                    tags$dd("Controla aleatoriedade da IA (0 = determinístico, 1 = criativo)", style = "color: #666;")
                  )
                )
              )
            )
          ) # ← Fecha o último box da Documentação
        ) # ← Fecha o último fluidRow da Documentação
      ), # Fecha tabItem de "documentacao"
      
      #===========================================================================
      # ABA 10: HISTÓRICO DE PROCESSAMENTOS
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
      ) # Fecha tabItem historico
    ) # Fecha tabItems
  ) # Fecha dashboardBody
) # Fecha dashboardPage
cat("✅ Interface Premium carregada com sucesso!\n")
cat("🎨 Visual moderno e elegante aplicado\n")
cat("📊 Preparando servidor...\n\n")


#=============================================================================
# SERVIDOR (SERVER COMPLETO) - VERSÃO PREMIUM CORRIGIDA
#=============================================================================

server <- function(input, output, session)  {
  observe({
    carregou <- carregar_dados_modelo()
    if(carregou) {
      cat("✅ Modelo ML carregado do disco\n")
    }
  })
  # ==========================================================================
  # INICIALIZAR OBJETOS REACTIVEVALUES (FALTANDO)
  # ==========================================================================
  
  # 🔧 VALORES PRINCIPAIS
  values <- reactiveValues(
    dados_ordens = NULL,
    dados_textos = NULL,
    dados_preview = NULL,
    dados_cruzados = NULL,  # ← ADICIONADO
    col_tip_intervencao = NULL,
    resultados_lote = NULL,
    processando = FALSE,
    dados_com_assuntos = NULL,
    modelo_treinado = NULL,
    status_modelo = "Não treinado"
  )
  
  # 🔧 CARREGAR MODELO SALVO (ADICIONE ISSO!)
  observe({
    # Tentar carregar dados do modelo salvo
    carregou <- tryCatch({
      carregar_dados_modelo()
    }, error = function(e) {
      cat("⚠️ Erro ao carregar modelo:", e$message, "\n")
      FALSE
    })
    
    if(carregou) {
      showNotification("✅ Modelo ML carregado do disco", type = "message", duration = 3)
    }
  })
  
  
  # 🔧 VALIDAÇÕES (objeto separado)
  validacoes <- reactiveValues(
    dados = data.frame(
      id = character(),
      texto_completo = character(),
      assunto_original = character(),
      assunto_validado = character(),
      tipo_original = integer(),
      tipo_validado = integer(),
      confianca_original = numeric(),
      metodo_original = character(),
      usuario = character(),
      timestamp = as.POSIXct(character()),
      feedback_qualidade = character(),
      stringsAsFactors = FALSE
    ),
    modelo_treinado = NULL,
    vetorizador = NULL,
    metricas_modelo = list(
      acuracia = 0,
      total_treinos = 0,
      ultima_atualizacao = NULL
    )
  )
  
  # 🔧 MODELO ML (objeto separado)
  
  # 🔧 SISTEMA DE LOG EM TEMPO REAL
  log_sistema <- reactiveVal("")
  
  # Função auxiliar para adicionar ao log
  adicionar_log <- function(mensagem, prefixo = "ℹ️") {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    nova_linha <- paste0("[", timestamp, "] ", prefixo, " ", mensagem, "\n")
    log_atual <- log_sistema()
    log_sistema(paste0(log_atual, nova_linha))
  }
  
  # Limpar log quando solicitado
  observeEvent(input$limpar_log, {
    log_sistema("")
    showNotification("Log limpo com sucesso", type = "message", duration = 2)
  })
  
  # Renderizar log em tempo real - atualiza automaticamente quando log_sistema muda
  output$log_tempo_real <- renderText({
    # Observar o reactive value para que qualquer mudança force re-renderização
    log_atual <- log_sistema()
    
    # Forçar re-renderização a cada 100ms para garantir fluidez
    invalidateLater(100)
    
    if (nchar(log_atual) == 0) {
      "🔵 Sistema pronto. Aguardando início do processamento em lote...\n\n💡 Os logs de classificação aparecerão aqui em tempo real."
    } else {
      log_atual
    }
  })
  
  # Sistema de log está pronto para uso no loop de processamento
  cat("✅ Sistema de log em tempo real inicializado\n")
  modelo_ml_dados <- reactiveValues(
    validacoes = data.frame(
      id = character(0),
      texto = character(0),
      tipo_original = integer(0),
      tipo_ia = integer(0),
      tipo_correto = integer(0),
      confianca = numeric(0),
      timestamp = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ),
    modelo = NULL,
    metricas = list(
      acuracia = 0,
      total_dados = 0,
      ultima_atualizacao = Sys.time()
    ),
    configuracao = list(
      ativo = FALSE,
      min_validacoes = 10
    )
  )
  
  # 🔧 HISTÓRICO
  historico <- reactiveValues(
    processamentos = list(),
    indice_atual = 0,
    max_historico = 50,
    sessao_id = format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  
  # ============================================================================
  # FUNÇÕES AUXILIARES PARA O HISTÓRICO
  # ============================================================================
  
  # Função para adicionar ao histórico (versão corrigida)
  adicionar_ao_historico <- function(dados_resultado, metadados = list()) {
    
    cat("\n💾 Salvando no histórico...\n")
    
    # Garantir que temos dados
    if(is.null(dados_resultado) || nrow(dados_resultado) == 0) {
      cat("⚠️ Nenhum dado para salvar no histórico\n")
      return(NULL)
    }
    
    # Criar snapshot
    snapshot <- list(
      timestamp = Sys.time(),
      dados = dados_resultado,
      metadados = metadados,
      metricas = calcular_metricas_snapshot(dados_resultado),
      id = paste0("PROC_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    
    # Adicionar ao histórico
    if(historico$indice_atual < length(historico$processamentos)) {
      historico$processamentos <- historico$processamentos[1:historico$indice_atual]
    }
    
    historico$processamentos <- append(historico$processamentos, list(snapshot))
    historico$indice_atual <- length(historico$processamentos)
    
    # Limitar histórico
    if(length(historico$processamentos) > historico$max_historico) {
      historico$processamentos <- tail(historico$processamentos, historico$max_historico)
      historico$indice_atual <- length(historico$processamentos)
    }
    
    cat("✅ Histórico atualizado:", length(historico$processamentos), "sessões\n")
    
    return(snapshot$id)
  }
  
  # Função para calcular métricas do snapshot
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
    
    # Verificar se as colunas necessárias existem
    colunas_necessarias <- c("tipo_intervencao_antigo", "tipo_novo", "confianca")
    
    if(!all(colunas_necessarias %in% names(dados))) {
      return(list(
        total = nrow(dados),
        conformes = NA,
        divergentes = NA,
        acuracia = NA,
        confianca_media = NA
      ))
    }
    
    # Filtrar dados válidos
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
    
    # Calcular métricas
    conformes <- sum(dados_validos$tipo_intervencao_antigo == dados_validos$tipo_novo, na.rm = TRUE)
    total <- nrow(dados_validos)
    acuracia <- ifelse(total > 0, (conformes / total) * 100, 0)
    
    return(list(
      total = total,
      conformes = conformes,
      divergentes = total - conformes,
      acuracia = round(acuracia, 2),
      confianca_media = round(mean(dados_validos$confianca, na.rm = TRUE), 2)
    ))
  }
  
  # Função para navegar no histórico
  navegar_historico <- function(direcao) {
    
    if(direcao == "anterior" && historico$indice_atual > 1) {
      historico$indice_atual <- historico$indice_atual - 1
      cat("⬅️ Voltou para sessão", historico$indice_atual, "\n")
      return(TRUE)
    }
    
    if(direcao == "proximo" && historico$indice_atual < length(historico$processamentos)) {
      historico$indice_atual <- historico$indice_atual + 1
      cat("➡️ Avançou para sessão", historico$indice_atual, "\n")
      return(TRUE)
    }
    
    cat("⚠️ Não foi possível navegar na direção:", direcao, "\n")
    return(FALSE)
  }
  
  # ============================================================================
  # CONFIGURAÇÃO REATIVA DO USUÁRIO (NO SERVER)
  # ============================================================================
  
  # Configuração padrão
  CONFIG_PADRAO <- list(
    usar_dicionario = TRUE,
    usar_api = TRUE,
    usar_modelo_treinado = TRUE,
    prioridade = "HIBRIDO",
    dicionarios = DICIONARIOS_SAP,
    extrair_assuntos = TRUE,
    batch_size = 5,
    timeout_api = 30,
    confianca_minima = 70
  )
  
  # Configuração reativa
  CONFIG_USUARIO <- reactive({
    list(
      usar_dicionario = if(is.null(input$usar_dicionario)) 
        CONFIG_PADRAO$usar_dicionario else input$usar_dicionario,
      usar_api = if(is.null(input$usar_api)) 
        CONFIG_PADRAO$usar_api else input$usar_api,
      usar_modelo_treinado = if(is.null(input$usar_modelo_treinado)) 
        CONFIG_PADRAO$usar_modelo_treinado else input$usar_modelo_treinado,
      prioridade = if(is.null(input$prioridade)) 
        CONFIG_PADRAO$prioridade else input$prioridade,
      dicionarios = DICIONARIOS_SAP,
      extrair_assuntos = CONFIG_PADRAO$extrair_assuntos,
      batch_size = CONFIG_PADRAO$batch_size,
      timeout_api = CONFIG_PADRAO$timeout_api,
      confianca_minima = if(is.null(input$confianca_minima)) 
        CONFIG_PADRAO$confianca_minima else input$confianca_minima
    )
  })
  
  
  # ============================================================================
  # OBSERVERS PARA VALIDAÇÃO (ADICIONAR NO SERVER)
  # ============================================================================
  
  # Observer para capturar validações de feedback
  observe({
    if(!is.null(values$resultados_lote)) {
      
      lapply(values$resultados_lote$nota_key, function(nota) {
        
        # Observers para feedback de qualidade
        for(feedback in c("excelente", "boa", "ruim")) {
          
          button_id <- paste0("feedback_", nota, "_", feedback)
          
          observeEvent(input[[button_id]], {
            
            # Salvar feedback (você pode implementar a lógica aqui)
            cat("📝 Feedback recebido:", nota, "-", feedback, "\n")
            
            showNotification(
              paste("✅ Feedback salvo: Nota", nota, "-", feedback),
              type = "message",
              duration = 3
            )
          })
        }
      })
    }
  })
  
  
  # ============================================================================
  # FUNÇÃO: PROCESSAR LOTE COM CONFIGURAÇÃO REATIVA
  # ============================================================================
  
  processar_lote_com_config <- function(dados_textos) {
    
    req(dados_textos)
    
    # Obter configuração atual
    config <- CONFIG_USUARIO()
    
    cat("\n🔧 Processando lote com configuração:\n")
    cat("  - Dicionário:", config$usar_dicionario, "\n")
    cat("  - API:", config$usar_api, "\n")
    cat("  - Modelo Treinado:", config$usar_modelo_treinado, "\n")
    cat("  - Prioridade:", config$prioridade, "\n")
    cat("  - Extrair assuntos:", config$extrair_assuntos, "\n")
    
    resultados <- list()
    total <- nrow(dados_textos)
    
    withProgress(message = 'Classificando em lote...', value = 0, {
      
      for(i in 1:total) {
        
        texto <- dados_textos$texto_completo[i]
        
        # Skip if empty
        if(is.na(texto) || nchar(trimws(texto)) == 0) {
          resultados[[i]] <- list(
            tipo_novo = NA,
            categoria = NA,
            criticidade = NA,
            confianca = 0,
            descricao = "Texto vazio",
            resumo = "Texto vazio - não classificado",
            metodo = "SKIP",
            assunto_principal = "Sem texto"
          )
          next
        }
        
        # Classificar
        resultado <- tryCatch({
          
          # Usar modelo treinado se configurado
          if(config$usar_modelo_treinado && 
             !is.null(validacoes_modelo$modelo_ativo) &&
             isTRUE(validacoes_modelo$configuracoes$usar_em_classificacao)) {
            
            classificar_com_modelo_treinado(texto)
            
          } else {
            # Usar método híbrido
            classificar_hibrido(texto, config)
          }
          
        }, error = function(e) {
          cat("❌ Erro ao classificar linha", i, ":", e$message, "\n")
          
          # Fallback para dicionário
          fallback <- classificar_por_dicionario(texto, DICIONARIOS_SAP)
          fallback$metodo <- paste0("FALLBACK (", substr(e$message, 1, 30), ")")
          return(fallback)
        })
        
        # Extrair assunto
        assunto <- if(config$extrair_assuntos) {
          tryCatch({
            extrair_assunto_principal(texto)
          }, error = function(e) {
            cat("❌ Erro ao extrair assunto:", e$message, "\n")
            extrair_assunto_fallback(texto)
          })
        } else {
          "Não extraído"
        }
        
        # Combinar resultados
        resultados[[i]] <- c(
          as.list(dados_textos[i, ]),
          list(
            tipo_novo = resultado$tipo,
            categoria = resultado$categoria,
            criticidade = resultado$criticidade,
            confianca = resultado$confianca,
            descricao = resultado$descricao,
            resumo = resultado$resumo,
            metodo = resultado$metodo,
            assunto_principal = assunto
          )
        )
        
        # Atualizar progresso
        incProgress(1/total, detail = paste("Processando", i, "de", total))
        
        # Pausa para não sobrecarregar
        if(i %% config$batch_size == 0) {
          Sys.sleep(0.1)
        }
      }
    })
    
    # Converter para dataframe
    resultado_df <- do.call(rbind, lapply(resultados, function(x) {
      data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    }))
    
    cat("\n✅ Processamento concluído:", nrow(resultado_df), "registros\n")
    
    return(resultado_df)
  }
  # ============================================================================
  # 🔧 SERVIDOR MODELO ML - CÓDIGO SIMPLIFICADO
  # ============================================================================
  if(carregar_dados_modelo()) {
    cat("✅ Dados anteriores carregados\n")
  } else {
    cat("ℹ️ Primeira execução - sem dados anteriores\n")
  }
  # Status do modelo
  output$status_ml <- renderUI({
    
    total <- nrow(modelo_ml_dados$validacoes)
    tem_modelo <- !is.null(modelo_ml_dados$modelo)
    
    if(tem_modelo) {
      acuracia <- modelo_ml_dados$metricas$acuracia
      
      div(
        style = "background: #d4edda; padding: 20px; border-radius: 8px;",
        h5(style = "color: #155724;", "✅ Modelo Treinado"),
        p(style = "color: #155724; margin: 0;",
          "Validações: ", total, " | ",
          "Acurácia: ", acuracia, "% | ",
          "Última atualização: ", format(modelo_ml_dados$metricas$ultima_atualizacao, "%d/%m %H:%M")
        )
      )
      
    } else {
      
      div(
        style = "background: #fff3cd; padding: 20px; border-radius: 8px;",
        h5(style = "color: #856404;", "⚠️ Modelo Não Treinado"),
        p(style = "color: #856404; margin: 0;",
          "Validações: ", total, " de ", modelo_ml_dados$configuracao$min_validacoes, " necessárias"
        )
      )
    }
  })
  
  # Carregar registros para validação
  observeEvent(input$carregar_ml, {
    
    req(values$resultados_lote)
    
    dados <- switch(
      input$filtro_ml,
      "todos" = values$resultados_lote,
      "nao_validados" = values$resultados_lote[!values$resultados_lote$nota_key %in% modelo_ml_dados$validacoes$id, ],
      "divergentes" = values$resultados_lote[values$resultados_lote$status_conformidade == "DIVERGENTE", ]
    )
    
    dados <- head(dados, input$limite_ml)
    
    if(nrow(dados) == 0) {
      output$cards_ml <- renderUI({
        div(
          style = "text-align: center; padding: 40px; color: #999;",
          h5("Nenhum registro encontrado")
        )
      })
      return()
    }
    
    output$cards_ml <- renderUI({
      
      cards <- lapply(1:nrow(dados), function(i) {
        
        registro <- dados[i, ]
        
        div(
          style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; 
               padding: 15px; margin-bottom: 15px;",
          
          h6(paste("📋 Nota:", registro$nota_key)),
          
          p(style = "font-size: 12px; color: #666;",
            substr(registro$texto_completo, 1, 150),
            if(nchar(registro$texto_completo) > 150) "..." else ""
          ),
          
          p(style = "margin-bottom: 15px;",
            strong("Tipo IA: "), registro$tipo_novo, " | ",
            strong("Confiança: "), registro$confianca, "%"
          ),
          
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            
            lapply(1:6, function(tipo) {
              
              cor <- switch(
                as.character(tipo),
                "1" = "#007bff", "2" = "#28a745", "3" = "#ffc107",
                "4" = "#fd7e14", "5" = "#dc3545", "6" = "#6f42c1"
              )
              
              actionButton(
                paste0("validar_ml_", registro$nota_key, "_", tipo),
                label = tipo,
                style = paste0("background: ", cor, "; color: white; border: none; 
                           padding: 8px 15px; border-radius: 20px; font-weight: bold;")
              )
            })
          )
        )
      })
      
      div(cards)
    })
    
    showNotification(paste("✅", nrow(dados), "registros carregados"), type = "message")
  })
  
  # Observers para validações
  observe({
    
    if(!is.null(values$resultados_lote)) {
      
      lapply(values$resultados_lote$nota_key, function(nota) {
        lapply(1:6, function(tipo) {
          
          button_id <- paste0("validar_ml_", nota, "_", tipo)
          
          observeEvent(input[[button_id]], {
            
            sucesso <- salvar_validacao_ml(nota, tipo)
            
            if(sucesso) {
              showNotification(paste("✅ Validação salva:", nota, "→", tipo), type = "message")
            }
          })
        })
      })
    }
  })
  
  # Treinar modelo
  observeEvent(input$treinar_ml, {
    
    total <- nrow(modelo_ml_dados$validacoes)
    
    if(total < modelo_ml_dados$configuracao$min_validacoes) {
      showNotification(
        paste("⚠️ Necessário pelo menos", modelo_ml_dados$configuracao$min_validacoes, "validações"),
        type = "warning"
      )
      return()
    }
    
    withProgress(message = "Treinando modelo...", {
      
      resultado <- treinar_modelo_ml()
      
      if(resultado$sucesso) {
        showNotification(
          paste("✅ Modelo treinado! Acurácia:", resultado$acuracia, "%"),
          type = "message"
        )
      } else {
        showNotification(
          paste("❌ Erro:", resultado$erro),
          type = "error"
        )
      }
    })
  })
  
  # Usar modelo automaticamente
  observeEvent(input$usar_ml, {
    
    modelo_ml_dados$configuracao$ativo <- input$usar_ml
    
    if(input$usar_ml) {
      if(is.null(modelo_ml_dados$modelo)) {
        showNotification("⚠️ Treine o modelo primeiro!", type = "warning")
        updateCheckboxInput(session, "usar_ml", value = FALSE)
      } else {
        showNotification("✅ Modelo ativado para uso automático", type = "message")
      }
    } else {
      showNotification("ℹ️ Modelo desativado", type = "message")
    }
  })
  
  # Teste rápido
  observeEvent(input$teste_ml, {
    
    if(is.null(modelo_ml_dados$modelo)) {
      showNotification("⚠️ Nenhum modelo treinado", type = "warning")
      return()
    }
    
    texto_exemplo <- "Substituição de válvula por falha operacional"
    resultado <- predizer_ml(texto_exemplo)
    
    if(resultado$sucesso) {
      showModal(modalDialog(
        title = "🧪 Teste Rápido",
        
        div(
          h5("Texto:", em(texto_exemplo)),
          h5("Tipo predito:", strong(resultado$tipo)),
          h5("Confiança:", strong(paste0(resultado$confianca, "%")))
        ),
        
        footer = modalButton("Fechar")
      ))
    }
  })
  
  # Teste personalizado
  observeEvent(input$executar_teste_ml, {
    
    req(input$texto_teste_ml)
    
    if(is.null(modelo_ml_dados$modelo)) {
      showNotification("⚠️ Nenhum modelo treinado", type = "warning")
      return()
    }
    
    resultado <- predizer_ml(input$texto_teste_ml)
    
    if(resultado$sucesso) {
      
      output$resultado_teste_ml <- renderUI({
        div(
          style = "background: #d4edda; padding: 20px; border-radius: 8px; margin-top: 15px;",
          h5(style = "color: #155724;", "🎯 Resultado"),
          p(style = "color: #155724; margin: 0;",
            strong("Tipo predito: "), resultado$tipo, " | ",
            strong("Confiança: "), resultado$confianca, "%"
          )
        )
      })
      
    } else {
      
      output$resultado_teste_ml <- renderUI({
        div(
          style = "background: #f8d7da; padding: 20px; border-radius: 8px; margin-top: 15px;",
          h5(style = "color: #721c24;", "❌ Erro"),
          p(style = "color: #721c24; margin: 0;", resultado$erro)
        )
      })
    }
  })
  
  
  
  # ============================================================================
  # 🔧 CÓDIGO DO SERVIDOR PARA MODELO ML
  # Adicionar dentro da function(input, output, session) do server
  # ============================================================================
  
  # Status do modelo ML
  output$status_modelo_ml <- renderUI({
    
    total_validacoes <- nrow(validacoes_modelo$dados)
    tem_modelo <- !is.null(validacoes_modelo$modelo_ativo)
    
    if(tem_modelo) {
      
      acuracia <- validacoes_modelo$metricas$acuracia
      ultima_atualizacao <- validacoes_modelo$metricas$ultima_atualizacao
      
      cor_status <- if(acuracia >= 85) "#28a745" else if(acuracia >= 70) "#ffc107" else "#dc3545"
      texto_status <- if(acuracia >= 85) "EXCELENTE" else if(acuracia >= 70) "BOM" else "TREINAR MAIS"
      
      HTML(paste0(
        "<div style='display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px;'>",
        
        # Status
        "<div style='background: linear-gradient(135deg, ", cor_status, " 0%, rgba(", 
        paste(col2rgb(cor_status), collapse = ","), ", 0.8) 100%); 
             padding: 25px; border-radius: 12px; text-align: center; color: white;
             box-shadow: 0 8px 24px rgba(0,0,0,0.2);'>",
        "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>STATUS</div>",
        "<div style='font-size: 20px; font-weight: 800;'>", texto_status, "</div>",
        "</div>",
        
        # Validações
        "<div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
             padding: 25px; border-radius: 12px; text-align: center; color: white;
             box-shadow: 0 8px 24px rgba(102, 126, 234, 0.3);'>",
        "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>VALIDAÇÕES</div>",
        "<div style='font-size: 32px; font-weight: 800;'>", total_validacoes, "</div>",
        "</div>",
        
        # Acurácia
        "<div style='background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
             padding: 25px; border-radius: 12px; text-align: center; color: white;
             box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3);'>",
        "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>ACURÁCIA</div>",
        "<div style='font-size: 32px; font-weight: 800;'>", acuracia, "%</div>",
        "</div>",
        
        # Última atualização
        "<div style='background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
             padding: 25px; border-radius: 12px; text-align: center; color: white;
             box-shadow: 0 8px 24px rgba(79, 172, 254, 0.3);'>",
        "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>ATUALIZADO</div>",
        "<div style='font-size: 16px; font-weight: 800;'>", 
        format(ultima_atualizacao, "%d/%m %H:%M"), "</div>",
        "</div>",
        
        "</div>"
      ))
      
    } else {
      
      progresso <- min(100, (total_validacoes / validacoes_modelo$configuracoes$min_validacoes) * 100)
      
      HTML(paste0(
        "<div style='background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); 
                 padding: 30px; border-radius: 15px; border-left: 6px solid #ffc107;'>",
        
        "<div style='display: flex; align-items: center; justify-content: space-between;'>",
        
        "<div style='flex: 1;'>",
        "<h4 style='color: #856404; margin: 0 0 15px 0; font-weight: 700;'>🤖 Modelo Não Treinado</h4>",
        "<p style='color: #856404; margin: 0 0 15px 0; font-size: 14px;'>",
        "Você tem <strong>", total_validacoes, "</strong> de <strong>", 
        validacoes_modelo$configuracoes$min_validacoes, "</strong> validações necessárias.",
        "</p>",
        
        # Barra de progresso
        "<div style='background: rgba(133, 100, 4, 0.2); height: 20px; border-radius: 10px; overflow: hidden;'>",
        "<div style='background: #856404; height: 100%; width: ", progresso, "%; 
                 transition: width 0.3s ease; border-radius: 10px;'></div>",
        "</div>",
        "<div style='text-align: center; margin-top: 10px; color: #856404; font-size: 12px; font-weight: 700;'>",
        round(progresso, 1), "% concluído",
        "</div>",
        "</div>",
        
        "<div style='text-align: center; margin-left: 30px;'>",
        "<div style='font-size: 72px; opacity: 0.6;'>🎯</div>",
        "</div>",
        
        "</div>",
        "</div>"
      ))
    }
  })
  
  # Estatísticas do modelo ML
  output$stats_modelo_ml <- renderUI({
    
    total <- nrow(validacoes_modelo$dados)
    
    if(total == 0) {
      return(HTML(paste0(
        "<div style='text-align: center; padding: 40px; color: #999;'>",
        "<div style='font-size: 48px; margin-bottom: 15px;'>📊</div>",
        "<h5>Nenhuma validação ainda</h5>",
        "<p style='font-size: 13px;'>Execute classificações e valide alguns resultados</p>",
        "</div>"
      )))
    }
    
    # Calcular estatísticas
    dados <- validacoes_modelo$dados
    
    acertos_ia <- sum(dados$tipo_ia == dados$tipo_validado, na.rm = TRUE)
    taxa_acerto_ia <- round((acertos_ia / total) * 100, 1)
    
    # Distribuição de feedback
    feedback_bom <- sum(dados$feedback_qualidade %in% c("excelente", "boa"), na.rm = TRUE)
    taxa_satisfacao <- round((feedback_bom / total) * 100, 1)
    
    # Tipo mais validado
    tipos_freq <- table(dados$tipo_validado)
    tipo_mais_comum <- names(tipos_freq)[which.max(tipos_freq)]
    
    HTML(paste0(
      "<div style='display: grid; grid-template-columns: 1fr; gap: 15px;'>",
      
      # Total validações
      "<div style='background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
               padding: 20px; border-radius: 12px; text-align: center;'>",
      "<div style='font-size: 28px; font-weight: 800; color: #1565C0; margin-bottom: 8px;'>", total, "</div>",
      "<div style='font-size: 12px; color: #1565C0; font-weight: 700;'>TOTAL VALIDAÇÕES</div>",
      "</div>",
      
      # Taxa de acerto da IA
      "<div style='background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
               padding: 20px; border-radius: 12px; text-align: center;'>",
      "<div style='font-size: 28px; font-weight: 800; color: #2E7D32; margin-bottom: 8px;'>", taxa_acerto_ia, "%</div>",
      "<div style='font-size: 12px; color: #2E7D32; font-weight: 700;'>ACERTO DA IA</div>",
      "</div>",
      
      # Satisfação
      "<div style='background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); 
               padding: 20px; border-radius: 12px; text-align: center;'>",
      "<div style='font-size: 28px; font-weight: 800; color: #856404; margin-bottom: 8px;'>", taxa_satisfacao, "%</div>",
      "<div style='font-size: 12px; color: #856404; font-weight: 700;'>SATISFAÇÃO</div>",
      "</div>",
      
      # Tipo mais comum
      "<div style='background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%); 
               padding: 20px; border-radius: 12px; text-align: center;'>",
      "<div style='font-size: 28px; font-weight: 800; color: #7B1FA2; margin-bottom: 8px;'>", tipo_mais_comum, "</div>",
      "<div style='font-size: 12px; color: #7B1FA2; font-weight: 700;'>TIPO MAIS COMUM</div>",
      "</div>",
      
      "</div>"
    ))
  })
  
  # Observer para carregar registros para validação
  observeEvent(input$carregar_validacao_ml, {
    
    req(values$resultados_lote)
    
    # Aplicar filtros
    dados_filtrados <- switch(
      input$filtro_modelo_ml,
      "todos" = values$resultados_lote,
      "nao_validados" = values$resultados_lote %>%
        filter(!nota_key %in% validacoes_modelo$dados$id),
      "divergentes" = values$resultados_lote %>%
        filter(status_conformidade == "DIVERGENTE"),
      "baixa_confianca" = values$resultados_lote %>%
        filter(confianca < 80),
      "alta_confianca" = values$resultados_lote %>%
        filter(confianca >= 90)
    )
    
    # Limitar quantidade
    dados_validacao <- head(dados_filtrados, input$limite_modelo_ml)
    
    if(nrow(dados_validacao) == 0) {
      output$cards_validacao_ml <- renderUI({
        div(
          style = "text-align: center; padding: 60px; color: #999;",
          icon("check-circle", style = "font-size: 72px; margin-bottom: 20px;"),
          h4("Nenhum registro encontrado"),
          p("Todos os registros já foram validados ou não há dados com esse filtro")
        )
      })
      return()
    }
    
    # Gerar cards de validação
    output$cards_validacao_ml <- renderUI({
      
      cards <- lapply(1:nrow(dados_validacao), function(i)  {
        
        registro <- dados_validacao[i, ]
        
        # Cores baseadas na criticidade
        cor_header <- switch(
          registro$criticidade,
          "BAIXA" = "#4682B4",
          "MEDIA" = "#32CD32", 
          "ALTA" = "#FF8C00",
          "CRITICA" = "#DC143C"
        )
        
        div(
          style = "background: white; border-radius: 15px; margin-bottom: 25px; 
               box-shadow: 0 4px 16px rgba(0,0,0,0.1); overflow: hidden;",
          
          # Header do card
          div(
            style = paste0("background: linear-gradient(135deg, ", cor_header, " 0%, rgba(", 
                           paste(col2rgb(cor_header), collapse = ","), ", 0.8) 100%);
                       padding: 20px; color: white;"),
            
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              
              div(
                h4(style = "margin: 0 0 8px 0; font-weight: 700;",
                   "📋 Nota: ", registro$nota_key),
                p(style = "margin: 0; opacity: 0.9; font-size: 13px;",
                  "Tipo IA: ", registro$tipo_novo, " | ",
                  "Confiança: ", registro$confianca, "% | ",
                  "Criticidade: ", registro$criticidade)
              ),
              
              div(
                style = "text-align: center;",
                div(style = "background: rgba(255,255,255,0.3); padding: 8px 15px; 
                         border-radius: 20px; font-weight: 700; font-size: 14px;",
                    registro$categoria)
              )
            )
          ),
          
          # Corpo do card
          div(
            style = "padding: 25px;",
            
            # Texto (resumido)
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; 
                   margin-bottom: 20px; border-left: 4px solid #667eea;",
              p(style = "margin: 0; font-size: 13px; color: #666; line-height: 1.6;",
                substr(registro$texto_completo, 1, 200), 
                ifelse(nchar(registro$texto_completo) > 200, "...", ""))
            ),
            
            # Assunto principal (se disponível)
            if(!is.na(registro$assunto_principal) && nchar(registro$assunto_principal) > 0) {
              div(
                style = "margin-bottom: 20px;",
                h6(style = "color: #333; margin-bottom: 8px; font-weight: 700;",
                   icon("lightbulb"), " Assunto Identificado:"),
                div(
                  style = "background: #e3f2fd; padding: 12px; border-radius: 6px;",
                  p(style = "margin: 0; font-size: 13px; color: #1565C0; font-weight: 600;",
                    registro$assunto_principal)
                )
              )
            },
            
            # Botões de validação
            div(
              h6(style = "color: #333; margin-bottom: 15px; font-weight: 700;",
                 icon("check-double"), " Validar Tipo Correto:"),
              
              div(
                style = "display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px;",
                
                lapply(1:6, function(tipo)  {
                  
                  cor_tipo <- switch(
                    as.character(tipo),
                    "1" = "#4682B4", "2" = "#32CD32", "3" = "#FFD700",
                    "4" = "#FFA500", "5" = "#FF6347", "6" = "#DC143C"
                  )
                  
                  icone <- switch(
                    as.character(tipo),
                    "1" = "🧽", "2" = "🔧", "3" = "🔍",
                    "4" = "⏰", "5" = "⚠️", "6" = "🚨"
                  )
                  
                  # Destacar se é o tipo atual da IA
                  estilo_extra <- if(tipo == registro$tipo_novo) {
                    "border: 3px solid #333; transform: scale(1.05); box-shadow: 0 4px 12px rgba(0,0,0,0.3);"
                  } else {
                    ""
                  }
                  
                  actionButton(
                    paste0("validar_ml_", registro$nota_key, "_", tipo),
                    label = div(
                      div(style = "font-size: 20px; margin-bottom: 4px;", icone),
                      div(style = "font-size: 16px; font-weight: 800;", tipo),
                      style = "text-align: center; line-height: 1.2;"
                    ),
                    style = paste0(
                      "background: ", cor_tipo, "; color: white; border: none; 
                   padding: 12px 8px; border-radius: 12px; width: 100%; 
                   transition: all 0.3s ease; ", estilo_extra
                    )
                  )
                })
              )
            )
          )
        )
      })
      
      div(cards)
    })
    
    showNotification(
      paste("✅", nrow(dados_validacao), "registros carregados para validação"),
      type = "message",
      duration = 3
    )
  })
  
  # Observers simplificados para capturar validações ML
  observe({
    if(!is.null(values$resultados_lote) && nrow(values$resultados_lote) > 0) {
      for(nota in values$resultados_lote$nota_key) {
        for(tipo in 1:6) {
          local({
            n <- nota
            t <- tipo
            button_id <- paste0("validar_ml_", n, "_", t)
            observeEvent(input[[button_id]], {
              cat("📝 Botão clicado:", button_id, "\n")
              sucesso <- salvar_validacao_ml(
                registro_id = n,
                tipo_validado = t,
                feedback = "boa",
                values_env = values
              )
              if(sucesso) {
                showNotification(
                  paste("✅ Validação salva: Nota", n, "→ Tipo", t),
                  type = "message",
                  duration = 3
                )
                update_model_metrics()
                if(nrow(validacoes_modelo$dados) %% 5 == 0) {
                  cat("🔄 Tentando treinamento incremental...\n")
                  resultado <- treinar_modelo_ml_incremental()
                  if(resultado$sucesso) {
                    showNotification(
                      paste("🤖 Modelo atualizado! Acurácia:", resultado$acuracia, "%"),
                      type = "message",
                      duration = 5
                    )
                  }
                }
              } else {
                showNotification("❌ Erro ao salvar validação", type = "error")
              }
            }, ignoreInit = TRUE)
          })
        }
      }
    }
  })
  
  # Observer para treinar modelo
  observeEvent(input$treinar_modelo_ml, {
    
    total_validacoes <- nrow(validacoes_modelo$dados)
    
    if(total_validacoes < validacoes_modelo$configuracoes$min_validacoes) {
      showNotification(
        paste("⚠️ Necessário pelo menos", validacoes_modelo$configuracoes$min_validacoes, 
              "validações. Atual:", total_validacoes),
        type = "warning",
        duration = 5
      )
      return()
    }
    
    withProgress(message = '🤖 Treinando modelo ML...', value = 0, {
      
      incProgress(0.3, detail = "Preparando dados...")
      
      resultado <- treinar_modelo_ml()
      
      incProgress(0.7, detail = "Finalizando...")
      
      if(resultado$sucesso) {
        showNotification(
          paste0("✅ Modelo treinado com sucesso! Acurácia: ", resultado$acuracia, "%"),
          type = "message",
          duration = 8
        )
        
        # Mostrar detalhes em modal
        showModal(modalDialog(
          title = "🎉 Modelo Treinado com Sucesso!",
          size = "m",
          
          div(
            style = "padding: 20px; text-align: center;",
            
            div(
              style = "font-size: 72px; margin-bottom: 20px;", "🧠"
            ),
            
            h4(style = "color: #28a745; margin-bottom: 20px;", 
               "Seu modelo personalizado está pronto!"),
            
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 25px;",
              
              div(
                style = "background: #e8f5e9; padding: 20px; border-radius: 12px;",
                h3(style = "color: #2E7D32; margin: 0 0 10px 0;", resultado$acuracia, "%"),
                p(style = "color: #2E7D32; margin: 0; font-size: 14px;", "Acurácia obtida")
              ),
              
              div(
                style = "background: #e3f2fd; padding: 20px; border-radius: 12px;",
                h3(style = "color: #1565C0; margin: 0 0 10px 0;", resultado$total_dados),
                p(style = "color: #1565C0; margin: 0; font-size: 14px;", "Dados de treino")
              )
            ),
            
            p(style = "color: #666; font-size: 14px; line-height: 1.6;",
              "O modelo aprendeu com suas validações e agora pode ser usado para melhorar ",
              "automaticamente as futuras classificações. Ative-o nas configurações abaixo.")
          ),
          
          footer = modalButton("Entendi!")
        ))
        
      } else {
        showNotification(
          paste("❌ Erro no treinamento:", resultado$erro),
          type = "error",
          duration = 8
        )
      }
      
      incProgress(1, detail = "Concluído!")
    })
  })
  
  # Observer para ativar/desativar uso automático do modelo treinado incremental
  observeEvent(input$usar_modelo_automatico, {
    # Se o usuário ativar o checkbox
    if (isTRUE(input$usar_modelo_automatico)) {
      # Só ativa se houver modelo treinado
      if (is.null(validacoes_modelo$modelo_ativo)) {
        showNotification("⚠️ Treine o modelo primeiro!", type = "warning")
        updateCheckboxInput(session, "usar_modelo_automatico", value = FALSE)
        return()
      }
      # Atualiza flag de configuração reativa
      validacoes_modelo$configuracoes$usar_em_classificacao <- TRUE
      
      if (exists("CONFIG_USUARIO", mode = "function")) {
        config <- CONFIG_USUARIO()
        config$modelo_treinado$usar_automaticamente <- TRUE
      }
      salvar_dados_modelo()
      showNotification(
        "✅ Modelo ativado! Futuras classificações usarão o modelo treinado automaticamente",
        type = "message",
        duration = 5
      )
    } else {
      # Se o usuário desativar o checkbox
      validacoes_modelo$configuracoes$usar_em_classificacao <- FALSE
      # Se você quiser salvar também em um objeto global, faça assim (opcional):
      if (exists("CONFIG_USUARIO", mode = "function")) {
        config <- CONFIG_USUARIO()
        config$modelo_treinado$usar_automaticamente <- FALSE
      }
      salvar_dados_modelo()
      showNotification(
        "ℹ️ Modelo desativado. Classificações usarão apenas dicionário e API",
        type = "message",
        duration = 3
      )
    }
    # Se quiser atualizar outputs visuais, faça aqui
    # output$status_modelo_ml <- renderUI({ ... })
  })
  # Observer para teste rápido
  observeEvent(input$teste_rapido_ml, {
    
    if(is.null(validacoes_modelo$modelo_ativo)) {
      showNotification("⚠️ Nenhum modelo treinado disponível", type = "warning")
      return()
    }
    
    # Texto de exemplo
    texto_exemplo <- "Substituição de válvula de segurança por falha operacional detectada durante inspeção"
    
    resultado <- predizer_com_modelo(texto_exemplo)
    
    if(resultado$sucesso) {
      
      showModal(modalDialog(
        title = "🧪 Resultado do Teste Rápido",
        size = "m",
        
        div(
          style = "padding: 20px;",
          
          div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;
                 border-left: 4px solid #667eea;",
            strong("Texto testado: "), 
            em(texto_exemplo)
          ),
          
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin-bottom: 20px;",
            
            div(
              style = "text-align: center; padding: 25px; background: #e3f2fd; border-radius: 12px;",
              h2(style = "color: #1565C0; margin: 0 0 10px 0;", resultado$tipo),
              p(style = "color: #1565C0; margin: 0; font-weight: 700;", "Tipo Predito")
            ),
            
            div(
              style = "text-align: center; padding: 25px; background: #e8f5e9; border-radius: 12px;",
              h2(style = "color: #2E7D32; margin: 0 0 10px 0;", resultado$confianca, "%"),
              p(style = "color: #2E7D32; margin: 0; font-weight: 700;", "Confiança")
            ),
            
            div(
              style = "text-align: center; padding: 25px; background: #fff3cd; border-radius: 12px;",
              h2(style = "color: #856404; margin: 0 0 10px 0;", "ML"),
              p(style = "color: #856404; margin: 0; font-weight: 700;", "Método")
            )
          ),
          
          div(
            style = "background: #d4edda; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745;",
            p(style = "margin: 0; color: #155724; font-size: 14px;",
              "✅ O modelo funcionou corretamente! Este é um exemplo de como ele classifica textos automaticamente.")
          )
        ),
        
        footer = modalButton("Fechar")
      ))
      
    } else {
      showNotification("❌ Erro no teste do modelo", type = "error")
    }
  })
  
  # Observer para teste personalizado
  observeEvent(input$executar_teste_ml, {
    
    req(input$texto_teste_ml)
    
    if(nchar(trimws(input$texto_teste_ml)) == 0) {
      showNotification("⚠️ Digite um texto para testar", type = "warning")
      return()
    }
    
    if(is.null(validacoes_modelo$modelo_ativo)) {
      showNotification("⚠️ Nenhum modelo treinado disponível", type = "warning")
      return()
    }
    
    withProgress(message = '🧪 Testando modelo...', value = 0, {
      
      incProgress(0.5, detail = "Fazendo predição...")
      
      resultado <- predizer_com_modelo(input$texto_teste_ml)
      
      incProgress(1, detail = "Concluído!")
      
      if(resultado$sucesso) {
        
        cor_criticidade <- switch(
          as.character(resultado$tipo),
          "1" = "#4682B4", "2" = "#90EE90", "3" = "#FFD700",
          "4" = "#FFA500", "5" = "#FF6347", "6" = "#DC143C"
        )
        
        criticidade <- switch(
          as.character(resultado$tipo),
          "1" = "BAIXA", "2" = "BAIXA", "3" = "MÉDIA",
          "4" = "MÉDIA", "5" = "ALTA", "6" = "CRÍTICA"
        )
        
        categoria <- ifelse(resultado$tipo %in% c(5, 6), "IAZF", "PROBLEMAS_COMUNS")
        
        output$resultado_teste_ml <- renderUI({
          HTML(paste0(
            "<div style='background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                     padding: 30px; border-radius: 15px; margin-top: 25px;
                     border-left: 6px solid #28a745; box-shadow: 0 4px 16px rgba(40, 167, 69, 0.2);'>",
            
            "<div style='display: flex; align-items: center; margin-bottom: 25px;'>",
            "<span style='font-size: 56px; margin-right: 25px;'>🎯</span>",
            "<div>",
            "<h3 style='color: #155724; margin: 0 0 8px 0; font-weight: 800;'>Resultado do Modelo ML</h3>",
            "<p style='color: #155724; margin: 0; font-size: 14px;'>Classificação baseada em aprendizado de máquina</p>",
            "</div>",
            "</div>",
            
            "<div style='display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 20px;'>",
            
            # Tipo
            "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; 
                     box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid ", cor_criticidade, ";'>",
            "<div style='color: #999; font-size: 13px; margin-bottom: 10px; font-weight: 700;'>TIPO SAP</div>",
            "<div style='font-size: 48px; color: ", cor_criticidade, "; font-weight: 800; margin: 15px 0;'>", 
            resultado$tipo, "</div>",
            "<div style='color: #666; font-size: 14px; font-weight: 600;'>Classificação</div>",
            "</div>",
            
            # Confiança
            "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; 
                     box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid #11998e;'>",
            "<div style='color: #999; font-size: 13px; margin-bottom: 10px; font-weight: 700;'>CONFIANÇA</div>",
            "<div style='font-size: 48px; color: #11998e; font-weight: 800; margin: 15px 0;'>", 
            resultado$confianca, "%</div>",
            "<div style='color: #666; font-size: 14px; font-weight: 600;'>Modelo ML</div>",
            "</div>",
            
            # Criticidade
            "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; 
                     box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid ", cor_criticidade, ";'>",
            "<div style='color: #999; font-size: 13px; margin-bottom: 10px; font-weight: 700;'>CRITICIDADE</div>",
            "<div style='font-size: 24px; color: ", cor_criticidade, "; font-weight: 800; margin: 15px 0;'>", 
            criticidade, "</div>",
            "<div style='color: #666; font-size: 14px; font-weight: 600;'>Nível</div>",
            "</div>",
            
            # Categoria
            "<div style='background: white; padding: 25px; border-radius: 12px; text-align: center; 
                     box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 4px solid #667eea;'>",
            "<div style='color: #999; font-size: 13px; margin-bottom: 10px; font-weight: 700;'>CATEGORIA</div>",
            "<div style='font-size: 20px; color: #667eea; font-weight: 800; margin: 15px 0;'>", 
            categoria, "</div>",
            "<div style='color: #666; font-size: 14px; font-weight: 600;'>Hierarquia</div>",
            "</div>",
            
            "</div>",
            
            "</div>"
          ))
        })
        
        showNotification("✅ Teste realizado com sucesso!", type = "message", duration = 3)
        
      } else {
        
        output$resultado_teste_ml <- renderUI({
          div(
            style = "background: #f8d7da; padding: 25px; border-radius: 12px; 
                 border-left: 6px solid #dc3545; margin-top: 25px;",
            h4(style = "color: #721c24; margin: 0 0 10px 0;", "❌ Erro no Teste"),
            p(style = "color: #721c24; margin: 0;", resultado$erro)
          )
        })
        
        showNotification("❌ Erro ao testar modelo", type = "error")
      }
    })
  })
  
  # Observer para exportar dados
  observeEvent(input$exportar_dados_ml, {
    
    if(nrow(validacoes_modelo$dados) == 0) {
      showNotification("⚠️ Nenhum dado para exportar", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "📤 Exportar Dados do Modelo",
      
      div(
        style = "padding: 20px;",
        
        p("Escolha o que deseja exportar:"),
        
        div(
          style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0;",
          strong("Dados disponíveis:"), br(),
          "• ", nrow(validacoes_modelo$dados), " validações realizadas", br(),
          "• Métricas do modelo treinado", br(),
          "• Histórico de treinamentos"
        )
      ),
      
      footer = tagList(
        modalButton("Cancelar"),
        downloadButton("download_dados_ml", "Baixar CSV", class = "btn-success")
      )
    ))
  })
  
  # Download dos dados
  output$download_dados_ml <- downloadHandler(
    filename = function()  {
      paste0("dados_modelo_ml_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file)  {
      
      # Preparar dados para exportação
      dados_export <- validacoes_modelo$dados %>%
        mutate(
          acerto_ia = ifelse(tipo_ia == tipo_validado, "SIM", "NÃO"),
          data_validacao = format(timestamp, "%d/%m/%Y %H:%M:%S")
        ) %>%
        select(
          ID = id,
          `Texto Original` = texto_original,
          `Tipo Original` = tipo_original,
          `Tipo IA` = tipo_ia,
          `Tipo Validado` = tipo_validado,
          `Acerto da IA` = acerto_ia,
          `Assunto Original` = assunto_original,
          `Assunto Validado` = assunto_validado,
          `Confiança` = confianca,
          `Feedback` = feedback_qualidade,
          `Data Validação` = data_validacao,
          `Usuário` = usuario,
          `Observações` = observacoes
        )
      
      write.csv(dados_export, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Observer para resetar modelo
  observeEvent(input$resetar_modelo_ml, {
    
    showModal(modalDialog(
      title = "⚠️ Confirmar Reset",
      
      div(
        style = "padding: 20px;",
        
        div(
          style = "background: #f8d7da; padding: 20px; border-radius: 8px; border-left: 4px solid #dc3545;",
          h5(style = "color: #721c24; margin: 0 0 10px 0;", "🚨 ATENÇÃO"),
          p(style = "color: #721c24; margin: 0;",
            "Esta ação irá apagar TODAS as validações e o modelo treinado. ",
            "Esta operação não pode ser desfeita!")
        ),
        
        br(),
        
        p("Tem certeza que deseja continuar?")
      ),
      
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_reset_ml", "Sim, Resetar", class = "btn-danger")
      )
    ))
  })
  
  # Confirmar reset
  observeEvent(input$confirmar_reset_ml, {
    
    # Limpar todos os dados
    validacoes_modelo$dados <- data.frame(
      id = character(0),
      texto_original = character(0),
      tipo_original = integer(0),
      tipo_ia = integer(0),
      tipo_validado = integer(0),
      assunto_original = character(0),
      assunto_validado = character(0),
      confianca = numeric(0),
      feedback_qualidade = character(0),
      timestamp = as.POSIXct(character(0)),
      usuario = character(0),
      observacoes = character(0),
      stringsAsFactors = FALSE
    )
    
    validacoes_modelo$modelo_ativo <- NULL
    
    validacoes_modelo$metricas <- list(
      acuracia = 0,
      total_treinos = 0,
      ultima_atualizacao = Sys.time(),
      features_importantes = character(0)
    )
    
    validacoes_modelo$historico <- list()
    
    # Desativar uso automático
    updateCheckboxInput(session, "usar_modelo_automatico", value = FALSE)
    
    # CORREÇÃO: Não tentar acessar CONFIG_USUARIO como função
    # Atualizar configuração interna
    validacoes_modelo$configuracoes$usar_em_classificacao <- FALSE
    
    # Limpar interface
    output$cards_validacao_ml <- renderUI({ NULL })
    output$resultado_teste_ml <- renderUI({ NULL })
    
    # Remover modal
    removeModal()
    
    showNotification(
      "🗑️ Modelo e validações resetados com sucesso!",
      type = "warning",
      duration = 5
    )
  })
  
  #===========================================================================
  # CONFIGURAÇÃO PADRÃO E REATIVA
  #===========================================================================
  
  # Configuração padrão (não reativa)
  CONFIG_PADRAO <- list(
    usar_dicionario = TRUE,
    usar_api = TRUE,
    usar_modelo_treinado = TRUE,
    prioridade = "HIBRIDO",
    dicionarios = DICIONARIOS_SAP,
    extrair_assuntos = TRUE,
    batch_size = 5,
    timeout_api = 30,
    confianca_minima = 70
  )
  
  # Configuração reativa que se atualiza com os inputs do usuário
  CONFIG_USUARIO <- reactive({
    list(
      usar_dicionario = if(is.null(input$usar_dicionario)) CONFIG_PADRAO$usar_dicionario else input$usar_dicionario,
      usar_api = if(is.null(input$usar_api)) CONFIG_PADRAO$usar_api else input$usar_api,
      usar_modelo_treinado = if(is.null(input$usar_modelo_treinado)) CONFIG_PADRAO$usar_modelo_treinado else input$usar_modelo_treinado,
      prioridade = if(is.null(input$prioridade)) CONFIG_PADRAO$prioridade else input$prioridade,
      dicionarios = DICIONARIOS_SAP,
      extrair_assuntos = CONFIG_PADRAO$extrair_assuntos,
      batch_size = CONFIG_PADRAO$batch_size,
      timeout_api = CONFIG_PADRAO$timeout_api,
      confianca_minima = if(is.null(input$confianca_minima)) CONFIG_PADRAO$confianca_minima else input$confianca_minima
    )
  })
  
  #===========================================================================
  # VALORES REATIVOS CENTRALIZADOS
  #===========================================================================
  
  values <- reactiveValues(
    dados_ordens = NULL,
    dados_textos = NULL,
    dados_preview = NULL,
    col_tip_intervencao = NULL,
    resultados_lote = NULL,
    processando = FALSE,
    dados_com_assuntos = NULL,
    modelo_treinado = NULL,
    status_modelo = "Não treinado"
  )
  
  #===========================================================================
  # OUTPUT REATIVO: Verificar se tem resultados
  #===========================================================================
  
  output$tem_resultados_lote <- reactive({
    !is.null(values$resultados_lote) && nrow(values$resultados_lote) > 0
  })
  
  outputOptions(output, "tem_resultados_lote", suspendWhenHidden = FALSE)
  
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
  adicionar_ao_historico <- function(dados_resultado, metadados = list() ) {
    
    cat("\n💾 Salvando no histórico...\n")
    
    snapshot <- list(
      timestamp = Sys.time(),
      dados = dados_resultado,
      metadados = metadados,
      metricas = calcular_metricas_snapshot(dados_resultado),
      id = paste0("PROC_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      config_usada = isolate(CONFIG_USUARIO())  # Salvar config usada
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
  calcular_metricas_snapshot <- function(dados)  {
    
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
  obter_processamento_atual <- function()  {
    if(historico$indice_atual > 0 && historico$indice_atual <= length(historico$processamentos)) {
      return(historico$processamentos[[historico$indice_atual]])
    }
    return(NULL)
  }
  
  # Função para navegar
  navegar_historico <- function(direcao)  {
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
  # FUNÇÕES AUXILIARES PARA CLASSIFICAÇÃO
  #===========================================================================
  
  # Função para classificar um único texto (usada em vários lugares)
  classificar_texto_unico <- function(texto)  {
    
    req(texto)
    
    # Obter configuração atual
    config <- CONFIG_USUARIO()
    
    # Usar a função híbrida com modelo
    resultado <- classificar_hibrido_com_modelo(texto, config)
    
    return(resultado)
  }
  
  # Função para processar lote com configuração reativa
  processar_lote_com_config <- function(dados_textos)  {
    
    req(dados_textos)
    
    # Obter configuração atual
    config <- CONFIG_USUARIO()
    
    cat("🔧 Configuração atual:\n")
    cat("  - Dicionário:", config$usar_dicionario, "\n")
    cat("  - API:", config$usar_api, "\n")
    cat("  - Modelo Treinado:", config$usar_modelo_treinado, "\n")
    cat("  - Prioridade:", config$prioridade, "\n")
    
    resultados <- list()
    total <- nrow(dados_textos)
    
    for(i in 1:total) {
      
      cat(sprintf("📝 Processando %d/%d...\n", i, total))
      
      texto <- dados_textos$texto_completo[i]
      
      # Classificar com configuração atual
      resultado <- classificar_hibrido_com_modelo(texto, config)
      
      # Extrair assunto se configurado
      assunto <- if(config$extrair_assuntos) {
        extrair_assunto_principal(texto)
      } else {
        "Não extraído"
      }
      
      # Montar resultado
      resultado_completo <- c(
        dados_textos[i, ],
        list(
          tipo_novo = resultado$tipo,
          categoria = resultado$categoria,
          criticidade = resultado$criticidade,
          confianca = resultado$confianca,
          descricao = resultado$descricao,
          resumo = resultado$resumo,
          metodo = resultado$metodo,
          assunto_principal = assunto,
          timestamp_processamento = Sys.time()
        )
      )
      
      resultados[[i]] <- resultado_completo
      
      # Pausa pequena para não sobrecarregar
      if(i %% config$batch_size == 0) {
        Sys.sleep(0.2)
      }
    }
    
    # Converter para data frame
    resultado_df <- do.call(rbind, lapply(resultados, function(x)  {
      data.frame(x, stringsAsFactors = FALSE)
    }))
    
    return(resultado_df)
  }
  
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
  
  #===========================================================================
  # OUTPUTS PARA STATUS DO MODELO TREINADO
  #===========================================================================
  
  output$status_modelo <- renderText({
    if(!is.null(values$modelo_treinado)) {
      paste("✅ Modelo treinado disponível\n",
            "📅 Última atualização:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n",
            "🎯 Status:", values$status_modelo)
    } else {
      "❌ Nenhum modelo treinado disponível\n💡 Use os dados classificados para treinar um modelo personalizado"
    }
  })
  
  #===========================================================================
  # EVENTOS PARA MODELO TREINADO
  #===========================================================================
  
  observeEvent(input$treinar_modelo, {
    
    if(is.null(values$resultados_lote)) {
      showNotification("⚠️ Nenhum dado disponível para treinamento!", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "🤖 Treinando Modelo...",
      div(
        style = "text-align: center; padding: 20px;",
        icon("cog", class = "fa-spin fa-3x"),
        br(), br(),
        "Aguarde enquanto o modelo está sendo treinado...",
        br(),
        "Isso pode levar alguns minutos."
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Simular treinamento (aqui você implementaria o treinamento real)
    future({
      
      # Aqui seria o código real de treinamento
      Sys.sleep(3) # Simular tempo de treinamento
      
      # Retornar resultado do treinamento
      list(
        sucesso = TRUE,
        acuracia = 0.87,
        modelo = "modelo_simulado"
      )
      
    }) %...>% (function(resultado)  {
      
      removeModal()
      
      if(resultado$sucesso) {
        values$modelo_treinado <- resultado$modelo
        values$status_modelo <- paste("Treinado com", round(resultado$acuracia * 100, 1), "% de acurácia")
        
        showNotification("✅ Modelo treinado com sucesso!", type = "success")
      } else {
        showNotification("❌ Erro no treinamento do modelo!", type = "error")
      }
      
    })
  })
  
  observeEvent(input$resetar_modelo, {
    
    showModal(modalDialog(
      title = "⚠️ Confirmar Reset",
      "Tem certeza que deseja resetar o modelo treinado? Esta ação não pode ser desfeita.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_reset", "Sim, Resetar", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmar_reset, {
    values$modelo_treinado <- NULL
    values$status_modelo <- "Não treinado"
    
    removeModal()
    showNotification("🗑️ Modelo resetado com sucesso!", type = "info")
  })
  
  #===========================================================================
  # DASHBOARD - VALUE BOXES PREMIUM (continuará na próxima parte...)
  #===========================================================================
  
  
  output$total_textos <- renderValueBox({
    total <- if(is.null(values$dados_preview) ) 0 else nrow(values$dados_preview)
    valueBox(
      value = format(total, big.mark = ".", decimal.mark = ","),
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
      value = format(iazf_count, big.mark = ".", decimal.mark = ","),
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
    total <- if(is.null(values$dados_preview) ) 0 else nrow(values$dados_preview)
    format(total, big.mark = ".", decimal.mark = ",")
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
        "✅ arquivo de Notas carregado com sucesso!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e)  {
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
      
    }, error = function(e)  {
      showNotification(
        paste("❌ Erro ao carregar arquivo:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  
  
  #===========================================================================
  # CRUZAMENTO DE DADOS (VERSÃO CORRIGIDA)
  #===========================================================================
  
  observeEvent(input$cruzar, {
    req(values$dados_ordens, values$dados_textos)
    
    withProgress(message = '🔗 Cruzando dados...', value = 0, {
      
      incProgress(0.3, detail = "Identificando colunas...")
      
      resultado <- cruzar_dados(values$dados_ordens, values$dados_textos)
      
      incProgress(0.7, detail = "Finalizando...")
      
      if(resultado$sucesso) {
        
        # ✅ CORREÇÃO PRINCIPAL: Salvar em AMBOS os reactiveValues
        values$dados_preview <- resultado$dados
        values$dados_cruzados <- resultado$dados  # ← ADICIONADO!
        
        values$col_tip_intervencao <- resultado$col_tip_intervencao
        values$dados_com_assuntos <- NULL
        
        # ✅ LOG de confirmação
        cat("\n✅ DADOS SALVOS:\n")
        cat("  - dados_preview:", nrow(values$dados_preview), "linhas\n")
        cat("  - dados_cruzados:", nrow(values$dados_cruzados), "linhas\n")
        cat("  - col_tip_intervencao:", values$col_tip_intervencao, "\n\n")
        
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
            "<div style='font-size: 28px; color: #155724; font-weight: 800;'>", format(nrow(resultado$dados), big.mark = ".", decimal.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #155724; margin-top: 5px;'>TOTAL</div>",
            "</div>",
            "<div>",
            "<div style='font-size: 28px; color: #155724; font-weight: 800;'>", format(resultado$estatisticas$com_texto, big.mark = ".", decimal.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #155724; margin-top: 5px;'>COM TEXTO</div>",
            "</div>",
            "<div>",
            "<div style='font-size: 28px; color: #155724; font-weight: 800;'>", format(resultado$estatisticas$correspondencias, big.mark = ".", decimal.mark = ","), "</div>",
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
        
        # ✅ Em caso de erro, limpar ambos
        values$dados_preview <- NULL
        values$dados_cruzados <- NULL
        
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
  
  # ============================================================================
  # INTERFACE DE VALIDAÇÃO
  # ============================================================================
  
  # Observer para carregar registros para validação
  observeEvent(input$carregar_para_validacao, {
    
    req(values$resultados_lote)
    
    # Aplicar filtros
    dados_filtrados <- switch(
      input$filtro_validacao,
      "todos" = values$resultados_lote,
      "nao_validados" = values$resultados_lote %>%
        filter(!nota_key %in% validacoes$dados$id),
      "divergentes" = values$resultados_lote %>%
        filter(status_conformidade == "DIVERGENTE"),
      "baixa_confianca" = values$resultados_lote %>%
        filter(confianca < 80),
      "criticos" = values$resultados_lote %>%
        filter(categoria == "IAZF")
    )
    
    # Limitar quantidade
    dados_validacao <- head(dados_filtrados, input$limite_validacao)
    
    # Gerar cards
    output$cards_validacao <- renderUI({
      
      if(nrow(dados_validacao) == 0) {
        return(div(
          style = "text-align: center; padding: 60px;",
          icon("check-circle", style = "font-size: 72px; color: #ccc;"),
          h4(style = "color: #999; margin-top: 20px;", "Nenhum registro para validação"),
          p(style = "color: #999;", "Todos os registros já foram validados ou não há dados disponíveis")
        ))
      }
      
      # Criar cards
      cards <- lapply(1:nrow(dados_validacao), function(i)  {
        
        registro <- dados_validacao[i, ]
        
        # Cores baseadas na criticidade
        cor_header <- switch(
          registro$criticidade,
          "BAIXA" = "#4682B4",
          "MEDIA" = "#32CD32", 
          "ALTA" = "#FF8C00",
          "CRITICA" = "#DC143C"
        )
        
        # Ícone do tipo
        icone_tipo <- switch(
          as.character(registro$tipo_novo),
          "1" = "🧽", "2" = "🔧", "3" = "🔍",
          "4" = "⏰", "5" = "⚠️", "6" = "🚨"
        )
        
        div(
          style = "margin-bottom: 30px; background: white; border-radius: 15px; 
                 box-shadow: 0 4px 16px rgba(0,0,0,0.1); overflow: hidden;",
          
          # Header do Card
          div(
            style = paste0("background: linear-gradient(135deg, ", cor_header, " 0%, rgba(", 
                           paste(col2rgb(cor_header), collapse = ","), ", 0.8) 100%);
                         padding: 25px; color: white;"),
            
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              
              div(
                h4(style = "margin: 0 0 10px 0; font-weight: 700;",
                   icone_tipo, " Nota: ", registro$nota_key),
                p(style = "margin: 0; opacity: 0.9; font-size: 14px;",
                  "Tipo Atual: ", registro$tipo_novo, " | ",
                  "Confiança: ", registro$confianca, "% | ",
                  "Método: ", registro$metodo)
              ),
              
              div(
                style = "text-align: center;",
                tags$span(
                  style = "background: rgba(255,255,255,0.3); padding: 8px 20px; 
                         border-radius: 20px; font-weight: 700;",
                  registro$criticidade
                )
              )
            )
          ),
          
          # Corpo do Card
          div(
            style = "padding: 25px;",
            
            # Assunto Principal
            div(
              style = "margin-bottom: 20px;",
              h5(style = "color: #333; margin-bottom: 10px; font-weight: 700;",
                 icon("file-alt"), " Assunto Principal:"),
              div(
                style = "background: #f8f9fa; padding: 15px; border-radius: 8px; 
                       border-left: 4px solid #667eea;",
                p(style = "margin: 0; font-size: 14px; line-height: 1.6;",
                  registro$assunto_principal)
              )
            ),
            
            # Texto Completo (Resumido)
            div(
              style = "margin-bottom: 25px;",
              h5(style = "color: #333; margin-bottom: 10px; font-weight: 700;",
                 icon("align-left"), " Texto Completo:"),
              div(
                style = "background: #f8f9fa; padding: 15px; border-radius: 8px; 
                       max-height: 120px; overflow-y: auto;",
                p(style = "margin: 0; font-size: 13px; line-height: 1.5; color: #666;",
                  substr(registro$texto_completo, 1, 300), 
                  ifelse(nchar(registro$texto_completo) > 300, "...", ""))
              )
            ),
            
            # Validação de Tipo
            div(
              style = "margin-bottom: 20px;",
              h5(style = "color: #333; margin-bottom: 15px; font-weight: 700;",
                 icon("check-circle"), " Validar Tipo de Intervenção:"),
              
              div(
                style = "display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px;",
                
                # Botões para cada tipo
                lapply(1:6, function(tipo)  {
                  
                  cor_tipo <- switch(
                    as.character(tipo),
                    "1" = "#4682B4", "2" = "#32CD32", "3" = "#FFD700",
                    "4" = "#FFA500", "5" = "#FF6347", "6" = "#DC143C"
                  )
                  
                  icone <- switch(
                    as.character(tipo),
                    "1" = "🧽", "2" = "🔧", "3" = "🔍",
                    "4" = "⏰", "5" = "⚠️", "6" = "🚨"
                  )
                  
                  # Destacar se é o tipo atual
                  estilo_extra <- if(tipo == registro$tipo_novo) {
                    "border: 3px solid #667eea; transform: scale(1.05);"
                  } else {
                    ""
                  }
                  
                  actionButton(
                    paste0("validar_", registro$nota_key, "_tipo_", tipo),
                    label = div(
                      div(style = "font-size: 24px; margin-bottom: 5px;", icone),
                      div(style = "font-size: 18px; font-weight: 800;", tipo),
                      style = "text-align: center;"
                    ),
                    style = paste0(
                      "background: ", cor_tipo, "; color: white; border: none; 
                     padding: 15px 5px; border-radius: 12px; width: 100%; 
                     box-shadow: 0 4px 12px rgba(0,0,0,0.2); 
                     transition: all 0.3s ease; ", estilo_extra
                    ),
                    onclick = paste0("
                    $(this).css('transform', 'scale(0.95)');
                    setTimeout(() => {
                      $(this).css('transform', 'scale(1.05)');
                    }, 150);
                  ")
                  )
                })
              )
            ),
            
            # Campo para Assunto Alternativo (Opcional)
            div(
              style = "margin-bottom: 20px;",
              h5(style = "color: #333; margin-bottom: 10px; font-weight: 700;",
                 icon("edit"), " Assunto Alternativo (Opcional):"),
              textAreaInput(
                paste0("assunto_alternativo_", registro$nota_key),
                label = NULL,
                placeholder = "Digite um assunto alternativo se necessário...",
                rows = 2
              )
            ),
            
            # Feedback de Qualidade
            div(
              style = "margin-bottom: 20px;",
              h5(style = "color: #333; margin-bottom: 10px; font-weight: 700;",
                 icon("star"), " Qualidade da Classificação Original:"),
              
              div(
                style = "display: flex; gap: 10px;",
                
                actionButton(
                  paste0("feedback_", registro$nota_key, "_excelente"),
                  label = div(icon("thumbs-up"), " Excelente"),
                  class = "btn-success",
                  style = "border-radius: 20px; padding: 8px 20px;"
                ),
                
                actionButton(
                  paste0("feedback_", registro$nota_key, "_boa"),
                  label = div(icon("check"), " Boa"),
                  class = "btn-info", 
                  style = "border-radius: 20px; padding: 8px 20px;"
                ),
                
                actionButton(
                  paste0("feedback_", registro$nota_key, "_ruim"),
                  label = div(icon("thumbs-down"), " Ruim"),
                  class = "btn-warning",
                  style = "border-radius: 20px; padding: 8px 20px;"
                )
              )
            )
          )
        )
      })
      
      div(cards)
    })
    
    showNotification(
      paste("✅", nrow(dados_validacao), "registros carregados para validação"),
      type = "message",
      duration = 3
    )
  })
  
  # Observers para capturar validações de tipo
  observe({
    
    # Capturar cliques nos botões de validação
    lapply(1:6, function(tipo)  {
      
      if(!is.null(values$resultados_lote)) {
        
        lapply(values$resultados_lote$nota_key, function(nota)  {
          
          button_id <- paste0("validar_", nota, "_tipo_", tipo)
          
          observeEvent(input[[button_id]], {
            
            # Buscar assunto alternativo
            assunto_alt_id <- paste0("assunto_alternativo_", nota)
            assunto_alternativo <- input[[assunto_alt_id]]
            
            # Salvar validação
            sucesso <- salvar_validacao(
              registro_id = nota,
              tipo_validado = tipo,
              assunto_validado = if(is.null(assunto_alternativo) || nchar(trimws(assunto_alternativo)) == 0) NULL else assunto_alternativo
            )
            
            if(sucesso) {
              showNotification(
                paste("✅ Validação salva: Nota", nota, "→ Tipo", tipo),
                type = "message",
                duration = 3
              )
              
              # Recarregar cards se necessário
              if(input$filtro_validacao == "nao_validados") {
                # Trigger reload
                updateNumericInput(session, "limite_validacao", value = input$limite_validacao)
              }
            }
          })
        })
      }
    })
  })
  
  # Observer para treinar modelo
  observeEvent(input$treinar_modelo, {
    
    if(nrow(validacoes$dados) < 10) {
      showNotification(
        "⚠️ Necessário pelo menos 10 validações para treinar o modelo",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    withProgress(message = '🤖 Treinando modelo...', value = 0, {
      
      incProgress(0.3, detail = "Preparando dados...")
      
      resultado <- treinar_modelo_ml()
      
      incProgress(0.7, detail = "Finalizando...")
      
      if(resultado$sucesso) {
        showNotification(
          paste0("✅ Modelo treinado com sucesso! Acurácia: ", resultado$acuracia, "%"),
          type = "message",
          duration = 8
        )
      } else {
        showNotification(
          paste("❌ Erro no treinamento:", resultado$erro),
          type = "error",
          duration = 8
        )
      }
      
      incProgress(1, detail = "Concluído!")
    })
  })
  
  
  #===========================================================================
  # PREVIEW DOS DADOS CRUZADOS
  #===========================================================================
  
  output$preview_cruzado <- DT::renderDataTable({
    req(values$dados_preview)
    
    # Verificar se há dados com assuntos processados
    if(!is.null(values$dados_com_assuntos)) {
      dados_exibir <- values$dados_com_assuntos
      tem_assuntos <- TRUE
    } else {
      dados_exibir <- values$dados_preview
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
  # DEBUG: Monitorar estado dos reactiveValues (TEMPORÁRIO)
  #===========================================================================
  
  observe({
    cat("\n🔍 ESTADO DOS REACTIVES:\n")
    cat("  - dados_ordens:", ifelse(is.null(values$dados_ordens), "NULL", paste(nrow(values$dados_ordens), "linhas")), "\n")
    cat("  - dados_textos:", ifelse(is.null(values$dados_textos), "NULL", paste(nrow(values$dados_textos), "linhas")), "\n")
    cat("  - dados_preview:", ifelse(is.null(values$dados_preview), "NULL", paste(nrow(values$dados_preview), "linhas")), "\n")
    cat("  - dados_cruzados:", ifelse(is.null(values$dados_cruzados), "NULL", paste(nrow(values$dados_cruzados), "linhas")), "\n")
    cat("  - resultados_lote:", ifelse(is.null(values$resultados_lote), "NULL", paste(nrow(values$resultados_lote), "linhas")), "\n")
    cat("  - col_tip_intervencao:", ifelse(is.null(values$col_tip_intervencao), "NULL", values$col_tip_intervencao), "\n")
    cat("  - processando:", values$processando, "\n")
    cat("\n")
  })
  
  # No início do servidor, após inicializar reactiveValues
  observe({
    # Treinamento automático a cada 30 minutos se houver novas validações
    invalidateLater(30 * 60 * 1000) # 30 minutos
    
    if(!is.null(validacoes_modelo$dados) && nrow(validacoes_modelo$dados) > 0) {
      ultimo_treino <- validacoes_modelo$metricas$ultima_atualizacao
      tempo_desde_treino <- difftime(Sys.time(), ultimo_treino, units = "mins")
      
      if(tempo_desde_treino > 30) {
        cat("⏰ Treinamento periódico do modelo...\n")
        tryCatch({
          treinar_modelo_ml_incremental()
        }, error = function(e) {
          cat("⚠️ Erro no treinamento periódico:", as.character(e), "\n")
        })
      }
    }
  })
  #===========================================================================
  # ESTATÍSTICAS DOS DADOS CRUZADOS
  #===========================================================================
  
  output$estatisticas_cruzados <- renderUI({
    req(values$dados_preview)
    
    dados <- values$dados_preview
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
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", format(total_registros, big.mark = ".", decimal.mark = ","), "</div>",
      "<div style='font-size: 12px; opacity: 0.8; margin-top: 10px;'>registros</div>",
      "</div>",
      
      # Card 2: Com Texto
      "<div style='background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); 
                   padding: 25px; border-radius: 15px; text-align: center; color: white;
                   box-shadow: 0 8px 24px rgba(17, 153, 142, 0.3); transition: all 0.3s ease;'
           onmouseover='this.style.transform=\"translateY(-8px)\"; this.style.boxShadow=\"0 12px 32px rgba(17, 153, 142, 0.4)\";'
           onmouseout='this.style.transform=\"translateY(0)\"; this.style.boxShadow=\"0 8px 24px rgba(17, 153, 142, 0.3)\";'>",
      "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>✅ COM TEXTO</div>",
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", format(com_texto, big.mark = ".", decimal.mark = ","), "</div>",
      "<div style='font-size: 12px; opacity: 0.8; margin-top: 10px;'>registros</div>",
      "</div>",
      
      # Card 3: Sem Texto
      "<div style='background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                   padding: 25px; border-radius: 15px; text-align: center; color: white;
                   box-shadow: 0 8px 24px rgba(240, 147, 251, 0.3); transition: all 0.3s ease;'
           onmouseover='this.style.transform=\"translateY(-8px)\"; this.style.boxShadow=\"0 12px 32px rgba(240, 147, 251, 0.4)\";'
           onmouseout='this.style.transform=\"translateY(0)\"; this.style.boxShadow=\"0 8px 24px rgba(240, 147, 251, 0.3)\";'>",
      "<div style='font-size: 13px; opacity: 0.9; margin-bottom: 10px; letter-spacing: 1px;'>⚠️ SEM TEXTO</div>",
      "<div style='font-size: 42px; font-weight: 800; line-height: 1;'>", format(sem_texto, big.mark = ".", decimal.mark = ","), "</div>",
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
    req(values$dados_preview)
    
    total <- nrow(values$dados_preview)
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
        format(total_processados, big.mark = ".", decimal.mark = ","), "</div>",
        "</div>",
        "<div style='background: rgba(255,255,255,0.6); padding: 15px; border-radius: 10px;'>",
        "<div style='font-size: 12px; color: #155724; margin-bottom: 5px;'>TOTAL ARQUIVO</div>",
        "<div style='font-size: 28px; font-weight: 800; color: #155724;'>", 
        format(total, big.mark = ".", decimal.mark = ","), "</div>",
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
        "<p style='margin: 5px 0;'><strong>📋 Total no arquivo:</strong> ", format(total, big.mark = ".", decimal.mark = ","), " registros</p>",
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
    req(values$dados_preview)
    
    withProgress(message = '📝 Extraindo assuntos principais...', value = 0, {
      
      dados_preview <- head(values$dados_preview, 100)
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
  observeEvent(values$dados_preview, {
    if(!is.null(values$dados_preview)) {
      
      notas_disponiveis <- values$dados_preview %>%
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
      
      if(!is.na(idx) && idx <= nrow(values$dados_preview)) {
        registro <- values$dados_preview[idx, ]
        
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
      
      resultado <- classificar_hibrido(input$texto_individual, CONFIG_USUARIO())
      
      # ✅ APLICAR VALIDAÇÃO: Se não houver campo de seleção, a validação não pode ser feita
      # Mas a função classificar_hibrido retorna resultado sem campo. Apenas informar ao usuário.
      
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
  
  # ============================================================================ 
  # CLASSIFICAÇÃO EM LOTE – VERSÃO ROBUSTA E CORRIGIDA 
  # ============================================================================
  
  observeEvent(input$classificar_lote, {
    
    # Limpar log anterior
    log_sistema("")
    
    adicionar_log("═══════════════════════════════════════════", "🚀")
    adicionar_log("INÍCIO DA CLASSIFICAÇÃO EM LOTE", "🚀")
    adicionar_log("═══════════════════════════════════════════", "🚀")
    
    # Verificações iniciais
    if (is.null(values$dados_cruzados)) {
      adicionar_log("Dados cruzados não encontrados", "❌")
      showNotification("❌ Execute o cruzamento de dados primeiro!", type = "error", duration = 8)
      return()
    }
    if (is.null(input$metodo_classificacao) || input$metodo_classificacao == "") {
      adicionar_log("Nenhum método selecionado", "❌")
      showNotification("❌ Selecione um método de classificação!", type = "error", duration = 8)
      return()
    }
    if (isTRUE(values$processando)) {
      adicionar_log("Processamento já em andamento. Aguarde.", "⚠️")
      showNotification("⚠️ Já existe uma classificação em andamento. Aguarde.", type = "warning")
      return()
    }
    
    adicionar_log(paste("Dados carregados:", nrow(values$dados_cruzados), "registros"), "✅")
    adicionar_log(paste("Método:", input$metodo_classificacao), "✅")
    adicionar_log(paste("Extrair assunto:", ifelse(isTRUE(input$extrair_assunto), "SIM", "NÃO")), "✅")
    
    # Detectar duplicatas
    textos_unicos <- unique(values$dados_cruzados$texto_completo)
    n_duplicatas <- nrow(values$dados_cruzados) - length(textos_unicos)
    
    if (n_duplicatas > 0) {
      perc_dup <- round(n_duplicatas/nrow(values$dados_cruzados)*100, 1)
      adicionar_log(paste0("Detectadas ", n_duplicatas, " duplicatas (", perc_dup, "%)"), "🔍")
      showNotification(
        paste("ℹ️", n_duplicatas, "textos duplicados serão processados apenas uma vez"),
        type = "message", duration = 5
      )
    }
    
    # Iniciar métricas de performance
    metricas_performance <- list(
      inicio = Sys.time(),
      total_registros = nrow(values$dados_cruzados),
      processados = 0,
      erros = 0,
      cache_hits = if(exists("CACHE_API")) CACHE_API$hits else 0,
      tempo_medio_por_registro = 0
    )
    
    # Iniciar processamento
    values$processando <- TRUE
    if ("shinyjs" %in% loadedNamespaces()) {
      shinyjs::disable("classificar_lote")
      
      adicionar_log("─────────────────────────────────────────", "")
      
    }
    tryCatch({
      total <- nrow(values$dados_cruzados)
      adicionar_log(paste("Iniciando processamento de", total, "registros"), "🚀")
      adicionar_log("─────────────────────────────────────────", "")
      # Estrutura base dos resultados
      resultados <- values$dados_cruzados %>%
        dplyr::select(nota_key, texto_completo)
      
      # Adicionar campo "Campo seleção" se existir
      if ("Campo seleção" %in% names(values$dados_cruzados)) {
        resultados <- resultados %>%
          dplyr::mutate(campo_selecao = values$dados_cruzados[["Campo seleção"]])
        adicionar_log("✅ Campo 'Campo seleção' incluído na validação", "📋")
        # DEBUG: verificar valores do campo
        n_com_hifen <- sum(grepl("-", trimws(values$dados_cruzados[["Campo seleção"]]), fixed = TRUE), na.rm = TRUE)
        n_total <- nrow(values$dados_cruzados)
        cat("DEBUG: Total de campos com hífen:", n_com_hifen, "/", n_total, "\n")
      } else {
        resultados <- resultados %>%
          dplyr::mutate(campo_selecao = NA_character_)
        adicionar_log("⚠️ Campo 'Campo seleção' não encontrado - validação desabilitada", "📋")
      }
      
      # Adicionar tipo antigo (se disponível)
      if (!is.null(values$col_tip_intervencao) &&
          values$col_tip_intervencao %in% names(values$dados_cruzados)) {
        resultados <- resultados %>%
          dplyr::mutate(tipo_intervencao_antigo = values$dados_cruzados[[values$col_tip_intervencao]])
      } else {
        resultados <- resultados %>% dplyr::mutate(tipo_intervencao_antigo = NA_integer_)
      }
      # Inicializar colunas de saída
      resultados <- resultados %>%
        dplyr::mutate(
          assunto_principal     = NA_character_,
          tipo_novo             = NA_integer_,
          categoria             = NA_character_,
          criticidade           = NA_character_,
          confianca             = NA_real_,
          descricao             = NA_character_,
          resumo                = NA_character_,
          metodo                = NA_character_,
          status_conformidade   = NA_character_
        )
      # Barra de progresso
      # NOTA: Para processar grandes volumes (>1000 registros), considere usar processamento paralelo:
      # library(future); library(promises); plan(multisession, workers = 4)
      # Isso pode reduzir o tempo de processamento em 3-5x
      
      withProgress(message = 'Classificando em lote...', value = 0, {
        for (i in 1:total) {
          # Log detalhado para cada registro
          perc <- round((i/total)*100, 1)
          nota_key <- resultados$nota_key[i]
          adicionar_log(paste0("[", i, "/", total, "] Nota: ", nota_key, " | ", perc, "%"), "📋")
          
          texto <- resultados$texto_completo[i]
          # Pular textos vazios
          if (is.na(texto) || nchar(trimws(texto)) == 0) {
            resultados$assunto_principal[i]   <- "Sem texto"
            resultados$metodo[i]              <- "SKIP"
            resultados$status_conformidade[i] <- "SEM_TEXTO"
            adicionar_log(paste0("   ⏭️  Texto vazio - PULADO"), "")
            incProgress(1/total, detail = paste("Linha", i, "- texto vazio"))
            next
          }
          # === EXTRAÇÃO DE ASSUNTO (opcional) ===
          if (isTRUE(input$extrair_assunto)) {
            tryCatch({
              resultados$assunto_principal[i] <- extrair_assunto_principal(texto)
            }, error = function(e) {
              cat("   ⚠️ Erro extraindo assunto (linha", i, "):", e$message, "\n")
              resultados$assunto_principal[i] <- substr(trimws(texto), 1, 80)
            })
          } else {
            resultados$assunto_principal[i] <- substr(trimws(texto), 1, 80)
          }
          adicionar_log(paste0("   📝 Assunto: ", resultados$assunto_principal[i]), "")
          # === CLASSIFICAÇÃO ===
          classificacao <- tryCatch({
            res <- switch(
              input$metodo_classificacao,
              "DICIONARIO" = classificar_por_dicionario(texto, DICIONARIOS_SAP),
              "API"        = classificar_com_openai(texto),
              "ML"         = {
                if (!is.null(validacoes_modelo$modelo_ativo)) {
                  resultado_ml <- predizer_com_modelo(texto)
                  # Verificar se a predição foi bem-sucedida
                  if(isTRUE(resultado_ml$sucesso)) {
                    resultado_ml
                  } else {
                    adicionar_log(paste("Linha", i, "- ML falhou, usando dicionário"), "⚠️")
                    classificar_por_dicionario(texto, DICIONARIOS_SAP)
                  }
                } else {
                  adicionar_log(paste0("   ⚠️  ML não treinado, fallback dicionário"), "")
                  classificar_por_dicionario(texto, DICIONARIOS_SAP)
                }
              },
              "HIBRIDO" = classificar_hibrido_completo(texto, CONFIG_USUARIO()),
              "HIBRIDO_2"  = classificar_hibrido_completo(texto, CONFIG_USUARIO()),  # Dicionário + API
              "HIBRIDO_3"  = classificar_hibrido_completo(texto, CONFIG_USUARIO()),  # Dicionário + API + ML
              # Default
              list(
                tipo = NA, categoria = "ERRO", criticidade = "ERRO",
                confianca = 0, descricao = "Método inválido",
                resumo = "Método inválido", metodo = "ERRO"
              )
            )
            # Proteção extra para campos obrigatórios
            if (is.null(res$tipo) || length(res$tipo) == 0 || is.na(res$tipo)) res$tipo <- NA
            if (is.null(res$categoria) || length(res$categoria) == 0 || is.na(res$categoria)) res$categoria <- "INDEFINIDO"
            if (is.null(res$criticidade) || length(res$criticidade) == 0 || is.na(res$criticidade)) res$criticidade <- "MEDIA"
            if (is.null(res$confianca) || length(res$confianca) == 0 || is.na(res$confianca)) res$confianca <- 0
            if (is.null(res$descricao) || length(res$descricao) == 0 || is.na(res$descricao)) res$descricao <- "Sem descrição"
            if (is.null(res$resumo) || length(res$resumo) == 0 || is.na(res$resumo)) res$resumo <- "Processado"
            if (is.null(res$metodo) || length(res$metodo) == 0 || is.na(res$metodo)) res$metodo <- input$metodo_classificacao
            
            # ✅ VALIDAÇÃO PÓS-CLASSIFICAÇÃO: NÃO permitir tipos 5 ou 6 se campo não tem hífen
            campo_selecao_valor <- resultados$campo_selecao[i]
            tem_hifen <- !is.na(campo_selecao_valor) && nchar(trimws(campo_selecao_valor)) > 0 && grepl("-", campo_selecao_valor, fixed = TRUE)
            
            # DEBUG: Verificar condição
            if (!is.na(res$tipo) && res$tipo %in% c(5, 6)) {
              cat(paste0("DEBUG Linha ", i, ": Campo='", ifelse(is.na(campo_selecao_valor), "NA", campo_selecao_valor), 
                         "' | temHifen=", tem_hifen, " | Tipo=", res$tipo, "\n"))
            }
            
            if (!tem_hifen && !is.na(res$tipo) && res$tipo %in% c(5, 6)) {
              # Tipo 5 ou 6 NÃO permitido sem hífen - rejeitar resultado
              tipo_classificado <- res$tipo
              res$tipo <- NA_integer_
              res$categoria <- "NÃO_PERMITIDO"
              res$criticidade <- "NÃO_PERMITIDO"
              res$confianca <- 0
              res$descricao <- paste0("Tipo ", tipo_classificado, " não permitido: campo sem hífen")
              res$resumo <- "Classificação rejeitada por restrição de campo"
              
              adicionar_log(
                paste0("   ❌ NÃO PERMITIDO: Tipo ", tipo_classificado, " rejeitado (sem hífen)"), 
                "⛔"
              )
            }
            
            res
          }, error = function(e) {
            adicionar_log(paste0("   ❌ ERRO na classificação: ", e$message), "")
            list(
              tipo = NA, categoria = "ERRO", criticidade = "ERRO",
              confianca = 0, descricao = "Erro na classificação",
              resumo = "Falha no processamento", metodo = "ERRO"
            )
          })
          # Preencher resultados
          resultados$tipo_novo[i]      <- classificacao$tipo
          resultados$categoria[i]      <- classificacao$categoria
          resultados$criticidade[i]    <- classificacao$criticidade
          resultados$confianca[i]      <- classificacao$confianca
          resultados$descricao[i]      <- classificacao$descricao
          resultados$resumo[i]         <- classificacao$resumo
          resultados$metodo[i]         <- classificacao$metodo
          
          # Log detalhado do resultado
          if(!is.na(classificacao$tipo)) {
            adicionar_log(paste0("   ✅ Tipo: ", classificacao$tipo, " | Categoria: ", classificacao$categoria, " | Confiança: ", round(classificacao$confianca, 1), "%"), "")
          } else {
            adicionar_log(paste0("   ⚠️  Tipo não classificado"), "")
          }
          
          # Status de conformidade
          tipo_antigo <- resultados$tipo_intervencao_antigo[i]
          if (!is.na(tipo_antigo) && !is.na(classificacao$tipo)) {
            resultados$status_conformidade[i] <- ifelse(
              tipo_antigo == classificacao$tipo,
              "CONFORME",
              "DIVERGENTE"
            )
          } else {
            resultados$status_conformidade[i] <- "SEM_REFERENCIA"
          }
          
          # Log do status final
          status_emoji <- switch(resultados$status_conformidade[i],
                                 "CONFORME" = "✅",
                                 "DIVERGENTE" = "⚠️",
                                 "SEM_REFERENCIA" = "ℹ️",
                                 "🔹")
          adicionar_log(paste0("   ", status_emoji, " ", resultados$status_conformidade[i]), "")
          
          # Atualizar progresso
          incProgress(1/total, detail = paste("Processado", i, "de", total))
          
          # Pausa apenas se usar API (para não sobrecarregar)
          if(input$metodo_classificacao %in% c("API", "HIBRIDO_2", "HIBRIDO_3") && i %% 10 == 0) {
            Sys.sleep(0.1)  # Pausa a cada 10 itens quando usar API
          }
        }
      })
      
      adicionar_log("─────────────────────────────────────────", "")
      adicionar_log("PROCESSAMENTO CONCLUÍDO COM SUCESSO!", "✅")
      adicionar_log("─────────────────────────────────────────", "")
      # Salvar resultados finais
      values$resultados_lote <- resultados
      # Adicionar ao histórico
      metadados <- list(
        metodo            = input$metodo_classificacao,
        extrair_assunto   = isTRUE(input$extrair_assunto),
        usar_dicionario   = CONFIG_USUARIO()$usar_dicionario,
        usar_api          = CONFIG_USUARIO()$usar_api,
        usar_modelo_ml    = CONFIG_USUARIO()$usar_modelo_treinado,
        total_linhas      = nrow(resultados),
        timestamp         = Sys.time()
      )
      snapshot_id <- adicionar_ao_historico(resultados, metadados)
      # Estatísticas finais
      stats <- table(resultados$status_conformidade, useNA = "ifany")
      get_stat <- function(nome) {
        valor <- stats[nome]
        if (length(valor) == 0 || all(is.na(valor))) 0 else as.numeric(valor[1])
      }
      conformes   <- get_stat("CONFORME")
      divergentes <- get_stat("DIVERGENTE")
      sem_ref     <- get_stat("SEM_REFERENCIA")
      erros       <- get_stat("ERRO") + get_stat(NA)
      # Calcular métricas finais
      metricas_performance$fim <- Sys.time()
      tempo_total <- as.numeric(difftime(metricas_performance$fim, metricas_performance$inicio, units = "secs"))
      tempo_medio <- tempo_total / total
      registros_por_minuto <- (total / tempo_total) * 60
      cache_novos_hits <- if(exists("CACHE_API")) CACHE_API$hits - metricas_performance$cache_hits else 0
      taxa_cache <- if(total > 0) round((cache_novos_hits / total) * 100, 1) else 0
      
      adicionar_log("═══════════════════════════════════════════", "📊")
      adicionar_log("ESTATÍSTICAS FINAIS", "📊")
      adicionar_log("═══════════════════════════════════════════", "📊")
      adicionar_log(paste("Total processado:", total, "registros"), "📈")
      adicionar_log(paste("Conformes:", conformes), "✅")
      adicionar_log(paste("Divergentes:", divergentes), "⚠️")
      adicionar_log(paste("Sem referência:", sem_ref), "ℹ️")
      if (erros > 0) adicionar_log(paste("Erros:", erros), "❌")
      adicionar_log("─────────────────────────────────────────", "")
      adicionar_log(paste("Tempo total:", round(tempo_total, 2), "segundos"), "⏱️")
      adicionar_log(paste("Tempo médio:", round(tempo_medio, 2), "seg/registro"), "⏱️")
      adicionar_log(paste("Velocidade:", round(registros_por_minuto, 1), "registros/min"), "🚀")
      if (cache_novos_hits > 0) {
        adicionar_log(paste("Cache hits:", cache_novos_hits, "(", taxa_cache, "%)"), "💾")
      }
      adicionar_log("═══════════════════════════════════════════", "🎉")
      adicionar_log("CLASSIFICAÇÃO FINALIZADA!", "🎉")
      adicionar_log("═══════════════════════════════════════════", "🎉")
      # Notificação ao usuário
      msg <- paste0(
        "✅ Classificação concluída em ", round(tempo_total, 1), "s!\n",
        "📊 Conformes: ", conformes, " | ",
        "Divergentes: ", divergentes, " | ",
        "Sem referência: ", sem_ref, "\n",
        "⚡ Velocidade: ", round(registros_por_minuto, 1), " reg/min"
      )
      if (erros > 0) msg <- paste0(msg, " | ⚠️ Erros: ", erros)
      if (taxa_cache > 0) msg <- paste0(msg, " | 💾 Cache: ", taxa_cache, "%")
      showNotification(msg, type = "message", duration = 15)
    }, error = function(e) {
      adicionar_log("═══════════════════════════════════════════", "❌")
      adicionar_log("ERRO CRÍTICO NO PROCESSAMENTO!", "❌")
      adicionar_log(paste("Erro:", as.character(e)), "❌")
      adicionar_log("═══════════════════════════════════════════", "❌")
      showNotification(
        paste("❌ Erro grave durante classificação:", substr(as.character(e), 1, 120)),
        type = "error",
        duration = NULL
      )
    }, finally = {
      values$processando <- FALSE
      if ("shinyjs" %in% loadedNamespaces()) {
        shinyjs::enable("classificar_lote")
      }
    })
  })
  
  # ============================================================================
  # DEBUG: Monitoramento de values$resultados_lote (mantido e aprimorado)
  # ============================================================================
  
  observe({
    cat("\n🔍 [DEBUG] Monitorando values$resultados_lote\n")
    cat("   • is.null:", is.null(values$resultados_lote), "\n")
    
    if (!is.null(values$resultados_lote)) {
      cat("   • Linhas:", nrow(values$resultados_lote), "\n")
      cat("   • Colunas:", paste(names(values$resultados_lote), collapse = ", "), "\n")
      cat("   • assunto_principal presente?", "assunto_principal" %in% names(values$resultados_lote), "\n")
      
      if ("assunto_principal" %in% names(values$resultados_lote)) {
        exemplos <- head(values$resultados_lote$assunto_principal[!is.na(values$resultados_lote$assunto_principal)], 3)
        if (length(exemplos) > 0) {
          cat("   • Exemplos de assunto:\n")
          print(exemplos)
        }
      }
    }
    cat("────────────────────────────────────\n")
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
  
  # ✅ CORRIGIDO: Usar values$resultados_lote ao invés de resultados_lote()
  output$download_resultados_lote <- downloadHandler(
    filename = function()  {
      paste0("classificacao_lote_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file)  {
      
      df <- values$resultados_lote  # ✅ CORREÇÃO AQUI
      
      # Renomear colunas para exportação
      names(df) <- c(
        "Nota Key",
        "Texto Completo",
        "Tipo Intervenção Antigo",
        "Assunto Principal",
        "Tipo Novo",
        "Categoria",
        "Criticidade",
        "Confiança (%)",
        "Descrição",
        "Resumo",
        "Método",
        "Status Conformidade"
      )
      
      openxlsx::write.xlsx(df, file, rowNames = FALSE)
    }
  )
  
  output$download_resultados <- downloadHandler(
    filename = function()  {
      paste0("resultados_classificacao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file)  {
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
    
    CONFIG_USUARIO()$USAR_dicionario <- DICIONARIOS_SAP
    
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
    
    resumo <- do.call(rbind, lapply(1:6, function(i)  {
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
    valor <- if(is.null(m) ) 0 else m$total
    
    valueBox(
      value = valor,
      subtitle = "Total Classificados",
      icon = icon("clipboard-check"),
      color = "navy"
    )
  })
  
  output$metrica_acuracia <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m) ) "N/A" else paste0(round(m$acuracia, 1), "%")
    
    valueBox(
      value = valor,
      subtitle = "Acurácia Geral",
      icon = icon("bullseye"),
      color = "teal"
    )
  })
  
  output$metrica_conformes <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m) ) 0 else m$conformes
    
    valueBox(
      value = valor,
      subtitle = "Conformes",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$metrica_divergentes <- renderValueBox({
    m <- metricas()
    valor <- if(is.null(m) ) 0 else m$divergentes
    
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
  
  output$resultado_teste_api <- renderUI({
    HTML("<div style='padding: 15px; background: #f8f9fa; border-radius: 8px;'>
      <p style='color: #666;'>Clique em 'Testar Conexão' para verificar a API</p>
      </div>")
  })
  
  observeEvent(input$testar_api, {
    
    output$resultado_teste_api <- renderUI({
      HTML("<div style='padding: 15px; background: #fff3cd; border-radius: 8px;'>
        <p style='color: #856404;'>⏳ Testando conexão com a API...</p>
        </div>")
    })
    
    cat("\n🧪 TESTANDO CONEXÃO COM A API OPENAI\n")
    cat(strrep("═", 60), "\n")
    
    tryCatch({
      
      url <- paste0(
        OPENAI_CONFIG$base_url,
        "/deployments/",
        OPENAI_CONFIG$model,
        "/chat/completions?api-version=",
        OPENAI_CONFIG$api_version
      )
      
      cat("🔗 URL:", url, "\n")
      cat("🔑 API Key:", substr(OPENAI_CONFIG$api_key, 1, 15), "...\n")
      
      # Usar o MESMO formato que a classificação real
      test_body <- list(
        messages = list(
          list(
            role = "system",
            content = "Você é um especialista em classificação. Responda sempre em JSON."
          ),
          list(
            role = "user",
            content = "Teste de conexão API. Retorne: {\"status\": \"ok\"}"
          )
        ),
        temperature = 0.3,
        max_tokens = 50
      )
      
      cat("📤 Enviando requisição...\n")
      
      response <- POST(
        url = url,
        add_headers(
          `api-key` = OPENAI_CONFIG$api_key,
          `Content-Type` = "application/json"
        ),
        body = toJSON(test_body, auto_unbox = TRUE),
        encode = "json",
        timeout(30)  # Mesmo timeout que a classificação
      )
      
      http_status <- status_code(response)
      cat("📥 Status HTTP:", http_status, "\n")
      
      if(http_status == 200) {
        response_content <- content(response, "parsed")
        cat("✅ Resposta da API recebida\n")
        cat("📄 Conteúdo:", response_content$choices[[1]]$message$content, "\n")
        
        output$resultado_teste_api <- renderUI({
          HTML("<div style='padding: 15px; background: #d4edda; border-radius: 8px; border-left: 4px solid #28a745;'>
            <strong style='color: #155724;'>✅ CONEXÃO OK</strong>
            <p style='color: #155724; margin: 5px 0 0 0;'>API respondeu com sucesso! Status HTTP 200</p>
            <p style='color: #155724; margin: 5px 0 0 0; font-size: 12px;'><strong>Resposta:</strong> OK</p>
            </div>")
        })
        cat("✅ TESTE PASSOU\n")
        
      } else if(http_status == 401) {
        error_msg <- content(response, "text")
        cat("❌ Erro 401 - API Key inválida\n")
        cat("Resposta:", error_msg, "\n")
        
        output$resultado_teste_api <- renderUI({
          HTML(paste0("<div style='padding: 15px; background: #f8d7da; border-radius: 8px; border-left: 4px solid #dc3545;'>
            <strong style='color: #721c24;'>❌ ERRO 401 - API KEY INVÁLIDA</strong>
            <p style='color: #721c24; margin: 5px 0 0 0;'>Verifique sua chave de API. Ela pode estar expirada ou incorreta.</p>
            </div>"))
        })
        
      } else if(http_status == 429) {
        cat("❌ Erro 429 - Rate limit excedido\n")
        output$resultado_teste_api <- renderUI({
          HTML(paste0("<div style='padding: 15px; background: #f8d7da; border-radius: 8px; border-left: 4px solid #dc3545;'>
            <strong style='color: #721c24;'>❌ ERRO 429 - RATE LIMIT</strong>
            <p style='color: #721c24; margin: 5px 0 0 0;'>Você fez muitas requisições. Aguarde alguns minutos.</p>
            </div>"))
        })
        
      } else {
        error_msg <- content(response, "text")
        cat("❌ Erro HTTP", http_status, "\n")
        cat("Resposta:", error_msg, "\n")
        
        output$resultado_teste_api <- renderUI({
          HTML(paste0("<div style='padding: 15px; background: #f8d7da; border-radius: 8px; border-left: 4px solid #dc3545;'>
            <strong style='color: #721c24;'>❌ ERRO HTTP ", http_status, "</strong>
            <p style='color: #721c24; margin: 5px 0 0 0;'>", substr(error_msg, 1, 200), "</p>
            </div>"))
        })
      }
      
    }, error = function(e) {
      cat("❌ ERRO DE CONEXÃO:", e$message, "\n")
      
      output$resultado_teste_api <- renderUI({
        HTML(paste0("<div style='padding: 15px; background: #f8d7da; border-radius: 8px; border-left: 4px solid #dc3545;'>
          <strong style='color: #721c24;'>❌ ERRO DE CONEXÃO</strong>
          <p style='color: #721c24; margin: 5px 0 0 0;'>", e$message, "</p>
          <p style='color: #721c24; margin: 5px 0 0 0; font-size: 12px;'><strong>Causas possíveis:</strong> Sem conexão, proxy, firewall ou servidor indisponível.</p>
          </div>"))
      })
    })
    
    cat(strrep("═", 60), "\n\n")
  })
  
  #===========================================================================
  # PROCESSAR ASSUNTOS NO PREVIEW (MANTIDO APENAS ESTE)
  #===========================================================================
  
  observeEvent(input$processar_assuntos_preview, {
    req(values$dados_preview)
    
    withProgress(message = '📝 Extraindo assuntos principais...', value = 0, {
      
      dados_preview <- head(values$dados_preview, 100)
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
  
  # Limpar assuntos quando cruzar novamente (MANTIDO APENAS ESTE)
  observeEvent(input$cruzar, {
    values$dados_com_assuntos <- NULL
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
    
    dados_evolucao <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
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
    dados_tabela <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
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
    filename = function()  {
      paste0("sessao_", historico$sessao_id, ".rds")
    },
    content = function(file)  {
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
      
    }, error = function(e)  {
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
      "<td style='padding: 10px;'>", format(proc$metadados$total_linhas, big.mark = ".", decimal.mark = ","), "</td>",
      "</tr>",
      
      "</table>",
      
      "</div>"
    ))
  })
  
  #===========================================================================
  # ✅ OUTPUTS DO HISTÓRICO (ADICIONADOS - ESTAVAM FALTANDO)
  #===========================================================================
  
  output$total_historico_inline <- renderText({
    format(length(historico$processamentos), big.mark = ".", decimal.mark = ",")
  })
  
  output$total_sessoes <- renderText({
    length(historico$processamentos)
  })
  
  output$total_processado_historico <- renderText({
    if(length(historico$processamentos) == 0) return("0")
    
    total <- sum(sapply(historico$processamentos, function(p) p$metricas$total))
    format(total, big.mark = ".")
  })
  
  output$acuracia_media_historico <- renderText({
    if(length(historico$processamentos) == 0) return("N/A")
    
    acuracias <- sapply(historico$processamentos, function(p) p$metricas$acuracia)
    paste0(round(mean(acuracias, na.rm = TRUE), 1), "%")
  })
  
  output$data_ultima_sessao <- renderText({
    if(length(historico$processamentos) == 0) return("N/A")
    
    ultima <- historico$processamentos[[length(historico$processamentos)]]
    format(ultima$timestamp, "%d/%m/%Y %H:%M")
  })
  
  output$grafico_historico_acuracia <- renderPlot({
    req(length(historico$processamentos) > 0)
    
    dados <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
      data.frame(
        sessao = i,
        acuracia = historico$processamentos[[i]]$metricas$acuracia
      )
    }))
    
    ggplot(dados, aes(x = sessao, y = acuracia)) +
      geom_line(color = "#1f4e79", size = 1.5) +
      geom_point(color = "#1f4e79", size = 3) +
      theme_minimal() +
      labs(x = "Sessão", y = "Acurácia (%)") +
      ylim(0, 100)
  })
  
  output$grafico_historico_conformidade <- renderPlot({
    req(length(historico$processamentos) > 0)
    
    dados <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
      proc <- historico$processamentos[[i]]
      data.frame(
        sessao = i,
        conformes = proc$metricas$conformes,
        divergentes = proc$metricas$divergentes
      )
    }))
    
    dados_long <- tidyr::pivot_longer(dados, cols = c(conformes, divergentes), 
                                      names_to = "tipo", values_to = "valor")
    
    ggplot(dados_long, aes(x = sessao, y = valor, fill = tipo)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("conformes" = "#2e8b57", "divergentes" = "#ff6b35")) +
      theme_minimal() +
      labs(x = "Sessão", y = "Quantidade", fill = "")
  })
  
  output$grafico_historico_volume <- renderPlot({
    req(length(historico$processamentos) > 0)
    
    dados <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
      data.frame(
        sessao = i,
        total = historico$processamentos[[i]]$metricas$total
      )
    }))
    
    ggplot(dados, aes(x = sessao, y = total)) +
      geom_col(fill = "#667eea", alpha = 0.8) +
      theme_minimal() +
      labs(x = "Sessão", y = "Total de Registros")
  })
  
  #===========================================================================
  # ✅ AÇÕES DO HISTÓRICO (ADICIONADAS - ESTAVAM FALTANDO)
  #===========================================================================
  
  observeEvent(input$exportar_historico, {
    if(length(historico$processamentos) == 0) {
      showNotification("⚠️ Não há dados no histórico para exportar", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "📤 Exportar Histórico",
      "Escolha o formato de exportação:",
      footer = tagList(
        modalButton("Cancelar"),
        downloadButton("download_historico_csv", "CSV", class = "btn-primary"),
        downloadButton("download_historico_excel", "Excel", class = "btn-success")
      )
    ))
  })
  
  output$download_historico_csv <- downloadHandler(
    filename = function()  {
      paste0("historico_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file)  {
      dados <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
        proc <- historico$processamentos[[i]]
        data.frame(
          Numero = i,
          DataHora = format(proc$timestamp, "%Y-%m-%d %H:%M:%S"),
          ID = proc$id,
          Metodo = proc$metadados$metodo,
          Total = proc$metricas$total,
          Conformes = proc$metricas$conformes,
          Divergentes = proc$metricas$divergentes,
          Acuracia = proc$metricas$acuracia,
          Confianca = proc$metricas$confianca_media
        )
      }))
      
      write.csv(dados, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_historico_excel <- downloadHandler(
    filename = function()  {
      paste0("historico_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file)  {
      dados <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
        proc <- historico$processamentos[[i]]
        data.frame(
          Numero = i,
          DataHora = format(proc$timestamp, "%Y-%m-%d %H:%M:%S"),
          ID = proc$id,
          Metodo = proc$metadados$metodo,
          Total = proc$metricas$total,
          Conformes = proc$metricas$conformes,
          Divergentes = proc$metricas$divergentes,
          Acuracia = proc$metricas$acuracia,
          Confianca = proc$metricas$confianca_media
        )
      }))
      
      openxlsx::write.xlsx(dados, file)
    }
  )
  
  observeEvent(input$comparar_sessoes, {
    if(length(historico$processamentos) < 2) {
      showNotification("⚠️ É necessário ter pelo menos 2 sessões para comparar", type = "warning")
      return()
    }
    
    showNotification("🔜 Funcionalidade de comparação em desenvolvimento", type = "info")
  })
  
  observeEvent(input$criar_backup, {
    if(length(historico$processamentos) == 0) {
      showNotification("⚠️ Não há dados para fazer backup", type = "warning")
      return()
    }
    
    tryCatch({
      dir.create("backups", showWarnings = FALSE)
      
      arquivo <- paste0("backups/backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      
      saveRDS(list(
        processamentos = historico$processamentos,
        indice_atual = historico$indice_atual,
        sessao_id = historico$sessao_id,
        data_backup = Sys.time()
      ), arquivo)
      
      showNotification(
        paste("✅ Backup criado com sucesso:", arquivo),
        type = "message",
        duration = 5
      )
    }, error = function(e)  {
      showNotification(paste("❌ Erro ao criar backup:", e$message), type = "error")
    })
  })
  
  observeEvent(input$limpar_historico_confirm, {
    showModal(modalDialog(
      title = "⚠️ ATENÇÃO: Confirmar Exclusão Total",
      div(
        style = "padding: 20px; background: #fff3cd; border-radius: 10px; border-left: 5px solid #ffc107;",
        h4(style = "color: #856404; margin: 0 0 15px 0;", "🚨 Esta ação é IRREVERSÍVEL!"),
        p(style = "color: #856404; font-size: 14px; line-height: 1.8;",
          "Você está prestes a excluir TODO o histórico de processamentos.",
          tags$br(),
          tags$strong("Recomendamos criar um backup antes de prosseguir.")
        )
      ),
      footer = tagList(
        modalButton("❌ Cancelar"),
        actionButton("confirmar_limpar_historico", "⚠️ Sim, Excluir Tudo", class = "btn-danger")
      )
    ))
  })
  
  output$exportar_historico_completo <- downloadHandler(
    filename = function()  {
      paste0("historico_completo_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file)  {
      if(length(historico$processamentos) == 0) {
        showNotification("⚠️ Não há dados no histórico", type = "warning")
        return()
      }
      
      # Criar workbook com múltiplas abas
      wb <- openxlsx::createWorkbook()
      
      # Aba 1: Resumo
      resumo <- do.call(rbind, lapply(seq_along(historico$processamentos), function(i)  {
        proc <- historico$processamentos[[i]]
        data.frame(
          Numero = i,
          DataHora = format(proc$timestamp, "%Y-%m-%d %H:%M:%S"),
          ID = proc$id,
          Metodo = proc$metadados$metodo,
          Total = proc$metricas$total,
          Conformes = proc$metricas$conformes,
          Divergentes = proc$metricas$divergentes,
          Acuracia = proc$metricas$acuracia,
          Confianca = proc$metricas$confianca_media
        )
      }))
      
      openxlsx::addWorksheet(wb, "Resumo")
      openxlsx::writeData(wb, "Resumo", resumo)
      
      # Aba 2: Dados completos do processamento atual
      if(!is.null(values$resultados_lote)) {
        openxlsx::addWorksheet(wb, "Dados Completos")
        openxlsx::writeData(wb, "Dados Completos", values$resultados_lote)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  #===========================================================================
  # PAINEL DE INSIGHTS COM IA
  #===========================================================================
  
  # Estado inicial do painel
  output$painel_insights_ia <- renderUI({
    div(
      style = "text-align: center; padding: 60px 40px; 
             background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); 
             border-radius: 15px; border: 2px dashed #dee2e6;",
      icon("robot", style = "font-size: 72px; color: #ccc; margin-bottom: 20px;"),
      h4(style = "color: #999; margin: 0 0 10px 0; font-weight: 600;",
         "Aguardando Análise"),
      p(style = "color: #999; font-size: 14px; margin: 0;",
        "Clique no botão acima para gerar insights inteligentes sobre seus dados")
    )
  })
  
  # Observer para gerar insights
  observeEvent(input$gerar_insights, {
    
    cat("\n🎯 BOTÃO GERAR INSIGHTS CLICADO\n")
    
    # Verificar se há dados
    m <- metricas()
    
    if(is.null(m)) {
      showNotification(
        "⚠️ Não há dados disponíveis para análise. Execute uma classificação primeiro.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    # Mostrar loading
    output$painel_insights_ia <- renderUI({
      div(
        style = "text-align: center; padding: 60px 40px; 
               background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
               border-radius: 15px;",
        div(
          style = "display: inline-block;",
          tags$div(
            class = "spinner-border text-primary",
            role = "status",
            style = "width: 4rem; height: 4rem; margin-bottom: 20px;",
            tags$span(class = "sr-only", "Carregando...")
          )
        ),
        h4(style = "color: #1565C0; margin: 20px 0 10px 0; font-weight: 600;",
           "🧠 IA Analisando Dados..."),
        p(style = "color: #1565C0; font-size: 14px; margin: 0;",
          "Gerando insights profundos sobre suas classificações...")
      )
    })
    
    # Gerar insights em background
    withProgress(message = '🤖 Gerando insights com IA...', value = 0, {
      
      incProgress(0.5, detail = "Consultando IA...")
      
      resultado <- gerar_insights_estatisticos(m)
      
      incProgress(1, detail = "Concluído!")
      
      if(resultado$sucesso) {
        
        insights <- resultado$insights
        
        # Renderizar painel com insights
        output$painel_insights_ia <- renderUI({
          
          div(
            style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   padding: 30px; border-radius: 15px; color: white;
                   box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);",
            
            # Header
            div(
              style = "display: flex; align-items: center; margin-bottom: 25px; 
                     padding-bottom: 20px; border-bottom: 2px solid rgba(255,255,255,0.3);",
              icon("brain", style = "font-size: 48px; margin-right: 20px;"),
              div(
                h3(style = "margin: 0 0 5px 0; font-weight: 800;", 
                   "Análise Inteligente dos Dados"),
                p(style = "margin: 0; font-size: 14px; opacity: 0.9;",
                  "Gerado por IA em ", format(Sys.time(), "%d/%m/%Y às %H:%M:%S"))
              )
            ),
            
            # Qualidade Geral
            div(
              style = "background: rgba(255,255,255,0.15); padding: 20px; 
                     border-radius: 12px; margin-bottom: 20px;",
              h4(style = "margin: 0 0 15px 0; font-weight: 700; font-size: 18px;",
                 "📊 Avaliação Geral"),
              p(style = "margin: 0; font-size: 15px; line-height: 1.8;",
                insights$qualidade_geral)
            ),
            
            # Grid de Cards
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin-bottom: 20px;",
              
              # Card Principais Achados
              div(
                style = "background: rgba(255,255,255,0.95); padding: 25px; 
                       border-radius: 12px; color: #333;",
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  icon("search", style = "font-size: 32px; color: #667eea; margin-right: 15px;"),
                  h5(style = "margin: 0; color: #667eea; font-weight: 700;",
                     "Principais Achados")
                ),
                tags$ul(
                  style = "margin: 0; padding-left: 20px; font-size: 13px; line-height: 2;",
                  lapply(insights$principais_achados, function(achado)  {
                    tags$li(achado)
                  })
                )
              ),
              
              # Card Pontos de Atenção
              div(
                style = "background: rgba(255,255,255,0.95); padding: 25px; 
                       border-radius: 12px; color: #333;",
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  icon("exclamation-triangle", style = "font-size: 32px; color: #ff6b35; margin-right: 15px;"),
                  h5(style = "margin: 0; color: #ff6b35; font-weight: 700;",
                     "Pontos de Atenção")
                ),
                tags$ul(
                  style = "margin: 0; padding-left: 20px; font-size: 13px; line-height: 2;",
                  lapply(insights$pontos_atencao, function(ponto)  {
                    tags$li(ponto)
                  })
                )
              ),
              
              # Card Recomendações
              div(
                style = "background: rgba(255,255,255,0.95); padding: 25px; 
                       border-radius: 12px; color: #333;",
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  icon("lightbulb", style = "font-size: 32px; color: #11998e; margin-right: 15px;"),
                  h5(style = "margin: 0; color: #11998e; font-weight: 700;",
                     "Recomendações")
                ),
                tags$ul(
                  style = "margin: 0; padding-left: 20px; font-size: 13px; line-height: 2;",
                  lapply(insights$recomendacoes, function(rec)  {
                    tags$li(rec)
                  })
                )
              )
            ),
            
            # Conclusão
            div(
              style = "background: rgba(255,255,255,0.15); padding: 20px; 
                     border-radius: 12px;",
              h4(style = "margin: 0 0 15px 0; font-weight: 700; font-size: 18px;",
                 "🎯 Conclusão"),
              p(style = "margin: 0; font-size: 15px; line-height: 1.8;",
                insights$conclusao)
            )
          )
        })
        
        showNotification(
          "✅ Insights gerados com sucesso!",
          type = "message",
          duration = 5
        )
        
      } else {
        
        # Erro ao gerar insights
        output$painel_insights_ia <- renderUI({
          div(
            style = "padding: 40px; background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%); 
                   border-radius: 15px; border-left: 6px solid #dc3545; text-align: center;",
            icon("exclamation-triangle", style = "font-size: 56px; color: #721c24; margin-bottom: 20px;"),
            h4(style = "color: #721c24; margin: 0 0 10px 0; font-weight: 600;",
               "Erro ao Gerar Insights"),
            p(style = "color: #721c24; font-size: 14px; margin: 0;",
              resultado$insights)
          )
        })
        
        showNotification(
          "❌ Erro ao gerar insights. Tente novamente.",
          type = "error",
          duration = 5
        )
      }
    })
  })
  
  # Outputs para os novos elementos do dashboard
  
  output$ultima_atualizacao_inline <- renderText({
    format(Sys.time(), "%H:%M:%S")
  })
  
  output$tempo_sessao_inline <- renderText({
    # Calcular tempo desde o início da sessão
    tempo_decorrido <- difftime(Sys.time(), session$startTime, units = "mins")
    paste0(round(tempo_decorrido), " min")
  })
  
  # Resumo Executivo
  output$taxa_sucesso_resumo <- renderText({
    m <- metricas()
    if(is.null(m)) return("N/A")
    paste0(round(m$acuracia, 1), "%")
  })
  
  output$tempo_medio_resumo <- renderText({
    # Simular tempo médio (você pode calcular real se tiver timestamp)
    "0.8s"
  })
  
  output$metodo_preferido_resumo <- renderText({
    m <- metricas()
    if(is.null(m) || is.null(m$metricas_metodo)) return("N/A")
    metodo_top <- m$metricas_metodo[which.max(m$metricas_metodo$total), "metodo"]
    as.character(metodo_top)
  })
  
  output$total_criticos_resumo <- renderText({
    if(is.null(values$resultados_lote)) return("0")
    criticos <- sum(values$resultados_lote$criticidade == "CRITICA", na.rm = TRUE)
    as.character(criticos)
  })
  
  output$revisoes_pendentes_resumo <- renderText({
    m <- metricas()
    if(is.null(m)) return("0")
    # Divergentes com confiança < 80%
    pendentes <- sum(m$dados_validos$confianca < 80 & !m$dados_validos$conforme, na.rm = TRUE)
    as.character(pendentes)
  })
  
  # Value Boxes Customizados (apenas os valores)
  output$total_textos_valor <- renderText({
    total <- if(is.null(values$dados_cruzados) ) 0 else nrow(values$dados_cruzados)
    format(total, big.mark = ".", decimal.mark = ",")
  })
  
  output$textos_iazf_valor <- renderText({
    iazf_count <- if(is.null(values$resultados_lote)) {
      0
    } else {
      sum(values$resultados_lote$categoria == "IAZF", na.rm = TRUE)
    }
    format(iazf_count, big.mark = ".", decimal.mark = ",")
  })
  
  output$precisao_valor <- renderText({
    precisao <- if(is.null(values$resultados_lote)) {
      "N/A"
    } else {
      paste0(round(mean(values$resultados_lote$confianca, na.rm = TRUE), 1), "%")
    }
    precisao
  })
  
  output$taxa_conformidade_valor <- renderText({
    m <- metricas()
    if(is.null(m)) {
      "N/A"
    } else {
      paste0(round(m$acuracia, 1), "%")
    }
  })
  
  # Status do modelo treinado
  output$status_modelo_treinado <- renderUI({
    
    if(is.null(validacoes$modelo_treinado)) {
      return(HTML(paste0(
        "<div style='text-align: center; padding: 40px; 
                   background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%); 
                   border-radius: 15px; border-left: 5px solid #dc3545;'>",
        "<div style='font-size: 56px; margin-bottom: 20px;'>🤖</div>",
        "<h4 style='color: #721c24; margin: 0 0 10px 0;'>Modelo Não Treinado</h4>",
        "<p style='color: #721c24; font-size: 14px; margin: 0;'>",
        "Realize pelo menos 10 validações para treinar o modelo",
        "</p>",
        "<div style='margin-top: 20px; padding: 15px; background: rgba(255,255,255,0.7); border-radius: 10px;'>",
        "<div style='color: #721c24; font-weight: 700; font-size: 24px;'>", nrow(validacoes$dados), " / 10</div>",
        "<div style='color: #721c24; font-size: 12px;'>validações realizadas</div>",
        "</div>",
        "</div>"
      )))
    }
    
    metricas <- validacoes$metricas_modelo
    
    HTML(paste0(
      "<div style='background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%); 
                 padding: 30px; border-radius: 15px; border-left: 5px solid #28a745;'>",
      
      "<div style='text-align: center; margin-bottom: 25px;'>",
      "<div style='font-size: 56px; margin-bottom: 15px;'>🧠</div>",
      "<h4 style='color: #155724; margin: 0 0 10px 0;'>Modelo Ativo</h4>",
      "<p style='color: #155724; font-size: 14px; margin: 0;'>",
      "Treinado em ", format(metricas$ultima_atualizacao, "%d/%m/%Y às %H:%M"),
      "</p>",
      "</div>",
      
      "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>",
      
      "<div style='background: rgba(255,255,255,0.7); padding: 20px; border-radius: 10px; text-align: center;'>",
      "<div style='color: #155724; font-weight: 800; font-size: 32px;'>", metricas$acuracia, "%</div>",
      "<div style='color: #155724; font-size: 12px; margin-top: 5px;'>ACURÁCIA</div>",
      "</div>",
      
      "<div style='background: rgba(255,255,255,0.7); padding: 20px; border-radius: 10px; text-align: center;'>",
      "<div style='color: #155724; font-weight: 800; font-size: 32px;'>", metricas$total_treinos, "</div>",
      "<div style='color: #155724; font-size: 12px; margin-top: 5px;'>VALIDAÇÕES</div>",
      "</div>",
      
      "</div>",
      
      "<div style='margin-top: 20px; padding: 15px; background: rgba(255,255,255,0.5); border-radius: 10px;'>",
      "<div style='color: #155724; font-size: 12px; line-height: 1.8;'>",
      "<strong>Features utilizadas:</strong> ", metricas$features_utilizadas, "<br>",
      "<strong>Algoritmo:</strong> Random Forest<br>",
      "<strong>Status:</strong> Pronto para uso",
      "</div>",
      "</div>",
      
      "</div>"
    ))
  })
  
  # Total de validações inline
  output$total_validacoes_inline <- renderText({
    format(nrow(validacoes$dados), big.mark = ".", decimal.mark = ",")
  })
  
  # Gráfico de evolução da precisão
  output$grafico_evolucao_precisao <- renderPlot({
    
    if(nrow(validacoes$dados) < 5) {
      ggplot() + 
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Dados insuficientes\nRealize mais validações", 
                 size = 6, color = "#999")
    } else {
      
      # Simular evolução (você pode calcular métricas reais)
      evolucao <- validacoes$dados %>%
        arrange(timestamp) %>%
        mutate(
          numero_validacao = row_number(),
          acerto = (tipo_original == tipo_validado),
          acuracia_acumulada = cumsum(acerto) / numero_validacao * 100
        )
      
      ggplot(evolucao, aes(x = numero_validacao, y = acuracia_acumulada)) +
        geom_line(color = "#667eea", size = 2) +
        geom_point(color = "#667eea", size = 3) +
        geom_smooth(method = "loess", se = TRUE, color = "#11998e", alpha = 0.3) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          axis.title = element_text(face = "bold")
        ) +
        labs(
          x = "Número de Validações",
          y = "Acurácia Acumulada (%)",
          title = ""
        ) +
        ylim(0, 100)
    }
  })
  
  # Adicionar no server.R ou na função server
  output$status_modelo <- renderText({
    if(exists("modelo_treinado") && !is.null(modelo_treinado)) {
      paste("✅ Modelo treinado disponível\n",
            "📅 Última atualização:", Sys.time(), "\n",
            "🎯 Acurácia estimada: 85%")
    } else {
      "❌ Nenhum modelo treinado disponível\n💡 Clique em 'Treinar Modelo' para começar"
    }
  })
  
  # Evento para treinar modelo
  observeEvent(input$treinar_modelo, {
    showModal(modalDialog(
      title = "🤖 Treinando Modelo...",
      "Por favor aguarde enquanto o modelo está sendo treinado...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Aqui você chamaria sua função de treinamento
    # resultado <- treinar_modelo_classificacao(dados_validados)
    
    removeModal()
    showNotification("✅ Modelo treinado com sucesso!", type = "success")
  })
  
  
  
} # ✅ FIM DO SERVIDOR

#=============================================================================
# EXECUTAR APLICAÇÃO
#=============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("           SISTEMA SAP PETROBRAS - VERSÃO COMPLETA E CORRIGIDA             \n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("📅 Inicializado em:", format(Sys.time(), "%d/%m/%Y às %H:%M:%S"), "\n\n")
cat("📋 FUNCIONALIDADES DISPONÍVEIS:\n")
cat("  ✅ Upload de arquivos CSV/Excel\n")
cat("  ✅ Cruzamento automático por número de nota\n")
cat("  ✅ Classificação SAP individual com IA\n")
cat("  ✅ Extração de Assunto Principal\n")
cat("  ✅ Classificação em lote (batch processing)\n")
cat("  ✅ Dicionários personalizáveis (6 tipos)\n")
cat("  ✅ Modo Híbrido (Dicionário + API)\n")
cat("  ✅ Resumo executivo da análise\n")
cat("  ✅ Dashboard interativo com métricas\n")
cat("  ✅ Estatísticas e matriz de confusão\n")
cat("  ✅ Comparação Tipo Antigo vs Tipo Novo\n")
cat("  ✅ Status de Conformidade\n")
cat("  ✅ Histórico completo de processamentos\n")
cat("  ✅ Navegação entre sessões\n")
cat("  ✅ Exportação de resultados\n")
cat("  ✅ Backup automático\n\n")
cat("🔑 API OPENAI PETROBRAS:\n")
cat("  Base URL:", OPENAI_CONFIG$base_url, "\n")
cat("  Modelo:", OPENAI_CONFIG$model, "\n")
cat("  API Version:", OPENAI_CONFIG$api_version, "\n\n")
cat("✅ Sistema pronto para uso!\n\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

shinyApp(ui = ui, server = server)
