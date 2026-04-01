# Smoke test mínimo offline para validar lógica de classificação
# Não depende de shiny, dplyr, httr ou outros pacotes externos.

classificar_minimo_offline <- function(texto) {
  txt <- tolower(trimws(ifelse(is.null(texto), "", texto)))

  if (nchar(txt) == 0) {
    return(list(tipo = NA_integer_, confianca = 0, criticidade = NA_character_, metodo = "MINIMO_OFFLINE"))
  }

  if (grepl("falha|quebrou|quebrada|quebrado|inoperante|indisponivel|indisponível|parada|emergencia|emergência|queimou", txt)) {
    return(list(tipo = 6L, confianca = 90, criticidade = "CRITICA", metodo = "MINIMO_OFFLINE"))
  }

  if (grepl("defeito|vazamento|restricao|restrição|degradad|vibracao|vibração|anomalia|limitação|limitacao", txt)) {
    return(list(tipo = 5L, confianca = 85, criticidade = "ALTA", metodo = "MINIMO_OFFLINE"))
  }

  if (grepl("preventiva|inspecao|inspeção|programada|preditiva|termografia|vibração", txt)) {
    return(list(tipo = 3L, confianca = 80, criticidade = "MEDIA", metodo = "MINIMO_OFFLINE"))
  }

  if (grepl("oportunidade|nao programada|não programada|eventual|standby", txt)) {
    return(list(tipo = 4L, confianca = 75, criticidade = "MEDIA", metodo = "MINIMO_OFFLINE"))
  }

  if (grepl("melhoria|modificacao|modificação|instalacao|instalação|teste|upgrade|calibracao|calibração", txt)) {
    return(list(tipo = 2L, confianca = 80, criticidade = "BAIXA", metodo = "MINIMO_OFFLINE"))
  }

  if (grepl("limpeza|pintura|preservacao|preservação|arrumacao|arrumação|lubrificacao|lubrificação", txt)) {
    return(list(tipo = 1L, confianca = 80, criticidade = "BAIXA", metodo = "MINIMO_OFFLINE"))
  }

  list(tipo = 3L, confianca = 70, criticidade = "MEDIA", metodo = "MINIMO_OFFLINE")
}


# Roda 2 cenários críticos (tipo 5 e 6) e gera status OK/ALERTA
# Retorna data.frame e também imprime resumo

teste_smoke_classificacao_minimo <- function() {
  casos <- data.frame(
    caso = c("Defeito com restrição", "Falha total"),
    texto = c(
      "Bomba principal com vazamento e vibração elevada, porém ainda operando com restrição.",
      "Compressor principal queimou, equipamento inoperante e unidade parada por emergência."
    ),
    esperado = c(5L, 6L),
    stringsAsFactors = FALSE
  )

  resultados <- do.call(rbind, lapply(seq_len(nrow(casos)), function(i) {
    res <- classificar_minimo_offline(casos$texto[i])

    data.frame(
      data_teste = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      caso = casos$caso[i],
      esperado = casos$esperado[i],
      tipo_predito = res$tipo,
      confianca = res$confianca,
      criticidade = res$criticidade,
      metodo = res$metodo,
      status = ifelse(!is.na(res$tipo) && res$tipo == casos$esperado[i], "OK", "ALERTA"),
      stringsAsFactors = FALSE
    )
  }))

  print(resultados)
  cat("\nResumo Smoke Test Mínimo:\n")
  cat("OK:", sum(resultados$status == "OK", na.rm = TRUE), "/", nrow(resultados), "\n")
  cat("ALERTA:", sum(resultados$status == "ALERTA", na.rm = TRUE), "/", nrow(resultados), "\n")

  if (!dir.exists("dados_modelo_treinado")) {
    dir.create("dados_modelo_treinado", recursive = TRUE)
  }

  arquivo_saida <- file.path(
    "dados_modelo_treinado",
    paste0("smoke_minimo_offline_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )

  write.csv(resultados, arquivo_saida, row.names = FALSE, fileEncoding = "UTF-8")
  cat("CSV gerado em:", normalizePath(arquivo_saida, winslash = "/"), "\n")

  invisible(resultados)
}
