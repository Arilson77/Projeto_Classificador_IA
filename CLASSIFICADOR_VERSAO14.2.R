# ── Criação da tabela revisada com links para manuais ──────
criar_tabela_hh_revisado <- function(dados_analise, resultado_ia) {
  if (!isTRUE(resultado_ia$sucesso) || is.null(dados_analise)) {
    return(NULL)
  }
  rev <- resultado_ia$revisao
  # Cria coluna com link HTML para o manual
  dados_analise$manual_link <- mapply(
    criar_link_manual,
    dados_analise$fonte_referencia,
    dados_analise$url_padrao
  )
  tabela <- dados_analise %>%
    select(
      numero_operacao, 
      descricao_operacao, 
      tipo_identificado,
      total_hh, 
      hh_referencia, 
      manual_link,
      desvio_pct, 
      classificacao
    ) %>%
    rename(
      "Nr. Operação" = numero_operacao,
      "Descrição" = descricao_operacao,
      "Tipo" = tipo_identificado,
      "HH Apropriado" = total_hh,
      "HH Padrão" = hh_referencia,
      "Manual SINPEP" = manual_link,
      "Desvio %" = desvio_pct,
      "Status" = classificacao
    )
  # Inicializa colunas vazias
  tabela$`HH Revisado` <- NA_real_
  tabela$`Ajuste` <- "—"
  tabela$`Ação` <- "MANTER"
  tabela$`Justificativa` <- "Dentro do padrão esperado"
  # Aplica revisões da IA
  ops_ia <- rev$operacoes_revisadas
  
  if (!is.null(ops_ia) && length(ops_ia) > 0) {
    for (op_ia in ops_ia) {
      # Busca índice da operação
      idx <- which(tabela$`Nr. Operação` == op_ia$numero)
      if (length(idx) == 0) next
      # CORREÇÃO: usa hh_revisado da IA
      hh_revisado_ia <- as.numeric(op_ia$hh_revisado)
      hh_apropriado <- as.numeric(tabela$`HH Apropriado`[idx])
      hh_padrao <- as.numeric(tabela$`HH Padrão`[idx])
      # Se a IA não retornou hh_revisado, usa o padrão como referência
      if (is.na(hh_revisado_ia) || hh_revisado_ia == 0) {
        if (!is.na(hh_padrao) && hh_padrao > 0) {
          hh_revisado_ia <- hh_padrao
        } else {
          hh_revisado_ia <- hh_apropriado
        }
      }
      # Atualiza HH Revisado
      tabela$`HH Revisado`[idx] <- round(hh_revisado_ia, 2)
      # Calcula ajuste CORRETAMENTE
      diferenca <- hh_revisado_ia - hh_apropriado
      
      if (!is.na(diferenca) && hh_apropriado > 0) {
        diferenca_pct <- (diferenca / hh_apropriado) * 100
        
        if (abs(diferenca) > 0.01) {  # Se houver diferença significativa
          tabela$`Ajuste`[idx] <- sprintf("%+.1f HH (%+.1f%%)", diferenca, diferenca_pct)
        } else {
          tabela$`Ajuste`[idx] <- "Sem ajuste"
        }
      }
      # Ação recomendada
      if (!is.null(op_ia$acao_recomendada)) {
        acao <- toupper(as.character(op_ia$acao_recomendada))
        # Padroniza nomes
        acao <- gsub("_", " ", acao)
        tabela$`Ação`[idx] <- acao
      }
      # Justificativa
      if (!is.null(op_ia$justificativa)) {
        just <- as.character(op_ia$justificativa)
        just <- sanitizar_texto(just, 120)
        tabela$`Justificativa`[idx] <- ifelse(
          nchar(just) > 100,
          paste0(substr(just, 1, 97), "..."),
          just
        )
      }
    }
  }
  # Preenche HH Revisado faltantes com o HH Apropriado
  idx_na <- is.na(tabela$`HH Revisado`)
  if (any(idx_na)) {
    tabela$`HH Revisado`[idx_na] <- tabela$`HH Apropriado`[idx_na]
  }
  return(tabela)
}