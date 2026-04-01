#!/usr/bin/env Rscript
# Teste de parsing do CLASSIFICADOR_VERSAO14.R

tryCatch({
  parse(file='CLASSIFICADOR_VERSAO14.R')
  cat('✅ File parsed successfully\n')
  cat('✅ No syntax errors found\n')
}, error = function(e) {
  cat('❌ Syntax error found:\n')
  cat(as.character(e), '\n')
})
