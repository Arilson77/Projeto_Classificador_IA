# Teste rápido da função
source('CLASSIFICADOR_VERSAO14 copy.R')

# Verificar se existe
cat("Função existe?", exists("classificar_hibrido"), "\n")

# Teste simples
resultado <- classificar_hibrido("Sistema parado", CONFIG_USUARIO())
cat("Tipo:", resultado$tipo, "\n")
cat("Criticidade:", resultado$criticidade, "\n")
cat("Método:", resultado$metodo, "\n")
