# Diagnóstico: Erro de Escopo do Objeto `validacoes`

## ❌ Problema Identificado

**ERRO DE ESCOPO** - NÃO é falta de validações salvas!

### Causa Raiz
Funções definidas **fora** do `server()` tentam acessar `validacoes` diretamente:

```r
# Linha 578 - FORA do server()
treinar_modelo_ml_incremental <- function() {
  if(is.null(validacoes$modelo_ativo)) {  # ❌ validacoes não existe aqui!
    ...
  }
}

# Linha 7802 - Início do server()
server <- function(input, output, session)  {
  
  # Linha 7844 - validacoes SÓ existe AQUI!
  validacoes <- reactiveValues(
    dados = data.frame(),
    modelo_ativo = NULL,
    ...
  )
}
```

### Funções Afetadas (definidas antes da linha 7802)
1. `treinar_modelo_ml_incremental()` - linha 578
2. `treinar_modelo_ml()` - linha 725
3. `predizer_com_modelo()` - linha 950
4. `classificar_com_modelo_treinado()` - linha 2349
5. `classificar_hibrido_com_modelo()` - linha 2310
6. `classificar_hibrido_dicionario_ml()` - linha 2266
7. Função anônima linha 1905 (dentro de `classificar_hibrido_completo`)

## ✅ Solução Aplicada

Adicionar `validacoes` como parâmetro em todas essas funções e passar nas chamadas:

### Exemplo:
```r
# ANTES:
treinar_modelo_ml <- function() {
  if (is.null(validacoes$dados)) { ... }  # ❌ Erro!
}

# DEPOIS:
treinar_modelo_ml <- function(validacoes) {
  if (is.null(validacoes$dados)) { ... }  # ✅ OK!
}

# CHAMADA (dentro do server):
resultado <- treinar_modelo_ml(validacoes)  # ✅ Passa o objeto
```

## 🔧 Correções Pendentes

Preciso ajustar as chamadas nas linhas:
- 1909: `classificar_hibrido_com_modelo(texto, config, validacoes)`
- 9855: `classificar_hibrido_com_modelo(texto, config, validacoes)`
- 9884: `classificar_hibrido_com_modelo(texto, config, validacoes)`
- 9367: `predizer_com_modelo(texto_exemplo, validacoes)`
- 9441: `predizer_com_modelo(input$texto_teste_ml, validacoes)`
- 11796: `predizer_com_modelo(texto, validacoes)`
- 8496: `classificar_com_modelo_treinado(texto, validacoes)`
- 2315: `classificar_com_modelo_treinado(texto, validacoes)`

## 📝 Nota Importante

A linha 1909 está dentro de `classificar_hibrido_completo()` que também é definida fora do server, então ela TAMBÉM precisa receber `validacoes` como parâmetro e todas as suas chamadas precisam passar `validacoes`.
