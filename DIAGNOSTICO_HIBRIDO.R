# ============================================================================
# 🔧 DIAGNÓSTICO: Por Que Método Híbrido Não Funciona
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("🔍 DIAGNÓSTICO DO MÉTODO HÍBRIDO\n")
cat("═══════════════════════════════════════════════════════════════════\n")

# ============================================================================
# PROBLEMA 1: Falta de verificação de retorno em classificar_por_dicionario
# ============================================================================

cat("\n❌ PROBLEMA 1: Retorno incompleto da função classificar_por_dicionario\n")
cat("   Status: A função retorna uma LISTA, mas classificar_hibrido_completo\n")
cat("   trata como se fosse um objeto com $tipo, $confianca diretamente\n")

# ============================================================================
# PROBLEMA 2: Falta de tratamento de NULL
# ============================================================================

cat("\n❌ PROBLEMA 2: Tratamento inadequado de retornos NULL\n")
cat("   Na linha 1368-1369:\n")
cat("   resultados$dicionario <- classificar_por_dicionario(texto, ...)\n")
cat("   Se isto retorna NULL, a próxima linha quebra!\n")

# ============================================================================
# PROBLEMA 3: Ordem de acesso aos campos
# ============================================================================

cat("\n❌ PROBLEMA 3: Ordem incorreta na votação híbrida\n")
cat("   Linha 1394-1396:\n")
cat("   votos[resultados$dicionario$tipo] <- votos[resultados$dicionario$tipo] + 1\n")
cat("   ⚠️ Se resultados$dicionario = NULL → ERRO!\n")
cat("   ⚠️ Se resultados$dicionario$tipo = NA → ERRO no índice!\n")

# ============================================================================
# PROBLEMA 4: Verificação condicional deficiente
# ============================================================================

cat("\n❌ PROBLEMA 4: Falta validação antes de acessar campos\n")
cat("   As verificações são DEPOIS de usar os valores!\n")
cat("   Deve-se verificar ANTES de usar qualquer campo.\n")

# ============================================================================
# TESTE: Demonstração do problema
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("📝 SIMULANDO O ERRO\n")
cat("═══════════════════════════════════════════════════════════════════\n")

# Simular retorno de classificar_por_dicionario
resultado_dicionario <- list(
  tipo = 5,
  categoria = "IAZF",
  criticidade = "ALTA",
  confianca = 80,
  descricao = "Intervenção para eliminação de defeito",
  resumo = "Texto com problema",
  metodo = "DICIONARIO",
  matches = 3,
  quando_utilizar = "Com restrição"
)

# Agora tentar usar como no código original
cat("\n✓ Teste 1: Acessar $tipo\n")
cat("  resultado_dicionario$tipo =", resultado_dicionario$tipo, "\n")

cat("\n✓ Teste 2: Usar em índice (isto causaria erro)\n")
votos <- numeric(6)
votos[resultado_dicionario$tipo] <- votos[resultado_dicionario$tipo] + 1
cat("  votos[5] =", votos[5], "\n")

cat("\n✓ Teste 3: Validar estrutura antes\n")
if(!is.null(resultado_dicionario) && !is.na(resultado_dicionario$tipo)) {
  cat("  ✅ Estrutura válida para usar\n")
} else {
  cat("  ❌ Estrutura inválida - não pode usar\n")
}

# ============================================================================
# SOLUÇÃO: Código corrigido
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("✅ SOLUÇÃO IDENTIFICADA\n")
cat("═══════════════════════════════════════════════════════════════════\n")

cat("\nAs correções necessárias são:\n")
cat("  1. Validar TODOS os retornos antes de usar\n")
cat("  2. Usar is.null() + is.na() para verificar campos\n")
cat("  3. Mover todas as verificações ANTES de acessar campos\n")
cat("  4. Adicionar logging mais detalhado para diagnóstico\n")
cat("  5. Garantir que votos[] apenas recebe índices válidos (1-6)\n")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("📊 PONTOS CRÍTICOS A VERIFICAR\n")
cat("═══════════════════════════════════════════════════════════════════\n")

checklist <- data.frame(
  Check = c(
    "Dicionário retorna válido",
    "API retorna sem erro",
    "ML modelo está ativo",
    "Votação tem índices válidos",
    "Tipos entre 1-6",
    "Confiança entre 0-100",
    "Método foi identificado",
    "Metadados de detalhe preenchidos"
  ),
  Status = c(
    "❌ VERIFICAR",
    "❌ VERIFICAR",
    "❌ VERIFICAR",
    "❌ VERIFICAR",
    "❌ VERIFICAR",
    "❌ VERIFICAR",
    "❌ VERIFICAR",
    "❌ VERIFICAR"
  ),
  Localização = c(
    "Linha 1368",
    "Linha 1373-1378",
    "Linha 1381-1389",
    "Linha 1402-1420",
    "Linha 1427-1443",
    "Linha 1444-1448",
    "Linha 1445-1462",
    "Linha 1474-1482"
  ),
  stringsAsFactors = FALSE
)

print(checklist)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("🎯 PRÓXIMA AÇÃO: Gerar código corrigido\n")
cat("═══════════════════════════════════════════════════════════════════\n")
