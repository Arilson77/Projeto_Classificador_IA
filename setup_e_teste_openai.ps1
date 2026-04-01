# Script: setup_e_teste_openai.ps1
# Objetivo: configurar variáveis de ambiente e validar chamada à API OpenAI Petrobras

param(
  [string]$OpenAIKey = "",
  [string]$Model = "gpt-5-nano-petrobras",
  [string]$BaseUrl = "https://apit.petrobras.com.br/ia/openai/v1/openai-azure/openai",
  [string]$EmbeddingBaseUrl = "https://apit.petrobras.com.br/ia/openai/v1/openai-azure/openai",
  [string]$ApiVersion = "2024-10-21",
  [switch]$PersistUser
)

$ErrorActionPreference = "Stop"

Write-Host "== Configurando variáveis de ambiente (sessão atual) ==" -ForegroundColor Cyan
$env:CHATGPT_MODEL = $Model
$env:AZURE_OPENAI_BASE_URL = $BaseUrl
$env:AZURE_OPENAI_BASE_URL_EMBEDDING = $EmbeddingBaseUrl
$env:OPENAI_API_VERSION = $ApiVersion

if (-not [string]::IsNullOrWhiteSpace($OpenAIKey)) {
  $env:OPENAI_API_KEY = $OpenAIKey
}

if ($PersistUser) {
  Write-Host "== Persistindo em escopo User ==" -ForegroundColor Cyan
  [Environment]::SetEnvironmentVariable("CHATGPT_MODEL", $env:CHATGPT_MODEL, "User")
  [Environment]::SetEnvironmentVariable("AZURE_OPENAI_BASE_URL", $env:AZURE_OPENAI_BASE_URL, "User")
  [Environment]::SetEnvironmentVariable("AZURE_OPENAI_BASE_URL_EMBEDDING", $env:AZURE_OPENAI_BASE_URL_EMBEDDING, "User")
  [Environment]::SetEnvironmentVariable("OPENAI_API_VERSION", $env:OPENAI_API_VERSION, "User")
  if (-not [string]::IsNullOrWhiteSpace($env:OPENAI_API_KEY)) {
    [Environment]::SetEnvironmentVariable("OPENAI_API_KEY", $env:OPENAI_API_KEY, "User")
  }
}

Write-Host "== Conferência rápida ==" -ForegroundColor Cyan
Write-Host "MODEL=$($env:CHATGPT_MODEL)"
Write-Host "BASE_URL=$($env:AZURE_OPENAI_BASE_URL)"
Write-Host "API_VERSION=$($env:OPENAI_API_VERSION)"
if ([string]::IsNullOrWhiteSpace($env:OPENAI_API_KEY)) {
  Write-Host "OPENAI_API_KEY=VAZIA" -ForegroundColor Yellow
  throw "OPENAI_API_KEY não definida. Passe -OpenAIKey ou defina a variável antes de testar."
} else {
  Write-Host "OPENAI_API_KEY=OK" -ForegroundColor Green
}

Write-Host "== Teste de chamada /chat/completions ==" -ForegroundColor Cyan
$uri = "$($env:AZURE_OPENAI_BASE_URL)/deployments/$($env:CHATGPT_MODEL)/chat/completions?api-version=$($env:OPENAI_API_VERSION)"
$headers = @{ "api-key" = $env:OPENAI_API_KEY; "Content-Type" = "application/json" }
$body = @{
  messages = @(
    @{ role = "system"; content = "Responda em JSON válido." },
    @{ role = "user"; content = "Teste de conectividade. Retorne {\"status\":\"ok\"}." }
  )
  temperature = 0.1
  max_tokens = 40
} | ConvertTo-Json -Depth 5

try {
  $resp = Invoke-RestMethod -Method Post -Uri $uri -Headers $headers -Body $body
  Write-Host "✅ API OK" -ForegroundColor Green
  if ($resp.choices -and $resp.choices.Count -gt 0) {
    Write-Host "Resposta:" -ForegroundColor DarkGray
    Write-Host $resp.choices[0].message.content
  }
}
catch {
  Write-Host "❌ ERRO API" -ForegroundColor Red
  Write-Host $_.Exception.Message
  exit 1
}
