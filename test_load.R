# Test script to verify CLASSIFICADOR_VERSAO14.R loads correctly
cat("Testing file load...\n")
cat("Current working directory:", getwd(), "\n\n")

# Try to source the file
result <- tryCatch({
  source("CLASSIFICADOR_VERSAO14.R", encoding = "UTF-8")
  cat("\n✅ File loaded successfully!\n")
  TRUE
}, error = function(e) {
  cat("\n❌ ERROR loading file:\n")
  cat(e$message, "\n")
  FALSE
})

if (result) {
  cat("\n✅ All systems operational!\n")
} else {
  cat("\n⚠️ Fix errors and try again.\n")
}
