#!/bin/bash
# Falha se algum arquivo tiver cara de segredo (ou de dado pessoal, no modo --public).
#
#   scripts/check-secrets.sh            repo privado: só credenciais de verdade
#   scripts/check-secrets.sh --public   repo público: credenciais + dados que identificam
#   --all                               inclui arquivos ainda não rastreados
set -uo pipefail
cd "$(dirname "$0")/.." || exit 1

mode=private
scope=tracked
for arg in "$@"; do
  case "$arg" in
    --public) mode=public ;;
    --all) scope=all ;;
    *) echo "argumento desconhecido: $arg" >&2; exit 2 ;;
  esac
done

if [ "$scope" = all ]; then
  files=$(find . -type f -not -path "./.git/*" \
    -not -name "*.png" -not -name "*.jpg" -not -name "*.jpeg" -not -name "*.mp4" \
    -not -name "*.ico" -not -name "*.woff2")
else
  files=$(git ls-files)
fi

# Sempre proibido, em repo público ou privado.
credentials=(
  'sk-[A-Za-z0-9]{16,}|chave de API OpenAI/Anthropic'
  'ghp_[A-Za-z0-9]{20,}|token do GitHub'
  'gho_[A-Za-z0-9]{20,}|token OAuth do GitHub'
  'AKIA[0-9A-Z]{16}|chave da AWS'
  '-----BEGIN [A-Z ]*PRIVATE KEY-----|chave privada'
  'eyJ[A-Za-z0-9_-]{20,}\.[A-Za-z0-9_-]{20,}\.|JWT'
  'xox[baprs]-[A-Za-z0-9-]{10,}|token do Slack'
  'AIza[0-9A-Za-z_-]{30,}|chave do Google'
)

# Só em repo público: num repo privado, IP da casa e ID de grupo são esperados.
personal=(
  '192\.168\.[0-9]+\.[0-9]+|IP da rede local'
  '10\.[0-9]+\.[0-9]+\.[0-9]+|IP da rede local'
  '[0-9]{10,}@g\.us|ID de grupo do WhatsApp'
  '[A-Za-z0-9._%+-]+@gmail\.com|email pessoal'
)

check_set() {
  local found=0 entry regex label hits
  for entry in "$@"; do
    regex="${entry%%|*}"
    label="${entry#*|}"
    hits=$(echo "$files" | xargs -r grep -nEI "$regex" 2>/dev/null \
      | grep -v "scripts/check-secrets.sh" \
      | grep -vEi "xxx|\.\.\.|<[a-z_]+>|example|placeholder|redacted" \
      | head -5)
    if [ -n "$hits" ]; then
      echo "✗ $label"
      echo "$hits" | sed 's/^/    /'
      found=1
    fi
  done
  return "$found"
}

status=0
check_set "${credentials[@]}" || status=1
if [ "$mode" = public ]; then
  check_set "${personal[@]}" || status=1
fi

if [ "$status" -eq 0 ]; then
  echo "✓ limpo ($mode, $scope)"
  exit 0
fi

echo
echo "Remova ou mova para fora do repositório antes de commitar."
exit 1
