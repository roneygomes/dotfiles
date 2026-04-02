#!/bin/sh
input=$(cat)

model=$(echo "$input" | jq -r '.model.display_name // ""')
used=$(echo "$input"  | jq -r '.context_window.used_percentage // empty')

# Profile indicator from CLAUDE_CONFIG_DIR
if [ -n "${CLAUDE_CONFIG_DIR:-}" ]; then
  profile_name=$(basename "$CLAUDE_CONFIG_DIR")
  profile_str=$(printf "\033[35m%s\033[0m" "$profile_name")
else
  profile_str=$(printf "\033[90mdefault\033[0m")
fi

# "Claude Sonnet 4.6 (1M context)" → "sonnet 4.6" + "1m"
short_model=$(echo "$model" | sed 's/^Claude //i' | sed 's/ ([^)]*context)//i' | tr '[:upper:]' '[:lower:]')
ctx_size=$(echo "$model" | grep -oiE '[0-9]+[km] context' | grep -oiE '[0-9]+[km]' | tr '[:upper:]' '[:lower:]')

if [ -n "$ctx_size" ]; then
  model_str=$(printf "\033[36m%s \033[90m%s\033[0m" "$short_model" "$ctx_size")
else
  model_str=$(printf "\033[36m%s\033[0m" "$short_model")
fi

if [ -n "$used" ]; then
  used_int=$(printf "%.0f" "$used")
  if   [ "$used_int" -ge 75 ]; then ctx_color="\033[31m"   # red
  elif [ "$used_int" -ge 50 ]; then ctx_color="\033[33m"   # yellow
  else                               ctx_color="\033[32m"   # green
  fi
  printf "%s \033[90m·\033[0m ${ctx_color}%d%%\033[0m \033[90m·\033[0m %s" "$model_str" "$used_int" "$profile_str"
else
  printf "%s \033[90m·\033[0m %s" "$model_str" "$profile_str"
fi
