.PHONY: install

install: brew stow oh-my-zsh p10k nvm cco ## Install all dependencies and symlink dotfiles

.PHONY: brew
brew: ## Install Homebrew packages from Brewfile
	brew update
	brew bundle install --cleanup --file=.config/Brewfile
	brew upgrade

.PHONY: stow
stow: ## Symlink dotfiles to home directory (per-file symlinks, never folds dirs)
	stow --verbose 1 --adopt --no-folding --target $(HOME) .

.PHONY: unfold-claude-profiles
unfold-claude-profiles: ## One-shot: convert ~/.claude-profiles from wholesale symlink to real dir. RUN FROM A NON-CLAUDE SHELL.
	@if [ -n "$$CLAUDECODE" ]; then \
		echo "ERROR: refusing to run inside a Claude Code session — runtime files are open."; \
		echo "Open a fresh terminal and try again."; \
		exit 1; \
	fi
	@if [ ! -L "$(HOME)/.claude-profiles" ]; then \
		echo "~/.claude-profiles is already a real dir — nothing to do."; \
		exit 0; \
	fi
	@echo "Breaking wholesale symlink at ~/.claude-profiles..."
	rm "$(HOME)/.claude-profiles"
	mkdir -p "$(HOME)/.claude-profiles/personal" "$(HOME)/.claude-profiles/work"
	@echo "Moving runtime data out of repo into ~/.claude-profiles/..."
	@for profile in personal work; do \
		for item in .claude.json backups cache file-history history.jsonl plans plugins projects session-env sessions shell-snapshots; do \
			src="$(CURDIR)/.claude-profiles/$$profile/$$item"; \
			dst="$(HOME)/.claude-profiles/$$profile/$$item"; \
			if [ -e "$$src" ]; then mv "$$src" "$$dst" && echo "  moved $$profile/$$item"; fi; \
		done; \
	done
	@$(MAKE) stow

.PHONY: oh-my-zsh
oh-my-zsh: ## Install oh-my-zsh if not present
	@if [ ! -d "$(HOME)/.oh-my-zsh" ]; then \
		sh -c "$$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" -- --unattended; \
	else \
		echo "oh-my-zsh already installed"; \
	fi

.PHONY: p10k
p10k: oh-my-zsh ## Install powerlevel10k theme if not present
	@if [ ! -d "$(HOME)/.oh-my-zsh/custom/themes/powerlevel10k" ]; then \
		git clone --depth=1 https://github.com/romkatv/powerlevel10k.git \
			"$(HOME)/.oh-my-zsh/custom/themes/powerlevel10k"; \
	else \
		echo "powerlevel10k already installed"; \
	fi

.PHONY: nvm
nvm: ## Install nvm if not present
	@if [ ! -d "$(HOME)/.nvm" ]; then \
		PROFILE=/dev/null bash -c "$$(curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh)"; \
	else \
		echo "nvm already installed"; \
	fi

.PHONY: cco
cco: ## Install cco sandbox wrapper if not present
	@if ! command -v cco >/dev/null 2>&1; then \
		bash -c "$$(curl -fsSL https://raw.githubusercontent.com/nikvdp/cco/main/install.sh)"; \
	else \
		echo "cco already installed"; \
	fi
