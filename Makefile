.PHONY: install

install: brew stow oh-my-zsh p10k nvm cco ## Install all dependencies and symlink dotfiles

.PHONY: brew
brew: ## Install Homebrew packages from Brewfile
	brew update
	brew bundle install --cleanup --file=.config/Brewfile
	brew upgrade

.PHONY: stow
stow: ## Symlink dotfiles to home directory
	stow --verbose 1 --adopt --target $(HOME) .

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
