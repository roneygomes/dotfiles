{
  description = "Development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
        in
        {
          default = pkgs.buildEnv {
            name = "dev-env";
            paths = with pkgs; [
              _1password-cli
              claude-code
              codex
              fzf
              gh
              git-lfs
              google-cloud-sdk
              jq
              jwt-cli
              nodejs
              oh-my-zsh
              openssh
              postgresql
              ripgrep
              socat
              terraform
              uv
              zsh-powerlevel10k
            ];
          };
        }
      );
    };
}
