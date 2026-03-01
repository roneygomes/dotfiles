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
              claude-code
              codex
              gh
              google-cloud-sdk
              ripgrep
              jq
              uv
              jwt-cli
              terraform
              git-lfs
              postgresql
            ];
          };
        }
      );
    };
}
