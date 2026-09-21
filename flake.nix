{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    elan.url = "github:ProfLander/elan";
  };

  outputs =
    { nixpkgs, elan, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          elan.packages.${system}.default
          pkgs.racket
        ];
      };
    };
}
