{
  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-23-11.url = "github:nixos/nixpkgs/nixos-23.11";
  };

  outputs = { self, nixpkgs-23-11, nixpkgs-unstable, ... }@inputs: {
    nixosConfigurations.default = nixpkgs-23-11.lib.nixosSystem {
      specialArgs = {
        inherit inputs;
        nixpkgs-23-11 = nixpkgs-23-11.outputs.legacyPackages.x86_64-linux;
        nixpkgs-unstable = nixpkgs-unstable.outputs.legacyPackages.x86_64-linux;
      };
      modules = [
        ./configuration.nix
      ];
    };
  };
}
