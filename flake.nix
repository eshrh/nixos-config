{
  description = "nixos config (esrh)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: {
    nixosConfigurations = {
      # x395 ()
      "helianthus" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/helianthus/configuration.nix
          ./nixos/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh = import ./home/home.nix;
          }
        ];
      };
      # x1 carbon g14 (21V7CT01WW)
      "iris" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/iris/configuration.nix
          ./nixos/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh.imports = [
              ./home/home.nix
              ./hosts/iris/iris-home.nix
            ];
          }
        ];
      };
    };
  };
}
