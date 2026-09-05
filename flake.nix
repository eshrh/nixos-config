{
  description = "nixos config (esrh)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aagl = {
      url = "github:ezKEa/aagl-gtk-on-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    aagl,
    ...
  } @ inputs: {
    nixosConfigurations = {
      # x395 ()
      "helianthus" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/helianthus/configuration.nix
          ./nixos/common.nix
          ./nixos/desktop.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh = import ./home/desktop.nix;
          }
        ];
      };
      # x1 carbon g14 (21V7CT01WW)
      "iris" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/iris/configuration.nix
          ./nixos/common.nix
          ./nixos/desktop.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh.imports = [
              ./home/desktop.nix
              ./hosts/iris/iris-home.nix
            ];
          }
        ];
      };
      # thinkcentre m70q
      "magnolia" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/magnolia/configuration.nix
          ./nixos/common.nix
          ./nixos/mullvad.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh = import ./home/common.nix;
          }
        ];
      };
      # pc
      "chrysanthemum" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/chrysanthemum/configuration.nix
          ./nixos/common.nix
          ./nixos/desktop.nix
          home-manager.nixosModules.home-manager
          aagl.nixosModules.default
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh = import ./home/desktop.nix;

            nix.settings = aagl.nixConfig;
            programs.anime-game-launcher.enable = true;
          }
        ];
      };
    };
  };
}
