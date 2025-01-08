{
  description = "nixos config (esrh)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stephen-huan = {
      url = "github:stephen-huan/nixos-config";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    stephen-huan,
    ...
  } @ inputs: {
    nixosConfigurations = {
      "helianthus" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./nixos/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "hm-backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.esrh = import ./home/home.nix;
          }
          {
            nixpkgs.overlays = with stephen-huan.overlays; [
              sioyek
            ];
          }
        ];
      };
    };
  };
}
