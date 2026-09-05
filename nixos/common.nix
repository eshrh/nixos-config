{pkgs, ...}: {
  imports = [
    ./networking.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  users.users.esrh = {
    isNormalUser = true;
    description = "Eshan Ramesh";
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.fish;
  };

  programs.fish.enable = true;

  services.openssh.enable = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
}
