{...}: {
  imports = [./hardware-configuration.nix];

  networking.hostName = "magnolia";

  system.stateVersion = "26.05";
}
