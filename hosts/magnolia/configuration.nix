{...}: {
  imports = [./hardware-configuration.nix];

  networking.hostName = "magnolia";

  console.keyMap = "us";
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  system.stateVersion = "26.05";
}
