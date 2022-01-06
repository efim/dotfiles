{ inputs, config, pkgs, ... }: {

  networking.hostName = "morpheus";

  imports = with inputs.self.myProfiles; [
    ./hardware-configuration.nix
    base
  ];

}
