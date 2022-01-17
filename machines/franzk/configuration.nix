{ inputs, config, pkgs, ... }:
{
  networking.hostName = "franzk";

  imports = with inputs.self.myRoles; [
    ./vpsadminos.nix
    base-server
  ];

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=900s
  '';

  time.timeZone = "Europe/Amsterdam";

  system.stateVersion = "22.05";
}
