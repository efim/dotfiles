{ inputs, config, pkgs, ... }:
{
  networking.hostName = "franzk";

  imports = with inputs.self.myRoles; [
    ./vpsadminos.nix
    ./nextcloud.nix
    base-server
  ];

  services.syncthing.enable = true;
  # nginx reverse proxy
  services.nginx.enable = true;
  services.nginx.virtualHosts."syncthing.franzk.nope" = {
    locations."/" = {
        proxyPass = "http://127.0.0.1:8384";
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 8080 ];

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=900s
  '';

  time.timeZone = "Europe/Amsterdam";

  system.stateVersion = "22.05";
}
