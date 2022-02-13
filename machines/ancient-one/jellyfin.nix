{ config, lib, pkgs, ... }:

{

  services.jellyfin = {
    enable = true;
    group = "transmission";
    openFirewall = false; # I'll open it manually
  };

  # nginx reverse proxy
  services.nginx.enable = true;
  services.nginx.virtualHosts."jellyfin.ancient-one.nope" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:8096";
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 8080 ];
  # from https://jellyfin.org/docs/general/networking/index.html
  # these are client discovery ports
  # so clients in local network should be able to find server on thier own
  networking.firewall.allowedUDPPorts = [ 1900 7359 ];

}
