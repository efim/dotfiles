{ inputs, config, pkgs, ... }:
{
  networking.hostName = "franzk";

  imports = with inputs.self.myRoles; [
    ./vpsadminos.nix
    base-server
    inputs.self.myModules.mail-secrets-os
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

  services.gitea = {
    enable = true;
    rootUrl = "http://gitea.nope/";
    disableRegistration = true;
  };
  services.nginx.virtualHosts."gitea.nope" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.gitea.httpPort}";
    };
  };

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=900s
  '';

  time.timeZone = "Europe/Amsterdam";

  system.stateVersion = "22.05";
}
