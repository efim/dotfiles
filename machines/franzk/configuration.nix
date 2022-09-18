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
  # yay, even without personal dns, I can have one domain per host with tailscala MagicDNS
  # not perfect, but ok
  services.nginx.virtualHosts."franzk.efim.github.beta.tailscale.net" = {
    locations."/" = {
        proxyPass = "http://127.0.0.1:8384";
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 8080 ];

  age.secrets.secret1.file = ../../secrets/franzk-server-secret.age;

  environment.etc."demo.secret".source = config.age.secrets.secret1.path;

  services.gitea = {
    enable = true;
    rootUrl = "http://git.sunshine.industries/";
    settings.service.DISABLE_REGISTRATION = true;
    # disableRegistration = true;
    settings.server = {
      DISABLE_SSH = false;
      SSH_PORT = 65433;
    };
    # ssh.enable = true;
    # ssh.clonePort = 65433;
  };
  services.nginx.virtualHosts."git.sunshine.industries" = {
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
