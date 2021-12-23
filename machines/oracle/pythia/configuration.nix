{ inputs, config, pkgs, ... }: {

  imports = [
    ./hardware-configuration.nix
    inputs.self.myProfiles.sops
  ];

  # This is the actual specification of the secrets.
  sops.secrets.example_key = { };
  sops.secrets.oi = { };

  # is this needed for rs-deploy?
  # https://github.com/serokell/deploy-rs/issues/25
  nix.trustedUsers = [ "@wheel" ];

  boot.cleanTmpDir = true;
  networking.hostName = "pythia";

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 21 51820 80 8080 ];
  networking.firewall.allowedUDPPorts = [ 21 51820 ];
  # networking.firewall.allowedTCPPortRanges = [ { from = 51000; to = 51999; } ];

  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru"
  ];

  environment.systemPackages = with pkgs; [
    cowsay
    hello
    fd
  ];

  users.users.efim = {
    isNormalUser = true;
    home = "/home/efim";
    description = "Foobar yups";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru" ];
  };

  services.tailscale.enable = true;

  # declaration of gratitude to https://christine.website/blog/prometheus-grafana-loki-nixos-2020-11-20

  # grafana configuration
  services.grafana = {
    enable = true;
    domain = "pythia.efim.github.beta.tailscale.net";
    port = 2342;
    addr = "127.0.0.1";
  };

  # nginx reverse proxy
  services.nginx.enable = true;
  services.nginx.virtualHosts.${config.services.grafana.domain} = {
    locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
        proxyWebsockets = true;
    };
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    scrapeConfigs = [
      {
        job_name = "pythia";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
        }];
      }
    ];
  };

}
