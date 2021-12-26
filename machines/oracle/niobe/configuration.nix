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
  networking.hostName = "niobe";

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 21 51820 9001 13338 ];
  networking.firewall.allowedUDPPorts = [ 21 51820 9001 13338 ];
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
        job_name = "niobe";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
        }];
      }
    ];
  };

  services.yggdrasil.enable = true;
  services.yggdrasil.persistentKeys = true;
  services.yggdrasil.config = {
    Listen = [
      "tcp://0.0.0.0:13338"
    ];
    Peers = [
      "tls://01.ffm.deu.ygg.yt:443"
      "tls://ygg.mkg20001.io:443"
      "tls://ygg.cofob.ru:443"
      "tcp://bunkertreff.ddns.net:5454"
      "tcp://phrl42.ydns.eu:8842"
    ];
  };

}
