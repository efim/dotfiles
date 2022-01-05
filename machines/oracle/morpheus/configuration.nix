{ inputs, config, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "morpheus";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru" 
  ];


  # is this needed for rs-deploy?
  # https://github.com/serokell/deploy-rs/issues/25
  nix.trustedUsers = [ "@wheel" ];

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 9001 ];
  networking.firewall.allowedUDPPorts = [ 9001 ];

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
        job_name = "morpheus";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
        }];
      }
    ];
  };

  #   services.yggdrasil.enable = true;
  # services.yggdrasil.persistentKeys = true;
  # services.yggdrasil.config = {
  #   Peers = [
  #     "tls://ygg.loskiq.dev:17314"
  #     "tls://box.paulll.cc:13338"
  #   ];
  # };



}
