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
  networking.firewall.allowedTCPPorts = [ 21 51820 ];
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

  #   # grafana configuration
  # services.grafana = {
  #   enable = true;
  #   domain = "grafana.pele";
  #   port = 2342;
  #   addr = "127.0.0.1";
  # };

  # # nginx reverse proxy
  # services.nginx.virtualHosts.${config.services.grafana.domain} = {
  #   locations."/" = {
  #       proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
  #       proxyWebsockets = true;
  #   };
  # };

}
