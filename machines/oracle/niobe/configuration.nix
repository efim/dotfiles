{ inputs, config, pkgs, ... }: {

  imports = with inputs.self.myRoles; [
    ./hardware-configuration.nix
    base-server
  ];

  networking.hostName = "niobe";

  # This is the actual specification of the secrets.
  sops.secrets.example_key = { };
  sops.secrets.oi = { };

  environment.systemPackages = with pkgs; [
    cowsay
    hello
    fd
  ];

  networking.firewall.allowedTCPPorts = [ 13338 ];
  networking.firewall.allowedUDPPorts = [ 13338 ];
  # networking.firewall.allowedTCPPortRanges = [ { from = 51000; to = 51999; } ];

  # networking.resolvconf.extraOptions = [ "rotate" ]; # this is a hack
  networking.nameservers = [ "100.116.45.26" ];

  sops.secrets."yggdrasil/niobe" = {};
  services.yggdrasil = {
    enable = true;
    configFile = config.sops.secrets."yggdrasil/niobe".path;
    config = {
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
  };

  services.boinc.enable = true;
  services.boinc.package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.boinc;

}
