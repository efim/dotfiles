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
    hello
    fd
  ];

  networking.firewall.allowedTCPPorts = [ 13338 80 ];
  networking.firewall.allowedUDPPorts = [ 13338 ];
  # networking.firewall.allowedTCPPortRanges = [ { from = 51000; to = 51999; } ];

  sops.secrets."yggdrasil/niobe" = { };
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


  # security.acme.acceptTerms = true;
  # security.acme.email = "efim.wool@gmail.com";
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    # recommendedTlsSettings = true;
    virtualHosts = {
      "searx.nope" = {
        # enableACME = true;
        # forceSSL = true;
        locations."/".proxyPass = "http://192.168.100.11:8080";
      };
    };
  };

  # https://blog.beardhatcode.be/2020/12/Declarative-Nixos-Containers.html
  networking.nat.enable = true;
  networking.nat.internalInterfaces = [ "ve-wasabi" ];
  networking.nat.externalInterface = "ens3"; # ??? ip addr , but do I control this name?

  containers.wasabi = {
    # ephemeral = true;
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.2";
    localAddress = "192.168.100.11";

    config = { config, pkgs, ... }: {
      services.searx.enable = true;
      services.searx.settings = {
        server.port = 8080;
        server.bind_address = "0.0.0.0";
        server.secret_key = "hello";
      };

      networking.firewall.allowedTCPPorts = [ 8080 ];
      networking.nameservers = [ "1.1.1.1" ];
    };
  };

}
