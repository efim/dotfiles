{ inputs, config, pkgs, ... }: {

  imports = with inputs.self.myRoles; [
    ./hardware-configuration.nix
    base-server
  ];

  networking.hostName = "pythia";

  # This is the actual specification of the secrets.
  sops.secrets.example_key = { };
  sops.secrets.oi = { };

  networking.firewall.allowedTCPPorts = [ 21 51820 80 8080 ];
  networking.firewall.allowedUDPPorts = [ 21 51820 ];
  # networking.firewall.allowedTCPPortRanges = [ { from = 51000; to = 51999; } ];

  # networking.resolvconf.extraOptions = [ "rotate" ]; # this is a hack
  networking.nameservers = [ "100.116.45.26" ];

  environment.systemPackages = with pkgs; [
    cowsay
    hello
    fd
  ];


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

  # doesn't seem to work on aarch64
  # services.boinc.enable = true;
  # services.boinc.package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.boinc;

}
