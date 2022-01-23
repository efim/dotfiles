{ config, lib, pkgs, ... }:

{
  options = {
    my-dns.isServer = lib.mkOption {
      description = "Whether server is a client of my private dns, or a server.";
      type = lib.types.bool;
      default = false;
    };
  };
  config = lib.mkMerge [
    (lib.mkIf config.my-dns.isServer {
      #my dns server (primary and only)
      services.bind = {
        enable = true;
        zones = {
          "nope" = {
            file = ./from-linux-guide.zone;
            master = true;
            slaves = [
              "127.0.0.1"
            ];
          };
        };
        cacheNetworks = [ "100.0.0.0/8" "127.0.0.1" ];
        forwarders = [ "1.1.1.1" "1.0.0.1" ];
        extraOptions = "dnssec-validation no;";
      };
      networking.firewall.allowedUDPPorts = [ 53 ];
    })
    (lib.mkIf (!config.my-dns.isServer) {
      # my dns clients
      networking.networkmanager.enable = true;
      networking.networkmanager.dns = "dnsmasq";
      services.dnsmasq.enable = true;
      services.dnsmasq.servers = [ "100.116.45.26" ];
      services.dnsmasq.resolveLocalQueries = false;
    })
  ];
}

# mkIf to get conditional config
# and also using "merge configuration" https://nixos.org/manual/nixos/stable/index.html#sec-option-definitions-merging
# so that I'd just put config on base level
