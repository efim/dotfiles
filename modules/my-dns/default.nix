{ config, lib, pkgs, ... }:

{
  options = {
    my-dns.isServer = lib.mkOption {
      description = "Whether server is a client of my private dns, or a server.";
      type = lib.types.bool;
      default = false;
    };
    my-dns.type =
      let
        my-dns-type = lib.types.enum [
          "server"
          "client-dnsmasq"
          "client-no-networkmanager"
        ];
      in lib.mkOption {
        description = "Whether machine is server, or a client. And what type of client config.";
        type = my-dns-type;
        default = "client-no-networkmanager";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf (config.my-dns.type == "server") {
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
          "badlist" = {
            file = ./master-badlist;
            master = true;
          };
        };
        cacheNetworks = [ "100.0.0.0/8" "127.0.0.1" ];
        forwarders = [ "1.1.1.1" "1.0.0.1" ];
        extraOptions = ''
dnssec-validation no;
response-policy { zone "badlist"; };
'';
        # extraConfig = ''
        #   response-policy { zone "badlist"; };
        # '';
      };
      networking.firewall.allowedUDPPorts = [ 53 ];
    })
    (lib.mkIf (config.my-dns.type == "client-dnsmasq") {
      # my dns clients, primarily for chunky
      # since he had problems with dhcp picking public dns as primary
      networking.networkmanager.enable = true;
      networking.networkmanager.dns = "dnsmasq";
      services.dnsmasq.enable = true;
      services.dnsmasq.servers = [ "100.116.45.26" ];
      services.dnsmasq.resolveLocalQueries = false;
    })
    (lib.mkIf (config.my-dns.type == "client-no-networkmanager") {
      # my dns clients
      # same as before for servers
      # for some reason franzk fails to install NetworkManager
      networking.nameservers = [ "100.116.45.26" "1.1.1.1" ];
    })
  ];
}

# mkIf to get conditional config
# and also using "merge configuration" https://nixos.org/manual/nixos/stable/index.html#sec-option-definitions-merging
# so that I'd just put config on base level
