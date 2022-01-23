{ config, lib, pkgs, ... }:

{

  networking.networkmanager.enable = true;
  networking.networkmanager.dns = "dnsmasq";
  services.dnsmasq.enable = true;
  services.dnsmasq.servers = [ "100.116.45.26" ];
  services.dnsmasq.resolveLocalQueries = false;

}
