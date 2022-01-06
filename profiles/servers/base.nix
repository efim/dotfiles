{ config, lib, pkgs, ... }:

{
  imports = [
    ./bare.nix
    ./server-prometheus.nix
  ];
}
