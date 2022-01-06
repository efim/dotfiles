{ inputs, config, lib, pkgs, ... }:

{
  imports = with inputs.self.myProfiles; [
    bare-server
    server-prometheus
    sops
  ];
}
