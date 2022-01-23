{ inputs, config, lib, pkgs, ... }:

{
  imports = with inputs.self.myProfiles; [
    bare-server
    server-prometheus
    sops
    inputs.self.myModules.my-dns # whether dns client (default) or dns-server (explicit)
  ];
}
