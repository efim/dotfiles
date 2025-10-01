{ inputs, config, pkgs, ... }:

{
    nixpkgs.overlays = [
      inputs.deploy-rs.overlays.default
    ];

    home.packages = with pkgs; [
      deploy-rs.deploy-rs
      inputs.agenix.packages.x86_64-linux.default
    ];
}
