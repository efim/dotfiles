{ inputs, config, pkgs, ... }:

{
    nixpkgs.overlays = [
      inputs.deploy-rs.overlay
    ];

    home.packages = with pkgs; [
      deploy-rs.deploy-rs
      age
      ssh-to-age
      sops
    ];
}
