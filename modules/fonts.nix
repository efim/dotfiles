{ config, lib, pkgs, ... }:

{
  fonts.fontconfig.enable = true;
  home.packages = with pkgs;
    let my-font =     callPackage ./my-playfair-font.nix {} ;
          in [
    source-code-pro
    iosevka
    etBook
    my-font
  ];
}
