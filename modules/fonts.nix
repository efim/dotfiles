{ config, lib, pkgs, ... }:

{
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    source-code-pro
    # iosevka
    etBook
  ];
}
