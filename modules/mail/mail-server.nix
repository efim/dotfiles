{ config, lib, pkgs, ... }:

{
  imports = [ ./mail-base.nix ];

  programs.notmuch.hooks.preNew = ''${pkgs.isync}/bin/mbsync --all --verbose '';

  home.packages = [ pkgs.muchsync ];

  programs.mbsync = {
    enable = true;
  };

}
