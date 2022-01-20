{ config, lib, pkgs, ... }:

{
  imports = [ ./mail-base.nix ];

  programs.notmuch.hooks.preNew = ''${pkgs.isync}/bin/mbsync --all --verbose '';

  home.packages = [ pkgs.muchsync ];

  programs.mbsync = {
    enable = true;
  };

  # services = {
  #   mbsync = {
  #     enable = true;
  #     # sync every 5 minutes, but alerts can be less frequent
  #     frequency = "*:0/5";
  #   };
  # };

}
