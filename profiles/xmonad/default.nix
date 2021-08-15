{ config, lib, pkgs, ... }:

{
  home-manager.users.efim = {

    xsession = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./xmonad.hs;
      };
    };

    xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc;
    xdg.configFile.".xmonad/smonad-session-rc".source = ./xmonad-session-rc;

  };
}
