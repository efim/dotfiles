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

    # start service for Notifications, e.g from Emacs timers
    services.dunst = {
      enable = true;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome3.adwaita-icon-theme;
        size = "16x16";
      };
      settings = {
        global = {
          monitor = 0;
          geometry = "600x50-50+65";
          shrink = "yes";
          transparency = 10;
          padding = 16;
          horizontal_padding = 16;
          font = "JetBrainsMono Nerd Font 10";
          line_height = 4;
          format = ''<b>%s</b>\n%b'';
        };
      };
    };
  };
}
