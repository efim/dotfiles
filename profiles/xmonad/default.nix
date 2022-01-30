{ config, lib, pkgs, ... }:

{

    xsession = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./xmonad.hs;
      };
    };
    # for lsp to work I guess I'd need to figure out cabal installation
    # https://www.srid.ca/haskell-new-project
    # and
    # https://www.srid.ca/xmonad-conf-ide - so like get cabal project in this directory
    # and add config.hs to Main.hs or something
    # and this is the config:
    # https://github.com/srid/nixos-config/tree/master/features/desktopish/xmonad/xmonad-srid

    xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc;
    xdg.configFile."xmobar/xmobar-tray-autopadding.sh" = {
      source = ./xmobar-tray-autopadding.sh;
      executable = true;
    };

    # start service for Notifications, e.g from Emacs timers
    services = {
      # picom = {
      #   enable = true;
      #   experimentalBackends = true;
      #   fade = true;
      #   fadeDelta = 5;
      #   package = pkgs.picom.overrideAttrs(o: {
      #     src = pkgs.fetchFromGitHub {
      #       repo = "picom";
      #       owner = "ibhagwan";
      #       rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
      #       sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
      #     };
      #   });
      #   vSync = true;
      # };
      dunst = {
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
      trayer = {
        enable = true;
        settings = {
          edge = "top";
          align = "right";
          widthtype = "request";
          expand = true;
          SetDockType = true;
          SetPartialStrut = true;
          transparent = true;
          alpha = 0;
          tint = "0x1A1918";
          heighttype = "pixel";
          height = 22;
          padding = 1;
        };
      };
    };
}
