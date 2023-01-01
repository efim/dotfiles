{ inputs, config, pkgs, ... }:

{
  # TODO - figure out why modules imports have to be in top level "home.nix"
  #  or chunky build goes into infinite recursion

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "efim";
  home.homeDirectory = "/home/efim";

  nixpkgs.config.allowUnfree = true;

  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.pop-gtk-theme;
    };
    iconTheme = {
      name = "Pop";
      package = pkgs.pop-icon-theme;
    };
    font.name = "Roboto Condenced 12";
  };
  # home.sessionVariables.GTK_THEME = "Pop-dark";
  home.pointerCursor.package = pkgs.quintom-cursor-theme;
  home.pointerCursor.name = "Quintom_Snow";
  home.pointerCursor.x11.enable = true;

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    fish = {
      enable = true; # combine with emacs (used for eshell completions)
    };
    bash.enable = true;
    git = {
      enable = true;
    };

    rofi = {
      enable = true;
      plugins = [
        pkgs.rofi-emoji
      ];
      extraConfig = {
        modi = "run,drun,window,emoji"; # and emoji for some reason do not work, oh well
        kb-row-up =                     "Up,Control+k,Shift+Tab,Shift+ISO_Left_Tab";
        kb-row-down =                   "Down,Control+j";
        kb-accept-entry =               "Control+m,Return,KP_Enter";
        kb-remove-to-eol =              "Control+Shift+e";
        kb-mode-next =                  "Shift+Right,Control+Tab";
        kb-mode-previous =              "Shift+Left,Control+Shift+Tab";
        kb-remove-char-back =           "BackSpace";
        font = "Iosevka 16";
        # Monokai paper-float are official themes I kind of consider
      };
    };
  };

  services = {
    syncthing.enable = true;
  };

  # TODO figure out what to do with bash config conflicts
  # (after some configration change started to get this error)
  # Previously had common .bashrc, from Ubuntu days. I suppose that's conflict with just having `home-manager.users.efim.programs.bash = enable;`
  # error: The option `home.file..bashrc.source' has conflicting definition values:
  # - In `/home/efim/.nix-defexpr/channels/home-manager/modules/programs/bash.nix': <derivation /nix/store/hc1bfndqf8n2qlpjk3kcm91y33928v52-bashrc.drv>
  # - In `/home/efim/dotfiles/nixpkgs/common.nix': /home/efim/dotfiles/.bashrc
  # (use '--show-trace' to show detailed location information)
  # home.file.".bashrc".source = ../.bashrc;
  programs.bash.initExtra = "profetch";
  home.packages = with pkgs; [
    cachix

    ammonite

    htop
    profetch
    killall
    tldr
    keepassxc

    openconnect
    shutter
    flameshot
    gnucash

    zip
    unzip

    xmobar
    dmenu
    xbrightness
    safeeyes
    nitrogen
    rescuetime

    vlc
    # transmission-qt
    transmission-gtk
    firefox
    chromium
    ferdium

    kubectl
  ];

}
