# put symlink to this file into ~/.config/nixpkgs/home.nix

{ inputs, config, pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    source-code-pro
    iosevka
  ];

  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    xmonad
    my-autorandr
    my-emacs
    my-screen-locker
  ];

  home-manager.users.efim = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "efim";
    home.homeDirectory = "/home/efim";

    # nixpkgs.config.allowUnfree = true;

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
    };

    services = {
      syncthing.enable = true;
    };

    # TODO also declare bash here -- what's the context of the todo???
    # TODO figure out why conflicts
    # error: The option `home.file..bashrc.source' has conflicting definition values:
    # - In `/home/efim/.nix-defexpr/channels/home-manager/modules/programs/bash.nix': <derivation /nix/store/hc1bfndqf8n2qlpjk3kcm91y33928v52-bashrc.drv>
    # - In `/home/efim/dotfiles/nixpkgs/common.nix': /home/efim/dotfiles/.bashrc
    # (use '--show-trace' to show detailed location information)
    # home.file.".bashrc".source = ../.bashrc;

    home.packages = with pkgs; [
      cachix

      ammonite
      sbcl

      htop
      killall
      keepass
      tdesktop
      discord
      element-desktop

      openconnect
      shutter
      gnucash

      zip
      unzip

      xmobar
      dmenu
      xbrightness
      safeeyes
      nitrogen
      rescuetime
      picom

      vlc
      transmission-qt
      firefox
      chromium
    ];

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "21.03";
  };
}