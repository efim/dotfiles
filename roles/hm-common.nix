# put symlink to this file into ~/.config/nixpkgs/home.nix

{ inputs, config, pkgs, ... }:

{
  # while this is wrapped in attrs of 'home-manager.users.efim' inner imports of profiles should be top level
  # TODO - make this one top level and have inner profiles define users.efim ?
  home-manager.users.efim = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "efim";
    home.homeDirectory = "/home/efim";

    imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
      my-emacs
      my-screen-locker
    ];

    # nixpkgs.config.allowUnfree = true; # fails after home-manager flake module

    programs = {
      # Let Home Manager install and manage itself.
      home-manager.enable = true;

      # TODO maybe move into gui gogether with xmonad
      # Enable autorandr. Profiles are specific to hosts, so they're not defined here
      # do I need to enable this? let's go into repo and look at what this does =C
      autorandr.enable = true;

      direnv = {
        enable = true;
        enableNixDirenvIntegration = true;
      };

      fish = {
        enable = true; # combine with emacs (used for eshell completions)
      };
      bash.enable = true;
      git = {
        enable = true;
        # userName = "efim"; # TODO define in hosts ?
        # userEmail = "efim.nefedov@nordigy.ru";
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
      ammonite

      htop
      killall
      keepass
      tdesktop
      discord
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

    xsession = {
      enable = true;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ../xmonad/xmonad.hs;
      };
    };

    xdg.configFile."xmobar/xmobarrc".source = ../xmobar/xmobarrc;


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
