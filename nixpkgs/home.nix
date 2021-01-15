# put symlink to this file into ~/.config/nixpkgs/home.nix

{ config, pkgs, ... }:

let
  metals = with pkgs; import ./test-metals.nix { inherit stdenv lib coursier jdk jre makeWrapper; };
in {

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "efim";
  home.homeDirectory = "/home/efim";

  nixpkgs.config.allowUnfree = true;

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };
  };

  # TODO also declare bash here
  home.file.".bashrc".source = ../.bashrc;

  home.packages = with pkgs; [
    emacs
    sqlite # for emacs org-roam
    metals
    ripgrep

    ammonite

    htop
    killall
    keepass
    tdesktop

    xmobar
    dmenu
    xscreensaver
    nitrogen

    chromium
  ];

  programs.git = {
    enable = true;
    userName = "efim";
    userEmail = "efim.nefedov@nordigy.ru";
  };

  home.file.".doom.d" = {
    source = ../emacs-doom/config;
    recursive = true;
    onChange = builtins.readFile ../emacs-doom/setup.sh;
  };

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../.xmonad/xmonad.hs;
    };
  };

  xdg.configFile."xmobar/xmobarrc".source = ../.config/xmobar/xmobarrc;


  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
