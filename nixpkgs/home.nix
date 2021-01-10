# put symlink to this file into ~/.config/nixpkgs/home.nix

{ config, pkgs, ... }:

let
  # taken from : https://github.com/vlaci/nix-doom-emacs
  doom-dotfiles = builtins.path { path = ~/.config/nixpkgs/dotfiles/.doom.d; name = "doom.d"; };
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
  }) {
    doomPrivateDir = doom-dotfiles;
    extraConfig = ''
      (after! org-agenda
        (load-file "${doom-dotfiles}/norang-ca-org-mode.el")
        (add-to-list 'org-agenda-custom-commands `,bh/org-agenda-view)      
      )
    '';
  };
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "efim";
  home.homeDirectory = "/home/efim";

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    doom-emacs
    sqlite # for emacs org-roam

    htop
    killall
    keepass
    tdesktop

    xmobar
    dmenu
  ];

  programs.git = {
    enable = true;
    userName = "efim";
    userEmail = "efim.nefedov@nordigy.ru";
  };

  home.file.".emacs.d/init.el".text = ''
       (load "default.el")
  '';

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
