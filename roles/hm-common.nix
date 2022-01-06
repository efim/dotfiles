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
    font.name = "Victor Mono SemiBold 12";
  };
  # home.sessionVariables.GTK_THEME = "Pop-dark";

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

  # TODO figure out what to do with bash config conflicts
  # (after some configration change started to get this error)
  # Previously had common .bashrc, from Ubuntu days. I suppose that's conflict with just having `home-manager.users.efim.programs.bash = enable;`
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
    keepassxc
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

    vlc
    transmission-qt
    firefox
    chromium
    inputs.nixpkgs-stable.legacyPackages.x86_64-linux.nyxt

    # https://discourse.nixos.org/t/local-flake-based-nix-search-nix-run-and-nix-shell/13433
    (writeShellScriptBin "nix-search-local" ''
      nix search "path:${inputs.nixpkgs}" "$@"
    '')
    (writeShellScriptBin "nix-shell-local" ''
      if [ "$#" -lt 1 ]; then
        echo "Requires an argument" >&2
        exit 1
      fi
      ATTRIBUTE="$1"
      shift
      nix shell "path:${inputs.nixpkgs}#$ATTRIBUTE" "$@"
    '')
    (writeShellScriptBin "nix-run-local" ''
      if [ "$#" -lt 1 ]; then
        echo "Requires an argument" >&2
        exit 1
      fi
      ATTRIBUTE="$1"
      shift
      nix run "path:${inputs.nixpkgs}#$ATTRIBUTE" "$@"
    '')
  ];

}
