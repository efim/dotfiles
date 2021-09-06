{ inputs, config, lib, pkgs, ... }:

with lib;

let
  metals = with pkgs; import ./test-metals.nix {
    inherit stdenv lib coursier makeWrapper;
    jdk = pkgs.openjdk11;
    jre = pkgs.openjdk11;
  };
in {

  nixpkgs.overlays = [
    inputs.emacs-community-overlay.overlay
  ];

  home-manager.users.efim = {
    programs = {
      emacs = {
        enable = true;
        extraPackages = epkgs: [ epkgs.vterm ];
        package = pkgs.emacsPgtkGcc;
        # TODO - add declarative cachix, it has repo & modules
        #   for some reason couldn't figure out how to import
        #   and whether I'd need my current ~/.config/nix/conf
        # until then would need `$ cachix use nix-community`
      };

    };

    home.packages = with pkgs; [
      sqlite # for emacs org-roam
      gcc # for emacs org-roam : "emacsql-sqlite" should be able to compile its binary
      metals
      ripgrep
      cowsay
    ];

    home.file.".doom.d" = {
      source = ./doom;
      recursive = true;
      onChange = builtins.readFile ./setup.sh;
    };

  };
}
