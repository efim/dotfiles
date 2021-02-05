{ config, lib, pkgs, ... }:

with lib;

let
  metals = with pkgs; import ./test-metals.nix { inherit stdenv lib coursier jdk jre makeWrapper; };
in {
  programs = {
    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.vterm ];
    };

  };

  home.packages = with pkgs; [
    sqlite # for emacs org-roam
    metals
    ripgrep
  ];

  home.file.".doom.d" = {
    source = ../emacs-doom/config;
    recursive = true;
    onChange = builtins.readFile ../emacs-doom/setup.sh;
  };


}
