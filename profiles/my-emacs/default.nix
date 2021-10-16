{ inputs, config, lib, pkgs, ... }:

with lib;

{
    nixpkgs.overlays = [
      inputs.emacs-community-overlay.overlay
    ];

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

    services.emacs.enable = true;

    home.packages = with pkgs; [
      sqlite # for emacs org-roam
      gcc # for emacs org-roam : "emacsql-sqlite" should be able to compile its binary
      metals
      ripgrep
      rnix-lsp # lsp server for nix files

      # for emacs-everywhere
      xclip
      xdotool
      xorg.xprop
      xorg.xwininfo
    ];

    home.file.".doom.d" = {
      source = ./doom;
      recursive = true;
      onChange = builtins.readFile ./setup.sh;
    };

}
