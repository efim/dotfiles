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
        package = pkgs.emacs29;
        # TODO - add declarative cachix, it has repo & modules
        #   for some reason couldn't figure out how to import
        #   and whether I'd need my current ~/.config/nix/conf
        # until then would need `$ cachix use nix-community`
      };
    bash.bashrcExtra = ''
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"
    ''; # for the emacs-eat optional integration start. for some reason __eat_enable_integration doesn't run

    };

    services.emacs.enable = true;

    home.sessionVariables = {
      ALTERNATE_EDITOR = "";
      EDITOR = "emacsclient -t";                  # $EDITOR opens in terminal
      VISUAL = "emacsclient -c -a emacs";         # $VISUAL opens in GUI mode
    };

    home.packages = with pkgs; [
      sqlite # for emacs org-roam
      gcc # for emacs org-roam : "emacsql-sqlite" should be able to compile its binary
      libgccjit # for emacs jit compilation
      metals
      nailgun
      scalafmt

      nixfmt
      pandoc

      aspell
      aspellDicts.ru
      aspellDicts.en
      aspellDicts.en-computers
      
      haskellPackages.haskell-language-server
      haskellPackages.hoogle
      # haskellPackages.Cabal_3_6_2_0
      cabal-install
      stack
      ghc

      ripgrep
      fd
      
      nodePackages.mermaid-cli

      # for emacs-everywhere
      xclip
      xdotool
      xorg.xprop
      xorg.xwininfo
    ];

    home.file.".doom.d" = {
      source = ./doom;
      recursive = true;
#      onChange = builtins.readFile ./setup.sh;
    };

    xdg.configFile."my-emacs-experiment" = {
      source = ./my-emacs-experiment;
      recursive = true;
    };

    programs.bash.shellAliases = {
        pure-emacs = "emacs --init-directory=~/.config/my-emacs-experiment &";
    };

}
