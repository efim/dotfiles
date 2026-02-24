{
  inputs,
  config,
  pkgs,
  ...
}:

{
  # TODO - figure out why modules imports have to be in top level "home.nix"
  #  or chunky build goes into infinite recursion

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  # home.username = "efim";
  # home.homeDirectory = "/home/efim";

  nixpkgs.config.allowUnfree = true;

  # Ensure Nix is in your PATH (if not already sourced in your shell)
  home.sessionVariables = {
    PATH = "/nix/var/nix/profiles/default/bin:${pkgs.nix}/bin:$PATH";
  };

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
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
  programs.bash.initExtra = "profetch";
  home.packages = with pkgs; [
    cachix

    ammonite

    htop
    profetch
    killall
    tldr
    keepassxc

    shutter
    flameshot

    zip
    unzip

    nitrogen

    vlc
    transmission_4-gtk
  ];

}
