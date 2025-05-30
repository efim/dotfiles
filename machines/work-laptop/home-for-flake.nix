# pre-flakes centralized home-manager config
#   imports base - common with office-machine
#   sets my-options
{ inputs, config, pkgs, ... }:

{
  targets.genericLinux.enable = true;

  dconf.enable = false;

  imports = with inputs.self.myModules; with inputs.self.myProfiles; [
    inputs.self.myRoles.hm-common
    # xmonad # see `chunky-notebook/home.nix` for some reason `roles` recurses infinitely
    my-emacs
    fonts
    with-deploy
    ./age-for-home-manager.nix
  ];

  # for some reason this config stays "allowUnfree = false" =C
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
    "rescuetime"
  ];

  # home.file.".bashrc".source = ../../.bashrc;

#   my-screen-locker.isNixManaged = false;

  # I dont' know why, but under Ubuntu these protective measures broke things
  # and i had
  # фев 16 22:16:46 efimnefedow-LT systemd[21533]: syncthing.service: Failed to set up user namespacing: Operation not permitted
  # фев 16 22:16:46 efimnefedow-LT systemd[21533]: syncthing.service: Failed at step USER spawning /nix/store/rgsxcg2c7m15a10jfq9wk7h4dy892viw-syncthing-1.19.0/bin/syncthing: Op

  systemd.user.services.syncthing.Service = {
            RestrictNamespaces = pkgs.lib.mkForce false;
            PrivateUsers = pkgs.lib.mkForce false;
            SystemCallFilter = pkgs.lib.mkForce [];
  };

  programs = {
    git = {
      userName = "efim";
      userEmail = "efim.nefedov@ringcentral.com";
    };
    bash.bashrcExtra = builtins.readFile ../../.bashrc; # returned old from Ubuntu bashrc things
  };


  # adding XDG_DATA_DIRS to drun / app files
    # export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\"
  # advice from https://github.com/nix-community/home-manager/issues/1439#issuecomment-1022576250

  # INFO on languages:
  # https://manpages.ubuntu.com/manpages/bionic/man5/keyboard.5.html
  #
  # Ubuntu exposes /etc/defaults/keyboard file
  #
  # XKBLAYOUT=us,ru
  # XKBVARIANT=,
  # BACKSPACE=guess
  # XKBOPTIONS=grp:caps_toggle
  #
  # took values from /nixos/config.nix
  #

  # Module `my-screen-locker` is set up to use
  # /usr/bin/xscreensaver-command
  #
  # Manual action `sudo install xscreensaver` needed.
  # (AFAIK to integrate into system PAM 'pluggable auth modules')

  # Manual action `cachix use nix-community`

  # Manual action - install fonts to mirror `fonts.fonts` in NixOS

  # module autorandr set's up common automatic display layouts
  # allows for modifying xrandr setup on system events of displays plug in\out

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

}
