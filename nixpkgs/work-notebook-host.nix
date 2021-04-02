{ config, pkgs, ... }:

{
  imports = [ ./common.nix ];

  my-screen-locker.isNixManaged = false;

  programs = {
    git = {
      userName = "efim";
      userEmail = "efim.nefedov@nordigy.ru";
    };
  };

   # TODO move to the dins workstation module
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

     # TODO SHIT need to use modules somehow to have different /hosts/ and in this 'Ubuntu' based one
     # to use /usr/bin/xscreensaver-command
     # and in NixOS based one use {pkgs.xscreensaver}/bin
      # https://nixos.org/manual/nixos/stable/index.html#sec-modularity
      # https://nixos.wiki/wiki/Module
      #
      # Home-manager help:
      # https://nix-community.github.io/home-manager/index.html#_how_to_set_up_a_configuration_for_multiple_users_machines

      # config example #1
      # https://github.com/hpfr/system/commit/6d138cdffb06fecd8efa9ae0a8d3bd035e056d83
      # https://github.com/hpfr/system/blob/master/hosts/hal.nix
      # https://github.com/hpfr/system/blob/master/profiles/user/base.nix

      # config example #2
      # https://github.com/chaoflow/nixos-configurations
      #
      # config example #3
      # https://github.com/barrucadu/nixfiles
      # https://github.com/barrucadu/nixfiles/blob/master/common.nix
      # https://github.com/barrucadu/nixfiles/blob/master/configuration.nix
      #
      # config example #4
      # https://github.com/KnairdA/nixos_home/blob/master/host/asterix.nix
      #
      # article with examples #5
      # https://hugoreeves.com/posts/2019/nix-home/

}
