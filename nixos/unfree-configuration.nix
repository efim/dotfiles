# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# put symlink to these files into /etc/nixos/
{ config, pkgs, ... }:

{
  imports = [ ./base-conf.nix ];

  # at least for 'displaylink' driver - display through usb-c
  nixpkgs.config.allowUnfree = true;

  # TODO modularize unfree, for easier install on new machine?
  # TODO 'nixos-rebuild dry-build --verbose' is different between merged configuration and this tip of separated configs
  #   lot's of additional derivations to be build, no idea why
  #   passwdpam, xlock.pam, unuser-1.pam, sshd.pam,
  services.xserver.videoDrivers = [ "displaylink" ];

}
