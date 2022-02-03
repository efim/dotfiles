# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs, config, pkgs, ... }:

{
  imports = with inputs.self.myRoles; [
    ./hardware-configuration.nix
    base-server
    ./transmission.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/4d63a15d-0a6d-47f9-a8ba-02ca8a68eed9";
  #:r!blkid /dev/sda2

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.hostName = "ancient-one";
  networking.useDHCP = false;
  networking.interfaces.enp2s0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.resolvconf.extraOptions = [ "rotate" ]; # this is a hack
  networking.networkmanager.enable = true;

  # TODO add syncthing for final files
  #       could be syncthing.ancient-one.nope

  services.logind.lidSwitch = "ignore";

  programs = {
    ssh.startAgent = true;
  };
  services.openssh.enable = true;

  services.boinc.enable = true;
  services.boinc.package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.boinc;


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}

