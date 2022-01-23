{ config, lib, pkgs, ... }:

{

  boot.cleanTmpDir = true;
  # nope this is can be accessed from config, and actually not used here, cool
  # networking.hostName = "morpheus";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru"
  ];

  environment.systemPackages = with pkgs; [
    vim
  ];

  # is this needed for rs-deploy?
  # https://github.com/serokell/deploy-rs/issues/25
  nix.trustedUsers = [ "@wheel" ];

  # from https://github.com/serokell/deploy-rs/tree/master/examples/system
  # due https://github.com/serokell/deploy-rs/issues/25
  security.sudo.extraRules = [{
    groups = [ "wheel" ];
    commands = [{
      command = "ALL";
      options = [ "NOPASSWD" ];
    }];
  }];


  networking.firewall.enable = true;

  users.users.efim = {
    isNormalUser = true;
    home = "/home/efim";
    description = "Efim N.";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru" ];
  };

  services.tailscale.enable = true;

}
