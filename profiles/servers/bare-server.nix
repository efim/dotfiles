{ inputs, config, lib, pkgs, ... }:

{
  imports = with inputs.self.myProfiles; [
    sops
  ];

  boot.cleanTmpDir = true;

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru"
  ];

  environment.systemPackages = with pkgs; [
    vim
  ];

  # is this needed for rs-deploy?
  # https://github.com/serokell/deploy-rs/issues/25
  nix.trustedUsers = [ "@wheel" "root" ];

  networking.firewall.enable = true;

  users.mutableUsers = false;
  sops.secrets.just_pass.neededForUsers = true;

  users.users.efim = {
    isNormalUser = true;
    home = "/home/efim";
    description = "Efim N";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru" ];
    passwordFile = config.sops.secrets.just_pass.path;
  };

  services.tailscale.enable = true;

}
