{ inputs, config, pkgs, ... }:
{
  networking.hostName = "franzk";

  imports = with inputs.self.myRoles; [
    ./vpsadminos.nix
    base-server
  ];

  # could be general and maybe I'd want mkforce or what was that mkoverride
  # and this would work only after tailscale login
  networking.nameservers = [ "100.116.45.26" "1.1.1.1" ];

  # Another option would be root on the server
  # from https://github.com/serokell/deploy-rs/tree/master/examples/system
  # due https://github.com/serokell/deploy-rs/issues/25
  security.sudo.extraRules = [{
    groups = [ "wheel" ];
    commands = [{
      command = "ALL";
      options = [ "NOPASSWD" ];
    }];
  }];

  environment.systemPackages = with pkgs; [
    vim
  ];

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  #users.extraUsers.root.openssh.authorizedKeys.keys =
  #  [ "..." ];

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=900s
  '';

  time.timeZone = "Europe/Amsterdam";

  system.stateVersion = "22.05";
}
