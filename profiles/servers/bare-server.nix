{ inputs, rev, config, lib, pkgs, ... }:

{
  imports = with inputs.self.myProfiles; [
    inputs.agenix.nixosModules.default
    ({ pkgs, ... }: {
        # Let 'nixos-version --json' know about the Git revision
        # of this flake.
        system.configurationRevision = inputs.nixpkgs.lib.mkIf (rev != null) rev;
      })
  ];

  age.secrets.main-user-pwd.file = ../../secrets/server-user-password.age;

  boot.cleanTmpDir = true;

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    ports = [ 65433 ];
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID7UaJQWWsKy59CLh7LNQOTwWL3AkQY4qhpnRbZ7sPVB efim@work-laptop"
  ];

  environment.systemPackages = with pkgs; [
    vim
    pfetch
  ];
  programs.bash.interactiveShellInit = "pfetch";

  # is this needed for rs-deploy?
  # https://github.com/serokell/deploy-rs/issues/25
  nix.trustedUsers = [ "@wheel" "root" ];

  networking.firewall.enable = true;

  users.mutableUsers = false;

  users.users.efim = {
    isNormalUser = true;
    home = "/home/efim";
    description = "Efim N";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID7UaJQWWsKy59CLh7LNQOTwWL3AkQY4qhpnRbZ7sPVB efim@work-laptop"
    ];
    # A hashed password can be generated using mkpasswd -m sha-512. Or root can set with `passwd`
    passwordFile = config.age.secrets.main-user-pwd.path;
  };

  services.tailscale.enable = true;

}
