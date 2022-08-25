{ inputs, rev, config, lib, pkgs, ... }:

{
  imports = with inputs.self.myProfiles; [
    sops
    ({ pkgs, ... }: {
        # Let 'nixos-version --json' know about the Git revision
        # of this flake.
        system.configurationRevision = inputs.nixpkgs.lib.mkIf (rev != null) rev;
      })
  ];

  boot.cleanTmpDir = true;

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
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
  sops.secrets.just_pass.neededForUsers = true;

  users.users.efim = {
    isNormalUser = true;
    home = "/home/efim";
    description = "Efim N";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNPzNPVCApezdx9JVaHMGU2ha1NsdnS2FMgCXnzPmLz efim.nefedov@nordigy.ru"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID7UaJQWWsKy59CLh7LNQOTwWL3AkQY4qhpnRbZ7sPVB efim@work-laptop"
    ];
    passwordFile = config.sops.secrets.just_pass.path;
  };

  services.tailscale.enable = true;

}
