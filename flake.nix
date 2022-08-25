{
  description = "One mans configurations";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/d4899e96eb336485b0f22e60282b999d5493f854";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-21.11";
    emacs-community-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
    # deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix/master";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    # simple-nixos-mailserver.url = "git+https://gitlab.com/simple-nixos-mailserver/nixos-mailserver";
    # simple-nixos-mailserver.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, emacs-community-overlay, deploy-rs, sops-nix, ... }@inputs:
    let
      utils = import ./kinda-utils.nix;
      findModules = utils.findModules;
      rev = if self ? rev then self.rev else null;
    in
    {
      myModules = builtins.listToAttrs (findModules ./modules);

      myProfiles = builtins.listToAttrs (findModules ./profiles);

      myRoles = import ./roles;

      nixosConfigurations.chunky-notebook = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/chunky-notebook/base-conf.nix
        ];
        specialArgs = { inherit inputs rev; };
      };

      homeConfigurations.work-laptop = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
          {
            imports = [ ./machines/work-laptop/home-for-flake.nix ];
            home = {
              homeDirectory = "/home/efim";
              username = "efim";
              stateVersion = "21.11";
            };
            nix.registry.nixpkgs.flake = nixpkgs;
          }
        ];
        extraSpecialArgs = { inherit inputs; };
      };

      nixosConfigurations.franzk = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/franzk/configuration.nix
        ];
        specialArgs = { inherit inputs rev; };
      };

      deploy = {
        magicRollback = true;
        nodes = {
          franzk = {
            hostname = "franzk";
            profiles = {
              system = {
                user = "root";
                sshUser = "root";
                path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.franzk;
              };
            };
          };
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    };
}
