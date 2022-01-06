{
  description = "One mans configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-21-05.url = "github:NixOS/nixpkgs/nixos-21.05";
    emacs-community-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix/master";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, emacs-community-overlay, deploy-rs, sops-nix, ... }@inputs:
    let
      # copied from github:belsoft/nixos
      findModules = dir:
        builtins.concatLists (
          builtins.attrValues (
            builtins.mapAttrs
              (
                name: type:
                if type == "regular" && ((builtins.match "(.*)\\.org" name) != null)
                  then []
                else if type == "regular" then
                    [
                      {
                        name = builtins.elemAt (builtins.match "(.*)\\.nix" name) 0;
                        value = dir + "/${name}";
                      }
                    ]
                else if (builtins.readDir (dir + "/${name}"))
                    ? "default.nix" then [
                    {
                      inherit name;
                      value = dir + "/${name}";
                    }
                  ] else
                    findModules (dir + "/${name}")
              )
              (builtins.readDir dir)
          )
        );
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
        specialArgs = { inherit inputs; };
      };

      homeConfigurations.work-laptop = home-manager.lib.homeManagerConfiguration {
        configuration = {
          imports = [ ./machines/work-laptop/home-for-flake.nix ];
        };
        extraSpecialArgs = { inherit inputs; };
        system = "x86_64-linux";
        homeDirectory = "/home/efim";
        username = "efim";
      };

      nixosConfigurations.pythia = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          ./machines/oracle/pythia/configuration.nix
          sops-nix.nixosModules.sops
        ];
        specialArgs = { inherit inputs; };
      };
      nixosConfigurations.niobe = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/oracle/niobe/configuration.nix
          sops-nix.nixosModules.sops
        ];
        specialArgs = { inherit inputs; };
      };
      nixosConfigurations.morpheus = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/oracle/morpheus/configuration.nix
          # sops-nix.nixosModules.sops
        ];
        specialArgs = { inherit inputs; };
      };


      deploy = {
        magicRollback = true;
        nodes = {
          pythia = {
            hostname = "pythia"; # taken from my ~/.ssh/config
            profiles.system = {
              user = "root";
              sshUser = "root"; # for some reason
              path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.pythia;
            };
          };
          niobe = {
            hostname = "niobe";
            profiles.system = {
              user = "root";
              sshUser = "root"; # for some reason
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.niobe;
            };
          };
          morpheus = {
            hostname = "morpheus";
            profiles.system = {
              user = "root";
              sshUser = "root"; # for some reason
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.morpheus;
            };
          };
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    };
}
