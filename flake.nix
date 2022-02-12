{
  description = "One mans configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/5aaed40d22f0d9376330b6fa413223435ad6fee5";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-21.11";
    emacs-community-overlay.url = "github:nix-community/emacs-overlay/87ec04a69354a3ea8061de205e87078a10d1fb2a";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
    # deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix/master";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
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

      nixosConfigurations.ancient-one = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/ancient-one/configuration.nix
        ];
        specialArgs = { inherit inputs rev; };
      };

      homeConfigurations.work-laptop = home-manager.lib.homeManagerConfiguration {
        configuration = {
          imports = [ ./machines/work-laptop/home-for-flake.nix ];
        };
        extraSpecialArgs = { inherit inputs; };
        system = "x86_64-linux";
        homeDirectory = "/home/efim";
        username = "efim";
        stateVersion = "21.11";
      };

      homeConfigurations.frankz-efim = home-manager.lib.homeManagerConfiguration {
        configuration = {
          imports = [ ./machines/franzk/home.nix ];
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
        ];
        specialArgs = { inherit inputs; };
      };
      nixosConfigurations.niobe = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/oracle/niobe/configuration.nix
        ];
        specialArgs = { inherit inputs; };
      };
      nixosConfigurations.morpheus = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/oracle/morpheus/configuration.nix
        ];
        specialArgs = { inherit inputs; };
      };

      nixosConfigurations.franzk = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/franzk/configuration.nix
        ];
        specialArgs = { inherit inputs; };
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
              efim = {
                path = deploy-rs.lib.x86_64-linux.activate.home-manager self.homeConfigurations.frankz-efim;
                user = "efim";
              };
            };
          };
          pythia = {
            hostname = "pythia"; # taken from my ~/.ssh/config
            profiles.system = {
              user = "root";
              sshUser = "root";
              path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.pythia;
            };
          };
          niobe = {
            hostname = "niobe";
            profiles.system = {
              user = "root";
              sshUser = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.niobe;
            };
          };
          morpheus = {
            hostname = "morpheus";
            profiles.system = {
              user = "root";
              sshUser = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.morpheus;
            };
          };
          ancient-one = {
            hostname = "ancient-one";
            profiles.system = {
              user = "root";
              sshUser = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.ancient-one;
            };
          };

        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    };
}
