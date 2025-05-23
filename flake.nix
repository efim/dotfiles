{
  description = "One mans configurations";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/d4899e96eb336485b0f22e60282b999d5493f854";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-21.11";
    # emacs-community-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
    # deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix"; # welp, that's only for servers
    homeage.url = "github:jordanisaacs/homeage";

    htmx-examples.url =
      "git+ssh://gitea@git.sunshine.industries:65433/efim/Learning-HTMX.git";
    planning-poker-kazbegi.url =
      "git+ssh://gitea@git.sunshine.industries:65433/efim/planning-poker-gwargh.git";
    go-ssr-oauth-attempt.url =
      "git+ssh://gitea@git.sunshine.industries:65433/efim/go-ssr-pocketbase-oauth-attempt-github-mirror.git";
    some-automoderation.url =
      "git+ssh://gitea@git.sunshine.industries:65433/efim/some-automoderation.git";
  };

  outputs = { self, nixpkgs, home-manager, deploy-rs, ... }@inputs:
    let
      utils = import ./kinda-utils.nix;
      findModules = utils.findModules;
      rev = if self ? rev then self.rev else null;
    in {
      myModules = builtins.listToAttrs (findModules ./modules);

      myProfiles = builtins.listToAttrs (findModules ./profiles);

      myRoles = import ./roles;

      # nixosConfigurations.chunky-notebook = nixpkgs.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   modules = [ ./machines/chunky-notebook/base-conf.nix ];
      #   specialArgs = { inherit inputs rev; };
      # };

      homeConfigurations.work-laptop =
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [{
            imports = [ ./machines/work-laptop/home-for-flake.nix ];
            home = {
              homeDirectory = "/home/efim-nefedov";
              username = "efim-nefedov";
              stateVersion = "21.11";
            };
            nix.registry.nixpkgs.flake = nixpkgs;
          }];
          extraSpecialArgs = { inherit inputs; };
        };

      nixosConfigurations.franzk = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/franzk/configuration.nix
          # agenix.nixosModule
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
                path = deploy-rs.lib.x86_64-linux.activate.nixos
                  self.nixosConfigurations.franzk;
              };
            };
          };
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    };
}
