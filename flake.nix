{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-21-05.url = "github:NixOS/nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-community-overlay.url = "github:nix-community/emacs-overlay";
    emacs-community-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, emacs-community-overlay, ... }@inputs:
    let
      # copied from github:belsoft/nixos
      findModules = dir:
        builtins.concatLists (
          builtins.attrValues (
            builtins.mapAttrs
              (
                name: type:
                  if type == "regular" then
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
              ) (builtins.readDir dir)
          )
        );
    in
      {
        nixosModules = builtins.listToAttrs (findModules ./modules);

        nixosProfiles = builtins.listToAttrs (findModules ./profiles);

        nixosRoles = import ./roles;

        nixosConfigurations.chunky-notebook = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";

          modules = [
            (import ./machines/chunky-notebook/base-conf.nix)
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

      };
}
