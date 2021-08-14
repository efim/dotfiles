{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";

  outputs = { self, nixpkgs }: {

    nixosConfigurations.nixos-notebook = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";

      modules = [
        ./nixos/base-conf.nix
        ({ pkgs, ... }: {
            # Let 'nixos-version --json' know about the Git revision
            # of this flake.
            system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          })
      ];
    };

  };
}
