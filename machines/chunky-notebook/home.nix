# pre-flakes centralized home-manager config
#   imports base - common with office-machine
#   sets my-options
{ inputs, config, pkgs, ... }:
{

  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    inputs.self.nixosRoles.hm-common
    my-autorandr # TODO probably can be in hm-common
  ];

  my-screen-locker.isNixManaged = true;

  # TODO ? use unstable from inputs of flake?
  # home.packages = let
  #   unstableTarball =
  #     fetchTarball {
  #       name = "nixos-unstable-2020-04-23";
  #       # Commit hash for nixos-unstable at https://github.com/NixOS/nixpkgs/tree/nixos-unstable
  #       url = https://github.com/NixOS/nixpkgs/archive/fd5c6aada3c38e7410f130ebf8fb01ef00257cd7.tar.gz;
  #       # Hash obtained using `nix-prefetch-url --unpack <url>`
  #       sha256 = "1aa55yca1z592ir4h499akyg2svg2fh730pxz8xkkcjdc9yq738m";
  #     };
  #   unstable = import unstableTarball {
  #     config = config.nixpkgs.config;
  #   };
  #   in [
  #     # unstable.nyxt # not really using this
  #     # unstable.nodePackages.mermaid-cli
  #     # TODO fails after home-manager flake module:
  #     # error: attribute 'currentSystem' missing
  #      # at /nix/store/9aq55g7bvwpxdg8wnvmdcn99ykhdnrz8-nixos-unstable-2020-04-23/pkgs/top-level/impure.nix:18:43:
  #      #     17|   # (build, in GNU Autotools parlance) platform.
  #      #     18|   localSystem ? { system = args.system or builtins.currentSystem; }
  #      #       |                                           ^
  #      #     19|
  # ];

  my-autorandr = {
    # laptop display
    display1 = {
      name = "eDP-1";
      fp = "00ffffffffffff0030e47e0500000000001a010495221378eadc95a35855a0260d5054000000010101010101010101010101010101012e3680a070381f403020350058c21000001a2e3680a070381f403020350058c21000001a00000000000000000000000000000000000000000002000833ff0a3c961e163696000000001c";
    };
    # samsung via VGA
    display2 = {
      name = "DP-3";
      fp = "00ffffffffffff004c2d23053232524c34130104a5301b78223581a656489a241250542308008100814081809500a940b30001010101023a801871382d40582c4500dd0c1100001e000000fd00383c1e5111000a202020202020000000fc0053796e634d61737465720a2020000000ff00484c4a534330333431300a202000f2";
    };
  };

  home-manager.users.efim.programs = {
    git = {
      userName = "efim";
      userEmail = "efim.wool@gmail.com";
    };
  };

}
