{ config, pkgs, ... }:

{
  imports = [
    ../../common.nix
    ../../my-autorandr.nix
  ];

  my-screen-locker.isNixManaged = true;


  home.packages = let
    unstableTarball =
      fetchTarball {
        name = "nixos-unstable-2020-04-23";
        # Commit hash for nixos-unstable at https://github.com/NixOS/nixpkgs/tree/nixos-unstable
        url = https://github.com/NixOS/nixpkgs/archive/d235056d6d6dcbd2999bd55fd120d831d4df6304.tar.gz;
        # Hash obtained using `nix-prefetch-url --unpack <url>`
        sha256 = "1n0a2wja7w58fkz57hppwhc81lzjzqf251m2xz602j86gh56g3fm";
      };
    unstable = import unstableTarball {
      config = config.nixpkgs.config;
    };
    in [
      unstable.nyxt
      unstable.nodePackages.mermaid-cli
    ];

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

  programs = {
    git = {
      userName = "efim";
      userEmail = "efim.wool@gmail.com";
    };
  };
}
