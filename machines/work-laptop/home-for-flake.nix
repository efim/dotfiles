# pre-flakes centralized home-manager config
#   imports base - common with office-machine
#   sets my-options
{ inputs, config, pkgs, ... }:

{

  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    inputs.self.nixosRoles.hm-common
    xmonad # see `chunky-notebook/home.nix` for some reason `roles` recurses infinitely
    my-autorandr
    my-emacs
    my-screen-locker
    personal
    mail
  ];

  # home.file.".bashrc".source = ../../.bashrc;

  my-screen-locker.isNixManaged = false;

  # # TODO refactor getting unstable in any home.nix
  # # and also for getting it in emacs module, since mermaid-cli is needed there
  # home.packages = let
  # unstableTarball =
  #   fetchTarball {
  #     name = "nixos-unstable-2020-04-23";
  #     # Commit hash for nixos-unstable at https://github.com/NixOS/nixpkgs/tree/nixos-unstable
  #     url = https://github.com/NixOS/nixpkgs/archive/d235056d6d6dcbd2999bd55fd120d831d4df6304.tar.gz;
  #     # Hash obtained using `nix-prefetch-url --unpack <url>`
  #     sha256 = "1n0a2wja7w58fkz57hppwhc81lzjzqf251m2xz602j86gh56g3fm";
  #   };
  # unstable = import unstableTarball {
  #   config = config.nixpkgs.config;
  # };
  # in [
  #   unstable.nodePackages.mermaid-cli
  # ];

  my-autorandr = {
    # laptop display
    display1 = {
      name = "eDP-1";
      fp = "00ffffffffffff0006af3d5700000000001c0104a51f1178022285a5544d9a270e505400000001010101010101010101010101010101b43780a070383e401010350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343048414e30352e37200a0070";
    };
    # samsung via DisplayLink dock station
    display2 = {
      name = "DP-1";
      fp = "00ffffffffffff004c2d23053232524c341301030e301b782a3581a656489a241250542308008100814081809500a940b30001010101023a801871382d40582c4500dd0c1100001e000000fd00383c1e5111000a202020202020000000fc0053796e634d61737465720a2020000000ff00484c4a534330333431300a20200082";
    };
  };

  programs = {
    git = {
      userName = "efim";
      userEmail = "efim.nefedov@nordigy.ru";
    };
    bash.bashrcExtra = builtins.readFile ../../.bashrc; # returned old from Ubuntu bashrc things
    bash.profileExtra = ''
    source /home/efim/.nix-profile/etc/profile.d/nix.sh
    syncthing -no-browser -no-restart -logflags=0 & # copied from `systemctl --user cat syncthing`
    ''; # TODO - figure out why service fails!
  };

  # INFO on languages:
  # https://manpages.ubuntu.com/manpages/bionic/man5/keyboard.5.html
  #
  # Ubuntu exposes /etc/defaults/keyboard file
  #
  # XKBLAYOUT=us,ru
  # XKBVARIANT=,
  # BACKSPACE=guess
  # XKBOPTIONS=grp:caps_toggle
  #
  # took values from /nixos/config.nix
  #

  # Module `my-screen-locker` is set up to use
  # /usr/bin/xscreensaver-command
  #
  # Manual action `sudo install xscreensaver` needed.
  # (AFAIK to integrate into system PAM 'pluggable auth modules')

  # Manual action `cachix use nix-community`

  # Manual action - install fonts to mirror `fonts.fonts` in NixOS

  # module autorandr set's up common automatic display layouts
  # allows for modifying xrandr setup on system events of displays plug in\out

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

}