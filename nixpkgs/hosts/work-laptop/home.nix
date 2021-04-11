{ config, pkgs, ... }:

{
  imports = [
    ../../common.nix
    ../../my-autorandr.nix
  ];

  my-screen-locker.isNixManaged = false;

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

  # Manual action `sudo install autorandr` is needed
  # module autorandr set's up common automatic display layouts
  # allows for modifying xrandr setup on system events of displays plug in\out
  # On NixOS system managed by OS, on Ubuntu must be managed by OS too, apt seems to add systemd service

    # Home-manager help:
    # https://nix-community.github.io/home-manager/index.html#_how_to_set_up_a_configuration_for_multiple_users_machines

    # config example #1
    # https://github.com/hpfr/system/commit/6d138cdffb06fecd8efa9ae0a8d3bd035e056d83
    # https://github.com/hpfr/system/blob/master/hosts/hal.nix
    # https://github.com/hpfr/system/blob/master/profiles/user/base.nix

    # config example #2
    # https://github.com/chaoflow/nixos-configurations
    #
    # config example #3
    # https://github.com/barrucadu/nixfiles
    # https://github.com/barrucadu/nixfiles/blob/master/common.nix
    # https://github.com/barrucadu/nixfiles/blob/master/configuration.nix
    #
    # config example #4
    # https://github.com/KnairdA/nixos_home/blob/master/host/asterix.nix
    #
    # article with examples #5
    # https://hugoreeves.com/posts/2019/nix-home/
    #
    # modular nixos & nixpkgs example #6
    # https://github.com/dmarcoux/dotfiles-nixos/

}
