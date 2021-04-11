{ config, pkgs, ... }:

{
  imports = [
    ../../common.nix
    ./autorandr.nix
  ];
  my-screen-locker.isNixManaged = true;
  programs = {
    git = {
      userName = "efim";
      userEmail = "efim.wool@gmail.com";
    };
  };
}
