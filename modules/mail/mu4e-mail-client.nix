{ config, lib, pkgs, ... }:

{
  programs.mu.enable = true;
  programs.msmtp.enable = true;
  programs.mbsync.enable = true;

}
