{ config, lib, pkgs, ... }:

let
  notmuchConfig = "${config.xdg.configHome}/notmuch/notmuchrc";
in {
  imports = [ ./mail-base.nix ];

  services.muchsync.remotes = {
    server.remote.host = "franzk"; # systemd cron job
  };

}
