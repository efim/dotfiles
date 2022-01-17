{ config, lib, pkgs, ... }:

{
    sops.secrets."nextcloud/admin_password" = {
      sopsFile = ./secrets.yaml;
      owner = config.systemd.services.nextcloud-cron.serviceConfig.User;
    };

    services.nextcloud = {
    enable = true;
    hostName = "nextcloud.nope";
    config = {
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      adminpassFile = config.sops.secrets."nextcloud/admin_password".path;
      adminuser = "root";
    };
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [
     { name = "nextcloud";
       ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
     }
    ];
  };

  # ensure that postgres is running *before* running the setup
  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
