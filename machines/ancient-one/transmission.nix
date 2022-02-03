{ config, lib, pkgs, ... }:

{

  sops.secrets.transmission-settings = {
    sopsFile = ./secrets.yaml;
    owner = config.systemd.services.transmission.serviceConfig.User;
  };

  services.transmission = {
    enable = true;
    openFirewall = true;
    downloadDirPermissions = "775";
    port = 9091;
    settings = {
      rpc-enabled = true;
      rpc-bind-address = "127.0.0.1";
      rpc-authentication-required = true;
      rpc-port = 9091;
      rpc-whitelist-enabled = true;
      rpc-host-whitelist-enabled = false;
    };
    credentialsFile = config.sops.secrets.transmission-settings.path;
  };
  # sample contents of the secrets file:
  #
  # transmission-settings: '{ "rpc-password": "MY_PASSWORD", "rpc-username": "USERNAME", "rpc-whitelist": "127.0.0.1 192.168.*.*" }'
  # anotherSecret: 1
  #
  # the value of string secret is content that would be put into file
  # in json format so it would be understood and merged by transmission options

  # nginx reverse proxy
  services.nginx.enable = true;
  services.nginx.virtualHosts."transmission.ancient-one.nope" = {
    locations."/" = {
        proxyPass =
          let
            conf = config.services.transmission.settings;
          in "http://${conf.rpc-bind-address}:${toString conf.rpc-port}";
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 8080 ];

  # to allow user access to download dir
  users.users.efim.extraGroups = [ config.services.transmission.group ];
}
