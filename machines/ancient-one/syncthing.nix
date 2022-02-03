{ config, lib, pkgs, ... }:

{
  services.syncthing.enable = true;
  # nginx reverse proxy
  services.nginx.enable = true;
  services.nginx.virtualHosts."syncthing.ancient-one.nope" = {
    locations."/" = {
      proxyPass = "http://${config.services.syncthing.guiAddress}";
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 8080 ];

  # welp, without home-manager module service runs from separate user
  # it's /var/lib/syncthing directory is readable only by owner
  # couldn't find how to add +r for group, so that I could add group to my user and access
  #
  # for now I'd copy files like this:
  # sudo cp /var/lib/transmission/Downloads/my-file /var/lib/syncthing/ancient-downloads

}
