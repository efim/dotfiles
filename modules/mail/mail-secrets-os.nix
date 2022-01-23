{ config, lib, pkgs, ... }:

{

  # password secret needed for mbsync on server and for msmtp on clients
  sops.secrets."email/efim.wool@gmail.com" = {
    owner = "efim"; # for home manager?
    path = "/home/efim/.config/secrets/email/efim.wool@gmail.com";
    # this is a hack to allow pure home-manager config to have access to secrets
    # default place is /run/secrets - tmpfs cleaned on reboot
    # path = "${config.xdg.configHome}/secrets/email/efim.wool@gmail.com"; # when hm integration exists can be used, but not yet
  };

}
