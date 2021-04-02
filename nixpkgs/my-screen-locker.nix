{ config, pkgs, ... }:

let
    mkOption = pkgs.lib.mkOption;
    types = pkgs.lib.types;

    isNixManaged = config.my-screen-locker.isNixManaged;

    lockLocation = if isNixManaged then
                   "${pkgs.xscreensaver}"
                 else
                   "/usr";
    lockCommand = "${lockLocation}/bin/xscreensaver-command -lock";

 in {

   options.my-screen-locker.isNixManaged = mkOption {
        type = types.bool;
        # default = true; # let's try without default first
        description = ''
          Specifies whether screensaver should be managed by nix.
          Hosts on Ubuntu need to have it installed via apt for PAM integration
        '';
      };

   config = {
    services = {
    xscreensaver.enable = isNixManaged;
    screen-locker = {
      enable = true;
      inactiveInterval = 5;
      lockCmd = lockCommand; # TODO factor out between xmonad.hs , how?
    };
    };
   };

}
