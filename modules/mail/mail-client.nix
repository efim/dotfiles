{ config, lib, pkgs, ... }:

let
  notmuchConfig = "${config.xdg.configHome}/notmuch/notmuchrc";
in {
  imports = [ ./mail-base.nix ];

  #   # A service to update the notmuch database for new messages.
  # systemd.user.services."notmuch-update-database" = {
  #   Unit.Description = "Update the notmuch database for new messages";
  #   Service.Type = "oneshot";
  #   Service.ExecStart = "${config.home.profileDirectory}/bin/notmuch --config ${notmuchConfig} new";
  #   # for some reason without explisit --config flag error "Error: cannot load config file." showed up
  #   Service.WorkingDirectory = "${config.accounts.email.maildirBasePath}/.notmuch";
  # };
  # # Copied from here: https://github.com/iamthememory/dotfiles/blob/5d463625e08477db8919b12b1dc15e2426121b2c/home/mail/default.nix
  # # Run the service to update the notmuch database every two minutes.
  # systemd.user.timers."notmuch-update-database" = {
  #   Unit.Description =
  #   config.systemd.user.services."notmuch-update-database".Unit.Description;
  #   Timer.OnCalendar = "*:0/2";
  #   Timer.RandomizedDelaySec = 30;
  #   Install.WantedBy = [
  #     "timers.target"
  #   ];
  # };


  #         ${pkgs.muchsync}/bin/muchsync --nonew --config ${notmuchConfig} franzk --config ${notmuchConfig} --nonew

#   programs.notmuch.hooks.postNew = ''
#         # ${pkgs.muchsync}/bin/muchsync --config ${notmuchConfig} franzk --config ${notmuchConfig}
#         echo hello
#   '';

  services.muchsync.remotes = {
    server.remote.host = "franzk";
  };

}
