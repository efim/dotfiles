{ config, lib, pkgs, ... }:

{
  accounts.email = let
    mailDir = "${config.home.homeDirectory}/.mail";
    mbsyncExtraConf = {
      Create = "Near";
      Expunge = "Both";
      SyncState = "*";
      Sync = "all";
    };
  in
    {
      maildirBasePath = mailDir;
      accounts = {
        personal = {
          realName = "Efim Nefedov";
          userName = "efim.wool@gmail.com";
          address = "efim.wool@gmail.com";
          flavor = "gmail.com";
          folders = {
            drafts = "drafts";
            inbox = "inbox";
            sent = "sent";
            trash = "trash";
          };
          mbsync = {
            enable = true;
            groups = {
              "personal".channels = {
                "inbox" = {
                  patterns = [ "INBOX" ];
                  extraConfig = mbsyncExtraConf;
                };
                "sent" = {
                  farPattern = "[Gmail]/Sent Mail";
                  nearPattern = "sent";
                  extraConfig = mbsyncExtraConf;
                };
                "archive" = {
                  farPattern = "[Gmail]/All Mail";
                  nearPattern = "archive";
                  extraConfig = mbsyncExtraConf;
                };
                "drafts" = {
                  farPattern = "[Gmail]/Drafts";
                  nearPattern = "drafts";
                  extraConfig = mbsyncExtraConf;
                };
                "trash" = {
                  farPattern = "[Gmail]/Trash";
                  nearPattern = "trash";
                  extraConfig = mbsyncExtraConf;
                };
                "spam" = {
                  farPattern = "[Gmail]/Spam";
                  nearPattern = "spam";
                  extraConfig = mbsyncExtraConf;
                };
              };
            };
          };
          msmtp = {
            enable = true;
          };
          notmuch.enable = true;

          # I guess this could be one way to use sops-nix with home-manager
          # if passwords can be issued with commands
          passwordCommand = "${pkgs.coreutils}/bin/cat /run/secrets/email/efim.wool@gmail.com";
          # passwordCommand = "${pkgs.pass}/bin/pass  email/efim.wool@gmail.com";

          primary = true;
        };
      };
    };
}
