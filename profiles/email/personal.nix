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

          passwordCommand = "${pkgs.pass}/bin/pass  email/efim.wool@gmail.com";

          primary = true;
        };
      };
    };
}
