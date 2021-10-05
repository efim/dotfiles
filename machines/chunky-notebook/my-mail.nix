{ config, lib, pkgs, ... }:

{

    # ALL of this will move to its own module
    # services.gpg-agent.enable = true;
    # # for some reason didn't work

    home.packages = [ pkgs.pass pkgs.afew ];

    xdg.configFile."afew/config".text = ''
      # This is the default filter chain
      [SpamFilter]
      [KillThreadsFilter]
      [ArchiveSentMailsFilter]
      # [FolderNameFilter]
      # folder_lowercases = true
      # folder_transforms = "All Mail":archive "Sent Mail":sent
      [MailMover]
      folders = Inbox Spam Trash "All Mail"
      rename = True
      Inbox = 'tag:spam':Spam 'NOT tag:inbox':"All Mail"
      Spam = 'NOT tag:spam AND tag:inbox':Inbox 'NOT tag:spam':"All Mail"
    '';

    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.notmuch = {
      enable = true;
      hooks = {
        preNew = "mbsync --all";
        postNew = ''
          afew --tag --new -v
      "notmuch tag +inbox -- path:/.*/Inbox/"
      "notmuch tag +draft -- path:/.*/Drafts/"
      "notmuch tag +sent  -- path:/.*/Sent/ Mail/"
      "notmuch tag +trash -- path:/.*/Trash/"
      "notmuch tag +spam  -- path:/.*/Spam/"
      "notmuch tag +starred  -- path:/.*/Starred/"
      "notmuch tag +list  -- path:/lists/.*/"
      "notmuch tag +todo -inbox -sent  -- tag:inbox and tag:sent"
      # If file was moved out of folder on server remove respective tag
      "notmuch tag -inbox -- not path:/.*/Inbox/ and tag:inbox"
      "notmuch tag -trash -- not path:/.*/Trash/ and tag:trash"
      "notmuch tag -spam  -- not path:/.*/Spam/  and tag:spam"

      "notmuch tag -new -- tag:new"
        '';
      };
    };
    accounts.email = {
      accounts."efim.wool@gmail.com" = {
        address = "efim.wool@gmail.com";
        # gpg = {
        #   key = "F9119EC8FCC56192B5CF53A0BF4F64254BD8C8B5";
        #   signByDefault = true;
        # };
        imap.host = "imap.gmail.com";
        mbsync = {
          enable = true;
          create = "maildir";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        primary = true;
        realName = "Efim Nefedov";
        # signature = {
        #   text = ''
        #     Mit besten Wünschen
        #     Ben Justus Bals
        #     https://keybase.io/beb
        #   '';
        #   showSignature = "append";
        # };
        passwordCommand = "pass email/efim.wool@gmail.com";
        smtp = {
          host = "smtp.gmail.com";
        };
        userName = "efim.wool@gmail.com";
      };
    };

}
