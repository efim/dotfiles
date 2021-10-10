# copied from https://github.com/junyi-hou/dotfiles/blob/main/modules/mail.nix
{ config, lib, pkgs, ... }:

{
  programs.notmuch = {
    enable = true;
    maildir.synchronizeFlags = true;
    new = {
      tags = [ "new" ]; # to be used together with afew
    };
    hooks = {
      postNew = ''
        ${pkgs.afew}/bin/afew --new --tag --verbose
        ${pkgs.afew}/bin/afew --all --move-mails --verbose
      '';
    };
    search.excludeTags = [ "trash" "spam" ];
  };

  # home.activation.fixNotmuchConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   $DRY_RUN_CMD ln $VERBOSE_ARG -sf ~/.config/notmuch/notmuchrc ~/.notmuch-config
  # '';
  programs.afew.enable = true;
  programs.password-store.enable = true;
  programs.password-store.settings.PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";

  programs.afew.extraConfig = with builtins;
    let
      folder-fn = mailboxes: account: with account;
        concatStringsSep " " (map (box: "${name}/${box}") mailboxes);

      rule-fn = account: with account;
        ''
          ${name}/inbox   = 'tag:spam':${name}/spam                       'NOT tag:inbox':${name}/archive 'tag:trash':${name}/trash
          ${name}/archive = 'tag:inbox':${name}/inbox                     'tag:spam':${name}/spam         'tag:trash':${name}/trash
          ${name}/spam    = 'NOT tag:spam AND tag:inbox':${name}/inbox    'NOT tag:spam':${name}/archive
          ${name}/trash   = 'tag:inbox AND (NOT tag:trash)':${name}/inbox 'NOT tag:trash':${name}/archive
        '';

      move-folders = "folders = " + concatStringsSep " "
        (
          map (folder-fn [ "inbox" "spam" "archive" "trash" ])
            (attrValues config.accounts.email.accounts)
        );

      move-rules = concatStringsSep
        "\n" (map rule-fn (attrValues config.accounts.email.accounts));
    in
      ''
        [MailMover]
        rename = True
      '' + move-folders + "\n" + move-rules + "\n" + ''
        [SpamFilter]
        [ArchiveSentMailsFilter]
        [Filter.1]
        query = NOT path:"/.*/inbox/**/"
        tags = -new
        message = remove the "new" tag for message not found in the inbox folder
        [Filter.2]
        query = from:github.com AND tag:new
        tags = -new +github
        [InboxFilter]
      '';

  programs.msmtp.enable = true;

  programs.mbsync = {
    enable = true;
  };

  services = {
    mbsync = {
      enable = true;
      # sync every 5 minutes, but alerts can be less frequent
      frequency = "*:0/5";
    };
  };

  # A service to update the notmuch database for new messages.
  systemd.user.services."notmuch-update-database" = {
    Unit.Description = "Update the notmuch database for new messages";
    Service.Type = "oneshot";
    Service.ExecStart = "${config.home.profileDirectory}/bin/notmuch --config ${config.xdg.configHome}/notmuch/notmuchrc new";
    # for some reason without explisit --config flag error "Error: cannot load config file." showed up
    Service.WorkingDirectory = "${config.accounts.email.maildirBasePath}/.notmuch";
  };

  # Copied from here: https://github.com/iamthememory/dotfiles/blob/5d463625e08477db8919b12b1dc15e2426121b2c/home/mail/default.nix
  # Run the service to update the notmuch database every two minutes.
  systemd.user.timers."notmuch-update-database" = {
    Unit.Description =
    config.systemd.user.services."notmuch-update-database".Unit.Description;
    Timer.OnCalendar = "*:0/2";
    Timer.RandomizedDelaySec = 30;
    Install.WantedBy = [
      "timers.target"
    ];
  };

  home.activation.initMailDir =
    let
      mailDir = "${config.home.homeDirectory}/.mail";
      mkAccountDir = with builtins; concatStringsSep "\n"
        (
          map (account: ''$DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${mailDir}/${account.name}'')
            (attrValues config.accounts.email.accounts)
        );
    in
      lib.hm.dag.entryAfter [ "writeBoundary" ] mkAccountDir;


}
