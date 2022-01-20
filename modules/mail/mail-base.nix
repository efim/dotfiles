# copied from https://github.com/junyi-hou/dotfiles/blob/main/modules/mail.nix
{ config, lib, pkgs, ... }:

let
  notmuchConfig = "${config.xdg.configHome}/notmuch/notmuchrc";
in {

  programs.notmuch = {
    enable = true;
    maildir.synchronizeFlags = false; # to optimize muchsync
    # since flags are stored by modigying file name. Stars and Read will not get propagated to gmail
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

  # this might save me from needing to specify path to config
  home.activation.fixNotmuchConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln $VERBOSE_ARG -sf ~/.config/notmuch/notmuchrc ~/.notmuch-config
  '';

  programs.afew.enable = true;
  # programs.password-store.enable = true;
  # programs.password-store.settings.PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";

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
