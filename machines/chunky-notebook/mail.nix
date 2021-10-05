# copied from https://github.com/junyi-hou/dotfiles/blob/main/modules/mail.nix
{ inputs, pkgs, lib, config, ... }: with inputs;
let
  homeDirectory = "/home/efim"; # TODO get from base flake definition
in
{
  imports = [
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
    }
    {
      programs.afew.enable = true;
      programs.password-store.enable = true;

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
          '' + move-folders + "\n" + move-rules;
    }
    {
      programs.afew.extraConfig = ''
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

    }
    {
      programs.msmtp.enable = true;
    }
  ];

  # # Attempt to make mbsync with oauth, by giving it it's own cyrus_sasl with oauth2 enabled
  # # (if cyrus_sasl is overridden for whole system, lot's of apps seem to rebuild from source)
  #
  # # if want to try again in the future:
  # # add following flake inputs:
  #     gmail-oauth2-tools = {
  #   url = "github:google/gmail-oauth2-tools";
  #   flake = false;
  # };
  # oauth2-lib = {
  #   url = "github:robn/sasl2-oauth";
  #   flake = false;
  # };
  #
  # nixpkgs.overlays = [
  #   (
  #     final: prev:
  #     rec {
  #       gmailOauth2Tools = with prev; stdenv.mkDerivation {
  #         name = "gmail-oauth2-tools";
  #         src = gmail-oauth2-tools;
  #         buildInputs = [ python2 pass oauth2Lib ];
  #         installPhase = ''
  #         mkdir -p $out/bin
  #         cp python/oauth2.py $out/bin/gmail-oauth2-tools
  #         chmod +x $out/bin/gmail-oauth2-tools
  #         # authorization script
  #         cat > $out/bin/gmail-get-token <<EOF
  #         #!${stdenv.shell}
  #         ID=\$(${pass}/bin/pass google-api/id)
  #         SECRET=\$(${pass}/bin/pass google-api/secret)
  #         TOKEN=\$(${pass}/bin/pass google-api/\$1)
  #         ACCOUNT=\$1
  #         $out/bin/gmail-oauth2-tools --user=\$ACCOUNT --client_id=\$ID --client_secret=\$SECRET --refresh_token=\$TOKEN --generate_oauth2_token | awk -F" " '{if(NR==1)print \$3}'
  #         EOF
  #         chmod +x $out/bin/gmail-get-token
  #       '';
  #       };
  #       oauth2Lib = with prev; stdenv.mkDerivation {
  #         name = "sasl2-oauth";
  #         src = oauth2-lib;
  #         nativeBuildInputs = [
  #           autoreconfHook
  #           cyrus_sasl # but is that ok? it might not return the bouncing call 'sasl_with_oauth2' -> 'oauth2Lib' -> 'pre.sasl'
  #           # cyrus_sasl_with_oauth2
  #         ];
  #       };
  #       cyrus_sasl_with_oauth2 = prev.cyrus_sasl.overrideAttrs (div: rec {
  #         postInstall = ''
  #           for lib in ${oauth2Lib}/lib/sasl2/*; do
  #             ln -sf $lib $out/lib/sasl2/
  #           done
  #         '';
  #       });
  #       mbsync_with_oauth2 = prev.isync.override { cyrus_sasl = cyrus_sasl_with_oauth2; };
  #     }
  #   )
  # ];
  #
  # home.packages = [ pkgs.gmailOauth2Tools ];

  programs.mbsync = {
    enable = true;
    # package = pkgs.mbsync_with_oauth2;
  };

  # A service to update the notmuch database for new messages.
  systemd.user.services."update-notmuch-database" = {
    Unit.Description = "Update the notmuch database for new messages";

    Service.Type = "oneshot";
    Service.ExecStart = "${config.home.profileDirectory}/bin/notmuch --config ${config.xdg.configHome}/notmuch/notmuchrc new";
    # for some reason without explisit --config flag error "Error: cannot load config file." showed up
    Service.WorkingDirectory = "${config.accounts.email.maildirBasePath}/.notmuch";
  };

  # Copied from here: https://github.com/iamthememory/dotfiles/blob/5d463625e08477db8919b12b1dc15e2426121b2c/home/mail/default.nix
  # Run the service to update the notmuch database every two minutes.
  systemd.user.timers."update-notmuch-database" = {
    Unit.Description =
      config.systemd.user.services."update-notmuch-database".Unit.Description;

    Timer.OnCalendar = "*:0/2";
    Timer.RandomizedDelaySec = 30;

    Install.WantedBy = [
      "timers.target"
    ];
  };

  services = {
    mbsync = {
      enable = true;
      # sync every 5 minutes, but alerts can be less frequent
      frequency = "*:0/5";
    };
  };

  accounts.email = let
    mailDir = "${homeDirectory}/.mail";
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
            # extraConfig.account = {
            #   AuthMechs = "XOAUTH2";
            #   PipelineDepth = 1;
            #   TimeOut = 60;
            # };
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
            extraConfig = {
              auth = "oauthbearer";
            };
          };
          notmuch.enable = true;
          # passwordCommand = "${pkgs.gmailOauth2Tools}/bin/gmail-get-token efim.wool@gmail.com";

          passwordCommand = "${pkgs.pass}/bin/pass  email/efim.wool@gmail.com";

          primary = true;
        };
      };
    };

  home.activation.initMailDir =
    let
      mailDir = "${homeDirectory}/.mail";
      mkAccountDir = with builtins; concatStringsSep "\n"
        (
          map (account: ''$DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${mailDir}/${account.name}'')
            (attrValues config.accounts.email.accounts)
        );
    in
      lib.hm.dag.entryAfter [ "writeBoundary" ] mkAccountDir;

}
