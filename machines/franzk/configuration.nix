{ inputs, config, pkgs, ... }: {
  networking.hostName = "franzk";

  imports = with inputs.self.myRoles; [
    ./vpsadminos.nix
    base-server
    inputs.htmx-examples.nixosModules.x86_64-linux.price-grid-app
    inputs.htmx-examples.nixosModules.x86_64-linux.order-summary
  ];

  # environment.systemPackages = [ inputs.htmx-examples.packages.x86_64-linux.price-grid-app ];

  services.priceGridService = {
    enable = true;
    host = "price-grid.frontendmentor.sunshine.industries";
    port = 12345;
  };
  services.orderSummaryComponent = {
    enable = true;
    host = "price-summary.frontendmentor.sunshine.industries";
    port = 49012;
  };

  services.syncthing.enable = true;
  # nginx reverse proxy
  services.nginx = {
    enable = true;
    serverNamesHashBucketSize = 128;  # to allow longer domain names
  };
  # yay, even without personal dns, I can have one domain per host with tailscala MagicDNS
  # not perfect, but ok
  services.nginx.virtualHosts."franzk.efim.github.beta.tailscale.net" = {
    locations."/" = { proxyPass = "http://127.0.0.1:8384"; };
  };
  networking.firewall.allowedTCPPorts = [ 80 ];

  age.secrets.secret1.file = ../../secrets/franzk-server-secret.age;

  environment.etc."demo.secret".source = config.age.secrets.secret1.path;

  services.gitea = {
    enable = true;
    settings.service.DISABLE_REGISTRATION = true;
    # disableRegistration = true;
    settings.server = {
      ROOT_URL = "http://git.sunshine.industries/";
      DISABLE_SSH = false;
      SSH_PORT = 65433;
    };
    # ssh.enable = true;
    # ssh.clonePort = 65433;
  };
  services.nginx.virtualHosts."git.sunshine.industries" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.gitea.settings.server.HTTP_PORT}";
    };
  };

  age.secrets.radicale-users = {
    file = ../../secrets/radicale-user.age;
    owner = "radicale";
    mode = "700";
  };

  # too bad I don't have actual dns over tailscale addresses,
  # i'd be very happy if I could add only reverse dns over the name
  # that resolves into an internal ip
  services.radicale = {
    enable = true;
    settings = {
      server = { hosts = [ "0.0.0.0:5232" "[::]:5232" ]; };
      auth = {
        type = "htpasswd";
        htpasswd_filename = config.age.secrets.radicale-users.path;
        htpasswd_encryption = "plain";
      };
      storage = { filesystem_folder = "/var/lib/radicale/collections"; };
    };
  };
  services.nginx.virtualHosts."radicale.sunshine.industries" = {
    locations."/" = { proxyPass = "http://127.0.0.1:5232"; };
  };

  services.nginx.virtualHosts."death-calendar.sunshine.industries" = {
    root = "/var/www/life-expectancy-cal";
  };

  # # most ugly MVP deploy yet:
  services.nginx.virtualHosts."planning-poker.sunshine.industries" = {
    root = "/var/www/planning-poker-grargh"; # copied manually
  };

  services.nginx.virtualHosts."planning-poker.sunshine.industries".locations."/api" =
    {
      proxyPass = "http://127.0.0.1:8080"; # started manually
      extraConfig = ''
        rewrite ^/api/(.*)$ /$1 break;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Add the following lines for WebSocket support
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
      '';
    };

  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=900s
  '';

  time.timeZone = "Europe/Amsterdam";

  system.stateVersion = "22.05";
}
