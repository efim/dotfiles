{ inputs, config, pkgs, ... }: {
  networking.hostName = "franzk";

  imports = with inputs.self.myRoles; [
    ./vpsadminos.nix
    base-server
    inputs.htmx-examples.nixosModules.x86_64-linux.price-grid-app
    inputs.htmx-examples.nixosModules.x86_64-linux.order-summary
    inputs.htmx-examples.nixosModules.x86_64-linux.testimonials-grid
    inputs.htmx-examples.nixosModules.x86_64-linux.rock-paper-scissors

    inputs.planning-poker-kazbegi.nixosModules.x86_64-linux.backendApp

    inputs.go-ssr-oauth-attempt.nixosModules.x86_64-linux.auth-pocketbase-attempt
    inputs.some-automoderation.nixosModules.x86_64-linux.some-automoderation-module
  ];

  # environment.systemPackages = [ inputs.htmx-examples.packages.x86_64-linux.price-grid-app ];

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "efim.wool@posteo.net";
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.auth-pocketbase-attempt = {
    enable = true;
    host = "go-ssr-oauth-attempt.sunshine.industries";
    port = 45001;
    useHostTls = true;
  };

  services.some-automoderation = {
    enable = true;
    host = "some-automoderation.sunshine.industries";
    useNginx = true;
    useHostTls = true;
    enablePrometheus = true;
    port = 45002;
    redisPort = 45003;
  };
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
  services."testimonials-grid-section" = {
    enable = true;
    host = "testimonials-grid.frontendmentor.sunshine.industries";
    port = 49013;
  };
  services."rock-paper-scissors" = {
    enable = true;
    host = "rock-paper-scissors.frontendmentor.sunshine.industries";
    port = 49014;
  };

  services.syncthing.enable = true;
  # nginx reverse proxy
  services.nginx = {
    enable = true;
    serverNamesHashBucketSize = 128; # to allow longer domain names
  };
  # yay, even without personal dns, I can have one domain per host with tailscala MagicDNS
  # not perfect, but ok
  services.nginx.virtualHosts."franzk.efim.github.beta.tailscale.net" = {
    locations."/" = { proxyPass = "http://127.0.0.1:8384"; };
  };

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
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${
          toString config.services.gitea.settings.server.HTTP_PORT
        }";
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
    forceSSL = true;
    enableACME = true;
    root = "/var/www/life-expectancy-cal";
  };

  # # most ugly MVP deploy yet:
  # the static front end manually copied to server
  services.nginx.virtualHosts."planning-poker.sunshine.industries" = {
    root = "/var/www/planning-poker-grargh"; # copied manually
  };

  # enables systemd service with backend, and nginx config with websocket
  services."planning-poker-kazbegi-backend" = {
    enable = true;
    host = "planning-poker.sunshine.industries";
    port = 8080;
    useHostTls = true;
  };

  services.grafana = {
    enable = true;
    settings.server.http_port = 44400;
    settings.server.http_addr = "0.0.0.0";
    provision.datasources = {
      settings.datasources = [{
        name = "local-prometheus";
        type = "prometheus";
        url = "http://localhost:${toString config.services.prometheus.port}";
      }];
    };
  };

  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=900s
  '';

  system.stateVersion = "22.05";
}
