{ config, lib, pkgs, ... }:

{

  # declaration of gratitude to https://christine.website/blog/prometheus-grafana-loki-nixos-2020-11-20
  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    scrapeConfigs = [
      {
        job_name = config.networking.hostName;
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
        }];
      }
    ];
  };

  networking.firewall.allowedTCPPorts = [ 9001 ];

}
