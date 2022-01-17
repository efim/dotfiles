{ inputs, config, pkgs, ... }: {

  networking.hostName = "morpheus";

  imports = with inputs.self.myRoles; [
    ./hardware-configuration.nix
    base-server
  ];

  services.bind = {
    enable = true;
    zones = {
      "nope" = {
        file = ./from-linux-guide.zone;
        master = true;
        slaves = [
          "127.0.0.1"
        ];
      };
    };
    cacheNetworks = [ "100.0.0.0/8" "127.0.0.1" ];
    # this is needed to allow recursive lookup
    # not sure if there's any way to set this up for ip6 yggdrasil addresses without making them public
    # well, this could be import of a nix attrset from gitencrypted dir
    # and if I do "override dns" in tailscale config - my machines use only that dns server, even the phone
    # NOTE : but then my attempts to set "networking.nameservers" is not quite useful
  };
  networking.firewall.allowedUDPPorts = [ 53 ];


  sops.secrets.example_key = { };
  sops.secrets.my_new_test_key = { };

  services.boinc.enable = true;
  services.boinc.package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.boinc;

}
