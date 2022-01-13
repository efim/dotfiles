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
        # file = ./dns-attempt.zone;
        master = true;
        slaves = [
          "127.0.0.1"
        ];
      };
    };
  };
  networking.firewall.allowedUDPPorts = [ 53 ];


  sops.secrets.example_key = { };
  sops.secrets.my_new_test_key = { };

  services.boinc.enable = true;
  services.boinc.package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.boinc;

}
