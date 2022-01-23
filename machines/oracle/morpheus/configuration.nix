{ inputs, config, pkgs, ... }: {

  networking.hostName = "morpheus";

  imports = with inputs.self.myRoles; [
    ./hardware-configuration.nix
    base-server
  ];

  my-dns.type = "server";

  sops.secrets.example_key = { };
  sops.secrets.my_new_test_key = { };

  services.boinc.enable = true;
  services.boinc.package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.boinc;

}
