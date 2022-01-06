{ inputs, config, pkgs, ... }: {

  networking.hostName = "morpheus";

  imports = with inputs.self.myRoles; [
    ./hardware-configuration.nix
    base-server
  ];

  sops.secrets.example_key = { };
  sops.secrets.my_new_test_key = { };

}
