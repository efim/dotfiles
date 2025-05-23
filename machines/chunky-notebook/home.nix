# pre-flakes centralized home-manager config
#   imports base - common with office-machine
#   sets my-options
{ inputs, config, pkgs, ... }:
{
  home-manager.useUserPackages = true; # needed to use fonts from User declaration
  services.tailscale.enable = true;

  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.settings.dns_enabled = true;
      # For Nixos version > 22.11
      #defaultNetwork.settings = {
      #  dns_enabled = true;
      #};
    };
  };

  networking.firewall = {
    enable = true;
    allowedTCPPortRanges = [
      { from = 1714; to = 1764; } # KDE Connect
    ];
    allowedUDPPortRanges = [
      { from = 1714; to = 1764; } # KDE Connect
    ];
  };

  home-manager.users.efim = {
    # to add `inputs` as call attribute to imported modules
    _module.args.inputs = inputs;

    imports = with inputs.self.myModules; with inputs.self.myProfiles; [
      inputs.self.myRoles.hm-common
      xmonad # moving these imports to `hm-common` results in infinite recursion.. somehow
      my-autorandr
      my-emacs
      my-screen-locker
      mu4e-mail-client
      fonts
      with-deploy
    ];

    programs.git = {
      userName = "efim";
      userEmail = "efim.wool@gmail.com";
    };

    services.kdeconnect = {
      enable = true;
      indicator = true;
    };

    my-screen-locker.isNixManaged = true;

    my-autorandr = {
      # laptop display
      display1 = {
        name = "DP-0";
        fp = "00ffffffffffff0030e47e0500000000001a010495221378eadc95a35855a0260d5054000000010101010101010101010101010101012e3680a070381f403020350058c21000001a2e3680a070381f403020350058c21000001a00000000000000000000000000000000000000000002000833ff0a3c961e163696000000001c";
      };
      # samsung via VGA
      display2 = {
        name = "DP-1";
        fp = "00ffffffffffff004c2d23053232524c34130104a5301b78223581a656489a241250542308008100814081809500a940b30001010101023a801871382d40582c4500dd0c1100001e000000fd00383c1e5111000a202020202020000000fc0053796e634d61737465720a2020000000ff00484c4a534330333431300a202000f2";
      };
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "21.03";
  };

}
