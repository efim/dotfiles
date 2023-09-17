# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# pre-flakes nixos centralized config
{ inputs, rev, config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      inputs.home-manager.nixosModules.home-manager # ? initializing home-manager flake integration ?
      ./home.nix                                    # importing pre-flakes centralized home-manager configuration
      ({ pkgs, ... }: {
          # Let 'nixos-version --json' know about the Git revision
          # of this flake.
          system.configurationRevision = inputs.nixpkgs.lib.mkIf (rev != null) rev;
        })
    ];

  boot.kernelPackages = pkgs.linuxKernel.packages.linux_5_15; # not sure why would I really want to pin that?

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 15;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # ^ from https://serverfault.com/questions/997055/nixos-rebuild-switch-fails-with-no-space-left-on-device
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub.useOSProber = true;


  # from researching remote deployment to arm systems
  # https://sgt.hootr.club/molten-matter/nixops-on-the-pi/
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.trusted-users = [ "root" "efim" ];
  };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.hostName = "chunky";
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  networking.networkmanager.enable = true;

  services.yggdrasil.enable = true;
  services.yggdrasil.persistentKeys = true;
  services.yggdrasil.settings = {
    Peers = [
      "tcp://130.61.94.233:13338" #niobe
    ];
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services = {
    # Enable the GNOME 3 Desktop Environment.
    xserver = {
      enable = true;
      # there's also `displaylink` - with manual actions
      # videoDrivers = [ "nvidia" ];
      # Configure keymap in X11
      layout = "us,ru";
      xkbOptions = "grp:caps_toggle";
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.mutableUsers = false;
  users.users.efim = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
    # A hashed password can be generated using mkpasswd -m sha-512. Or root can set with `passwd`
    hashedPassword = "$6$cfSalLM6exqFaA7x$BjOLrT1bGxps3GDXIUOP/5NSVz3OSEPqxmwiILGK5n455Yq4yvwVwYe0vljREHsXgDIG/4p2d4RvdhFo3cKub.";
    # passwordFile = ""; # TODO when I recover my chunky, fix this
  };

  services.localtimed.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
   environment.systemPackages = with pkgs; [
     wget vim
     firefox
     mkpasswd
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    # enableSSHSupport = true;
    # pinentryFlavor = "gtk2";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs = {
    ssh.startAgent = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  fonts.fonts = with pkgs; [
    source-code-pro
    iosevka
    etBook
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

