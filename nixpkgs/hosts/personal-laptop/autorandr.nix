# User configuration for autorandr
# helped by [[https://github.com/dmarcoux/dotfiles-nixos/blob/d784c35f0b2468e1801bf60fde12211eef5485ba/hosts/laptop-work/autorandr.nix]]
# and [[https://github.com/nix-community/home-manager/blob/ef4370bedc9e196aa4ca8192c6ceaf34dca589ee/tests/modules/programs/autorandr/basic-configuration.nix]]
#
# Custom configurations:
# [ both primary external ]
# Default configurations:
# [ horizontal vertical common clone-largest ]

# TODO maybe expose these as module options?
# TODO - autorandr service has to be enabled on system level.
#         in nixos it is in 'base-config.nix'
#         but in Ubuntu, is it enought to install 'autorandr' from apt?
let
  eDP-1-fingerprint = "00ffffffffffff0030e47e0500000000001a010495221378eadc95a35855a0260d5054000000010101010101010101010101010101012e3680a070381f403020350058c21000001a2e3680a070381f403020350058c21000001a00000000000000000000000000000000000000000002000833ff0a3c961e163696000000001c";
  DP-3-fingerprint = "00ffffffffffff004c2d23053232524c34130104a5301b78223581a656489a241250542308008100814081809500a940b30001010101023a801871382d40582c4500dd0c1100001e000000fd00383c1e5111000a202020202020000000fc0053796e634d61737465720a2020000000ff00484c4a534330333431300a202000f2";
in {
  # If no monitor layout can be detected, fallback to the primary profile (it's defined below)
  programs.autorandr = {
    profiles = {
      both = {
        config = {
          "eDP-1" = {
            enable = true;
            mode = "1920x1080";
            primary = true;
            position = "0x0";
            rate = "60.00";
            crtc = 1;
          };
          "DP-3" = {
            enable = true;
            mode = "1920x1080";
            position = "1920x0";
            rate = "60.00";
            crtc = 0;
          };
          # "DP-1".enable = false;
          # "DP-2".enable = false;
          # "HDMI-2".enable = false;
        };
        fingerprint = {
          "DP-3" = DP-3-fingerprint;
          "eDP-1" = eDP-1-fingerprint;
        };
      };
      external = {
        config = {
          "DP-3" = {
            enable = true;
            mode = "1920x1080";
            position = "0x0";
            primary = true;
            rate = "60.00";
            crtc = 0;
          };
          # "eDP-1".enable = false;
          # "DP-1".enable = false;
          # "DP-2".enable = false;
          # "HDMI-2".enable = false;
        };
        fingerprint = {
          "DP-3" = DP-3-fingerprint;
          "eDP-1" = eDP-1-fingerprint;
        };
      };
      primary = {
        config = {
          "eDP-1" = {
            enable = true;
            mode = "1920x1080";
            position = "0x0";
            primary = true;
            rate = "60.00";
            crtc = 0;
          };
        #   "DP-1".enable = false;
        #   "DP-2".enable = false;
        #   "DP-3".enable = false;
        #   "HDMI-1".enable = false;
        #   "HDMI-2".enable = false;
        };
        fingerprint = {
          "eDP-1" = eDP-1-fingerprint;
        };
      };
    };
  };
}
