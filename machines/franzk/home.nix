{ inputs, config, lib, pkgs, ... }:

{

  imports =  [
    inputs.self.myModules.mail-server
    inputs.self.myProfiles.personal
  ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "efim";
  home.homeDirectory = "/home/efim";

  programs.git.enable = true;

  programs.bash.enable = true;

}
