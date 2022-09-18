{ inputs, config, ... }:
{
  homeage = {
      # Absolute path to identity (created not through home-manager)
      identityPaths = [ "~/.ssh/id_ed25519_for_age" ];

      # "activation" if system doesn't support systemd
      installationType = "systemd";

      file."email-pass" = {
        # Path to encrypted file tracked by the git repository
        source = ../../secrets/email-pass.age;
        symlinks = [ "${config.xdg.configHome}/secrets/email/efim.wool@gmail.com" ];
      };

      # from guide on home-manager integration
      # https://github.com/jordanisaacs/homeage#nix-flakes
      # file."pijulsecretkey" = {
      #   # Path to encrypted file tracked by the git repository
      #   source = ../../secrets/secret1.age;
      #   symlinks = [ "${config.xdg.configHome}/pijul/secretkey.json" ];
      #   copies = [ "${config.xdg.configHome}/no-symlink-support/secretkey.json" ];
      # };
  };

  imports = [ inputs.homeage.homeManagerModules.homeage ];
}
