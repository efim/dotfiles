{ lib, fetchFromGitHub }:

fetchFromGitHub rec {
  rev = "7ae68c5da1c379fec062735cd702473fe3fb11f6";
  name = "playfair-display-${builtins.substring 0 6 rev}";
  owner = "clauseggers";
  repo = "Playfair-Display";
  sha256 = "KhI7VE3S/BhQWdbO9hofp7UIlUMXAPGG/Ay7g5MRcmE=";

  postFetch = ''
    tar -xf $downloadedFile
    mkdir -p $out/share/fonts/truetype
    cp */fonts/TTF/*.ttf $out/share/fonts/truetype
  '';

  meta = with lib; {
    description = "An Open Source typeface family for display and titling use";
    license = licenses.ofl;
    platforms = platforms.all;
  };
}
