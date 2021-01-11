#{ pkgs ? <nixpkgs>, ... }:
with import <nixpkgs> {};

let
  openvpn-resolv = "${update-resolv-conf}/libexec/openvpn/update-resolv-conf";
  ovpn = "${openvpn}/bin/openvpn";
  dinsvpn-opts = " --script-security 2" +
                 " --up ${openvpn-resolv}" +
                 " --down ${openvpn-resolv}" +
                 " --auth-user-pass" +
                 " --auth-retry";
  openvpn-dins = writeShellScriptBin "vpn-dins" ''
    set -eo pipefail
    VPN_CONF=~/.config/openvpn/client.ovpn
    if [ ! -f "$VPN_CONF" ]; then
      echo "$VPN_CONF does not exist" 1>&2
      exit 1
    fi
    sudo ${ovpn} --config $VPN_CONF ${dinsvpn-opts} interact
  '';

in stdenv.mkDerivation {
    name = "openvpn-dins";
    buildInputs = [openvpn-dins];
  }
