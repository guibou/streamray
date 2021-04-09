let
  rev = "4795e7f";
  sha256 = "03xqwcli1cw96x5d21siq9ph9g37igp35fv9rgsl3wv4gvm8y48c";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
