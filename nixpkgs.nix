let
  rev = "ecce29eba2206e4fc8db91c61d5e6272583ece9e";
  sha256 = "0nldd5671702spyr2j8y1hq0s56429rzg1vr2l4sghwdg0sifyq1"; 
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
