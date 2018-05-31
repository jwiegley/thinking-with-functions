{ rev      ? "95b1827682dc30ff1ccffb4f46c197289cea3e1c"
, sha256   ? "0v5s2918a04h6h1m18pzp36l5f41rhkipwqgysamsz7h0q4zwhwz"
, pkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
}:

let
  texFull = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full texdoc latex2e-help-texinfo;
    pkgFilter = pkg:
       pkg.tlType == "run"
    || pkg.tlType == "bin"
    || pkg.pname == "latex2e-help-texinfo";
  };

  ignoredDirs = [
    "html" ".diagrams_cache" "_minted-org-beamer-template"
    "auto" "dist" "svg-inkscape"
  ];

  ignoredSuffixes = [
    ".aux" ".cabal" ".log" ".nav" ".out" ".pdf" ".snm" ".svg"
    ".tex" ".toc" ".upa" ".vrb"
  ];

  filterSource = src: with pkgs.stdenv.lib; cleanSourceWith {
    inherit src;
    filter = path: type:
      let baseName = baseNameOf path; in
      !( type == "directory"
         && builtins.elem baseName ([".git"] ++ ignoredDirs))
      &&
      !( type == "unknown"
         || baseName == "result"
         || any (suf: hasSuffix suf path) ignoredSuffixes);
  };

in pkgs.stdenv.mkDerivation rec {
  name = "org-beamer-template";
  version = "1.0";

  src =
    if pkgs.lib.inNixShell
    then null
    else filterSource ./.;

  buildInputs = [ pkgs.emacsPackagesNg.org texFull ];
  enableParallelBuilding = true;

  buildPhase = "make";
  installPhase = ''
    mkdir -p $out/share/pdf
    cp -p org-beamer-template.pdf $out/share/pdf
  '';

  env = pkgs.buildEnv { name = name; paths = buildInputs; };
}
