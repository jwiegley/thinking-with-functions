{ rev      ? "4477cf04b6779a537cdb5f0bd3dd30e75aeb4a3b"
, sha256   ? "1i39wsfwkvj9yryj8di3jibpdg3b3j86ych7s9rb6z79k08yaaxc"
, pkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
}:

let
  org = pkgs.stdenv.mkDerivation rec {
    name = "emacs-org-${version}";
    version = "20160421";
    src = pkgs.fetchFromGitHub {
      owner  = "jwiegley";
      repo   = "org-mode";
      rev    = "db5257389231bd49e92e2bc66713ac71b0435eec";
      sha256 = "073cmwgxga14r4ykbgp8w0gjp1wqajmlk6qv9qfnrafgpxic366m";
    };
    preBuild = ''
      rm -f contrib/lisp/org-jira.el
      makeFlagsArray=(
        prefix="$out/share"
        ORG_ADD_CONTRIB="org* ox*"
      );
    '';
    preInstall = ''
      perl -i -pe "s%/usr/share%$out%;" local.mk
    '';
    buildInputs = [ pkgs.emacs26 ] ++ (with pkgs; [ texinfo perl which ]);
    meta = {
      homepage = "https://elpa.gnu.org/packages/org.html";
      license = pkgs.stdenv.lib.licenses.free;
    };
  };

  texFull = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full texdoc latex2e-help-texinfo;
    pkgFilter = pkg:
       pkg.tlType == "run"
    || pkg.tlType == "bin"
    || pkg.pname == "latex2e-help-texinfo";
  };

  ignoredDirs = [
    "html" "_minted-denotational-design"
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
in rec {
  org-support-code =
    pkgs.haskellPackages.callCabal2nix "org-support-code"
      (with pkgs.stdenv.lib; cleanSourceWith {
         src = ./.;
         filter = path: type:
           let baseName = baseNameOf path; in
           builtins.elem baseName [
             "Main.hs"
             "package.yaml"
           ];
       }) {};

  denotational-design = pkgs.stdenv.mkDerivation rec {
    name = "denotational-design";
    version = "1.0";

    src =
      if pkgs.lib.inNixShell
      then null
      else filterSource ./.;

    buildInputs = [
      org-support-code
      org
      texFull
      pkgs.fontconfig
      pkgs.liberation_ttf
      pkgs.bash
      pkgs.jdk8
      pkgs.plantuml
      pkgs.ditaa
      pkgs.emacs26
      pkgs.python27Packages.pygments
      pkgs.inkscape.out
      pkgs.which
    ];

    patchPhase = ''
      substituteInPlace java \
        --replace "/run/current-system/sw/bin/java" \
                  "${pkgs.jdk8}/bin/java"
      substituteInPlace java \
        --replace "/bin/bash" \
                  "${pkgs.bash}/bin/bash"
      substituteInPlace support.el \
        --replace "/run/current-system/sw/lib/plantuml.jar" \
                  "${pkgs.plantuml}/lib/plantuml.jar"
      substituteInPlace support.el \
        --replace "/run/current-system/sw/lib/ditaa.jar" \
                  "${pkgs.ditaa}/lib/ditaa.jar"
    '';

    preConfigure = ''
      export HOME=$NIX_BUILD_TOP

      mkdir chroot-fontconfig
      cat ${pkgs.fontconfig.out}/etc/fonts/fonts.conf > chroot-fontconfig/fonts.conf
      sed -e 's@</fontconfig>@@' -i chroot-fontconfig/fonts.conf
      echo "<dir>${pkgs.liberation_ttf}</dir>" >> chroot-fontconfig/fonts.conf
      echo "</fontconfig>" >> chroot-fontconfig/fonts.conf

      export FONTCONFIG_FILE=$(pwd)/chroot-fontconfig/fonts.conf
    '';

    buildPhase = with pkgs.emacs26PackagesNg; ''
      export PATH=$PATH:${pkgs.python27Packages.pygments}/bin
      make EMACS_ARGS="-L ${org}/share/emacs/site-lisp/org \
                       -L ${haskell-mode}/share/emacs/site-lisp/elpa/$(echo ${haskell-mode.name} | sed 's/^emacs-//')"
    '';
    installPhase = ''
      mkdir -p $out/share/pdf
      cp -p denotational-design.pdf $out/share/pdf
    '';

    env = pkgs.buildEnv { name = name; paths = buildInputs; };
  };
}.denotational-design
