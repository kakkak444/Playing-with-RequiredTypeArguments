{
	pkgs ? import <nixpkgs> {},
}:

let
	hpkgs = pkgs.haskell.packages.ghc910;

	depends = [
		"type-natural"
		"equational-reasoning"
		"ghc-typelits-natnormalise"
		"ghc-typelits-knownnat"
	];
in
	pkgs.mkShell {
		buildInputs = with hpkgs; [
			ghc
			cabal-install
			haskell-language-server
		];

		shellHook = ''
			alias repl="cabal repl --build-depends '${pkgs.lib.strings.concatStringsSep ", " depends}'"
		'';
	}

