.PHONY: nix_files shell

# Generate .nix files from a .cabal file.
nix_files:
	cabal2nix ./. > default.nix
	cabal2nix --shell ./. > shell.nix

shell:
	nix-shell
