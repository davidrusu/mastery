FROM nixos/nix:1.9
RUN nix-channel --add http://nixos.org/releases/nixpkgs/nixpkgs-15.06pre63766.8e56452/ dev
RUN nix-channel --update
RUN             