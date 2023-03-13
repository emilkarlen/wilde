Programs that serves as demonstrations of how to use different parts of the
library.

# Video application

A simple database for videos and recordings on different media.

## Application Tool

`video-tool` is a tool for generating database DDL etc.

For help:

    $ cabal run video-tool help

## Standalone web server

`video-web-server` is a standalone web server.

To show help:

    $ cabal run video-web-server -- --help

To show builtin defaults:

    $ cabal run video-web-server -- --dump-defaults

To run with configuration from file:

    $ cabal run video-web-server -- --config-file video-web-server.yaml
