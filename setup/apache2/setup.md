Config of Apache on GNU Linux/Debian

# Enable CGI

In `/etc/apache2`:

`mods-enabled` should have a symlink to

  - `mods-available/cgi.load`, or
  - `mods-available/cgid.*`

Install this file into `/etc/apache2/conf-available/` and symlink to from 
`/etc/apache2/conf-enabled/`.

# Install Apache config for paths

There should be such a config file in this dir.
This file should be installed in `/etc/apache2/conf-available`.
Enable the conf with a symlink in `/etc/apache2/conf-enabled`
to the config file.

Make sure the directories exist.

# Install Wilde standard icons

Install the wilde-icons (`/web/icons`) into a directory
that is in line with the apache configuration.

There should be such a config file in this dir.

The paths specified there must match the apps paths spedified in its
`Wilde/ApplicationConstruction`.

# Restart Apache

    $ sudo systemctl restart apache2
