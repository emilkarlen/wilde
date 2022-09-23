# Enable CGI

In `/etc/apache2`:

`mods-enabled` should have a symlink to `mods-available/cgid.*`.
Install this file into /etc/apache2/conf-available/ and symlink to from 
/etc/apache2/conf-enabled/

# Paths for css and icons
#
# Must match value set in Wilde/ApplicationConfiguration

    Alias /style/ /var/www/style/
    Alias /icon/ /var/www/icon/
