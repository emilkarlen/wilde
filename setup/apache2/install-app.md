Conf of a Wilde app for Apache2 on GNU Linux/Debian

# cgi-bin

Install app executable in web server's cgi-bin/
(see `/etc/apache2/conf-available/serve-cgi-bin.conf`)

# css

The apps path to css file specified in its
`ApplicationConfiguration` must match
a path specified by Apaches config:

    Alias /style/ /var/www/style/

There should be such a file in this dir.

# html start page

Install in servers root: `/var/www/html/`.

See file in `/etc/apache2/conf-enabled` for details.
