# cgi-bin

Install app cgi-bin in web server's cgi-bin (see `/etc/apache2/conf-available/serve-cgi-bin.conf`)

# css

Configure directories for css that matches the css dir specified by the app's `ApplicationConfiguration`.

Do this via a conf file in `/etc/apache2/conf-enabled/`
that says, e.g.

    Alias /style/ /var/www/style/

Here, `/style` should be the css path set by the app's
`ApplicationConfiguration`.

# icons

Install the wilde-icons (`/web/icons`) into a directory
that is in line with the apache configuration.

Conf apache via a conf file in `/etc/apache2/conf-enabled/`
that says, e.g.

    Alias /icon/ /var/www/icon/
