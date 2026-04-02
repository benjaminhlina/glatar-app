#!/bin/sh
set -e

# Write env vars to shiny user's Renviron
mkdir -p /etc/R
touch /etc/R/Renviron.site

# Write env vars # cat <<EOF > /home/shiny/.Renviron
cat > /srv/shiny-server/GLATAR-App/.Renviron <<EOF

POSTGRES_HOST=${POSTGRES_HOST}
POSTGRES_USER=${POSTGRES_USER}
POSTGRES_PORT=${POSTGRES_PORT}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
POSTGRES_DB=${POSTGRES_DB}
POSTGRES_SSLMODE=${POSTGRES_SSLMODE}
SHINY_USER=${SHINY_USER}
SHINY_PASSWORD=${SHINY_PASSWORD}
SHINY_PASSWORD=${SHINY_PASSWORD}
SMTP_PASSWORD=${SMTP_PASSWORD}
EOF

#chmod 600 /home/shiny/.Renviron
chown shiny:shiny /srv/shiny-server/GLATAR-App/.Renviron
chmod 600 /srv/shiny-server/GLATAR-App/.Renviron

echo "--- Renviron written ---"
cat /srv/shiny-server/GLATAR-App/.Renviron
# Start Shiny Server
exec shiny-server
