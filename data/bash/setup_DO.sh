#!/bin/bash
# first dump files 

ssh root@45.55.144.247

sudo apt remove $(dpkg --get-selections docker.io docker-compose docker-compose-v2 docker-doc podman-docker containerd runc | cut -f1)

# Add Docker's official GPG key:
sudo apt update
sudo apt install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
sudo tee /etc/apt/sources.list.d/docker.sources <<EOF
Types: deb
URIs: https://download.docker.com/linux/ubuntu
Suites: $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}")
Components: stable
Signed-By: /etc/apt/keyrings/docker.asc
EOF

sudo apt update

sudo apt install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-pluginY
sudo systemctl status docker
sudo systemctl start docker
sudo docker run hello-world

# Add the PostgreSQL apt repo
sudo apt install -y curl ca-certificates
sudo install -d /usr/share/postgresql-common/pgdg
curl -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc --fail https://www.postgresql.org/media/keys/ACCC4CF8.asc

sudo sh -c 'echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'

sudo apt update
sudo apt install postgresql-client-18



PGPASSWORD=AVNS_biGDnossu2EFhlFGwo3 psql -U doadmin \
 -h db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com \
 -p 25060 \
 -d GLATARdb --set=sslmode=require




scp NAAEDdb_backup_2_March_2026.dump root@45.55.144.247:~




PGPASSWORD=AVNS_biGDnossu2EFhlFGwo3 pg_restore -v \
   -U doadmin \
   -h private-db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com\
   -p 25060 \
   -d NAAEDdb \
     NAAEDdb_backup_2_March_2026.dump
AVNS_biGDnossu2EFhlFGwo3

PGPASSWORD=AVNS_biGDnossu2EFhlFGwo3 psql -U doadmin \
 -h db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com \
 -p 25060 

 ssh-keygen -t ed25519 -C "github-actions-deploy" -f ~/.ssh/gh_actions_deploy

 ssh-copy-id -i ~/.ssh/gh_actions_deploy.pub root@45.55.144.247
 cat ~/.ssh/gh_actions_deploy

 ssh -i /Users/benhlina/.ssh/gh_actions_deploy 'root@45.55.144.247'

sudo apt update
sudo apt install nginx

sudo ufw allow OpenSSH
sudo ufw enable

sudo ufw app list
sudo ufw allow 'Nginx HTTP'
sudo ufw allow 'Nginx HTTPS'
sudo ufw status

 sudo systemctl status nginx    
 curl -I http://localhost
 sudo ss -lntp | grep -E '(:80\s|:80$)'

 sudo systemctl stop nginx
sudo systemctl disable nginx
sudo ss -lntp | grep ':80'
docker compose up -d
docker compose ps

# 1. Copy your existing certs to the new server (don't request new ones yet)
/etc/letsencrypt/   # copy this entire directory

# 2. Update DNS A record → new server IP
# Wait for propagation before doing anything else

# 3. Verify DNS has propagated
dig glatar.org+short

# 4. Start your stack on the new server
docker compose up -d

# 5. Test renewal works on the new server
certbot renew --dry-run
ls /opt/glatar/certbot/conf/

curl -4 ifconfig.me

cd /opt/glatar
docker compose up -d nginx

docker compose run --rm certbot certonly --webroot \
  -w /var/www/certbot \
  -d glatar.org \
  -d www.glatar.org \
  -d glatar.com \
  -d www.glatar.com \
  --email benjamin.hlina@gmail.com \
  --agree-tos --no-eff-email
  docker compose run --rm certbot certificates

  docker exec glatar-nginx-1 cat /var/log/letsencrypt/letsencrypt.log 2>/dev/null \
  || docker compose logs certbot

  curl -v http://glatar.org/.well-known/acme-challenge/test

docker compose exec nginx cat /etc/nginx/conf.d/default.conf | grep -A5 'well-known'

  watch -n 30 "dig glatar.org +short"
  curl -v http://glatar.org/.well-known/acme-challenge/test

  cd /opt/glatar
  docker compose restart nginx
docker compose ps
docker logs glatar-nginx-1

docker compose run --rm certbot certonly --webroot \
  -w /var/www/certbot \
  -d glatar.org \
  -d www.glatar.org \
  -d glatar.com \
  -d www.glatar.com \
  --email benjamin.hlina@gmail.com \
  --agree-tos --no-eff-email

docker compose run --rm certbot certonly --webroot \
    -w /var/www/certbot \
    -d glatar.org \
    -d www.glatar.org \
    -d glatar.com \
    -d www.glatar.com \
    --email benjamin.hlina@gmail.com \
    --agree-tos --no-eff-email

    docker compose run --rm certbot certificates

    docker compose run --rm certbot renew
docker compose run --rm certbot certonly --force-renewal

    curl http://glatar.org

certbot renew --webroot -w /var/www/certbot;

docker compose run --rm --entrypoint certbot certbot \
certonly \
--webroot -w /var/www/certbot \
-d glatar.org \
-d www.glatar.org \
--email benjamin.hlina@gmail.com \
--agree-tos --no-eff-email

docker compose run --rm --entrypoint certbot certbot cat /var/log/letsencrypt/letsencrypt.log

dig glatar.org +short
dig glatar.com +short
dig www.glatar.org +short
dig www.glatar.com +short

docker compose exec -it glatar-shiny-1 bash

docker compose exec shiny_1 bash

docker compose logs glatar-shiny-1
ls /var/log/shiny-server/GLATAR-App/
tail -n 50 *.log

ssh -N -T \
  -L  25060:private-db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com\
  -i  ~/.ssh/gh_actions_deploy \
  root@45.55.144.247

  ssh -N -T \
  -L 25060:private-db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com:25060 \
  -i ~/.ssh/gh_actions_deploy \
  root@45.55.144.247

  ssh -N -T \
  -L 5434:private-db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com:25060 \
  -i ~/.ssh/gh_actions_deploy \
  root@45.55.144.247

  data.frame(
    dbname = Sys.getenv("POSTGRES_DB"),
                      host = Sys.getenv("POSTGRES_HOST"),
                      port = Sys.getenv("POSTGRES_PORT"),
                      user = Sys.getenv("POSTGRES_USER"),
                      password = Sys.getenv("POSTGRES_PASSWORD"), 
                      shiny_user =  Sys.getenv("SHINY_USER"), 
                      shiny_password = Sys.getenv("SHINY_PASSWORD")
  )


best="" 
best_ts=0

for i in 1 2 3; do
  out=$(timeout 2 docker exec glatar-shiny-$i sh -c '
    f=$(ls -t /var/log/shiny-server/GLATAR-App-shiny-*.log 2>/dev/null | head -n 1)
    if [ -n "$f" ]; then
      date -r "$f" "+%s"
    fi
  ')

  if [ -n "$out" ] && [ "$out" -gt "$best_ts" ]; then
    best_ts=$out
    best="glatar-shiny-$i"
  fi
done
echo "👉 Most active: $best"  

docker exec -it $best sh
  

  docker exec -it $best R -e "shiny::runApp('/srv/shiny-server/GLATAR-App', port=3838, host='0.0.0.0')"
  # Inside the container, add to /etc/environment

docker exec -it $best sh -c "
  latest_log=\$(ls -t /var/log/shiny-server/GLATAR-App-shiny-*.log | head -n 1) && \
  cat \$latest_log | tail -n 200"

docker exec -it $best ls -l /srv/shiny-server/GLATAR-App  
docker ps -a
docker stats $best --no-stream

docker ps
# Hit the app directly from inside the container
curl -I http://localhost/GLATAR-App/
curl -Ik https://localhost/GLATAR-App/

# Terminal 1 - watch logs
docker exec -it glatar-shiny-3 sh -c "tail -n 200 -f /var/log/shiny-server/GLATAR-App-shiny-*.log"
docker network ls
docker inspect glatar-nginx-1 | grep -A 20 "Networks"
docker inspect glatar-shiny-1 | grep -A 20 "Networks"
docker exec -it $best sh

docker run -d \
  -e POSTGRES_DB=GLATARdb \
  -e POSTGRES_HOST=private-db-postgresql-nyc3-95449-do-user-33608798-0.k.db.ondigitalocean.com \
  -e POSTGRES_PORT=25060 \
  -e POSTGRES_USER=doadmin \
  -e POSTGRES_PASSWORD=AVNS_biGDnossu2EFhlFGwo3 \
  -e POSTGRES_SSLMODE=require \
  -p 3838:3838 \
  ghcr.io/benjaminhlina/glatar-app:latest 

docker exec -it glatar-shiny-1 env | grep POSTGRES

env | grep POSTGRES 

getent passwd shiny
# Look for the home dir field, e.g.: shiny:x:999:999::/home/shiny:/bin/sh