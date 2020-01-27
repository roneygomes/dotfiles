PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

function pgadmin {
    docker run -p 9999:80 \
        -e 'PGADMIN_DEFAULT_EMAIL=roney477@gmail.com' \
        -e 'PGADMIN_DEFAULT_PASSWORD=1234' \
        -v $HOME/.pgadmin:/var/lib/pgadmin \
        -d dpage/pgadmin4;

    sleep 1
    open http://localhost:9999
}
