# include conf.cnf #
sgServer_node=s@192.168.1.11
cookie=ty


# internel
log_writer_cookie=$(cookie)

config:
    @-sed -r -i "s/-(s?)name[\t ]+[\\'a-zA-Z0-9_@\\.]+/-\1name '$(sgServer_node)'/" sgServer/start
    @-sed -r -i "s/-setcookie[ \t]+[\\'a-zA-Z0-9_@\\.]+/-setcookie '$(log_writer_cookie)'/" sgServer/start
    @-sed -r -i "s/-setcookie[ \t]+[\\'a-zA-Z0-9_@\\.]+/-setcookie $(log_writer_cookie)/" sgServer/rel/files/vm.args
    @-sed -r -i "s|\{[ \t]*game_server_node[ \t]*,[ \t]*'.+'}|{game_server_node,'$(sgServer_node)'}}|" sgServer/src/sgServer.app.src
