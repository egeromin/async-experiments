def get_rabbit_url():
    username = "email_cruncher"
    password = "L0ti3isk83pkSxfXhQ8w"
    vhost = "email_cruncher"

    host = "localhost"
    port = 5779

    url = "amqp://{}:{}@{}:{}/{}".format(
        username, password, host, port, vhost
    )
    return url
