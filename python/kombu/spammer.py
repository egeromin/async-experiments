from argparse import ArgumentParser

from kombu import Connection, Consumer, Queue

from config import get_rabbit_url, spam_domain
from exchanges import spammer_exchange


class Spammer:

    def __init__(self, name):
        self.name = name

    def spam(self, body, message):
        print('About to spam {}@{}'.format(
            self.name, spam_domain
        ))
        message.ack()


def main():
    parser = ArgumentParser(description="Spam a particular individual in `spam_domain`")
    parser.add_argument("--name", help='The name of the URL to spam',
                        default='egeromin')
    args = parser.parse_args()

    spammer = Spammer(args.name)
    with Connection(get_rabbit_url()) as connection:
        main_queue = Queue(args.name, exchange=spammer_exchange)
        with Consumer(
            connection,
            queues=[main_queue],
            callbacks=[spammer.spam],
            prefetch_count=1
        ):
            while True:
                connection.drain_events()  # start consuming


if __name__ == "__main__":
    main()
