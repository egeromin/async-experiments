"""
Slow version of the email cruncher

Note: this is identical to the regex version, except that it sleeps for
5 seconds per request. Todo: implement a proper alternative!
"""
import re
from time import sleep

from kombu import Connection, Queue, Consumer

from config import get_rabbit_url
from exchanges import email_exchange

email_regex = r"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+"


class EmailCruncherSlow:

    def __init__(self, connection):
        self.producer = connection.Producer()

    def crunch(self, filename, message):
        print("Parsing file {}".format(filename))
        try:
            with open(filename, "r") as fh:
                contents = fh.read()
                emails_found = set(re.findall(email_regex, contents))
                sleep(5)  # 'slow' method
                print("Found emails: {}".format(emails_found))
                self.producer.publish(
                    list(emails_found),
                    exchange='',  # default exchange is a direct exchange
                    routing_key='status'  # send to status Q
                )

        except FileNotFoundError:
            print("File does not exist!")

        message.ack()  # must acknowledge manually!


def main():
    with Connection(get_rabbit_url()) as connection:
        cruncher = EmailCruncherSlow(connection)
        main_queue = Queue('slow-cruncher', exchange=email_exchange)
        with Consumer(
            connection,
            queues=[main_queue],
            callbacks=[cruncher.crunch],
            prefetch_count=1
        ):
            while True:
                connection.drain_events()  # start consuming


if __name__ == "__main__":
    main()
