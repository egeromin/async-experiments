"""
Regex version of the email cruncher
"""
import re

from kombu import Connection, Queue, Consumer

from config import get_rabbit_url
from exchanges import email_exchange

email_regex = r"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+"


def email_cruncher_regex(filename, message):
    print("Parsing file {}".format(filename))
    try:
        with open(filename, "r") as fh:
            contents = fh.read()
            emails_found = set(re.findall(email_regex, contents))
            print("Found emails: {}".format(emails_found))

    except FileNotFoundError:
        print("File does not exist!")

    message.ack()  # must acknowledge manually!


def main():
    with Connection(get_rabbit_url()) as connection:
        main_queue = Queue(exchange=email_exchange)
        with Consumer(
            connection,
            queues=[main_queue],
            callbacks=[email_cruncher_regex],
            prefetch_count=1
        ):
            while True:
                connection.drain_events()  # start consuming


if __name__ == "__main__":
    main()
