"""
Prompt until the user enters quit
"""
from kombu import Connection

from config import get_rabbit_url
from exchanges import email_exchange


def main():
    with Connection(get_rabbit_url()) as connection:
        producer = connection.Producer()
        prompt(producer)


def prompt(producer):
    filename = input("> ")

    while filename != "quit":
        producer.publish(filename, exchange=email_exchange,
                         declare=[email_exchange], retry=True)
        filename = input("> ")

    print("Quitting")


if __name__ == "__main__":
    main()
