import smtplib
from argparse import ArgumentParser

from kombu import Connection, Consumer, Queue

import config
from exchanges import spammer_exchange


class Spammer:

    def __init__(self, name):
        self.name = name
        self.msg = """Dear Friend,

Some interesting information about you was found by the Email Cruncher. Do take a look.

Best,

The Email Cruncher
"""

    def send_email(self, email):
        server = smtplib.SMTP(config.mx_record)
        print(server.ehlo())
        print(server.starttls())
        print(server.login(config.spam_sender, config.spam_password))
        print(server.sendmail(from_addr=config.spam_sender, to_addrs=[email], msg=self.msg))
        server.quit()

    def spam(self, body, message):
        email = "{}@{}".format(self.name, config.spam_domain)
        print('About to spam {}'.format(email))
        try:
            self.send_email(email)
        except Exception as exc:
            print("Got exception {} when sending email, ignoring", exc)
        message.ack()


def main():
    parser = ArgumentParser(description="Spam a particular individual in `spam_domain`")
    parser.add_argument("--name", help='The local name of the email to spam',
                        default='randomuser')
    args = parser.parse_args()

    spammer = Spammer(args.name)
    with Connection(config.get_rabbit_url()) as connection:
        main_queue = Queue(args.name, exchange=spammer_exchange,
                           routing_key=args.name)
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
