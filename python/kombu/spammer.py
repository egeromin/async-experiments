import smtplib
from argparse import ArgumentParser
import dns.resolver

from kombu import Connection, Consumer, Queue

from config import get_rabbit_url, spam_domain, spam_sender
from exchanges import spammer_exchange


class Spammer:

    def __init__(self, name):
        self.name = name
        self.find_mx_record()
        self.msg = """Dear Colleague,
        
Some interesting information about you was found by the Email Cruncher. Do take a look.

Best,

The Email Cruncher
"""

    def find_mx_record(self):
        resolver = dns.resolver.Resolver()
        records = resolver.query(spam_domain, 'MX')
        mx_record = records[0].exchange
        self.mx_record = str(mx_record).strip().lower()
        print("Set MX record {}".format(self.mx_record))

    def send_email(self, email):
        server = smtplib.SMTP(self.mx_record)
        print(server.ehlo('emanuelgeromin.com'))
        print(server.sendmail(spam_sender, [email], self.msg))
        server.quit()

    def spam(self, body, message):
        email = "{}@{}".format(self.name, spam_domain)
        print('About to spam {}'.format(email))
        try:
            self.send_email(email)
        except Exception as exc:
            print("Got exception {} when sending email, ignoring", exc)
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
