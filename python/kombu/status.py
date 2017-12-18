from kombu import Connection, Consumer, Queue

from config import get_rabbit_url, spam_domain


class EmailStatus:

    def __init__(self, connection):
        self.spammable_emails_found = set()
        self.producer = connection.Producer()

    def find_new_spammable_emails(self, emails):
        emails_split = [email.split('@') for email in emails]
        spammable_emails = [name for name, domain in emails_split
                            if domain == spam_domain]
        new_spammable_emails = set(spammable_emails) - self.spammable_emails_found
        self.spammable_emails_found.update(new_spammable_emails)
        return new_spammable_emails

    def spam(self, spammable_email):
        self.producer.publish('spam me!', routing_key=spammable_email)

    def update(self, emails, message):
        new_spammable_emails = self.find_new_spammable_emails(emails)
        print("New emails: {}".format(new_spammable_emails))
        for email in new_spammable_emails:
            self.spam(email)  # send on to spammer exchange

        message.ack()


def main():
    with Connection(get_rabbit_url()) as connection:
        status_queue = Queue('status', exchange='')
        email_status = EmailStatus(connection)
        with Consumer(
            connection,
            queues=[status_queue],
            callbacks=[email_status.update],
            prefetch_count=1
        ):
            while True:
                connection.drain_events()  # start consuming


if __name__ == "__main__":
    main()
