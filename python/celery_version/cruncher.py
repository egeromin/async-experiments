import celery
from celery import chain, group
import re
import os

from kombu import Queue, Exchange

import config
import dns.resolver
import smtplib


spammer_queue_name = os.getenv("SPAMMER_QUEUE", 'spammer')

# set up celery
# set up queues
app = celery.Celery('cruncher', broker=config.get_rabbit_url())
app.conf.task_queues = {
    Queue('crunch', routing_key='crunch'),  # to default exchange
    Queue('status', routing_key='status'),  # to default exchange
    Queue('trigger', routing_key='trigger'),  # to default exchange
    Queue(spammer_queue_name, routing_key=spammer_queue_name,
          exchange=Exchange('spammer_exchange', type='direct'))
}
# set up routes to make sure each task gets routed to the right queue
app.conf.task_routes = {
    'cruncher.crunch_regex': {'queue': 'crunch', 'routing_key': 'crunch'},
    'cruncher.find_new_spammable_emails':
        {'queue': 'status', 'routing_key': 'status'},
    'cruncher.trigger_spammers':
        {'queue': 'trigger', 'routing_key': 'trigger'},
    'cruncher.spam':
        {'queue': spammer_queue_name,
         'routing_key': spammer_queue_name}
}


email_regex = r"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+"


@app.task
def crunch_regex(filename):
    """Find emails by regex"""
    print("Parsing file {}".format(filename))
    try:
        with open(filename, "r") as fh:
            contents = fh.read()
            emails_found = set(re.findall(email_regex, contents))
            print("Found emails: {}".format(emails_found))
            return list(emails_found)  # return a list, needs to be JSON serializable

    except FileNotFoundError:
        print("File does not exist!")
        raise


spammable_emails_found = set()
# if we restrict ourselves to 1 worker process with
# concurrency=1, then we can reliably store the info
# in a global variable


@app.task
def find_new_spammable_emails(emails):
    """Find new spammable emails not yet stored in
    the global set"""
    print("Got emails {}".format(emails))
    emails_split = [email.split('@') for email in emails]
    spammable_emails = [name for name, domain in emails_split
                        if domain == config.spam_domain]
    new_spammable_emails = set(spammable_emails) - spammable_emails_found
    spammable_emails_found.update(new_spammable_emails)
    print("Found {} new spammable emails".format(len(new_spammable_emails)))
    return list(new_spammable_emails)


msg = """Dear Friend,

Some interesting information about you was found by the Email Cruncher. Do take a look.

Best,

The Email Cruncher
"""


@app.task
def spam(email):
    """Spam the end user!"""
    print("Spam got {}".format(email))
    email = "{}@{}".format(email, config.spam_domain)
    server = smtplib.SMTP(config.mx_record)
    print(server.ehlo())
    print(server.starttls())
    print(server.login(config.spam_sender, config.spam_password))
    print(server.sendmail(from_addr=config.spam_sender, to_addrs=[email], msg=msg))
    server.quit()


@app.task
def trigger_spammers(spammable_emails):
    """Invoke spam asynchronously and in parallel for all
    spammable emails."""
    print("Trigger spammers got {}".format(spammable_emails))
    for email in spammable_emails:
        spam.apply_async([email], routing_key=email, exchange='spammer_exchange')


def make_pipeline():
    """Create a pipeline by chaining together
    the various microservices"""
    pipeline = chain(
        crunch_regex.s(),
        find_new_spammable_emails.s(),
        trigger_spammers.s()
    )
    return pipeline


def prompt():
    pipeline = make_pipeline()
    filename = input("> ")

    while filename != "quit":
        pipeline(filename)
        filename = input("> ")

    print("Quitting")


if __name__ == "__main__":
    prompt()
