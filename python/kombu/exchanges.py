from kombu import Exchange

email_exchange = Exchange('emails', type='fanout')
spammer_exchange = Exchange('spammer', type='direct')
