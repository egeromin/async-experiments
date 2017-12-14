from kombu import Exchange

email_exchange = Exchange('emails', type='fanout')