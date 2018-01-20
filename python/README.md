# Python Version of the Email Cruncher

To run:

1. Use python3, not python2
1. Install the requirements: `pip install -r requirements.txt`
1. Fill in `email_config.py.example` to use the spammer. In particular, you need to fill in:
    * `mx_record` - the address of your email provider's SMTP server
    * `spam_domain` - the domain of the email addresses you want to spam, for example `gmail.com`, if you want to spam gmail users.
1. Copy the email config to celery_version and kombu_version:
    * `cp email_config.py.example celery_version/email_config.py`
    * `cp email_config.py.example kombu_version/email_config.py`
1. Install docker and docker-compose
1. Start RabbitMQ by running `docker-compose up`

From now on the instructions differ for celery_version and kombu_version -- read the corresponding READMEs to continue.
