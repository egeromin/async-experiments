# Celery version of the Email Cruncher

To run:

1. Follow the instructions in ../README.md
1. Start each of the services except the spammer:

    ```bash
    python cruncher.py  # prompt
    celery -A cruncher -c 1 worker -Q crunch
    celery -A cruncher -c 1 worker -Q status
    celery -A cruncher -c 1 worker -Q trigger
    ```

3. Start the spammer for `randomuser@spam_domain`:

    ```bash
    export SPAMMER_QUEUE=randomuser
    celery -A cruncher -c 1 worker -Q $SPAMMER_QUEUE
    ```

It's best to run each of these in a separate terminal tab or, even better, in a separate terminal window.
