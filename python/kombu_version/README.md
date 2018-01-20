# Kombu version of the Email Cruncher

To run:

1. Follow the instructions in ../README.md
1. Start each of the services:

```bash
python prompt.py
python email_cruncher_regex.py
python email_cruncher_slow.py
python status.py
python spammer.py --name randomuser  # start spammer for randomuser@spam_domain
```

It's best to run each of these in a separate terminal tab or, even better, in a separate terminal window.
